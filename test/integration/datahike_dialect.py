"""
SQLAlchemy dialect for Datahike PgWire.

Subclasses the PostgreSQL dialect with overrides for:
- Version detection (avoids complex pg_catalog.version() parsing)
- Table listing (uses information_schema instead of pg_class)
- Column introspection (uses information_schema instead of pg_attribute + format_type)
- Index/FK/PK introspection (returns empty — Datahike EAV doesn't have traditional indexes)

Usage:
    from datahike_dialect import DatahikeDialect
    from sqlalchemy import create_engine

    engine = create_engine("datahike://user@host:port/db", creator=...)
    # or register and use URL scheme:
    # engine = create_engine("postgresql+datahike://...")

For quick use without registration, pass the dialect class directly:
    from sqlalchemy.engine import create_engine
    from sqlalchemy.pool import StaticPool

    engine = create_engine(
        "postgresql+psycopg2://datahike:@127.0.0.1:15432/datahike",
        connect_args={"options": "-c client_encoding=UTF8"},
    )
    # Then monkey-patch or use the approach below
"""

from sqlalchemy.dialects.postgresql.psycopg2 import PGDialect_psycopg2
from sqlalchemy import types as sqltypes, text


# Map PostgreSQL type names (from information_schema.data_type) to SQLAlchemy types
_TYPE_MAP = {
    "bigint": sqltypes.BigInteger,
    "integer": sqltypes.Integer,
    "smallint": sqltypes.SmallInteger,
    "double precision": sqltypes.Float,
    "real": sqltypes.Float,
    "numeric": sqltypes.Numeric,
    "text": sqltypes.Text,
    "character varying": sqltypes.String,
    "varchar": sqltypes.String,
    "character": sqltypes.String,
    "boolean": sqltypes.Boolean,
    "timestamp": sqltypes.DateTime,
    "timestamp without time zone": sqltypes.DateTime,
    "timestamp with time zone": sqltypes.DateTime,
    "date": sqltypes.Date,
    "uuid": sqltypes.Uuid,
    "bytea": sqltypes.LargeBinary,
}


class DatahikeDialect(PGDialect_psycopg2):
    """SQLAlchemy dialect for Datahike's PostgreSQL wire protocol.

    Uses information_schema for reflection instead of complex pg_catalog
    queries that require PostgreSQL-specific functions.
    """

    name = "datahike"

    # Disable features Datahike doesn't support
    supports_sequences = False
    supports_native_enum = False
    supports_native_boolean = True
    supports_comments = False
    supports_default_values = False
    supports_default_metavalue = False

    def _get_server_version_info(self, connection):
        # Return a reasonable PG version that avoids both too-old and too-new code paths
        # PG 9.5 is what CockroachDB uses — well-tested middle ground
        return (9, 5, 0)

    def get_table_names(self, connection, schema=None, **kw):
        schema = schema or "public"
        result = connection.execute(
            text(
                "SELECT table_name FROM information_schema.tables "
                "WHERE table_schema = :schema AND table_type = 'BASE TABLE'"
            ),
            {"schema": schema},
        )
        return [row[0] for row in result]

    def get_view_names(self, connection, schema=None, **kw):
        return []

    def get_columns(self, connection, table_name, schema=None, **kw):
        schema = schema or "public"
        result = connection.execute(
            text(
                "SELECT column_name, data_type, is_nullable, column_default, ordinal_position "
                "FROM information_schema.columns "
                "WHERE table_schema = :schema AND table_name = :table "
                "ORDER BY ordinal_position"
            ),
            {"schema": schema, "table": table_name},
        )
        columns = []
        for row in result:
            col_name, data_type, nullable, default, pos = row
            # Skip the implicit db_id column
            if col_name == "db_id":
                continue
            sa_type = _TYPE_MAP.get(data_type, sqltypes.NullType)()
            columns.append(
                {
                    "name": col_name,
                    "type": sa_type,
                    "nullable": nullable == "YES",
                    "default": default,
                    "autoincrement": False,
                    "comment": None,
                }
            )
        return columns

    def get_pk_constraint(self, connection, table_name, schema=None, **kw):
        # Datahike EAV doesn't have traditional primary keys
        # Return db_id as the implicit PK
        return {"constrained_columns": ["db_id"], "name": None}

    def get_foreign_keys(self, connection, table_name, schema=None, **kw):
        return []

    def get_indexes(self, connection, table_name, schema=None, **kw):
        return []

    def get_unique_constraints(self, connection, table_name, schema=None, **kw):
        return []

    def get_check_constraints(self, connection, table_name, schema=None, **kw):
        return []

    def has_table(self, connection, table_name, schema=None, **kw):
        schema = schema or "public"
        result = connection.execute(
            text(
                "SELECT COUNT(*) FROM information_schema.tables "
                "WHERE table_schema = :schema AND table_name = :table"
            ),
            {"schema": schema, "table": table_name},
        )
        return result.scalar() > 0

    def get_schema_names(self, connection, **kw):
        return ["public"]

    def get_temp_table_names(self, connection, schema=None, **kw):
        return []

    def get_sequence_names(self, connection, schema=None, **kw):
        return []

    def get_table_comment(self, connection, table_name, schema=None, **kw):
        return {"text": None}

    def get_multi_columns(self, connection, schema=None, filter_names=None, **kw):
        """Batch column introspection — used by newer SQLAlchemy versions."""
        schema = schema or "public"
        tables = filter_names or self.get_table_names(connection, schema)
        result = {}
        for table_name in tables:
            cols = self.get_columns(connection, table_name, schema)
            result[(schema, table_name)] = cols
        return result


# Register the dialect so it can be used via URL scheme
from sqlalchemy.dialects import registry
registry.register("datahike", __name__, "DatahikeDialect")
registry.register("datahike.psycopg2", __name__, "DatahikeDialect")
