package datahike.pg;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.ArrayDeque;
import java.util.UUID;

/**
 * Decodes bound parameter values from the PostgreSQL wire Bind message.
 *
 * <p>PG's extended query protocol transmits each parameter with a length
 * prefix followed by raw bytes. Format is per-parameter: 0 = text, 1 =
 * binary. Text format is UTF-8 and we delegate interpretation to the
 * per-OID {@link #decodeText} switch (which still returns typed Java
 * objects so the Datalog executor doesn't have to re-parse). Binary
 * format is per-type (see {@link #decodeBinary}).
 *
 * <p>Binary formats are defined in {@code src/backend/utils/adt/*.c} —
 * e.g. {@code int4recv} just reads a 4-byte big-endian integer,
 * {@code timestamp_recv} reads a 64-bit microsecond count since the
 * 2000-01-01 epoch. This class implements the handful of {@code _recv}
 * formats we support. Unsupported OIDs in binary format raise
 * {@link PgWireServer.PgProtocolException} with SQLSTATE {@code 0A000}
 * ({@code feature_not_supported}) so clients get a clean failure rather
 * than silent corruption.
 */
public final class PgParamCodec {

    private PgParamCodec() {}

    // ========================================================================
    // Array OID dispatch — pairs an array OID (`_T`) with its scalar
    // element OID (`T`). Mirrors `element-oid->array-oid` in
    // datahike.pg.types so adding a scalar type only needs three rows
    // (here, in types.clj, in elem-kw->oid).
    // ========================================================================

    private static final java.util.Map<Integer, Integer> ARRAY_TO_ELEM;
    static {
        java.util.Map<Integer, Integer> m = new java.util.HashMap<>();
        m.put(PgWireServer.OID_BOOL_ARRAY,        PgWireServer.OID_BOOL);
        m.put(PgWireServer.OID_BYTEA_ARRAY,       PgWireServer.OID_BYTEA);
        m.put(PgWireServer.OID_NAME_ARRAY,        PgWireServer.OID_NAME);
        m.put(PgWireServer.OID_INT2_ARRAY,        PgWireServer.OID_INT2);
        m.put(PgWireServer.OID_INT4_ARRAY,        PgWireServer.OID_INT4);
        m.put(PgWireServer.OID_TEXT_ARRAY,        PgWireServer.OID_TEXT);
        m.put(PgWireServer.OID_INT8_ARRAY,        PgWireServer.OID_INT8);
        m.put(PgWireServer.OID_FLOAT4_ARRAY,      PgWireServer.OID_FLOAT4);
        m.put(PgWireServer.OID_FLOAT8_ARRAY,      PgWireServer.OID_FLOAT8);
        m.put(PgWireServer.OID_OID_ARRAY,         PgWireServer.OID_OID);
        m.put(PgWireServer.OID_VARCHAR_ARRAY,     PgWireServer.OID_VARCHAR);
        m.put(PgWireServer.OID_DATE_ARRAY,        PgWireServer.OID_DATE);
        m.put(PgWireServer.OID_TIME_ARRAY,        PgWireServer.OID_TIME);
        m.put(PgWireServer.OID_TIMESTAMP_ARRAY,   PgWireServer.OID_TIMESTAMP);
        m.put(PgWireServer.OID_TIMESTAMPTZ_ARRAY, PgWireServer.OID_TIMESTAMPTZ);
        m.put(PgWireServer.OID_NUMERIC_ARRAY,     PgWireServer.OID_NUMERIC);
        m.put(PgWireServer.OID_UUID_ARRAY,        PgWireServer.OID_UUID);
        m.put(PgWireServer.OID_JSON_ARRAY,        PgWireServer.OID_JSON);
        m.put(PgWireServer.OID_JSONB_ARRAY,       PgWireServer.OID_JSONB);
        ARRAY_TO_ELEM = java.util.Collections.unmodifiableMap(m);
    }

    /** True iff `oid` is one of the known `_T` array OIDs. */
    public static boolean isArrayOid(int oid) {
        return ARRAY_TO_ELEM.containsKey(oid);
    }

    // ========================================================================
    // Composite (named row type) field-OID registry. Populated from Clojure
    // (datahike.pg.composite/*) on CREATE TYPE and lazily at describe time so
    // the binary record codec knows each field's OID — PG's record wire format
    // is [int32 nfields][per field: int32 field-oid, int32 len(-1=NULL), bytes].
    // The canonical record_out text we already produce carries the VALUES but
    // not the per-field OIDs; this map supplies them.
    // ========================================================================

    private static final java.util.Map<Integer, int[]> COMPOSITE_FIELDS =
        new java.util.concurrent.ConcurrentHashMap<>();

    /** Register (or update) a composite type's ordered field OIDs. */
    public static void registerComposite(int oid, int[] fieldOids) {
        COMPOSITE_FIELDS.put(oid, fieldOids);
    }

    /** Ordered field OIDs for a registered composite, or null if unknown. */
    public static int[] compositeFields(int oid) {
        return COMPOSITE_FIELDS.get(oid);
    }

    // Per-value field-OID layout for ANONYMOUS records (record OID 2249), which
    // have no composite registry. value->string registers a PgRecord's field
    // OIDs keyed by its canonical record_out text (recursively for nested
    // records) right before emitting it; encodeBinary(2249, text) looks it up.
    // The canonical text uniquely determines the layout for a given ROW(...).
    private static final java.util.Map<String, int[]> RECORD_LAYOUTS =
        new java.util.concurrent.ConcurrentHashMap<>();

    /** Register an anonymous record's field OIDs, keyed by its record_out text. */
    public static void registerRecordLayout(String recordText, int[] fieldOids) {
        RECORD_LAYOUTS.put(recordText, fieldOids);
    }

    /**
     * Split a PG record_out text — `(f1,f2,...)` — into its field cells.
     * A bare-empty field (nothing between the delimiters) is SQL NULL
     * (returned as java null); a quoted field (even `""`) is a present value
     * with its surrounding quotes removed and `""`/`\x` unescaped. Nested
     * records/arrays arrive quoted and come back as their raw inner text
     * (`(42,42)`, `{9,NULL,11}`) ready to recurse through encodeBinary.
     * Returns null if the text isn't a parenthesised record.
     */
    static String[] parseRecordFields(String text) {
        if (text == null) return null;
        String s = text.trim();
        if (s.length() < 2 || s.charAt(0) != '(' || s.charAt(s.length() - 1) != ')') return null;
        s = s.substring(1, s.length() - 1);
        java.util.List<String> fields = new java.util.ArrayList<>();
        if (s.isEmpty()) return new String[0];
        StringBuilder cur = new StringBuilder();
        boolean inQuote = false, escape = false, quoted = false, hasContent = false;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (escape) { cur.append(c); escape = false; continue; }
            if (inQuote) {
                if (c == '\\') { escape = true; continue; }
                if (c == '"') {
                    if (i + 1 < s.length() && s.charAt(i + 1) == '"') { cur.append('"'); i++; continue; }
                    inQuote = false; continue;
                }
                cur.append(c); continue;
            }
            if (c == '"') { inQuote = true; quoted = true; hasContent = true; continue; }
            if (c == ',') {
                fields.add((quoted || hasContent) ? cur.toString() : null);
                cur.setLength(0); quoted = false; hasContent = false; continue;
            }
            cur.append(c); hasContent = true;
        }
        fields.add((quoted || hasContent) ? cur.toString() : null);
        return fields.toArray(new String[0]);
    }

    /**
     * Encode a record/composite to PG's binary wire format from its
     * record_out text and the ordered field OIDs. Each non-NULL field is
     * encoded by recursing into {@link #encodeBinary} (so nested records,
     * arrays and scalars all work). Returns null on any field-count mismatch
     * or unsupported field type — the caller then falls back to text.
     */
    static byte[] encodeRecordBinary(int[] fieldOids, String text) {
        String[] cells = parseRecordFields(text);
        if (cells == null || cells.length != fieldOids.length) return null;
        byte[][] fb = new byte[cells.length][];
        for (int i = 0; i < cells.length; i++) {
            if (cells[i] == null) { fb[i] = null; continue; }
            byte[] enc = encodeBinary(fieldOids[i], cells[i]);
            if (enc == null) return null;   // unsupported field type
            fb[i] = enc;
        }
        int total = 4;
        for (byte[] b : fb) total += 4 + 4 + (b == null ? 0 : b.length);
        ByteBuffer buf = ByteBuffer.allocate(total).order(ByteOrder.BIG_ENDIAN);
        buf.putInt(fieldOids.length);
        for (int i = 0; i < fb.length; i++) {
            buf.putInt(fieldOids[i]);
            if (fb[i] == null) buf.putInt(-1);
            else { buf.putInt(fb[i].length); buf.put(fb[i]); }
        }
        return buf.array();
    }

    /** Returns the scalar T element OID for `_T`, or -1 if not an array OID. */
    public static int elementOidOf(int arrayOid) {
        Integer e = ARRAY_TO_ELEM.get(arrayOid);
        return e == null ? -1 : e.intValue();
    }

    /** Decode a PG hex string (the part after `\x`) to raw bytes. */
    static byte[] hexToBytes(String hex) {
        int n = hex.length() / 2;
        byte[] out = new byte[n];
        for (int i = 0; i < n; i++) {
            out[i] = (byte) Integer.parseInt(hex.substring(2 * i, 2 * i + 2), 16);
        }
        return out;
    }

    // ------------------------------------------------------------------------
    // Canonical-text array parser
    //
    // Walks `{...}` text in PG's array_out format into per-dim sizes plus
    // a flat (row-major) leaf list of token strings. Each leaf is paired
    // with a `quoted` flag so `"NULL"` (literal string) and unquoted NULL
    // (SQL NULL marker) can be distinguished. Used by encodeArrayBinary
    // to rebuild PG's wire format. Lbound prefix `[lo:hi]…=` is consumed
    // and applied to the result; default lbound is 1 per dim.
    // ------------------------------------------------------------------------

    private static final class ParsedArray {
        final int[] dims;
        final int[] lbounds;
        final String[] leafTokens;     // null entries denote SQL NULL
        ParsedArray(int[] dims, int[] lbounds, String[] leafTokens) {
            this.dims = dims;
            this.lbounds = lbounds;
            this.leafTokens = leafTokens;
        }
    }

    private static ParsedArray parseArrayText(String text) {
        String s = text.trim();
        // Optional `[lo:hi][lo:hi]…=` lbound prefix.
        java.util.List<Integer> lbounds = new java.util.ArrayList<>();
        while (s.startsWith("[")) {
            int eq = s.indexOf('=');
            int closeBracket = s.indexOf(']');
            if (closeBracket < 0 || (eq >= 0 && closeBracket > eq)) break;
            int colon = s.indexOf(':');
            if (colon < 0 || colon > closeBracket) break;
            int lo = Integer.parseInt(s.substring(1, colon).trim());
            lbounds.add(lo);
            s = s.substring(closeBracket + 1);
            if (s.startsWith("=")) {
                s = s.substring(1);
                break;
            }
        }
        s = s.trim();

        // Parse the body. We track depth and per-level element counts to
        // derive dims; leaves accumulate in row-major order.
        java.util.List<String> leaves = new java.util.ArrayList<>();
        // Size of each depth, recorded ONCE on that depth's first close
        // (PG arrays are rectangular). Keyed by depth so depth 1 = outermost
        // sorts first — fixes the prior logic that double-recorded the inner
        // dim and dropped the outer (2-D `{{1,2},{4,5},{6,7}}` → [2,2] not [3,2]).
        java.util.TreeMap<Integer, Integer> dimByDepth = new java.util.TreeMap<>();
        StringBuilder cur = new StringBuilder();
        boolean inQuote = false, escape = false, hasContent = false;
        int depth = 0;
        // Track count of children at each depth level when we open it.
        ArrayDeque<Integer> levelCounts = new ArrayDeque<>();

        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (escape) {
                cur.append(c); escape = false; hasContent = true;
                continue;
            }
            if (inQuote) {
                if (c == '\\') { escape = true; continue; }
                if (c == '"')  { inQuote = false; hasContent = true; continue; }
                cur.append(c); hasContent = true;
                continue;
            }
            if (c == '"') {
                // Quoted token: don't strip surrounding quotes — caller's
                // coerce handles them via the `quoted` semantics. We pass
                // the raw string through, knowing it was once quoted.
                inQuote = true; hasContent = true;
                continue;
            }
            if (c == '{') {
                depth++;
                levelCounts.push(0);
                cur.setLength(0);
                hasContent = false;
                continue;
            }
            if (c == '}') {
                if (hasContent) {
                    String tok = cur.toString();
                    leaves.add(tok.equalsIgnoreCase("NULL") ? null : tok);
                    levelCounts.push(levelCounts.pop() + 1);
                    cur.setLength(0);
                    hasContent = false;
                }
                int closedCount = levelCounts.pop();
                // Record each depth's size once, on its first close.
                dimByDepth.putIfAbsent(depth, closedCount);
                if (!levelCounts.isEmpty()) {
                    levelCounts.push(levelCounts.pop() + 1);
                }
                depth--;
                continue;
            }
            if (c == ',') {
                if (hasContent) {
                    String tok = cur.toString();
                    leaves.add(tok.equalsIgnoreCase("NULL") ? null : tok);
                    levelCounts.push(levelCounts.pop() + 1);
                    cur.setLength(0);
                    hasContent = false;
                }
                continue;
            }
            cur.append(c);
            if (!Character.isWhitespace(c)) hasContent = true;
        }

        int[] dims = new int[dimByDepth.size()];
        int di = 0;
        for (int v : dimByDepth.values()) dims[di++] = v;  // ascending depth = outer→inner
        if (dims.length == 0) {
            dims = new int[]{0};
        }
        int[] lbs = new int[dims.length];
        for (int i = 0; i < lbs.length; i++) {
            lbs[i] = i < lbounds.size() ? lbounds.get(i) : 1;
        }
        String[] leafArr = leaves.toArray(new String[0]);
        return new ParsedArray(dims, lbs, leafArr);
    }

    // ------------------------------------------------------------------------
    // Binary array encode — wire format per `arrayfuncs.c:array_send`:
    //   int32 ndim
    //   int32 hasnull (0/1)
    //   int32 element OID
    //   ndim × (int32 dim, int32 lbound)
    //   nitems × (int32 size  | -1 for NULL,  bytes)
    // ------------------------------------------------------------------------

    public static byte[] encodeArrayBinary(int arrayOid, String text) {
        int elemOid = elementOidOf(arrayOid);
        if (elemOid < 0) return null;
        if (text == null) return null;
        ParsedArray pa;
        try {
            pa = parseArrayText(text);
        } catch (Exception e) {
            return null;
        }
        int ndim = pa.dims.length;
        int nitems = 1;
        for (int d : pa.dims) nitems *= d;
        // Edge case: PG sends ndim=0 for `{}` (empty 1-D array). Match that.
        if (nitems == 0) {
            ndim = 0;
        }

        // Encode each leaf via the scalar codec.
        byte[][] leafBytes = new byte[pa.leafTokens.length][];
        boolean hasNull = false;
        for (int i = 0; i < pa.leafTokens.length; i++) {
            String tok = pa.leafTokens[i];
            if (tok == null) {
                leafBytes[i] = null;
                hasNull = true;
            } else {
                // Strip outer quotes if the token was quoted in text form.
                // (Our parser preserves them when extracted; for binary
                // encoding we want the raw value.)
                String val = tok;
                Object parsed = parseScalarToken(elemOid, val);
                byte[] enc = encodeBinary(elemOid, parsed);
                if (enc == null) return null;  // unsupported element type
                leafBytes[i] = enc;
            }
        }

        int totalBytes = 4 + 4 + 4 + 8 * ndim;
        for (byte[] b : leafBytes) totalBytes += 4 + (b == null ? 0 : b.length);

        ByteBuffer buf = ByteBuffer.allocate(totalBytes).order(ByteOrder.BIG_ENDIAN);
        buf.putInt(ndim);
        buf.putInt(hasNull ? 1 : 0);
        buf.putInt(elemOid);
        for (int d = 0; d < ndim; d++) {
            buf.putInt(pa.dims[d]);
            buf.putInt(pa.lbounds[d]);
        }
        for (byte[] b : leafBytes) {
            if (b == null) {
                buf.putInt(-1);
            } else {
                buf.putInt(b.length);
                buf.put(b);
            }
        }
        return buf.array();
    }

    /**
     * Coerce the raw text token from the canonical array form to a typed
     * Java value matching the element OID. Mirrors the scalar text-format
     * parsers (decodeText switch) — used by encodeArrayBinary so each
     * leaf can be passed to encodeBinary as the right runtime type.
     */
    private static Object parseScalarToken(int elemOid, String tok) {
        try {
            return switch (elemOid) {
                case PgWireServer.OID_BOOL -> {
                    String t = tok.toLowerCase();
                    yield t.equals("t") || t.equals("true") || t.equals("1")
                       || t.equals("yes") || t.equals("y") || t.equals("on");
                }
                case PgWireServer.OID_INT2,
                     PgWireServer.OID_INT4,
                     PgWireServer.OID_INT8,
                     PgWireServer.OID_OID -> Long.parseLong(tok.trim());
                case PgWireServer.OID_FLOAT4,
                     PgWireServer.OID_FLOAT8 -> Double.parseDouble(tok.trim());
                case PgWireServer.OID_NUMERIC -> new BigDecimal(tok.trim());
                case PgWireServer.OID_UUID -> UUID.fromString(tok.trim());
                case PgWireServer.OID_DATE,
                     PgWireServer.OID_TIME,
                     PgWireServer.OID_TIMESTAMP,
                     PgWireServer.OID_TIMESTAMPTZ -> tok;   // delegate to encodeBinary's parser
                default -> tok;                              // text/varchar/etc.
            };
        } catch (Exception e) {
            return tok;
        }
    }

    // ------------------------------------------------------------------------
    // Binary array decode — inverse. Returns canonical PG text form
    // (`{...}` or `[lo:hi]={...}`) so downstream Clojure code routes
    // through the existing from-pg-text path.
    // ------------------------------------------------------------------------

    public static String decodeArrayBinary(int arrayOid, byte[] bytes) {
        int elemOid = elementOidOf(arrayOid);
        if (elemOid < 0) return null;

        ByteBuffer buf = ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN);
        int ndim    = buf.getInt();
        int hasNull = buf.getInt();
        int sentOid = buf.getInt();
        if (sentOid != elemOid && sentOid != 0) {
            // PG allows OID=0 meaning "use my type"; otherwise should match.
            // We trust the array OID we were called with.
        }
        if (ndim == 0) return "{}";

        int[] dims = new int[ndim];
        int[] lbs  = new int[ndim];
        int   nitems = 1;
        for (int i = 0; i < ndim; i++) {
            dims[i] = buf.getInt();
            lbs[i]  = buf.getInt();
            nitems *= dims[i];
        }

        // Decode each leaf to its text form.
        String[] leaves = new String[nitems];
        for (int i = 0; i < nitems; i++) {
            int sz = buf.getInt();
            if (sz < 0) {
                leaves[i] = null;
            } else {
                byte[] b = new byte[sz];
                buf.get(b);
                Object decoded = decodeBinary(elemOid, b);
                leaves[i] = decoded == null ? "NULL" : leafToText(decoded);
            }
        }

        // Build the canonical text form.
        StringBuilder out = new StringBuilder();
        // Lbound prefix only when any lbound != 1.
        boolean nonDefault = false;
        for (int lb : lbs) if (lb != 1) { nonDefault = true; break; }
        if (nonDefault) {
            for (int i = 0; i < ndim; i++) {
                out.append('[').append(lbs[i]).append(':')
                   .append(lbs[i] + dims[i] - 1).append(']');
            }
            out.append('=');
        }
        emitArrayLevel(out, leaves, dims, 0, 0);
        return out.toString();
    }

    /** Recursive emitter that walks the row-major leaves array level by level. */
    private static int emitArrayLevel(StringBuilder out, String[] leaves,
                                      int[] dims, int dim, int offset) {
        out.append('{');
        int sub = 1;
        for (int i = dim + 1; i < dims.length; i++) sub *= dims[i];
        for (int i = 0; i < dims[dim]; i++) {
            if (i > 0) out.append(',');
            if (dim == dims.length - 1) {
                String s = leaves[offset++];
                if (s == null) out.append("NULL");
                else out.append(quoteIfNeeded(s));
            } else {
                offset = emitArrayLevel(out, leaves, dims, dim + 1, offset);
            }
        }
        out.append('}');
        return offset;
    }

    private static String quoteIfNeeded(String s) {
        if (s.isEmpty() || s.equalsIgnoreCase("NULL")) return "\"" + s + "\"";
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == ',' || c == '"' || c == '\\' || c == '{' || c == '}'
                || Character.isWhitespace(c)) {
                StringBuilder b = new StringBuilder("\"");
                for (int j = 0; j < s.length(); j++) {
                    char cc = s.charAt(j);
                    if (cc == '"' || cc == '\\') b.append('\\');
                    b.append(cc);
                }
                return b.append('"').toString();
            }
        }
        return s;
    }

    private static String leafToText(Object v) {
        if (v == null) return "NULL";
        if (v instanceof Boolean b) return b ? "t" : "f";
        return v.toString();
    }

    /** Days between the Unix epoch (1970-01-01) and the PG epoch (2000-01-01). */
    private static final long PG_EPOCH_DAYS = 10957L;
    /** Microseconds between the Unix epoch and the PG epoch. */
    private static final long PG_EPOCH_MICROS = PG_EPOCH_DAYS * 86_400_000_000L;

    /**
     * Decode a parameter value given its type OID, wire format, and raw
     * bytes. Returns a typed Java object suitable for direct use as a
     * Datalog `:in-args` value.
     *
     * <p>Format codes: 0 = text, 1 = binary (from the Bind message's
     * parameter format code array).
     */
    public static Object decode(int oid, short format, byte[] bytes) {
        if (format == 0) {
            return decodeText(oid, bytes);
        } else if (format == 1) {
            return decodeBinary(oid, bytes);
        } else {
            throw new PgWireServer.PgProtocolException("0A000",
                "unknown parameter format code: " + format);
        }
    }

    // ========================================================================
    // Text format — values arrive as UTF-8 strings, converted per OID to the
    // matching Clojure/Java type. We don't rely on the downstream translator
    // to re-parse, so bindings carry typed values identical to binary.
    // ========================================================================

    public static Object decodeText(int oid, byte[] bytes) {
        String s = new String(bytes, StandardCharsets.UTF_8);
        return switch (oid) {
            case PgWireServer.OID_BOOL ->
                "t".equalsIgnoreCase(s) || "true".equalsIgnoreCase(s) || "1".equals(s);
            // Integer types all land in Java Long — Datahike's schema coerces to
            // int/long/short as needed, and we want consistent Clojure value types.
            case PgWireServer.OID_INT2,
                 PgWireServer.OID_INT4,
                 PgWireServer.OID_INT8,
                 PgWireServer.OID_OID ->
                Long.parseLong(s);
            case PgWireServer.OID_FLOAT4,
                 PgWireServer.OID_FLOAT8 ->
                Double.parseDouble(s);
            case PgWireServer.OID_UUID ->
                UUID.fromString(s);
            // Date/time come in as ISO-8601 or PG's text forms; the downstream
            // INSERT value-coercion in sql.clj handles the variants (it already
            // did for the text-interpolated path). Leave as String here.
            case PgWireServer.OID_DATE,
                 PgWireServer.OID_TIMESTAMP,
                 PgWireServer.OID_TIMESTAMPTZ,
                 PgWireServer.OID_TIME ->
                s;
            // jsonb / json — pass through as text; jsonb.clj parses on demand.
            case PgWireServer.OID_JSONB,
                 PgWireServer.OID_JSON ->
                s;
            // bytea in text form uses `\x…` hex or legacy octal escapes. The
            // existing parse-bytea-hex in sql.clj handles both; leave as string.
            case PgWireServer.OID_BYTEA ->
                s;
            // Anything else (text, varchar, name, user types) → String as-is.
            default -> s;
        };
    }

    // ========================================================================
    // Binary encode (send-side). Mirrors {@link #decodeBinary} — same
    // wire format, inverse direction. Used when the client requests binary
    // result columns via Bind's result-format array and we know how to
    // encode the column's OID. Fallback is to send text.
    // ========================================================================

    private static boolean parseBool(Object v) {
        if (v instanceof Boolean b) return b;
        String s = v.toString();
        return "t".equalsIgnoreCase(s) || "true".equalsIgnoreCase(s) || "1".equals(s);
    }

    private static long parseLong(Object v) {
        if (v instanceof Number n) return n.longValue();
        return Long.parseLong(v.toString());
    }

    private static double parseDouble(Object v) {
        if (v instanceof Number n) return n.doubleValue();
        return Double.parseDouble(v.toString());
    }

    /**
     * Encode a Java/Clojure value as PG binary for the given OID, or
     * return null when binary is not supported for this type. Caller
     * then falls back to text. Never throws — coercion failures fall
     * back too. The handler stringifies values before sendDataRow
     * sees them, so most cases parse the text representation on the
     * fly to the target type.
     */
    /**
     * Whether {@link #encodeBinary} has an implementation for this OID.
     * The wire layer uses this at RowDescription time to downgrade any
     * binary result-format request to text for OIDs we can't encode,
     * keeping the advertised format consistent with the bytes we send.
     */
    public static boolean supportsBinaryEncode(int oid) {
        return oid == PgWireServer.OID_BOOL
            || oid == PgWireServer.OID_INT2
            || oid == PgWireServer.OID_INT4
            || oid == PgWireServer.OID_OID
            || oid == PgWireServer.OID_INT8
            || oid == PgWireServer.OID_FLOAT4
            || oid == PgWireServer.OID_FLOAT8
            || oid == PgWireServer.OID_NUMERIC
            || oid == PgWireServer.OID_UUID
            || oid == PgWireServer.OID_DATE
            || oid == PgWireServer.OID_TIMESTAMP
            || oid == PgWireServer.OID_TIMESTAMPTZ
            || oid == PgWireServer.OID_TIME
            || oid == PgWireServer.OID_TEXT
            || oid == PgWireServer.OID_VARCHAR
            || oid == PgWireServer.OID_NAME
            || oid == PgWireServer.OID_JSON
            || oid == PgWireServer.OID_BYTEA
            || oid == PgWireServer.OID_JSONB;
    }

    // ========================================================================
    // NUMERIC binary (numeric_send). Format:
    //   int16 ndigits
    //   int16 weight  (base-10000 position of the first digit, relative
    //                  to the implied decimal point — 0 means first
    //                  digit is the units place; -1 means first digit
    //                  is the first fractional group; positive means
    //                  whole-number magnitude)
    //   int16 sign    (0x0000 positive, 0x4000 negative, 0xC000 NaN)
    //   int16 dscale  (display scale = digits after the decimal point)
    //   int16[] digits  (each in 0..9999, base-10000)
    //
    // Ported from pgjdbc's ByteConverter.numeric(BigDecimal) so our
    // encoding round-trips through its decoder byte-for-byte (e.g. we
    // preserve the trailing-zero scale of "9.90E-28" — the distinction
    // testgetBigDecimal asserts on).
    // ========================================================================

    private static final short NUMERIC_POS = 0x0000;
    private static final short NUMERIC_NEG = 0x4000;
    private static final BigInteger BI_TEN_THOUSAND = BigInteger.valueOf(10000);
    private static final BigInteger BI_MAX_LONG = BigInteger.valueOf(Long.MAX_VALUE);

    private static BigInteger tenPow(int n) {
        return BigInteger.TEN.pow(n);
    }

    private static byte[] encodeNumeric(BigDecimal nbr) {
        int scale = nbr.scale();
        BigInteger unscaled = nbr.unscaledValue().abs();

        // PG zero: ndigits=0, weight=-1, sign=+, dscale=max(0,scale).
        if (unscaled.signum() == 0) {
            byte[] out = new byte[8];
            ByteBuffer.wrap(out).order(ByteOrder.BIG_ENDIAN)
                .putShort((short) 0)
                .putShort((short) -1)
                .putShort(NUMERIC_POS)
                .putShort((short) Math.max(0, scale));
            return out;
        }

        ArrayDeque<Short> shorts = new ArrayDeque<>();
        int weight = -1;

        if (scale <= 0) {
            // Integer (possibly with negative scale = trailing zeros).
            if (scale < 0) {
                int abs = -scale;
                weight += abs / 4;
                int mod = abs % 4;
                unscaled = unscaled.multiply(tenPow(mod));
                scale = 0;
            }
            while (unscaled.compareTo(BI_MAX_LONG) > 0) {
                BigInteger[] qr = unscaled.divideAndRemainder(BI_TEN_THOUSAND);
                unscaled = qr[0];
                short s = qr[1].shortValue();
                if (s != 0 || !shorts.isEmpty()) shorts.push(s);
                ++weight;
            }
            long v = unscaled.longValueExact();
            do {
                short s = (short) (v % 10000);
                if (s != 0 || !shorts.isEmpty()) shorts.push(s);
                v /= 10000;
                ++weight;
            } while (v != 0);
        } else {
            // Value has fractional digits.
            BigInteger[] split = unscaled.divideAndRemainder(tenPow(scale));
            BigInteger wholes = split[0];
            BigInteger decimal = split[1];
            weight = -1;
            if (decimal.signum() != 0) {
                int mod = scale % 4;
                int segments = scale / 4;
                if (mod != 0) {
                    decimal = decimal.multiply(tenPow(4 - mod));
                    ++segments;
                }
                do {
                    BigInteger[] qr = decimal.divideAndRemainder(BI_TEN_THOUSAND);
                    decimal = qr[0];
                    short s = qr[1].shortValue();
                    if (s != 0 || !shorts.isEmpty()) shorts.push(s);
                    --segments;
                } while (decimal.signum() != 0);
                // Leading-zero segments in the fractional part: if
                // there are no wholes, collapse into weight; otherwise
                // pad the digit stack so alignment holds.
                if (wholes.signum() == 0) {
                    weight -= segments;
                } else {
                    for (int i = 0; i < segments; i++) shorts.push((short) 0);
                }
            }
            while (wholes.signum() != 0) {
                ++weight;
                BigInteger[] qr = wholes.divideAndRemainder(BI_TEN_THOUSAND);
                wholes = qr[0];
                short s = qr[1].shortValue();
                if (s != 0 || !shorts.isEmpty()) shorts.push(s);
            }
        }

        byte[] out = new byte[8 + 2 * shorts.size()];
        ByteBuffer buf = ByteBuffer.wrap(out).order(ByteOrder.BIG_ENDIAN);
        buf.putShort((short) shorts.size());
        buf.putShort((short) weight);
        buf.putShort(nbr.signum() < 0 ? NUMERIC_NEG : NUMERIC_POS);
        buf.putShort((short) Math.max(0, scale));
        for (short s : shorts) buf.putShort(s);
        return out;
    }

    /** PG numeric NaN sign word. */
    private static final int NUMERIC_NAN = 0xC000;

    /**
     * Decode PG numeric_recv (the inverse of {@link #encodeNumeric}) to a
     * BigDecimal. asyncpg sends numeric parameters in this binary form, so
     * we must decode it (the text path alone left OID 1700 unsupported).
     *   int16 ndigits, int16 weight, int16 sign, int16 dscale,
     *   then ndigits base-10000 digits (each 0..9999).
     */
    private static BigDecimal decodeNumeric(ByteBuffer buf) {
        int ndigits = buf.getShort() & 0xFFFF;
        int weight  = buf.getShort();           // signed base-10000 position
        int sign    = buf.getShort() & 0xFFFF;
        int dscale  = buf.getShort() & 0xFFFF;
        if (sign == NUMERIC_NAN) {
            // BigDecimal has no NaN; surface clearly instead of corrupting.
            throw new PgWireServer.PgProtocolException("0A000",
                "NUMERIC 'NaN' is not supported as a binary parameter");
        }
        BigInteger unscaled = BigInteger.ZERO;
        for (int i = 0; i < ndigits; i++) {
            int d = buf.getShort() & 0xFFFF;    // base-10000 digit
            unscaled = unscaled.multiply(BI_TEN_THOUSAND).add(BigInteger.valueOf(d));
        }
        // The least-significant digit sits at base-10000 position
        // (weight - ndigits + 1); shift the concatenated integer there.
        int exp = weight - ndigits + 1;
        BigDecimal value = (exp >= 0)
            ? new BigDecimal(unscaled.multiply(BI_TEN_THOUSAND.pow(exp)))
            : new BigDecimal(unscaled, -exp * 4);   // 10000 = 10^4
        if ((sign & 0xFFFF) == (NUMERIC_NEG & 0xFFFF)) value = value.negate();
        // Honour PG's display scale (so 1.50 keeps two fractional digits).
        if (dscale != value.scale()) {
            value = value.setScale(dscale, java.math.RoundingMode.HALF_UP);
        }
        return value;
    }

    public static byte[] encodeBinary(int oid, Object value) {
        if (value == null) return null;
        try {
            return switch (oid) {
                case PgWireServer.OID_BOOL ->
                    new byte[] { (byte) (parseBool(value) ? 1 : 0) };

                // PG "char" (OID 18): a single byte (charsend / pq_sendbyte).
                // pg_type.typtype/typcategory are this type; asyncpg
                // binary-decodes it to bytes for its is_scalar_type check.
                case PgWireServer.OID_CHAR -> {
                    String s = value.toString();
                    yield s.isEmpty() ? new byte[0] : new byte[] { (byte) s.charAt(0) };
                }

                case PgWireServer.OID_INT2 ->
                    ByteBuffer.allocate(2).order(ByteOrder.BIG_ENDIAN)
                        .putShort((short) parseLong(value)).array();
                case PgWireServer.OID_INT4,
                     PgWireServer.OID_OID ->
                    ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN)
                        .putInt((int) parseLong(value)).array();
                case PgWireServer.OID_INT8 ->
                    ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN)
                        .putLong(parseLong(value)).array();

                case PgWireServer.OID_FLOAT4 ->
                    ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN)
                        .putInt(Float.floatToRawIntBits((float) parseDouble(value))).array();
                case PgWireServer.OID_FLOAT8 ->
                    ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN)
                        .putLong(Double.doubleToRawLongBits(parseDouble(value))).array();

                case PgWireServer.OID_NUMERIC -> {
                    BigDecimal bd;
                    if (value instanceof BigDecimal b) bd = b;
                    else if (value instanceof BigInteger bi) bd = new BigDecimal(bi);
                    else if (value instanceof Number n) bd = new BigDecimal(n.toString());
                    else bd = new BigDecimal(value.toString());
                    yield encodeNumeric(bd);
                }

                case PgWireServer.OID_UUID -> {
                    UUID u = (value instanceof UUID uu) ? uu : UUID.fromString(value.toString());
                    yield ByteBuffer.allocate(16).order(ByteOrder.BIG_ENDIAN)
                              .putLong(u.getMostSignificantBits())
                              .putLong(u.getLeastSignificantBits())
                              .array();
                }

                case PgWireServer.OID_DATE -> {
                    LocalDate d = (value instanceof LocalDate ld) ? ld
                                 : (value instanceof java.util.Date jd)
                                     ? jd.toInstant().atZone(ZoneOffset.UTC).toLocalDate()
                                 : LocalDate.parse(value.toString());
                    int days = (int) (d.toEpochDay() - PG_EPOCH_DAYS);
                    yield ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(days).array();
                }

                case PgWireServer.OID_TIMESTAMP,
                     PgWireServer.OID_TIMESTAMPTZ -> {
                    // value->string produces "2024-01-15 10:30:00" (no 'T', no 'Z').
                    Instant inst;
                    if (value instanceof Instant i) {
                        inst = i;
                    } else if (value instanceof java.util.Date jd) {
                        inst = jd.toInstant();
                    } else {
                        String s = value.toString().trim();
                        // Normalize "YYYY-MM-DD hh:mm:ss[.fff]" → ISO form for Instant.parse.
                        if (!s.endsWith("Z") && !s.contains("T")) {
                            s = s.replace(' ', 'T') + "Z";
                        }
                        inst = Instant.parse(s);
                    }
                    long unixMicros = inst.getEpochSecond() * 1_000_000L + inst.getNano() / 1_000L;
                    long pgMicros   = unixMicros - PG_EPOCH_MICROS;
                    yield ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).putLong(pgMicros).array();
                }

                case PgWireServer.OID_TIME -> {
                    LocalTime t = (value instanceof LocalTime lt) ? lt
                                 : LocalTime.parse(value.toString());
                    long micros = t.toNanoOfDay() / 1_000L;
                    yield ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).putLong(micros).array();
                }

                case PgWireServer.OID_TEXT,
                     PgWireServer.OID_VARCHAR,
                     PgWireServer.OID_NAME,
                     PgWireServer.OID_JSON ->
                    value.toString().getBytes(StandardCharsets.UTF_8);

                case PgWireServer.OID_BYTEA -> {
                    if (value instanceof byte[] b) yield b;
                    String s = value.toString();
                    // value->string renders bytea as PG hex text `\x<hex>`;
                    // decode it back to raw bytes. (Plain strings fall through
                    // as their UTF-8 bytes.)
                    yield (s.startsWith("\\x")) ? hexToBytes(s.substring(2))
                                                : s.getBytes(StandardCharsets.UTF_8);
                }

                case PgWireServer.OID_JSONB -> {
                    byte[] text = value.toString().getBytes(StandardCharsets.UTF_8);
                    byte[] out  = new byte[text.length + 1];
                    out[0] = 1;  // jsonb binary format version
                    System.arraycopy(text, 0, out, 1, text.length);
                    yield out;
                }

                default -> {
                    // Array OIDs: route through the array codec which
                    // parses the canonical text form and encodes each
                    // leaf via encodeBinary(elemOid, leaf).
                    if (isArrayOid(oid)) {
                        yield encodeArrayBinary(oid, value.toString());
                    }
                    // Composite (named row type) or anonymous record (2249):
                    // encode the record_out text as a binary record. Field OIDs
                    // come from the composite registry, or for an anonymous
                    // record from the per-value layout registered by value->string.
                    int[] cf = COMPOSITE_FIELDS.get(oid);
                    if (cf == null) cf = RECORD_LAYOUTS.get(value.toString());
                    if (cf != null) {
                        yield encodeRecordBinary(cf, value.toString());
                    }
                    yield null;   // caller falls back to text encoding
                }
            };
        } catch (Exception e) {
            return null;  // fall back to text encoding
        }
    }

    /** Quote a record field cell for record_out text (see records.clj). */
    private static String quoteRecordCell(String raw) {
        if (raw.isEmpty() || raw.matches("(?s).*[(),\"\\\\\\s].*"))
            return "\"" + raw.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
        return raw;
    }

    /**
     * Decode a PG binary record — [int32 nfields][per field: int32 oid,
     * int32 len(-1=NULL), bytes] — to its canonical record_out text, decoding
     * each field via decodeBinary by the inline field OID (so composite and
     * anonymous records both work without a registry). The text then flows
     * through the normal value path and re-encodes via encodeRecordBinary.
     */
    static String decodeRecordBinary(byte[] bytes) {
        ByteBuffer buf = ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN);
        int n = buf.getInt();
        StringBuilder sb = new StringBuilder("(");
        for (int i = 0; i < n; i++) {
            if (i > 0) sb.append(',');
            int foid = buf.getInt();
            int len = buf.getInt();
            if (len < 0) continue;            // NULL → empty cell
            byte[] fb = new byte[len];
            buf.get(fb);
            Object v = decodeBinary(foid, fb);
            if (v == null) continue;
            String t = (v instanceof Boolean b) ? (b ? "t" : "f") : v.toString();
            sb.append(quoteRecordCell(t));
        }
        sb.append(')');
        return sb.toString();
    }

    // ========================================================================
    // Binary format — PG's typreceive wire encoding. See pg_type.dat plus the
    // *_recv functions in src/backend/utils/adt/*.c for the canonical spec.
    // ========================================================================

    public static Object decodeBinary(int oid, byte[] bytes) {
        ByteBuffer buf = ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN);
        return switch (oid) {
            case PgWireServer.OID_BOOL ->
                bytes.length > 0 && bytes[0] != 0;

            // int2recv / int4recv / int8recv / oidrecv — big-endian integers.
            // Widen all to Long so Datahike/Clojure see one consistent type.
            case PgWireServer.OID_INT2 ->
                (long) buf.getShort();
            case PgWireServer.OID_INT4,
                 PgWireServer.OID_OID ->
                (long) buf.getInt();
            case PgWireServer.OID_INT8 ->
                buf.getLong();

            // float4recv / float8recv — IEEE 754 bits in big-endian.
            case PgWireServer.OID_FLOAT4 ->
                (double) Float.intBitsToFloat(buf.getInt());
            case PgWireServer.OID_FLOAT8 ->
                Double.longBitsToDouble(buf.getLong());

            // numeric_recv — base-10000 digit vector (see decodeNumeric).
            case PgWireServer.OID_NUMERIC ->
                decodeNumeric(buf);

            // uuid_recv — 16 raw bytes (big-endian msb/lsb long pair).
            case PgWireServer.OID_UUID ->
                new UUID(buf.getLong(), buf.getLong());

            // date_recv — int32 days since 2000-01-01.
            case PgWireServer.OID_DATE ->
                LocalDate.ofEpochDay(PG_EPOCH_DAYS + buf.getInt());

            // timestamp_recv / timestamptz_recv — int64 microseconds since
            // 2000-01-01 00:00:00 UTC. Return java.util.Date to match
            // existing conventions elsewhere in sql.clj / value->string.
            case PgWireServer.OID_TIMESTAMP,
                 PgWireServer.OID_TIMESTAMPTZ -> {
                long pgMicros = buf.getLong();
                long unixMicros = pgMicros + PG_EPOCH_MICROS;
                long millis = Math.floorDiv(unixMicros, 1000L);
                int  nanos  = (int) Math.floorMod(unixMicros, 1000L) * 1000;
                yield java.util.Date.from(Instant.ofEpochSecond(
                    Math.floorDiv(millis, 1000L),
                    (millis % 1000L) * 1_000_000L + nanos));
            }

            // time_recv — int64 microseconds since midnight.
            case PgWireServer.OID_TIME ->
                LocalTime.ofNanoOfDay(buf.getLong() * 1000L);

            // text/varchar/name — bytes are already UTF-8.
            case PgWireServer.OID_TEXT,
                 PgWireServer.OID_VARCHAR,
                 PgWireServer.OID_NAME ->
                new String(bytes, StandardCharsets.UTF_8);

            // bytea — already raw; pass through.
            case PgWireServer.OID_BYTEA ->
                bytes;

            // jsonb_recv — first byte is the version (currently 1), remainder
            // is UTF-8 JSON. We decode to a string and let jsonb.clj parse.
            case PgWireServer.OID_JSONB -> {
                if (bytes.length < 1 || bytes[0] != 1) {
                    throw new PgWireServer.PgProtocolException("0A000",
                        "unsupported jsonb binary version: "
                        + (bytes.length == 0 ? "empty" : bytes[0]));
                }
                yield new String(bytes, 1, bytes.length - 1, StandardCharsets.UTF_8);
            }

            // json — UTF-8 JSON directly.
            case PgWireServer.OID_JSON ->
                new String(bytes, StandardCharsets.UTF_8);

            // Array OIDs: decode through the array codec, returning the
            // canonical text form so downstream coerce-pg-array hits the
            // existing from-pg-text path. NULL elements re-emit as the
            // unquoted `NULL` token.
            default -> {
                if (isArrayOid(oid)) {
                    String s = decodeArrayBinary(oid, bytes);
                    if (s != null) yield s;
                }
                // Composite (registered) or anonymous record (2249): decode the
                // binary record to canonical record_out text. Field OIDs are
                // inline in the wire bytes, so no registry lookup is needed.
                if (oid == 2249 || compositeFields(oid) != null) {
                    String s = decodeRecordBinary(bytes);
                    if (s != null) yield s;
                }
                throw new PgWireServer.PgProtocolException("0A000",
                    "binary decoding not implemented for OID " + oid
                    + " (use text parameter format)");
            }
        };
    }
}
