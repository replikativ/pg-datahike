package datahike.pg;

import java.io.*;
import java.net.*;
import java.nio.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Minimal PostgreSQL wire protocol (v3) server for Datahike.
 *
 * <p>Implements the Simple Query protocol and minimal Extended Query protocol —
 * enough for psql, DBeaver, JDBC drivers, and Python/pandas to connect and
 * execute SQL queries.
 *
 * <p>Protocol flow:
 * <ol>
 *   <li>SSL negotiation → reject with 'N'</li>
 *   <li>StartupMessage → AuthenticationOk + ParameterStatus + BackendKeyData + ReadyForQuery</li>
 *   <li>Query ('Q') → delegate to QueryHandler → RowDescription + DataRow* + CommandComplete + ReadyForQuery</li>
 *   <li>Terminate ('X') → close connection</li>
 * </ol>
 *
 * <p>Adapted from Stratum's PgWireServer for Datahike's SQL compatibility layer.
 */
public final class PgWireServer {

    /**
     * A protocol-level exception carrying a PostgreSQL SQLSTATE code.
     * Thrown from message handlers when a specific error class is known
     * (e.g., feature_not_supported, invalid_text_representation). The
     * connection loop catches it and emits an ErrorResponse with the
     * matching code rather than the generic XX000.
     *
     * <p>For errors originating in the Clojure query handler, see
     * {@link QueryResult#error} plus {@link QueryResult#sqlstate}; this
     * exception is specifically for protocol-layer failures.
     */
    public static final class PgProtocolException extends RuntimeException {
        public final String sqlstate;
        /** Optional ErrorResponse detail fields (D/H/n/t/c/...); may be null. */
        public final java.util.Map<String, String> errorFields;

        public PgProtocolException(String sqlstate, String message) {
            this(sqlstate, message, null);
        }

        public PgProtocolException(String sqlstate, String message,
                                   java.util.Map<String, String> errorFields) {
            super(message);
            this.sqlstate = sqlstate;
            this.errorFields = errorFields;
        }
    }

    // PostgreSQL type OIDs (authoritative: src/include/catalog/pg_type.dat)
    public static final int OID_BOOL        = 16;
    public static final int OID_BYTEA       = 17;
    public static final int OID_NAME        = 19;
    public static final int OID_INT8        = 20;   // bigint
    public static final int OID_INT2        = 21;   // smallint
    public static final int OID_INT4        = 23;   // integer
    public static final int OID_TEXT        = 25;
    public static final int OID_OID         = 26;
    public static final int OID_JSON        = 114;
    public static final int OID_FLOAT4      = 700;  // real
    public static final int OID_FLOAT8      = 701;  // double precision
    public static final int OID_VARCHAR     = 1043;
    public static final int OID_DATE        = 1082;
    public static final int OID_TIME        = 1083;
    public static final int OID_TIMESTAMP   = 1114; // timestamp without time zone
    public static final int OID_TIMESTAMPTZ = 1184; // timestamp with time zone
    public static final int OID_NUMERIC     = 1700;
    public static final int OID_UUID        = 2950;
    public static final int OID_JSONB       = 3802;

    // Array OIDs — exposed so PgParamCodec can dispatch binary
    // encode/decode through the scalar element codec without
    // duplicating the registry.
    public static final int OID_BOOL_ARRAY        = 1000;
    public static final int OID_BYTEA_ARRAY       = 1001;
    public static final int OID_NAME_ARRAY        = 1003;
    public static final int OID_INT2_ARRAY        = 1005;
    public static final int OID_INT4_ARRAY        = 1007;
    public static final int OID_TEXT_ARRAY        = 1009;
    public static final int OID_INT8_ARRAY        = 1016;
    public static final int OID_FLOAT4_ARRAY      = 1021;
    public static final int OID_FLOAT8_ARRAY      = 1022;
    public static final int OID_OID_ARRAY         = 1028;
    public static final int OID_VARCHAR_ARRAY     = 1015;
    public static final int OID_DATE_ARRAY        = 1182;
    public static final int OID_TIME_ARRAY        = 1183;
    public static final int OID_TIMESTAMP_ARRAY   = 1115;
    public static final int OID_TIMESTAMPTZ_ARRAY = 1185;
    public static final int OID_NUMERIC_ARRAY     = 1231;
    public static final int OID_UUID_ARRAY        = 2951;
    public static final int OID_JSON_ARRAY        = 199;
    public static final int OID_JSONB_ARRAY       = 3807;

    /**
     * Callback interface for query execution.
     * Implementations may hold per-connection state (transaction state,
     * row-level locks, etc.) that should be cleaned up when the client
     * disconnects. The default {@code close()} is a no-op so existing
     * implementations remain source-compatible.
     */
    public interface QueryHandler {
        /** Simple Query protocol: execute one SQL statement. */
        QueryResult execute(String sql);

        /**
         * Extended Query protocol Parse phase. Translate the SQL once
         * and return an opaque state object that subsequent Bind/
         * Describe/Execute calls on the same statement reuse. Must
         * return a non-null value for any SQL the handler accepts;
         * may throw on parse failure (the caller converts to
         * ErrorResponse).
         *
         * @param sql        SQL query with `$N` / `?` placeholders.
         * @param paramOids  type OIDs from the Parse message (one per
         *                   `$N`, in declaration order). May be empty;
         *                   an OID of 0 means the client wants the
         *                   server to infer (we typically don't).
         */
        Object parse(String sql, int[] paramOids);

        /**
         * Return the parameter type OIDs this prepared statement
         * expects. Called for {@code Describe('S', stmtName)}.
         * Length equals the number of `$N` placeholders.
         */
        int[] describeParams(Object parsed);

        /**
         * Describe the result columns a prepared statement would
         * produce. Returns a QueryResult with {@code columnNames} /
         * {@code columnOids} populated and {@code rows} empty; or
         * {@code null} if the statement returns no tuples (DML).
         */
        QueryResult describeResult(Object parsed);

        /**
         * Execute a previously-parsed statement with bound parameter
         * values. {@code boundParams} is a 1-indexed aligned vector
         * (element 0 unused, element i = value for `$i`). A null
         * element represents SQL NULL.
         */
        QueryResult executePrepared(Object parsed, Object[] boundParams);

        /**
         * Invoked when the pgwire client disconnects. Implementations
         * should release any resources (locks, pending transactions)
         * associated with this connection — analogous to a PostgreSQL
         * backend process terminating, which implicitly rolls back and
         * releases all row/advisory locks.
         */
        default void close() {}

        /**
         * COPY-IN sub-protocol: the server has emitted CopyInResponse
         * (because a previous {@link #execute} returned a QueryResult
         * with {@link QueryResult#copyInMode} set). The client now
         * streams CopyData ('d') messages, each delivering a chunk of
         * the COPY data; the wire layer routes each chunk's payload
         * here. Chunk boundaries do NOT have to align with row
         * boundaries — the handler is responsible for buffering /
         * line-splitting.
         *
         * Default: throw — handlers that haven't opted in to COPY
         * support get a clean 22023 from the wire layer.
         */
        default void copyChunk(byte[] chunk) {
            throw new IllegalStateException("COPY-IN not implemented for this handler");
        }

        /**
         * COPY-IN sub-protocol: the client sent CopyDone ('c'). The
         * handler should flush any pending state and return a
         * QueryResult whose {@code commandTag} is "COPY &lt;n&gt;"
         * (n = rows committed). The wire layer emits CommandComplete
         * + ReadyForQuery and clears COPY-IN state.
         *
         * Default: throw — same as {@link #copyChunk}.
         */
        default QueryResult copyComplete() {
            throw new IllegalStateException("COPY-IN not implemented for this handler");
        }

        /**
         * COPY-IN sub-protocol: the client sent CopyFail ('f'). The
         * handler should discard any pending state without
         * transacting. The wire layer then emits ErrorResponse with
         * the supplied reason and ReadyForQuery, clears COPY-IN
         * state, and resumes normal message dispatch.
         *
         * Default: no-op (the wire layer's ErrorResponse handles the
         * client visible part).
         */
        default void copyAbort(String reason) { /* default: nothing */ }
    }

    /**
     * Factory for creating per-connection QueryHandler instances.
     * Each connection gets its own handler with independent transaction state.
     *
     * The SAM is {@link #create()} — a parameterless lambda that always
     * returns the same handler remains a valid factory. Factories that
     * need to route on StartupMessage parameters (e.g. {@code database}
     * for multi-DB registries) override {@link #create(java.util.Map)}
     * instead; the default delegates to {@link #create()} for back-compat.
     */
    @FunctionalInterface
    public interface QueryHandlerFactory {
        /** Legacy entry — no access to startup parameters. */
        QueryHandler create();

        /**
         * Startup-parameter-aware entry. Default delegates to {@link #create()}
         * so lambda implementations of the legacy form continue to compile.
         */
        default QueryHandler create(java.util.Map<String, String> startupParams) {
            return create();
        }
    }

    /**
     * Cached prepared statement (result of Parse). Holds the opaque
     * handler state plus the parameter OIDs so Bind knows how to
     * decode each parameter value.
     */
    static final class PreparedStmt {
        final String sql;
        final int[] paramOids;
        final Object parsed;  // opaque; handler-specific
        /**
         * Set to true once Describe('S', name) has emitted
         * RowDescription for this statement. All Portals bound from
         * it then skip RowDescription in Execute — PG's contract says
         * if the client did any Describe (S or P), Execute sends only
         * DataRows + CommandComplete.
         */
        boolean described;

        PreparedStmt(String sql, int[] paramOids, Object parsed) {
            this.sql = sql;
            this.paramOids = paramOids;
            this.parsed = parsed;
            this.described = false;
        }
    }

    /**
     * A bound portal (result of Bind). References the prepared
     * statement and carries the decoded parameter values ready for
     * Execute.
     */
    static final class Portal {
        final PreparedStmt stmt;
        /** 1-indexed; element 0 unused. A {@code null} element is SQL NULL. */
        final Object[] boundParams;
        /**
         * Client-requested result column formats. Length 0 = all text,
         * length 1 = apply to every column, length N = per-column.
         * Set from Bind's result-format array and read at Execute time
         * to encode each DataRow value as text or binary per OID.
         */
        final short[] resultFormats;
        /** True once a Describe('P', …) has emitted RowDescription; Execute then skips it. */
        boolean described;

        Portal(PreparedStmt stmt, Object[] boundParams, short[] resultFormats) {
            this.stmt = stmt;
            this.boundParams = boundParams;
            this.resultFormats = resultFormats;
            this.described = false;
        }
    }

    /**
     * Result of a SQL query execution.
     */
    public static final class QueryResult {
        public final String[] columnNames;
        public final int[] columnOids;
        public final String[][] rows;
        public final String commandTag;
        /**
         * Per-column source table OID. A non-zero value here lets pgjdbc's
         * {@code PgResultSetMetaData.getBaseColumnName} call through to
         * its {@code fetchFieldMetaData()} catalog lookup (it short-
         * circuits to empty string when tableOid == 0), which is what
         * updatable ResultSets need for the SET column name. Parallels
         * {@code columnOids} in length. May be null — {@link #sendRowDescription}
         * treats that as "0 for every column" (non-table expressions).
         */
        public int[] columnTableOids;
        /**
         * Per-column 1-based position in the source table ({@code pg_attribute.attnum}).
         * Paired with {@link #columnTableOids} as the key into the field-metadata
         * catalog join. May be null (treated as "0 for every column").
         */
        public short[] columnAttnums;
        /**
         * Per-column PG-style atttypmod. Encodes {@code NUMERIC(p, s)}
         * precision+scale (and varchar(n) length when DDL captures it).
         * Drives pgjdbc's {@code ResultSetMetaData.getPrecision/getScale}
         * and Metabase's column-type rendering. {@code -1} for any column
         * means "unspecified" — that's also the default emitted when
         * this array is null. Lengths must match {@code columnNames}.
         */
        public int[] columnTypmods;
        public final String error;
        /** PostgreSQL SQLSTATE code when {@link #error} is non-null. Defaults to "XX000" (internal error). */
        public String sqlstate;
        /**
         * Optional extra ErrorResponse fields keyed by the single-byte
         * field type per the PG protocol (e.g. "D" detail, "H" hint,
         * "n" constraint name, "t" table name, "c" column name,
         * "d" data type name, "P" character position, "W" where). Only
         * emitted when {@link #error} is non-null. Absent fields are
         * skipped.
         */
        public java.util.Map<String, String> errorFields;
        /** Transaction status: 'I' = idle, 'T' = in transaction, 'E' = error in transaction. */
        public char txStatus;

        /**
         * COPY-IN sub-protocol entry signal. When true, the wire
         * layer emits CopyInResponse instead of CommandComplete and
         * transitions to COPY-IN mode; subsequent CopyData / CopyDone
         * / CopyFail messages route through {@link QueryHandler#copyChunk}
         * / {@link QueryHandler#copyComplete} / {@link QueryHandler#copyAbort}.
         *
         * Number of columns in the COPY target: clients use this to
         * pre-size their per-column format-code arrays. We always send
         * format code 0 (text) for every column — pg-datahike doesn't
         * yet support binary COPY. {@code copyColumnCount} of 0 is
         * accepted by PG (a zero-column table is a degenerate but
         * valid case).
         */
        public boolean copyInMode;
        public int copyColumnCount;

        /** Successful result with rows. */
        public QueryResult(String[] columnNames, int[] columnOids,
                           String[][] rows, String commandTag) {
            this.columnNames = columnNames;
            this.columnOids = columnOids;
            this.rows = rows;
            this.commandTag = commandTag;
            this.error = null;
            this.sqlstate = null;
            this.txStatus = 'I';
        }

        /** Error result with default XX000 SQLSTATE. */
        public QueryResult(String error) {
            this(error, "XX000");
        }

        /** Error result with explicit SQLSTATE. */
        public QueryResult(String error, String sqlstate) {
            this.columnNames = null;
            this.columnOids = null;
            this.rows = null;
            this.commandTag = null;
            this.error = error;
            this.sqlstate = sqlstate;
            this.txStatus = 'I';
        }

        /** Empty result (e.g., SET command). */
        public static QueryResult empty(String commandTag) {
            return new QueryResult(new String[0], new int[0], new String[0][], commandTag);
        }

        /** Set transaction status and return this for chaining. */
        public QueryResult withTxStatus(char status) {
            this.txStatus = status;
            return this;
        }

        /** Set SQLSTATE and return this for chaining. */
        public QueryResult withSqlstate(String sqlstate) {
            this.sqlstate = sqlstate;
            return this;
        }

        /** Attach optional ErrorResponse detail fields (detail / hint / constraint / table / column / …). */
        public QueryResult withErrorFields(java.util.Map<String, String> fields) {
            this.errorFields = fields;
            return this;
        }

        /**
         * Attach per-column (tableOid, attnum) metadata so RowDescription
         * can populate pgjdbc's Field.tableOid / Field.positionInTable.
         * Lengths must match {@code columnNames} — caller's responsibility.
         */
        public QueryResult withColumnSources(int[] tableOids, short[] attnums) {
            this.columnTableOids = tableOids;
            this.columnAttnums = attnums;
            return this;
        }

        /**
         * Attach per-column atttypmod values for RowDescription.
         * See {@link #columnTypmods}.
         */
        public QueryResult withColumnTypmods(int[] typmods) {
            this.columnTypmods = typmods;
            return this;
        }

        /**
         * Mark this result as the COPY-IN entry signal. The wire
         * layer will emit CopyInResponse with {@code columnCount}
         * columns (all text format) and transition to COPY-IN state
         * before reading the next message.
         */
        public QueryResult withCopyInMode(int columnCount) {
            this.copyInMode = true;
            this.copyColumnCount = columnCount;
            return this;
        }
    }

    /** Maximum message body size: 64 MB. */
    private static final int MAX_MESSAGE_LENGTH = 64 * 1024 * 1024;

    /** Maximum SSL/startup negotiation rounds before closing connection. */
    private static final int MAX_STARTUP_ROUNDS = 5;

    private final int port;
    private final String host;
    private final QueryHandlerFactory handlerFactory;
    private ServerSocket serverSocket;
    private final AtomicBoolean running = new AtomicBoolean(false);
    private Thread acceptThread;

    /**
     * Registry of active connections keyed by (pid, secret) — the two
     * 32-bit values we send in BackendKeyData at startup. A CancelRequest
     * (protocol code 80877102) looks up the target via these keys and
     * flips its `cancelled` flag. Static so a CancelRequest opened on a
     * *new* connection can find the original connection's registration.
     */
    private static final java.util.concurrent.ConcurrentHashMap<Long, AtomicBoolean>
        CANCEL_REGISTRY = new java.util.concurrent.ConcurrentHashMap<>();

    /**
     * Parallel registry of the backend thread currently handling each
     * connection. A CancelRequest flips the flag (observed cooperatively
     * by the query engine) AND — when the backend is not currently
     * blocked reading the next wire message — interrupts the thread as
     * a safety net for blocking I/O paths (konserve store reads, socket
     * writes) that wouldn't otherwise notice the flag promptly.
     */
    private static final java.util.concurrent.ConcurrentHashMap<Long, Thread>
        CANCEL_THREADS = new java.util.concurrent.ConcurrentHashMap<>();

    /**
     * Registry of the `doing-command-read` state per connection. True
     * while the backend is blocked reading the next wire message from
     * the client (the gap between statements). Analogous to Postgres'
     * `DoingCommandRead` in src/backend/tcop/postgres.c — a cancel
     * arriving in that window is silently discarded, matching PG's
     * "cancel is a no-op on an idle session." A cancel arriving outside
     * that window (during dispatch: Parse/Bind/Describe/Execute/Query)
     * sets the flag; the query engine observes it at the next
     * check-cancel! site, or the thread interrupt wakes blocking I/O.
     */
    private static final java.util.concurrent.ConcurrentHashMap<Long, AtomicBoolean>
        COMMAND_READ_REGISTRY = new java.util.concurrent.ConcurrentHashMap<>();

    private static long cancelKey(int pid, int secret) {
        return (((long) pid) << 32) | (secret & 0xFFFFFFFFL);
    }

    /**
     * Thread-local pointing at the current connection's cancel flag so
     * query-execution code paths can cheaply check it between statements.
     * Null when outside a pgwire connection.
     */
    public static final ThreadLocal<AtomicBoolean> CANCEL_FLAG = new ThreadLocal<>();

    /**
     * Shared scheduler for statement_timeout tasks. Virtual-thread per
     * task keeps contention negligible; used by the Clojure layer to
     * flip the current session's cancel flag after statement_timeout ms.
     */
    public static final java.util.concurrent.ScheduledExecutorService TIMEOUT_SCHED =
        java.util.concurrent.Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = Thread.ofVirtual().unstarted(r);
            t.setName("pgwire-stmt-timeout");
            return t;
        });

    /**
     * Create a server bound to localhost (127.0.0.1) on the given port.
     */
    /** Create a server with a single shared handler (legacy — no per-connection tx state). */
    public PgWireServer(int port, QueryHandler handler) {
        this(port, "127.0.0.1", () -> handler);
    }

    /** Create a server with a single shared handler (legacy). */
    public PgWireServer(int port, String host, QueryHandler handler) {
        this(port, host, () -> handler);
    }

    /**
     * Create a server with per-connection handler factory.
     * Each connection creates a fresh handler with independent transaction state.
     */
    public PgWireServer(int port, String host, QueryHandlerFactory factory) {
        this.port = port;
        this.host = host;
        this.handlerFactory = factory;
    }

    /**
     * Start the server. Non-blocking — spawns a daemon thread to accept connections.
     */
    public void start() throws IOException {
        // Create unbound socket so we can set SO_REUSEADDR *before* bind —
        // otherwise a restart hits "Address already in use" until the
        // previous socket's TIME_WAIT clears.
        serverSocket = new ServerSocket();
        serverSocket.setReuseAddress(true);
        serverSocket.bind(new InetSocketAddress(InetAddress.getByName(host), port), 50);
        running.set(true);

        acceptThread = Thread.ofVirtual().name("pgwire-accept").start(() -> {
            while (running.get()) {
                try {
                    Socket client = serverSocket.accept();
                    Thread.ofVirtual().name("pgwire-conn-" + client.getRemoteSocketAddress())
                        .start(() -> handleConnection(client));
                } catch (IOException e) {
                    if (running.get()) {
                        System.err.println("PgWire accept error: " + e.getMessage());
                    }
                }
            }
        });
    }

    /**
     * Graceful shutdown.
     */
    public void stop() {
        running.set(false);
        try {
            if (serverSocket != null && !serverSocket.isClosed()) {
                serverSocket.close();
            }
        } catch (IOException ignored) {}
    }

    public int getPort() {
        return serverSocket != null ? serverSocket.getLocalPort() : port;
    }

    public boolean isRunning() {
        return running.get();
    }

    // ========================================================================
    // Connection handling
    // ========================================================================

    private void handleConnection(Socket client) {
        QueryHandler handler = null;
        AtomicBoolean[] cancelState = new AtomicBoolean[]{null};
        long[] cancelKeys = new long[]{0L};
        @SuppressWarnings("unchecked")
        java.util.Map<String, String>[] startupParamsHolder = new java.util.Map[]{null};
        try (client;
             DataInputStream in = new DataInputStream(new BufferedInputStream(client.getInputStream()));
             DataOutputStream out = new DataOutputStream(new BufferedOutputStream(client.getOutputStream()))) {

            if (!handleStartup(in, out, cancelState, cancelKeys, startupParamsHolder)) {
                return;
            }

            // Per-connection handler (each connection gets independent tx state).
            // Factory sees the parsed StartupMessage params (database, user,
            // application_name, …) so multi-DB registries can route on `database`.
            java.util.Map<String, String> startupParams = startupParamsHolder[0];
            if (startupParams == null) startupParams = java.util.Collections.emptyMap();
            handler = handlerFactory.create(startupParams);

            // Per-connection state
            // Prepared statements keyed by name. "" is the unnamed statement,
            // replaceable on each Parse. Named statements persist until Close.
            java.util.Map<String, PreparedStmt> statements = new java.util.HashMap<>();
            // Portals keyed by name. "" is the unnamed portal, replaceable on
            // each Bind. Named portals persist until Close.
            java.util.Map<String, Portal> portals = new java.util.HashMap<>();
            // Transaction status: 'I' = idle (no tx), 'T' = in tx, 'E' = aborted tx.
            char[] txStatus = new char[]{'I'};
            // Extended-query error state. Per PG protocol: after an error
            // processing P/B/D/E/C, the backend discards messages until Sync,
            // then sends ReadyForQuery. Simple Query ('Q') is self-contained
            // and doesn't enter this state — handleQuery always ends with
            // ReadyForQuery regardless of errors inside.
            boolean[] inError = new boolean[]{false};
            // COPY-IN sub-protocol state. 0 = NONE, 1 = COPY_IN. The
            // wire loop dispatches 'd' (CopyData), 'c' (CopyDone),
            // 'f' (CopyFail) messages to the handler's copyChunk /
            // copyComplete / copyAbort callbacks while in COPY_IN.
            // 'H' (Flush) and 'S' (Sync) are silently ignored in this
            // state, per PG protocol.sgml:1313-1318.
            int[] copyState = new int[]{0};

            boolean debug = System.getenv("DATAHIKE_WIRE_DEBUG") != null;

            // Lookup the command-read gate registered at startup. Set
            // true around each blocking socket read so a CancelRequest
            // arriving between statements is silently consumed by the
            // post-read barrier below (PG's DoingCommandRead protocol).
            AtomicBoolean doingCommandRead = COMMAND_READ_REGISTRY.get(cancelKeys[0]);

            while (running.get() && !client.isClosed()) {
                if (doingCommandRead != null) doingCommandRead.set(true);
                int msgType = in.read();
                if (msgType == -1) break;

                int msgLen = in.readInt();
                if (msgLen < 4 || msgLen > MAX_MESSAGE_LENGTH) {
                    sendError(out, "FATAL", "08P01",
                            "Invalid message length: " + msgLen);
                    return;
                }
                byte[] body = new byte[msgLen - 4];
                in.readFully(body);

                // Commit to dispatching this message. From this point
                // on, any CancelRequest arriving for this connection
                // will be treated as a real cancel (flag + interrupt).
                // No flag-clearing barrier here: cancels arriving during
                // the previous command-read gap were silently dropped
                // at the source by the CancelRequest handler.
                if (doingCommandRead != null) doingCommandRead.set(false);

                if (debug) {
                    String extra = "";
                    if (msgType == 'Q') {
                        extra = " sql=" + new String(body, 0, Math.min(body.length - 1, 120), java.nio.charset.StandardCharsets.UTF_8);
                    } else if (msgType == 'P') {
                        ByteBuffer pb = ByteBuffer.wrap(body);
                        readCString(pb); // stmt name
                        extra = " sql=" + readCString(pb);
                        if (extra.length() > 130) extra = extra.substring(0, 130) + "...";
                    }
                    System.err.println("[WIRE] recv " + (char) msgType + " len=" + msgLen
                            + (inError[0] ? " [ERROR-STATE]" : "") + extra);
                }

                // Extended-query error state: skip all messages except Sync
                // (which clears the state and sends RFQ) and Terminate.
                if (inError[0]) {
                    if (msgType == 'S') {
                        sendReadyForQuery(out, txStatus[0]);
                        out.flush();
                        inError[0] = false;
                    } else if (msgType == 'X') {
                        return;
                    }
                    // Silently discard all other messages until Sync arrives.
                    continue;
                }

                try {
                    // COPY-IN routing: while in COPY_IN, normal message
                    // dispatch is suspended. The frontend streams 'd'
                    // (CopyData) until either 'c' (CopyDone) or 'f'
                    // (CopyFail). 'H' (Flush) and 'S' (Sync) are
                    // ignored here, per PG protocol.sgml:1313-1318.
                    // Anything else aborts the copy with an error.
                    if (copyState[0] == 1) {
                        switch (msgType) {
                            case 'd' -> {
                                handler.copyChunk(body);
                            }
                            case 'c' -> {
                                QueryResult cr = handler.copyComplete();
                                copyState[0] = 0;
                                if (cr != null && cr.error != null) {
                                    sendError(out, "ERROR",
                                            cr.sqlstate != null ? cr.sqlstate : "XX000",
                                            cr.error, cr.errorFields);
                                    if (txStatus[0] == 'T') txStatus[0] = 'E';
                                } else {
                                    sendCommandComplete(out,
                                            cr != null && cr.commandTag != null
                                                ? cr.commandTag : "COPY 0");
                                }
                                sendReadyForQuery(out, txStatus[0]);
                                out.flush();
                            }
                            case 'f' -> {
                                String reason = readCopyFailReason(body);
                                handler.copyAbort(reason);
                                copyState[0] = 0;
                                sendError(out, "ERROR", "57014",
                                        "COPY from stdin failed: " + reason);
                                if (txStatus[0] == 'T') txStatus[0] = 'E';
                                sendReadyForQuery(out, txStatus[0]);
                                out.flush();
                            }
                            // Flush / Sync — ignored during COPY-IN
                            case 'H', 'S' -> { /* no-op */ }
                            case 'X' -> { return; }
                            default -> {
                                handler.copyAbort("unexpected message type during COPY-IN: " + (char) msgType);
                                copyState[0] = 0;
                                sendError(out, "ERROR", "08P01",
                                        "Unsupported message during COPY-IN: " + (char) msgType);
                                if (txStatus[0] == 'T') txStatus[0] = 'E';
                                sendReadyForQuery(out, txStatus[0]);
                                out.flush();
                            }
                        }
                        // Skip the regular dispatch for this iteration
                        continue;
                    }

                    switch (msgType) {
                        case 'Q' -> handleQuery(body, out, txStatus, handler, copyState);
                        case 'X' -> { return; }
                        case 'P' -> handleParse(body, out, statements, handler);
                        case 'B' -> handleBind(body, out, statements, portals);
                        case 'D' -> handleDescribe(body, out, statements, portals, handler);
                        case 'E' -> handleExecuteMsg(body, out, portals, txStatus, handler, copyState);
                        case 'S' -> handleSync(out, txStatus);
                        case 'C' -> handleClose(body, out, statements, portals);
                        // Flush ('H'): PG requires pending responses to be sent immediately.
                        // We flush after every handler anyway; still, acknowledge the message
                        // so clients don't get out of sync.
                        case 'H' -> out.flush();
                        default -> {
                            // Unknown top-level message — e.g. F/d/c/f/p
                            // sent out-of-context. PG requires that
                            // every top-level error is followed by
                            // ReadyForQuery so the client can resync;
                            // otherwise pgJDBC CallableStatement +
                            // libpq-based clients block in
                            // PQgetResult waiting for it
                            // (postgres.c:4650-4700).
                            sendError(out, "ERROR", "08P01",
                                    "Unsupported message type: " + (char) msgType);
                            sendReadyForQuery(out, txStatus[0]);
                            out.flush();
                        }
                    }
                } catch (Exception e) {
                    String sqlstate = (e instanceof PgProtocolException pe) ? pe.sqlstate : "XX000";
                    java.util.Map<String, String> fields =
                        (e instanceof PgProtocolException pe) ? pe.errorFields : null;
                    if (debug) {
                        System.err.println("[WIRE] ERROR handling " + (char) msgType
                                + " sqlstate=" + sqlstate + ": " + e.getMessage());
                        if (!(e instanceof PgProtocolException)) e.printStackTrace(System.err);
                    }
                    sendError(out, "ERROR", sqlstate,
                            e.getMessage() != null ? e.getMessage() : e.getClass().getName(),
                            fields);
                    out.flush();
                    // Auto-transition T→E on any error during an open transaction.
                    // Matches PG: the first error in a tx aborts it, subsequent
                    // statements get 25P02 until ROLLBACK.
                    if (txStatus[0] == 'T') {
                        txStatus[0] = 'E';
                    }
                    // Extended-query messages enter error-skip mode; Simple Query
                    // recovers on its own (handleQuery catches internally and
                    // sends its own ReadyForQuery).
                    if (msgType == 'P' || msgType == 'B' || msgType == 'D'
                            || msgType == 'E' || msgType == 'C') {
                        inError[0] = true;
                    }
                }
                // Statement-boundary cleanup (runs after success or
                // error). Clear any pending cancel flag so an
                // observed-late cancel (or a cancel that raced in just
                // after the last check-cancel! site) cannot leak into
                // the next statement. Also clear any stray interrupt
                // bit from the safety-net path.
                if (msgType == 'E' || msgType == 'Q' || msgType == 'S') {
                    if (cancelState[0] != null) cancelState[0].set(false);
                    Thread.interrupted();
                }
            }
        } catch (IOException e) {
            // Client disconnected
        } catch (Exception e) {
            System.err.println("PgWire connection error: " + e.getMessage());
            if (System.getenv("DATAHIKE_WIRE_DEBUG") != null) {
                e.printStackTrace(System.err);
            }
        } finally {
            // Release any per-connection state (row locks, pending tx) —
            // analogous to PG's backend termination implicitly rolling back.
            if (handler != null) {
                try { handler.close(); } catch (Exception ignored) {}
            }
            if (cancelKeys[0] != 0L) {
                CANCEL_REGISTRY.remove(cancelKeys[0]);
                CANCEL_THREADS.remove(cancelKeys[0]);
                COMMAND_READ_REGISTRY.remove(cancelKeys[0]);
            }
            CANCEL_FLAG.remove();
            // Defensive: clear any lingering interrupt status on the
            // connection thread so a late CancelRequest doesn't bleed
            // into subsequent work (relevant when threads are pooled).
            Thread.interrupted();
        }
    }

    /**
     * Parse a StartupMessage payload — a sequence of null-terminated UTF-8
     * key/value pairs, terminated by an empty key. PG's libpq sends
     * {@code user}, {@code database}, {@code application_name},
     * {@code client_encoding}, etc. this way.
     */
    private static java.util.Map<String, String> parseStartupParams(byte[] body) {
        java.util.Map<String, String> params = new java.util.LinkedHashMap<>();
        int i = 0;
        while (i < body.length) {
            int keyStart = i;
            while (i < body.length && body[i] != 0) i++;
            if (i == keyStart) break; // empty key terminates
            String key = new String(body, keyStart, i - keyStart,
                                    java.nio.charset.StandardCharsets.UTF_8);
            i++; // skip null
            int valStart = i;
            while (i < body.length && body[i] != 0) i++;
            String val = new String(body, valStart, i - valStart,
                                    java.nio.charset.StandardCharsets.UTF_8);
            i++; // skip null
            params.put(key, val);
        }
        return params;
    }

    private boolean handleStartup(DataInputStream in, DataOutputStream out,
                                   AtomicBoolean[] cancelState, long[] cancelKeys,
                                   java.util.Map<String, String>[] startupParamsHolder) throws IOException {
        for (int round = 0; round < MAX_STARTUP_ROUNDS; round++) {
            int len = in.readInt();
            int code = in.readInt();

            if (code == 80877103) {
                // SSLRequest — reject
                out.writeByte('N');
                out.flush();
                continue;
            }

            if (code == 80877104) {
                // GSSENCRequest (protocol code 80877104) — clients that
                // build libpq with GSSAPI support (psql on RHEL/Debian,
                // pgJDBC with default gssEncMode=prefer, asyncpg,
                // pgAdmin 4) send this before the StartupMessage, ahead
                // of SSLRequest. PG's backend_startup.c treats it
                // symmetrically to SSL: reply 'N' ("not supported"),
                // loop for the next message. We neither support nor
                // plan to support GSS encryption; 'N' is the correct
                // "proceed in plaintext" signal.
                out.writeByte('N');
                out.flush();
                continue;
            }

            if (code == 196608) {
                // StartupMessage v3.0
                byte[] params = new byte[len - 8];
                in.readFully(params);
                // Parse key/value pairs and surface them to the caller so
                // handlerFactory.create(params) can route on `database`.
                startupParamsHolder[0] = parseStartupParams(params);

                // AuthenticationOk
                out.writeByte('R');
                out.writeInt(8);
                out.writeInt(0);
                out.flush();

                sendParameterStatus(out, "server_version", "15.0");
                sendParameterStatus(out, "server_encoding", "UTF8");
                sendParameterStatus(out, "client_encoding", "UTF8");
                sendParameterStatus(out, "DateStyle", "ISO, MDY");
                sendParameterStatus(out, "integer_datetimes", "on");
                sendParameterStatus(out, "standard_conforming_strings", "on");
                sendParameterStatus(out, "TimeZone", "UTC");
                sendParameterStatus(out, "is_superuser", "on");
                sendParameterStatus(out, "application_name", "datahike");

                // BackendKeyData — random (pid, secret) per connection
                // so CancelRequest can address this specific backend.
                // Register before sending so a fast client can issue a
                // CancelRequest immediately after reading the key data.
                int pid = java.util.concurrent.ThreadLocalRandom.current().nextInt(1, Integer.MAX_VALUE);
                int secret = java.util.concurrent.ThreadLocalRandom.current().nextInt();
                AtomicBoolean cancelFlag = new AtomicBoolean(false);
                AtomicBoolean doingCommandRead = new AtomicBoolean(false);
                long ckey = cancelKey(pid, secret);
                CANCEL_REGISTRY.put(ckey, cancelFlag);
                COMMAND_READ_REGISTRY.put(ckey, doingCommandRead);
                CANCEL_THREADS.put(ckey, Thread.currentThread());
                cancelState[0] = cancelFlag;
                cancelKeys[0] = ckey;
                CANCEL_FLAG.set(cancelFlag);
                out.writeByte('K');
                out.writeInt(12);
                out.writeInt(pid);
                out.writeInt(secret);

                sendReadyForQuery(out, 'I');
                out.flush();
                return true;
            }

            if (code == 80877102) {
                // CancelRequest: body is 4 bytes pid + 4 bytes secret.
                // Look up the target session and flip its flag — the
                // target's query loop notices between statements and
                // raises 57014.
                byte[] rest = new byte[len - 8];
                in.readFully(rest);
                if (rest.length >= 8) {
                    java.nio.ByteBuffer bb = java.nio.ByteBuffer.wrap(rest);
                    int tgtPid = bb.getInt();
                    int tgtSecret = bb.getInt();
                    long tgtKey = cancelKey(tgtPid, tgtSecret);
                    AtomicBoolean flag = CANCEL_REGISTRY.get(tgtKey);
                    AtomicBoolean inReadState = COMMAND_READ_REGISTRY.get(tgtKey);
                    if (flag != null && (inReadState == null || !inReadState.get())) {
                        // Target is currently dispatching a message
                        // (Parse/Bind/Describe/Execute/Query) — raise a
                        // real cancel. PG equivalent: the backend is
                        // not in DoingCommandRead, so CHECK_FOR_INTERRUPTS
                        // will surface 57014 at the next observation
                        // point inside the handler.
                        //
                        // Intentionally silent if inRead=true: PG's
                        // "query cancel is a no-op when there is no
                        // query in progress" (postgres.c line 4731). We
                        // implement that by never setting the flag in
                        // the first place — so it cannot leak into the
                        // next statement the way it would if we set it
                        // now and tried to clear it later.
                        flag.set(true);
                        Thread tgt = CANCEL_THREADS.get(tgtKey);
                        if (tgt != null) tgt.interrupt();
                    }
                }
                return false;
            }

            return false;
        }
        return false;
    }

    // ========================================================================
    // Simple Query protocol
    // ========================================================================

    private void handleQuery(byte[] body, DataOutputStream out, char[] txStatus, QueryHandler handler, int[] copyState) throws IOException {
        String sql = new String(body, 0, body.length - 1, StandardCharsets.UTF_8).trim();

        if (sql.isEmpty()) {
            sendEmptyQueryResponse(out);
            sendReadyForQuery(out, txStatus[0]);
            out.flush();
            return;
        }

        String[] statements = splitStatements(sql);

        // On first error, stop processing remaining statements (PG semantics:
        // a Simple Query with multiple statements aborts the whole batch on
        // the first failure).
        boolean errored = false;
        for (String stmt : statements) {
            if (errored) break;
            stmt = stripComments(stmt);
            if (stmt.isEmpty()) continue;

            // Between-statement cancellation: a CancelRequest arriving on a
            // parallel connection flips the flag; we raise 57014 for the
            // remaining statements and mark the batch errored. Also clear
            // any interrupt status left over from the safety-net interrupt
            // — check-cancel! already observed the flag and threw, but
            // the interrupt bit persists on the thread until consumed.
            AtomicBoolean flag = CANCEL_FLAG.get();
            if (flag != null && flag.getAndSet(false)) {
                Thread.interrupted();
                sendError(out, "ERROR", "57014", "canceling statement due to user request");
                if (txStatus[0] == 'T') txStatus[0] = 'E';
                errored = true;
                break;
            }

            try {
                QueryResult result = handler.execute(stmt);
                // Clear any pending interrupt bit left by the safety-net
                // cancel path so it doesn't bleed into the next
                // statement's socket read on this thread.
                Thread.interrupted();

                // The handler uses 'I' as "don't change wire-level tx status"
                // (most non-tx results default to 'I'). Only T/E from the
                // handler represent explicit transitions (BEGIN/COMMIT/
                // ROLLBACK/SAVEPOINT ops).
                if (result.txStatus != 'I') {
                    txStatus[0] = result.txStatus;
                }

                if (result.error != null) {
                    sendError(out, "ERROR",
                            result.sqlstate != null ? result.sqlstate : "XX000",
                            result.error, result.errorFields);
                    if (txStatus[0] == 'T') txStatus[0] = 'E';
                    errored = true;
                } else if (result.copyInMode) {
                    // Server-side enters COPY-IN. We send CopyInResponse
                    // and DON'T emit ReadyForQuery — the client now
                    // streams CopyData until CopyDone/CopyFail. The
                    // outer loop's COPY-IN dispatch handles those.
                    sendCopyInResponse(out, result.copyColumnCount);
                    out.flush();
                    copyState[0] = 1;
                    return;
                } else if (result.columnNames.length == 0) {
                    sendCommandComplete(out, result.commandTag);
                } else {
                    sendRowDescription(out, result.columnNames, result.columnOids,
                                      result.columnTableOids, result.columnAttnums,
                                      result.columnTypmods, null);
                    for (String[] row : result.rows) {
                        sendDataRow(out, row);
                    }
                    sendCommandComplete(out, result.commandTag);
                }
            } catch (Exception e) {
                sendError(out, "ERROR", "XX000",
                        e.getMessage() != null ? e.getMessage() : e.getClass().getName());
                if (txStatus[0] == 'T') txStatus[0] = 'E';
                errored = true;
            }
        }

        sendReadyForQuery(out, txStatus[0]);
        out.flush();  // Single flush for all response messages
    }

    private String[] splitStatements(String sql) {
        if (!sql.contains(";")) {
            return new String[]{sql};
        }
        List<String> stmts = new ArrayList<>();
        boolean inSingleQuote = false;
        boolean inDoubleQuote = false;
        boolean inDollarQuote = false;
        String dollarTag = null;
        int start = 0;
        int len = sql.length();
        for (int i = 0; i < len; i++) {
            char c = sql.charAt(i);

            // Dollar-quoted string: $$...$$ or $tag$...$tag$
            if (!inSingleQuote && !inDoubleQuote) {
                if (inDollarQuote) {
                    if (c == '$' && i + dollarTag.length() <= len
                            && sql.substring(i, i + dollarTag.length()).equals(dollarTag)) {
                        i += dollarTag.length() - 1;
                        inDollarQuote = false;
                        dollarTag = null;
                    }
                    continue;
                }
                if (c == '$') {
                    // Check for dollar-quote start: $$ or $tag$
                    int tagEnd = i + 1;
                    while (tagEnd < len && (Character.isLetterOrDigit(sql.charAt(tagEnd))
                            || sql.charAt(tagEnd) == '_')) tagEnd++;
                    if (tagEnd < len && sql.charAt(tagEnd) == '$') {
                        dollarTag = sql.substring(i, tagEnd + 1);
                        i = tagEnd;
                        inDollarQuote = true;
                        continue;
                    }
                }
            }

            // Single-quoted string
            if (c == '\'' && !inDoubleQuote && !inDollarQuote) {
                if (inSingleQuote && i + 1 < len && sql.charAt(i + 1) == '\'') {
                    i++; // escaped ''
                } else {
                    inSingleQuote = !inSingleQuote;
                }
                continue;
            }

            // Double-quoted identifier
            if (c == '"' && !inSingleQuote && !inDollarQuote) {
                inDoubleQuote = !inDoubleQuote;
                continue;
            }

            // Line comment: -- to end of line
            if (c == '-' && i + 1 < len && sql.charAt(i + 1) == '-'
                    && !inSingleQuote && !inDoubleQuote && !inDollarQuote) {
                while (i < len && sql.charAt(i) != '\n') i++;
                continue;
            }

            // Block comment: /* ... */
            if (c == '/' && i + 1 < len && sql.charAt(i + 1) == '*'
                    && !inSingleQuote && !inDoubleQuote && !inDollarQuote) {
                i += 2;
                while (i + 1 < len && !(sql.charAt(i) == '*' && sql.charAt(i + 1) == '/')) i++;
                i++; // skip closing /
                continue;
            }

            // Semicolon outside all quoted contexts
            if (c == ';' && !inSingleQuote && !inDoubleQuote && !inDollarQuote) {
                String part = stripComments(sql.substring(start, i));
                if (!part.trim().isEmpty()) {
                    stmts.add(part);
                }
                start = i + 1;
            }
        }
        if (start < len) {
            String part = stripComments(sql.substring(start));
            if (!part.trim().isEmpty()) {
                stmts.add(part);
            }
        }
        return stmts.isEmpty() ? new String[]{sql} : stmts.toArray(new String[0]);
    }

    /**
     * Strip SQL comments from a statement string.
     * Removes -- line comments and /* block comments, respecting quoted strings.
     * Returns the SQL with comments replaced by whitespace.
     */
    private static String stripComments(String sql) {
        StringBuilder sb = new StringBuilder(sql.length());
        boolean inSingleQuote = false;
        boolean inDoubleQuote = false;
        int i = 0;
        int len = sql.length();
        while (i < len) {
            char c = sql.charAt(i);
            // Track quoted state
            if (c == '\'' && !inDoubleQuote) {
                if (inSingleQuote && i + 1 < len && sql.charAt(i + 1) == '\'') {
                    sb.append("''");
                    i += 2;
                    continue;
                }
                inSingleQuote = !inSingleQuote;
                sb.append(c);
                i++;
                continue;
            }
            if (c == '"' && !inSingleQuote) {
                inDoubleQuote = !inDoubleQuote;
                sb.append(c);
                i++;
                continue;
            }
            if (!inSingleQuote && !inDoubleQuote) {
                // Line comment: -- to end of line
                if (c == '-' && i + 1 < len && sql.charAt(i + 1) == '-') {
                    // Replace with space (preserve whitespace for token separation)
                    sb.append(' ');
                    i += 2;
                    while (i < len && sql.charAt(i) != '\n') i++;
                    continue;
                }
                // Block comment: /* ... */
                if (c == '/' && i + 1 < len && sql.charAt(i + 1) == '*') {
                    sb.append(' ');
                    i += 2;
                    while (i + 1 < len && !(sql.charAt(i) == '*' && sql.charAt(i + 1) == '/')) i++;
                    if (i + 1 < len) i += 2; // skip */
                    continue;
                }
            }
            // Inside string literals: replace newlines/tabs with spaces
            // (JSqlParser's lexer cannot handle these inside string literals)
            if (inSingleQuote && (c == '\n' || c == '\r' || c == '\t')) {
                sb.append(' ');
                i++;
                continue;
            }
            // Inside string literals: replace backslash escape sequences with spaces
            // psycopg2 sends \n \r \t as literal characters in SQL strings
            if (inSingleQuote && c == '\\' && i + 1 < len) {
                char next = sql.charAt(i + 1);
                if (next == 'n' || next == 'r' || next == 't') {
                    sb.append(' ');
                    i += 2;
                    continue;
                }
            }
            sb.append(c);
            i++;
        }
        return sb.toString().trim();
    }

    // ========================================================================
    // Extended Query protocol
    //
    // Flow: Parse(name, sql, paramOids[]) → PreparedStmt cache entry.
    //       Bind(portal, stmt, formats[], values[], resultFormats[])
    //          → decode each param via PgParamCodec → Portal cache entry.
    //       Describe('S', name) → ParameterDescription + RowDescription/NoData.
    //       Describe('P', name) → RowDescription/NoData.
    //       Execute(portal, maxRows) → run the cached statement with bound args.
    //       Close('S'|'P', name)   → drop from cache.
    //       Sync → RFQ, error-state clear.
    //
    // Names: "" is the unnamed statement/portal, replaceable on each
    // Parse/Bind. Named entries persist until Close.
    // ========================================================================

    /**
     * Wire-level trace. Set DATAHIKE_WIRE_DEBUG in the environment to
     * stream every extended-query send/recv to stderr; indispensable
     * for diagnosing pgJDBC / asyncpg state-machine divergence.
     */
    private static final boolean WIRE_TRACE = System.getenv("DATAHIKE_WIRE_DEBUG") != null;

    private static void trace(String msg) {
        if (WIRE_TRACE) System.err.println("[WIRE] " + msg);
    }

    private void handleParse(byte[] body, DataOutputStream out,
                             java.util.Map<String, PreparedStmt> statements,
                             QueryHandler handler) throws IOException {
        ByteBuffer buf = ByteBuffer.wrap(body);
        String stmtName = readCString(buf);
        String rawQuery = readCString(buf);
        short numParamOids = buf.getShort();
        int[] paramOids = new int[numParamOids];
        for (int i = 0; i < numParamOids; i++) paramOids[i] = buf.getInt();

        // Strip SQL comments and trim. The simple-query path runs
        // splitStatements + stripComments; the extended-query path
        // (this method) historically passed the raw query to
        // JSqlParser, which chokes on trailing line comments after
        // a `;`. Mirroring stripComments here closes that gap so
        // pgjdbc's Statement.execute (which always uses extended
        // query for single statements) accepts the same SQL psql does.
        String query = rawQuery.isEmpty() ? rawQuery : stripComments(rawQuery);

        if (WIRE_TRACE) {
            trace("recv Parse stmt=\"" + stmtName + "\" paramOids="
                  + java.util.Arrays.toString(paramOids) + " sql="
                  + (query.length() > 80 ? query.substring(0, 80) + "..." : query));
        }

        // Empty query: pgJDBC's Connection.isValid sends an empty-SQL
        // prepared statement as a ping. Don't try to translate — store
        // a PreparedStmt with parsed=null and emit EmptyQueryResponse
        // at Execute time.
        Object parsed = query.isEmpty() ? null : handler.parse(query, paramOids);
        statements.put(stmtName, new PreparedStmt(query, paramOids, parsed));

        out.writeByte('1'); // ParseComplete
        out.writeInt(4);
        out.flush();
    }

    private void handleBind(byte[] body, DataOutputStream out,
                            java.util.Map<String, PreparedStmt> statements,
                            java.util.Map<String, Portal> portals) throws IOException {
        ByteBuffer buf = ByteBuffer.wrap(body);
        String portalName = readCString(buf);
        String stmtName   = readCString(buf);
        trace("recv Bind portal=\"" + portalName + "\" stmt=\"" + stmtName + "\"");

        PreparedStmt stmt = statements.get(stmtName);
        if (stmt == null) {
            throw new PgProtocolException("26000",
                "prepared statement \"" + stmtName + "\" does not exist");
        }

        // Parameter format codes: 0 codes = all text, 1 code = apply to
        // all, N codes = one per param.
        short numFormatCodes = buf.getShort();
        short[] formatCodes = new short[numFormatCodes];
        for (int i = 0; i < numFormatCodes; i++) formatCodes[i] = buf.getShort();

        short numParams = buf.getShort();
        // 1-indexed so ParamRef{idx=N} → bound[N]. Length = numParams + 1,
        // index 0 stays null.
        Object[] bound = new Object[numParams + 1];
        for (int i = 0; i < numParams; i++) {
            int paramLen = buf.getInt();
            if (paramLen == -1) {
                bound[i + 1] = null; // SQL NULL
                continue;
            }
            byte[] bytes = new byte[paramLen];
            buf.get(bytes);
            short format = (numFormatCodes == 0) ? 0
                         : (numFormatCodes == 1) ? formatCodes[0]
                         : formatCodes[i];
            int oid = (i < stmt.paramOids.length) ? stmt.paramOids[i] : 0;
            // OID 0 means the client didn't specify a type. We treat it
            // as text and let the downstream coerce (same behaviour as
            // the prior text-interpolation path).
            if (oid == 0) {
                bound[i + 1] = new String(bytes, StandardCharsets.UTF_8);
            } else {
                bound[i + 1] = PgParamCodec.decode(oid, format, bytes);
            }
        }

        // Result format codes. 0 = text, 1 = binary, per-column (or one
        // code applying to all, or zero codes for all-text). Captured on
        // the Portal so sendDataRow can encode each column as the client
        // requested — matching the format field we advertise in
        // RowDescription at Execute time.
        short[] resultFormats;
        if (buf.hasRemaining()) {
            short numResultFormats = buf.getShort();
            resultFormats = new short[numResultFormats];
            for (int i = 0; i < numResultFormats; i++) resultFormats[i] = buf.getShort();
        } else {
            resultFormats = new short[0];
        }

        portals.put(portalName, new Portal(stmt, bound, resultFormats));

        out.writeByte('2'); // BindComplete
        out.writeInt(4);
        out.flush();
        trace("send BindComplete");
    }

    private void handleDescribe(byte[] body, DataOutputStream out,
                                java.util.Map<String, PreparedStmt> statements,
                                java.util.Map<String, Portal> portals,
                                QueryHandler handler) throws IOException {
        ByteBuffer buf = ByteBuffer.wrap(body);
        byte descType = buf.get();  // 'S' = statement, 'P' = portal
        String name = readCString(buf);
        trace("recv Describe '" + (char) descType + "' name=\"" + name + "\"");

        PreparedStmt stmt;
        if (descType == 'S') {
            stmt = statements.get(name);
            if (stmt == null) {
                throw new PgProtocolException("26000",
                    "prepared statement \"" + name + "\" does not exist");
            }
            // Statement Describe always starts with ParameterDescription.
            int[] oids = (stmt.parsed == null)
                ? stmt.paramOids
                : handler.describeParams(stmt.parsed);
            if (oids == null) oids = stmt.paramOids;
            out.writeByte('t');
            out.writeInt(4 + 2 + oids.length * 4);
            out.writeShort(oids.length);
            for (int oid : oids) out.writeInt(oid);
        } else if (descType == 'P') {
            Portal p = portals.get(name);
            if (p == null) {
                throw new PgProtocolException("34000",
                    "portal \"" + name + "\" does not exist");
            }
            stmt = p.stmt;
        } else {
            throw new PgProtocolException("08P01",
                "invalid Describe type: " + (char) descType);
        }

        // Empty-query statement: emit NoData regardless of type.
        QueryResult meta = (stmt.parsed == null) ? null
                                                 : handler.describeResult(stmt.parsed);
        if (meta == null || meta.columnNames == null || meta.columnNames.length == 0) {
            out.writeByte('n'); // NoData
            out.writeInt(4);
            trace("send NoData");
        } else {
            sendRowDescription(out, meta.columnNames, meta.columnOids,
                              meta.columnTableOids, meta.columnAttnums,
                              meta.columnTypmods, null);
            trace("send RowDescription cols=" + meta.columnNames.length);
            // Mark BOTH the portal and the underlying statement as
            // described. pgJDBC caches row metadata per-statement: once
            // it has seen RowDescription for a server-side prepared
            // statement, it expects subsequent re-Binds + Executes of
            // the same statement to skip RowDescription (it parses the
            // responses against the cached metadata). Sending a
            // duplicate RowDescription on iteration 2+ leaves pgJDBC's
            // response queue empty when it tries to consume the (not-
            // expected) RowDescription — manifests as NoSuchElement on
            // its internal ArrayDeque. Mirrors PG backend behavior:
            // once the frontend has received metadata for a statement,
            // the backend doesn't resend it on re-Execute.
            stmt.described = true;
            if (descType == 'P') {
                Portal p = portals.get(name);
                if (p != null) p.described = true;
            }
        }
        out.flush();
    }

    private void handleExecuteMsg(byte[] body, DataOutputStream out,
                                  java.util.Map<String, Portal> portals,
                                  char[] txStatus,
                                  QueryHandler handler,
                                  int[] copyState) throws IOException {
        ByteBuffer buf = ByteBuffer.wrap(body);
        String portalName = readCString(buf);
        int maxRows = buf.getInt();
        trace("recv Execute portal=\"" + portalName + "\" maxRows=" + maxRows);

        Portal portal = portals.get(portalName);
        if (portal == null) {
            throw new PgProtocolException("34000",
                "portal \"" + portalName + "\" does not exist");
        }

        // Empty-query prepared statement (pgJDBC isValid ping): emit
        // EmptyQueryResponse instead of running anything. Matches PG:
        // src/backend/tcop/postgres.c — exec_execute_message, where an
        // empty portal produces 'I' and clients know it's a no-op.
        if (portal.stmt.parsed == null) {
            sendEmptyQueryResponse(out);
            out.flush();
            return;
        }

        QueryResult result = handler.executePrepared(portal.stmt.parsed, portal.boundParams);
        // Clear any interrupt bit left by the safety-net cancel path.
        Thread.interrupted();

        if (result != null && result.txStatus != 'I') {
            txStatus[0] = result.txStatus;
        }

        if (result == null) {
            // Shouldn't happen — handler contract returns a QueryResult.
            sendCommandComplete(out, "SELECT 0");
        } else if (result.error != null) {
            // Propagate as a protocol exception so the connection loop
            // sends ErrorResponse, transitions T→E, and enters extended-
            // query error-skip until the next Sync.
            throw new PgProtocolException(
                result.sqlstate != null ? result.sqlstate : "XX000",
                result.error,
                result.errorFields);
        } else if (result.copyInMode) {
            // Extended-Query path also supports COPY-IN — though in
            // practice clients use Simple Query for COPY. We send
            // CopyInResponse and trip copyState; the next message
            // will be a 'd'/'c'/'f' which the outer loop routes.
            sendCopyInResponse(out, result.copyColumnCount);
            out.flush();
            copyState[0] = 1;
            return;
        } else if (result.columnNames.length == 0) {
            sendCommandComplete(out, result.commandTag);
            trace("send CommandComplete \"" + result.commandTag + "\"");
        } else {
            // PG contract: if Describe has already emitted RowDescription
            // (either on the statement or the portal), Execute sends only
            // DataRows + CommandComplete. Sending a duplicate leaves
            // pgJDBC's response queue confused (NoSuchElementException
            // on its internal ArrayDeque).
            boolean alreadyDescribed = portal.described || portal.stmt.described;
            if (!alreadyDescribed) {
                sendRowDescription(out, result.columnNames, result.columnOids,
                                  result.columnTableOids, result.columnAttnums,
                                  result.columnTypmods,
                                  portal.resultFormats);
                trace("send RowDescription cols=" + result.columnNames.length);
            } else {
                trace("skip RowDescription (already described: portal=" + portal.described
                      + " stmt=" + portal.stmt.described + ")");
            }
            for (String[] row : result.rows) {
                sendDataRow(out, row, result.columnOids, portal.resultFormats);
            }
            trace("send DataRow x" + result.rows.length);
            sendCommandComplete(out, result.commandTag);
            trace("send CommandComplete \"" + result.commandTag + "\"");
        }

        // Per PG spec, unnamed portals persist until end-of-transaction
        // (typically the next Sync in autocommit mode) and are
        // implicitly overwritten by subsequent Binds. We don't eagerly
        // clean up here — the next Bind to the unnamed name replaces
        // the entry, and Sync can do broader cleanup if needed.

        out.flush();
    }

    private void handleSync(DataOutputStream out, char[] txStatus) throws IOException {
        trace("recv Sync → send RFQ '" + txStatus[0] + "'");
        sendReadyForQuery(out, txStatus[0]);
        out.flush();
    }

    private void handleClose(byte[] body, DataOutputStream out,
                             java.util.Map<String, PreparedStmt> statements,
                             java.util.Map<String, Portal> portals) throws IOException {
        ByteBuffer buf = ByteBuffer.wrap(body);
        byte kind = buf.get();
        String name = readCString(buf);
        trace("recv Close '" + (char) kind + "' name=\"" + name + "\"");
        if (kind == 'S') statements.remove(name);
        else if (kind == 'P') portals.remove(name);
        out.writeByte('3');
        out.writeInt(4);
        out.flush();
        trace("send CloseComplete");
    }

    // ========================================================================
    // Protocol message builders
    // ========================================================================

    private void sendParameterStatus(DataOutputStream out, String name, String value) throws IOException {
        byte[] nameBytes = name.getBytes(StandardCharsets.UTF_8);
        byte[] valueBytes = value.getBytes(StandardCharsets.UTF_8);
        int len = 4 + nameBytes.length + 1 + valueBytes.length + 1;

        out.writeByte('S');
        out.writeInt(len);
        out.write(nameBytes);
        out.writeByte(0);
        out.write(valueBytes);
        out.writeByte(0);
        // No flush here — caller flushes after all messages are written
    }

    private void sendReadyForQuery(DataOutputStream out, char status) throws IOException {
        out.writeByte('Z');
        out.writeInt(5);
        out.writeByte(status);
    }

    /**
     * Pick the per-column format code for result column {@code i} from
     * the client-requested {@code formats} array. PG convention: empty
     * = all text (0), length 1 = apply to every column, length N = one
     * per column.
     */
    private static short formatFor(short[] formats, int i) {
        if (formats == null || formats.length == 0) return 0;
        if (formats.length == 1) return formats[0];
        return formats[i];
    }

    private void sendRowDescription(DataOutputStream out, String[] names, int[] oids,
                                    int[] tableOids, short[] attnums,
                                    int[] typmods,
                                    short[] formats) throws IOException {
        int bodyLen = 2;
        byte[][] nameBytes = new byte[names.length][];
        for (int i = 0; i < names.length; i++) {
            nameBytes[i] = names[i].getBytes(StandardCharsets.UTF_8);
            bodyLen += nameBytes[i].length + 1 + 4 + 2 + 4 + 2 + 4 + 2;
        }

        out.writeByte('T');
        out.writeInt(4 + bodyLen);
        out.writeShort(names.length);

        for (int i = 0; i < names.length; i++) {
            out.write(nameBytes[i]);
            out.writeByte(0);
            out.writeInt(tableOids != null && i < tableOids.length ? tableOids[i] : 0);
            out.writeShort(attnums  != null && i < attnums.length  ? attnums[i]  : 0);
            out.writeInt(oids[i]);    // type OID
            out.writeShort(typeSize(oids[i]));
            out.writeInt(typmods != null && i < typmods.length ? typmods[i] : -1);  // type modifier
            out.writeShort(formatFor(formats, i));  // format (0=text, 1=binary)
        }

    }

    /**
     * Send a DataRow. When {@code formats} requests binary for a column
     * and {@link PgParamCodec#encodeBinary} supports the OID, encode
     * binary; otherwise fall back to UTF-8 text (the caller's already-
     * stringified value, matching the text format).
     */
    private void sendDataRow(DataOutputStream out, String[] values, int[] oids, short[] formats) throws IOException {
        int n = values.length;
        byte[][] valBytes = new byte[n][];
        int bodyLen = 2;
        for (int i = 0; i < n; i++) {
            if (values[i] == null) {
                valBytes[i] = null;
                bodyLen += 4;
                continue;
            }
            short fmt = formatFor(formats, i);
            byte[] bytes = null;
            if (fmt == 1 && oids != null && i < oids.length) {
                // Try binary encode. PgParamCodec returns null when it
                // doesn't know how to encode the OID — fall back to text.
                bytes = PgParamCodec.encodeBinary(oids[i], values[i]);
                if (bytes == null) bytes = values[i].getBytes(StandardCharsets.UTF_8);
            } else {
                bytes = values[i].getBytes(StandardCharsets.UTF_8);
            }
            valBytes[i] = bytes;
            bodyLen += 4 + bytes.length;
        }

        out.writeByte('D');
        out.writeInt(4 + bodyLen);
        out.writeShort(n);
        for (byte[] vb : valBytes) {
            if (vb == null) {
                out.writeInt(-1);
            } else {
                out.writeInt(vb.length);
                out.write(vb);
            }
        }
    }

    /** Text-only overload (Simple Query path). */
    private void sendDataRow(DataOutputStream out, String[] values) throws IOException {
        sendDataRow(out, values, null, null);
    }

    private void sendCommandComplete(DataOutputStream out, String tag) throws IOException {
        byte[] tagBytes = tag.getBytes(StandardCharsets.UTF_8);
        out.writeByte('C');
        out.writeInt(4 + tagBytes.length + 1);
        out.write(tagBytes);
        out.writeByte(0);

    }

    /**
     * Emit CopyInResponse ('G') with the given column count. We always
     * declare overall format = 0 (text) and per-column format = 0
     * (text) for every column — pg-datahike's wire path doesn't yet
     * support binary COPY.
     *
     *   Byte1('G') | Int32(length) | Int8(format) | Int16(numColumns) |
     *     Int16[numColumns](perColumnFormatCodes)
     *
     * Spec: protocol.sgml:636-672.
     */
    private void sendCopyInResponse(DataOutputStream out, int numColumns) throws IOException {
        // 4 (length) + 1 (overall format) + 2 (numColumns) + 2*numColumns (formats)
        int payloadLen = 4 + 1 + 2 + (2 * numColumns);
        out.writeByte('G');
        out.writeInt(payloadLen);
        out.writeByte(0);                       // overall format: 0 = text
        out.writeShort((short) numColumns);
        for (int i = 0; i < numColumns; i++) {
            out.writeShort((short) 0);          // per-column format: 0 = text
        }
    }

    /**
     * Decode a CopyFail message body. Per protocol.sgml:670-682, the
     * body is a single C-string with an optional human-readable
     * reason.
     */
    private String readCopyFailReason(byte[] body) {
        // Strip trailing NUL byte if present
        int end = body.length;
        while (end > 0 && body[end - 1] == 0) end--;
        return new String(body, 0, end, StandardCharsets.UTF_8);
    }

    private void sendError(DataOutputStream out, String severity, String code, String message) throws IOException {
        sendError(out, severity, code, message, null);
    }

    /**
     * Send an ErrorResponse. Always emits S (severity), C (SQLSTATE), M
     * (message); {@code extras} contributes zero or more additional
     * fields keyed by single-byte type code per PG protocol:
     *
     * <ul>
     *   <li>"V" non-localized severity</li>
     *   <li>"D" detail (free text, extra info)</li>
     *   <li>"H" hint (suggested fix for user)</li>
     *   <li>"P" original position (character offset in query)</li>
     *   <li>"p" internal position</li>
     *   <li>"q" internal query (e.g. for failed-inside-function errors)</li>
     *   <li>"W" where (stack context)</li>
     *   <li>"s" schema, "t" table, "c" column — ORMs use these to map
     *       violations to user-facing field names</li>
     *   <li>"d" data type</li>
     *   <li>"n" constraint name — unique/FK violations</li>
     *   <li>"F" file, "L" line, "R" routine (PG backend source location)</li>
     * </ul>
     *
     * Field order follows PG's convention: S / V / C / M / D / H / P /
     * p / q / W / s / t / c / d / n / F / L / R. We emit whatever is
     * present in {@code extras}; all are optional except the three
     * required fields above.
     */
    private void sendError(DataOutputStream out, String severity, String code, String message,
                           java.util.Map<String, String> extras) throws IOException {
        if (WIRE_TRACE) {
            trace("send ERROR severity=" + severity + " code=" + code
                  + " msg=" + message + (extras == null ? "" : " extras=" + extras));
        }

        // Build in PG's conventional field order so any downstream parser
        // that assumes ordering (wire-level log tools) reads what it
        // expects. S / V / C / M / D / H / P / p / q / W / s / t / c / d
        // / n / F / L / R.
        java.util.List<byte[]> fieldBytes = new java.util.ArrayList<>(16);
        java.util.List<Byte> fieldCodes = new java.util.ArrayList<>(16);

        fieldCodes.add((byte) 'S'); fieldBytes.add(severity.getBytes(StandardCharsets.UTF_8));
        fieldCodes.add((byte) 'V'); fieldBytes.add(severity.getBytes(StandardCharsets.UTF_8));
        fieldCodes.add((byte) 'C'); fieldBytes.add(code.getBytes(StandardCharsets.UTF_8));
        fieldCodes.add((byte) 'M'); fieldBytes.add(message.getBytes(StandardCharsets.UTF_8));

        if (extras != null) {
            for (String key : new String[] {"D", "H", "P", "p", "q", "W", "s", "t", "c", "d", "n", "F", "L", "R"}) {
                String v = extras.get(key);
                if (v != null) {
                    fieldCodes.add((byte) key.charAt(0));
                    fieldBytes.add(v.getBytes(StandardCharsets.UTF_8));
                }
            }
        }

        int bodyLen = 0;
        for (byte[] b : fieldBytes) bodyLen += 1 + b.length + 1;
        bodyLen += 1;  // trailing zero byte

        out.writeByte('E');
        out.writeInt(4 + bodyLen);
        for (int i = 0; i < fieldCodes.size(); i++) {
            out.writeByte(fieldCodes.get(i));
            out.write(fieldBytes.get(i));
            out.writeByte(0);
        }
        out.writeByte(0);  // end of fields
    }

    private void sendEmptyQueryResponse(DataOutputStream out) throws IOException {
        out.writeByte('I');
        out.writeInt(4);

    }

    // ========================================================================
    // Helpers
    // ========================================================================

    private static short typeSize(int oid) {
        return switch (oid) {
            case OID_BOOL -> 1;
            case OID_INT4 -> 4;
            case OID_INT8 -> 8;
            case OID_FLOAT4 -> 4;
            case OID_FLOAT8 -> 8;
            case OID_TEXT, OID_VARCHAR -> -1;
            case OID_DATE -> 4;
            case OID_TIMESTAMP -> 8;
            case OID_UUID -> 16;
            default -> -1;
        };
    }

    private static String readCString(ByteBuffer buf) {
        int start = buf.position();
        while (buf.get() != 0) {}
        int end = buf.position() - 1;
        byte[] bytes = new byte[end - start];
        buf.position(start);
        buf.get(bytes);
        buf.get(); // skip null terminator
        return new String(bytes, StandardCharsets.UTF_8);
    }
}
