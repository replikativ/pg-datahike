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

    public static byte[] encodeBinary(int oid, Object value) {
        if (value == null) return null;
        try {
            return switch (oid) {
                case PgWireServer.OID_BOOL ->
                    new byte[] { (byte) (parseBool(value) ? 1 : 0) };

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

                case PgWireServer.OID_BYTEA ->
                    (value instanceof byte[] b) ? b : value.toString().getBytes(StandardCharsets.UTF_8);

                case PgWireServer.OID_JSONB -> {
                    byte[] text = value.toString().getBytes(StandardCharsets.UTF_8);
                    byte[] out  = new byte[text.length + 1];
                    out[0] = 1;  // jsonb binary format version
                    System.arraycopy(text, 0, out, 1, text.length);
                    yield out;
                }

                default -> null;   // caller falls back to text encoding
            };
        } catch (Exception e) {
            return null;  // fall back to text encoding
        }
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

            // Types we don't support in binary today: numeric (variable-length
            // NBASE=10000 digit encoding), interval (composite), arrays (every
            // element type × multi-dimensional), range types, network types,
            // composite/record types. Clients should send these in text format.
            default ->
                throw new PgWireServer.PgProtocolException("0A000",
                    "binary decoding not implemented for OID " + oid
                    + " (use text parameter format)");
        };
    }
}
