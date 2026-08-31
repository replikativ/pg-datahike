package datahike.pg;

/**
 * Allocation-free primitive kernels for the bounded primary vector scan.
 *
 * <p>The SQL type layer remains responsible for validating vectors and the
 * authoritative query rechecks the retained rows. These methods deliberately
 * mirror pgvector.clj's float32 accumulation order so candidate membership and
 * the visible distance calculation cannot drift at a top-k boundary.</p>
 */
public final class PgVectorMath {
    public static final int EUCLIDEAN = 0;
    public static final int INNER_PRODUCT = 1;
    public static final int COSINE = 2;

    private PgVectorMath() {}

    public static float squaredNorm(float[] vector) {
        float sum = 0.0f;
        for (int i = 0; i < vector.length; i++) {
            float value = vector[i];
            sum = (float) (sum + (float) (value * value));
        }
        return sum;
    }

    public static double distance(int metric, float[] stored, float[] query,
                                  float querySquaredNorm) {
        if (stored.length != query.length) {
            throw new IllegalArgumentException("different vector dimensions");
        }

        switch (metric) {
            case EUCLIDEAN:
                return Math.sqrt(l2Squared(stored, query));
            case INNER_PRODUCT:
                return -innerProduct(stored, query);
            case COSINE:
                return cosineDistance(stored, query, querySquaredNorm);
            default:
                throw new IllegalArgumentException("unknown vector metric: " + metric);
        }
    }

    private static double l2Squared(float[] left, float[] right) {
        float sum = 0.0f;
        for (int i = 0; i < left.length; i++) {
            float difference = (float) (left[i] - right[i]);
            sum = (float) (sum + (float) (difference * difference));
        }
        return (double) sum;
    }

    private static double innerProduct(float[] left, float[] right) {
        float sum = 0.0f;
        for (int i = 0; i < left.length; i++) {
            sum = (float) (sum + (float) (left[i] * right[i]));
        }
        return (double) sum;
    }

    private static double cosineDistance(float[] stored, float[] query,
                                         float querySquaredNorm) {
        float similarity = 0.0f;
        float storedSquaredNorm = 0.0f;
        for (int i = 0; i < stored.length; i++) {
            float x = stored[i];
            float y = query[i];
            similarity = (float) (similarity + (float) (x * y));
            storedSquaredNorm =
                (float) (storedSquaredNorm + (float) (x * x));
        }

        double normalized = (double) similarity
            / Math.sqrt((double) storedSquaredNorm
                        * (double) querySquaredNorm);
        if (Double.isNaN(normalized)) {
            return Double.NaN;
        }
        if (normalized > 1.0d) {
            return 0.0d;
        }
        if (normalized < -1.0d) {
            return 2.0d;
        }
        return 1.0d - normalized;
    }
}
