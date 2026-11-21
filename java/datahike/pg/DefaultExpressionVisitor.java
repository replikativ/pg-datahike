package datahike.pg;

import java.util.Locale;
import java.util.Set;
import java.util.function.Consumer;
import net.sf.jsqlparser.expression.AnalyticExpression;
import net.sf.jsqlparser.expression.ExpressionVisitorAdapter;
import net.sf.jsqlparser.expression.Function;
import net.sf.jsqlparser.expression.JsonAggregateFunction;
import net.sf.jsqlparser.schema.Column;
import net.sf.jsqlparser.statement.select.ParenthesedSelect;
import net.sf.jsqlparser.statement.select.Select;

/** Walks DEFAULT expressions without evaluating them. */
public final class DefaultExpressionVisitor extends ExpressionVisitorAdapter<Void> {
    private static final Set<String> SQL_VALUE_NAMES = Set.of(
            "current_catalog", "current_date", "current_role", "current_schema",
            "current_time", "current_timestamp", "current_user", "localtime",
            "localtimestamp", "session_user", "user");

    private final Consumer<String> reject;
    private final Consumer<Function> checkFunction;

    public DefaultExpressionVisitor(Consumer<String> reject, Consumer<Function> checkFunction) {
        this.reject = reject;
        this.checkFunction = checkFunction;
    }

    @Override
    public <S> Void visit(Column column, S context) {
        if (column.getTable() != null
                || !SQL_VALUE_NAMES.contains(column.getColumnName().toLowerCase(Locale.ROOT))) {
            reject.accept("column");
        }
        return null;
    }

    @Override
    public <S> Void visit(Function function, S context) {
        super.visit(function, context);
        checkFunction.accept(function);
        return null;
    }

    @Override
    public <S> Void visit(AnalyticExpression expression, S context) {
        reject.accept("window");
        return null;
    }

    @Override
    public <S> Void visit(JsonAggregateFunction expression, S context) {
        super.visit(expression, context);
        reject.accept("aggregate");
        return null;
    }

    @Override
    public <S> Void visit(ParenthesedSelect expression, S context) {
        reject.accept("subquery");
        return null;
    }

    @Override
    public <S> Void visit(Select expression, S context) {
        reject.accept("subquery");
        return null;
    }
}
