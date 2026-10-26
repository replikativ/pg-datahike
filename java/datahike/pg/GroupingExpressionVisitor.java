package datahike.pg;

import java.util.Collection;
import java.util.Objects;
import java.util.function.Consumer;
import net.sf.jsqlparser.expression.AnalyticExpression;
import net.sf.jsqlparser.expression.Expression;
import net.sf.jsqlparser.expression.ExpressionVisitorAdapter;
import net.sf.jsqlparser.expression.Function;
import net.sf.jsqlparser.expression.JsonAggregateFunction;
import net.sf.jsqlparser.expression.JsonFunction;

/** Keeps grouping validation in control when wrapper visitors call child.accept directly. */
public final class GroupingExpressionVisitor extends ExpressionVisitorAdapter<Void> {
    private final Consumer<Expression> forward;

    public GroupingExpressionVisitor(Consumer<Expression> forward) {
        this.forward = Objects.requireNonNull(forward);
    }

    @Override
    public <S> Void visit(Function expression, S context) {
        forward.accept(expression);
        return null;
    }

    @Override
    public <S> Void visit(AnalyticExpression expression, S context) {
        forward.accept(expression);
        return null;
    }

    @Override
    public <S> Void visit(JsonFunction expression, S context) {
        forward.accept(expression);
        return null;
    }

    @Override
    public <S> Void visit(JsonAggregateFunction expression, S context) {
        forward.accept(expression);
        return null;
    }

    @Override
    protected <S> Void visitExpressions(Expression expression, S context,
                                        Collection<Expression> children) {
        children.forEach(forward);
        return null;
    }
}
