package datahike.pg;

import java.util.Map;
import java.util.function.Function;

import net.sf.jsqlparser.expression.ArrayConstructor;
import net.sf.jsqlparser.expression.CollateExpression;
import net.sf.jsqlparser.expression.Expression;
import net.sf.jsqlparser.expression.JdbcParameter;
import net.sf.jsqlparser.expression.JsonAggregateFunction;
import net.sf.jsqlparser.expression.JsonExpression;
import net.sf.jsqlparser.expression.JsonFunction;
import net.sf.jsqlparser.expression.OverlapsCondition;
import net.sf.jsqlparser.expression.operators.relational.IsDistinctExpression;
import net.sf.jsqlparser.schema.Column;
import net.sf.jsqlparser.statement.select.ParenthesedSelect;
import net.sf.jsqlparser.statement.select.Select;
import net.sf.jsqlparser.util.deparser.ExpressionDeParser;

/**
 * Deparses an expression back to SQL with every column reference replaced
 * by the text a function returns for it, failing closed on shapes whose
 * columns it cannot reach (Unsupported).
 *
 * Used to ANALYSE expressions, not to evaluate them: DDL reads the columns
 * a CHECK references (constraint naming) and refuses subqueries in CHECK
 * and domain constraints; row-level evaluation (row-eval) quotes a
 * domain's VALUE so its text parses again. Evaluation itself goes through
 * the SELECT translator, whose own scoping binds column references.
 *
 * A subclass rather than a Clojure proxy: the deparser visits nested
 * expressions through {@code this}, and proxy-super disables the override
 * for exactly those nested calls.
 */
public final class ColumnSubstitutingDeParser extends ExpressionDeParser {

    /** An expression shape this deparser cannot substitute columns in. */
    public static final class Unsupported extends RuntimeException {
        public Unsupported(String shape) {
            super(shape);
        }
    }

    private final Function<Column, String> replacement;

    private ColumnSubstitutingDeParser(Function<Column, String> replacement) {
        this.replacement = replacement;
    }

    @Override
    public <S> StringBuilder visit(Column column, S context) {
        ArrayConstructor subscript = column.getArrayConstructor();
        if (subscript == null) {
            return getBuilder().append(replacement.apply(column));
        }
        // `x[i]` parses as a Column carrying its subscript, whose index
        // expressions can themselves reference columns.
        getBuilder().append('(').append(replacement.apply(column)).append(")[");
        boolean first = true;
        for (Expression index : subscript.getExpressions()) {
            if (!first) getBuilder().append(',');
            first = false;
            index.accept(this, context);
        }
        return getBuilder().append(']');
    }

    /** `$n` always: the SELECT numbers its column parameters after these. */
    @Override
    public <S> StringBuilder visit(JdbcParameter parameter, S context) {
        if (parameter.getIndex() == null) {
            throw new Unsupported("an unnumbered parameter");
        }
        return getBuilder().append('$').append(parameter.getIndex());
    }

    // The shapes ExpressionDeParser renders with their operands' toString.

    @Override
    public <S> StringBuilder visit(IsDistinctExpression e, S context) {
        e.getLeftExpression().accept(this, context);
        getBuilder().append(e.isNot() ? " IS NOT DISTINCT FROM " : " IS DISTINCT FROM ");
        e.getRightExpression().accept(this, context);
        return getBuilder();
    }

    @Override
    public <S> StringBuilder visit(JsonExpression e, S context) {
        e.getExpression().accept(this, context);
        for (Map.Entry<Expression, String> ident : e.getIdentList()) {
            getBuilder().append(ident.getValue());
            ident.getKey().accept(this, context);
        }
        return getBuilder();
    }

    @Override
    public <S> StringBuilder visit(CollateExpression e, S context) {
        e.getLeftExpression().accept(this, context);
        return getBuilder().append(" COLLATE ").append(e.getCollate());
    }

    @Override
    public <S> StringBuilder visit(JsonFunction e, S context) {
        throw new Unsupported("JSON_OBJECT / JSON_ARRAY");
    }

    @Override
    public <S> StringBuilder visit(JsonAggregateFunction e, S context) {
        throw new Unsupported("a JSON aggregate");
    }

    @Override
    public <S> StringBuilder visit(OverlapsCondition e, S context) {
        throw new Unsupported("OVERLAPS");
    }

    @Override
    public <S> StringBuilder visit(Select select, S context) {
        throw new Unsupported("a subquery");
    }

    @Override
    public <S> StringBuilder visit(ParenthesedSelect select, S context) {
        throw new Unsupported("a subquery");
    }

    public static String deparse(Expression expression, Function<Column, String> replacement) {
        ColumnSubstitutingDeParser deParser = new ColumnSubstitutingDeParser(replacement);
        StringBuilder builder = new StringBuilder();
        deParser.setBuilder(builder);
        expression.accept(deParser, null);
        return builder.toString();
    }
}
