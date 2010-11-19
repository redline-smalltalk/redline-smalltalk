package st.redline.compiler;

public class AssignmentExpression extends Expression {

	private final Variable variable;
	private final Expression expression;

	public AssignmentExpression(boolean answered, Variable variable, Expression expression) {
		super(answered);
		this.variable = variable;
		this.expression = expression;
	}

	public Variable variable() {
		return variable;
	}

	public Expression expression() {
		return expression;
	}

	public int lineNumber() {
		return expression.lineNumber();
	}
}