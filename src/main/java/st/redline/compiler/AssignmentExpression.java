package st.redline.compiler;

public class AssignmentExpression extends Expression {

	private final Variable variable;
	private final Expression expression;

	public AssignmentExpression(boolean answered, Variable variable, Expression expression) {
		super(answered);
		this.variable = variable;
		this.expression = expression;
	}
}
