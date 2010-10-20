package st.redline.compiler;

public class PrimaryExpression extends Primary {

	private final Expression expression;

	public PrimaryExpression(Expression expression) {
		this.expression = expression;
	}
}
