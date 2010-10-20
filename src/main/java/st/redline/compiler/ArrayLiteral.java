package st.redline.compiler;

public class ArrayLiteral extends Literal {

	private final Literal literal;
	private final Token token;

	public ArrayLiteral(Literal literal) {
		this.literal = literal;
		this.token = null;
	}

	public ArrayLiteral(Token token) {
		this.token = token;
		this.literal = null;
	}
}
