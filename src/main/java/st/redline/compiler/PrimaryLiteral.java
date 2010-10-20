package st.redline.compiler;

public class PrimaryLiteral extends Primary {

	private final Literal literal;

	public PrimaryLiteral(Literal literal) {
		this.literal = literal;
	}

	public String toString() {
		return literal.toString();
	}

	public Token token() {
		return literal.token();
	}

	public Literal literal() {
		return literal;
	}
}
