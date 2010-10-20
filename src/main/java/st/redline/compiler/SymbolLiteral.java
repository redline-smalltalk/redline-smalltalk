package st.redline.compiler;

public class SymbolLiteral extends Literal {

	private final Token token;

	public SymbolLiteral(Token token) {
		this.token = token;
	}

	public String toString() {
		return token.toString();
	}

	public int lineNumber() {
		return token.beginLine;
	}
}
