package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class StringLiteral extends Literal {

	private final List<Token> strings;

	public StringLiteral(Token token) {
		strings = new ArrayList<Token>();
		add(token);
	}

	public String toString() {
		String result = "";
		for (Token string : strings)
			result = result + ' ' + string.toString();
		return result.substring(1);
	}

	public void add(Token token) {
		strings.add(token);
	}

	public Token token() {
		return strings.get(0);
	}

	public int lineNumber() {
		return strings.get(0).beginLine;
	}
}
