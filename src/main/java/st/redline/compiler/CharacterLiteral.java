package st.redline.compiler;

public class CharacterLiteral extends Literal {

	private final Token token;

	public CharacterLiteral(Token token) {
		this.token = token;
	}
}
