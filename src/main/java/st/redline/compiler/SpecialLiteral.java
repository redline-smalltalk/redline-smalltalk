package st.redline.compiler;

public class SpecialLiteral extends Literal {

	private final Token special;

	public SpecialLiteral(Token special) {
		this.special = special;
	}
}
