package st.redline.compiler;

public class ClassComment {

	private final Token subclass;
	private final Token comment;
	private final Token prior;

	public ClassComment(Token subclass, Token comment, Token prior) {
		this.subclass = subclass;
		this.comment = comment;
		this.prior = prior;
	}
}
