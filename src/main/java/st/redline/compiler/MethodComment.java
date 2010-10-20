package st.redline.compiler;

public class MethodComment {

	private final Token subclass;
	private final boolean forClass;
	private final Token comment;
	private final Token version;

	public MethodComment(Token subclass, boolean forClass, Token comment, Token version) {
		this.subclass = subclass;
		this.forClass = forClass;
		this.comment = comment;
		this.version = version;
	}

	public boolean isForClass() {
		return forClass;
	}
}