package st.redline.compiler;

public abstract class MessagePattern {

	private final Token pattern;

	abstract int argumentCount();

	public MessagePattern(Token pattern) {
		this.pattern = pattern;
	}

	public int lineNumber() {
		return pattern.beginLine;
	}

	public String pattern() {
		return pattern.toString();
	}
}
