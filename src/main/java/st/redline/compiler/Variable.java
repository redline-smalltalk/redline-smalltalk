package st.redline.compiler;

public class Variable {

	private final Token name;
	private boolean isClass;
	private boolean isEnvironment;
	private int offset;

	public Variable(Token name, int offset) {
		this.name = name;
		this.offset = offset;
	}

	public String toString() {
		return name.toString();
	}

	public Token token() {
		return name;
	}

	public int lineNumber() {
		return name.beginLine;
	}

	public int offset() {
		return offset;
	}

	public void isClass(boolean isClass) {
		this.isClass = isClass;
	}

	public boolean isClass() {
		return isClass;
	}

	public void isEnvironment(boolean isEnvironment) {
		this.isEnvironment = isEnvironment;
	}

	public boolean isEnvironment() {
		return isEnvironment;
	}

	public boolean isTemporary() {
		return false;
	}

	public boolean isArgument() {
		return false;
	}
}