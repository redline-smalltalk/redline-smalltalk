package st.redline.compiler;

public class Variable {

	private final Token name;
	private boolean isClass;
	private boolean isEnvironment;
	private boolean isTemporary;

	public Variable(Token name) {
		this(name, false);
	}

	public Variable(Token name, boolean isTemporary) {
		this.name = name;
		this.isTemporary = isTemporary;
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
		return isTemporary;
	}
}