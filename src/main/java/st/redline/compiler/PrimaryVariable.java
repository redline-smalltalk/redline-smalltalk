package st.redline.compiler;

public class PrimaryVariable extends Primary {

	private final Variable variable;

	public PrimaryVariable(Variable variable) {
		this.variable = variable;
	}

	public String toString() {
		return variable.toString();
	}

	public int lineNumber() {
		return variable.lineNumber();
	}

	public Variable variable() {
		return variable;
	}
}
