package st.redline.compiler;

public class ReferenceToVariable extends Variable {

	private final Variable referencedVariable;
	private Token reference;

	public ReferenceToVariable(Variable referencedVariable, Token reference) {
		super(null, 0);
		this.referencedVariable = referencedVariable;
		this.reference = reference;
	}

	public String toString() {
		return referencedVariable.toString();
	}

	public Variable referencedVariable() {
		return referencedVariable;
	}

	public int lineNumber() {
		return reference.beginLine;
	}
}
