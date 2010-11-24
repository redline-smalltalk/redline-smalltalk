package st.redline.compiler;

import java.util.List;

public class Block {

	private final int identifier;

	private List<Variable> arguments;
	private List<Variable> temporaries;
	private Statements statements;
	private int lineNumber = 0;
	private boolean definedInClassMethod = false;

	public Block(int identifier) {
		this.identifier = identifier;
	}

	public void lineNumber(int lineNumber) {
		this.lineNumber = lineNumber;
	}

	public int lineNumber() {
		return lineNumber;
	}

	public int identifier() {
		return identifier;
	}

	public boolean definedInClassMethod() {
		return definedInClassMethod;
	}

	public void definedInClassMethod(boolean definedInClassMethod) {
		this.definedInClassMethod = definedInClassMethod;
	}

	public void addArguments(List<Variable> blockArguments) {
		arguments = blockArguments;
	}

	public void addTemporaries(List<Variable> blockTemporaries) {
		temporaries = blockTemporaries;
	}

	public void add(Statements statements) {
		this.statements = statements;
	}

	public boolean hasAnsweredExpression() {
		if (statements == null)
			return false;
		return statements.hasAnsweredExpression();
	}
}
