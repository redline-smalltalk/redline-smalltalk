package st.redline.compiler;

import java.util.List;

public class Block {

	private List<Variable> arguments;
	private List<Variable> temporaries;
	private Statements statements;

	public void addArguments(List<Variable> blockArguments) {
		arguments = blockArguments;
	}

	public void addTemporaries(List<Variable> blockTemporaries) {
		temporaries = blockTemporaries;
	}

	public void add(Statements statements) {
		this.statements = statements;
	}
}
