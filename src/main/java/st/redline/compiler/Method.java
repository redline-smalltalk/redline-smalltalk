package st.redline.compiler;

import java.util.List;

public class Method {

	private MessagePattern messagePattern;
	private Pragma[] pragmas;
	private List<Variable> temporaries;
	private Statements statements;

	public void add(MessagePattern messagePattern) {
		this.messagePattern = messagePattern;
	}

	public void add(Pragma[] pragmas) {
		this.pragmas = pragmas;
	}

	public Pragma[] pragmas() {
		return pragmas;
	}

	public void add(List<Variable> temporaries) {
		this.temporaries = temporaries;
	}

	public List<Variable> temporaries() {
		return temporaries;
	}

	public void add(Statements statements) {
		this.statements = statements;
	}

	public Statements statements() {
		return statements;
	}

	public MessagePattern messagePattern() {
		return messagePattern;
	}
}