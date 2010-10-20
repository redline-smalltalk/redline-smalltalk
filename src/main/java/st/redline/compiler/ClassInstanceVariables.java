package st.redline.compiler;

public class ClassInstanceVariables {

	private final Token subclass;
	private final Token instanceVariableNames;

	public ClassInstanceVariables(Token subclass, Token instanceVariableNames) {
		this.subclass = subclass;
		this.instanceVariableNames = instanceVariableNames;
	}

	public String[] names() {
		return instanceVariableNames.toString().split(" ");
	}
}
