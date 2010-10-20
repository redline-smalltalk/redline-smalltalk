package st.redline.compiler;

public class BinaryMessagePattern extends MessagePattern {

	private final Variable variable;

	public BinaryMessagePattern(Token name, Variable variable) {
		super(name);
		this.variable = variable;
	}

	@Override
	int argumentCount() {
		return 1;
	}
}
