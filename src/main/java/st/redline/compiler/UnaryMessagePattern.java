package st.redline.compiler;

public class UnaryMessagePattern extends MessagePattern {

	public UnaryMessagePattern(Token name) {
		super(name);
	}

	@Override
	int argumentCount() {
		return 0;
	}
}
