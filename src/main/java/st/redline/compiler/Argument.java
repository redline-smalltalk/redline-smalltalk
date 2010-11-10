package st.redline.compiler;

public class Argument extends Variable {

	public Argument(Token name, int offset) {
		super(name, offset);
	}

	public boolean isArgument() {
		return true;
	}
}
