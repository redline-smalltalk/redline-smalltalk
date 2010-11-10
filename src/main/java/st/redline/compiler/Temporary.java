package st.redline.compiler;

public class Temporary extends Variable {

	public Temporary(Token name, int offset) {
		super(name, offset);
	}

	public boolean isTemporary() {
		return true;
	}
}
