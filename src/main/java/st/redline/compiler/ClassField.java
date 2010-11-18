package st.redline.compiler;

public class ClassField extends Variable {

	public ClassField(Token name, int offset) {
		super(name, offset);
	}

	public boolean isClassField() {
		return true;
	}
}
