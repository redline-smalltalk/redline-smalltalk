package st.redline.compiler;

public class InstanceField extends Variable {

	private final boolean onInstance;

	public InstanceField(Token name, int offset, boolean onInstance) {
		super(name, offset);
		this.onInstance = onInstance;
	}

	public boolean isInstanceField() {
		return true;
	}

	public boolean onInstance() {
		return onInstance;
	}
}
