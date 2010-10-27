package st.redline.compiler;

public class Pragma {

	private final Primitive primitive;

	public Pragma(Primitive primitive) {
		this.primitive = primitive;
	}

	public Primitive primitive() {
		return primitive;
	}
}
