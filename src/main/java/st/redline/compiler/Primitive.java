package st.redline.compiler;

public class Primitive {

	private final Object firstPart;
	private final Token secondPart;

	public Primitive(Object firstPart) {
		this.firstPart = firstPart;
		this.secondPart = null;
	}

	public Primitive(Object firstPart, Token secondPart) {
		this.firstPart = firstPart;
		this.secondPart = secondPart;
	}
}