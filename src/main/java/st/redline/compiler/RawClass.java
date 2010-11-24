package st.redline.compiler;

public class RawClass {

	private final String name;
	private final byte[] bytes;

	public RawClass(String name, byte[] bytes) {
		this.name = name;
		this.bytes = bytes;
	}

	public String name() {
		return name;
	}

	public byte[] bytes() {
		return bytes;
	}
}
