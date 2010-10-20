package st.redline.compiler;

public class BinarySend extends MessageSend {

	private final Token binarySymbol;
	private final UnarySend unarySend;

	public BinarySend(Token binarySymbol, UnarySend unarySend) {
		this.binarySymbol = binarySymbol;
		this.unarySend = unarySend;
	}

	public Primary primary() {
		return unarySend.primary();
	}
}