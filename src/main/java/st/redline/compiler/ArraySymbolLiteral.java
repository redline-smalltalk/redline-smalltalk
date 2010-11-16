package st.redline.compiler;

public class ArraySymbolLiteral extends SymbolLiteral {

	private final LiteralArray literalArray;
	private final Token hash;

	public ArraySymbolLiteral(LiteralArray literalArray, Token hash) {
		super(null);
		this.literalArray = literalArray;
		this.hash = hash;
	}

	public int lineNumber() {
		return hash.beginLine;
	}

	public boolean hasElements() {
		return literalArray != null;
	}
}
