package st.redline.compiler;

public class ArraySymbolLiteral extends SymbolLiteral {

	private final LiteralArray literalArray;

	public ArraySymbolLiteral(LiteralArray literalArray) {
		super(null);
		this.literalArray = literalArray;
	}
}
