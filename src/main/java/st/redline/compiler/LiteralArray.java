package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class LiteralArray extends Literal {

	private final List<ArrayLiteral> arrayLiterals;

	public LiteralArray(ArrayLiteral arrayLiteral) {
		this.arrayLiterals = new ArrayList<ArrayLiteral>();
		add(arrayLiteral);
	}

	public void add(ArrayLiteral arrayLiteral) {
		arrayLiterals.add(arrayLiteral);
	}
}
