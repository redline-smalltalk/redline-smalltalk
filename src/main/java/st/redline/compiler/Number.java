package st.redline.compiler;

public class Number {

	private final Token number;
	private final boolean negative;

	public Number(Token number, boolean negative) {
		this.number = number;
		this.negative = negative;
	}

	public int lineNumber() {
		return number.beginLine;
	}

	public String toString() {
		return (negative ? "-" + number.toString() : number.toString()); 
	}
}
