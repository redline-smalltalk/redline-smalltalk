package st.redline.compiler;

public class Expression {

	private final boolean answered;
	private final Primary primary;
	private final Cascade cascade;

	public Expression(boolean answered) {
		this.answered = answered;
		this.primary = null;
		this.cascade = null;
	}

	public Expression(boolean answered, Primary primary) {
		this.answered = answered;
		this.primary = primary;
		this.cascade = null;
	}

	public Expression(boolean answered, Cascade cascade) {
		this.answered = answered;
		this.cascade = cascade;
		this.primary = null;
	}

	public boolean isAnswered() {
		return answered;
	}

	public boolean isPrimary() {
		return primary != null;
	}

	public Primary primary() {
		return primary;
	}

	public Cascade cascade() {
		return cascade;
	}

	public int lineNumber() {
		return primary.lineNumber();
	}
}
