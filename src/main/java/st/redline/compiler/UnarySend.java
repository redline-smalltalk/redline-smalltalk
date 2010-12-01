package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class UnarySend extends MessageSend {

	private final Primary primary;
	private final List<Token> selectors;

	public UnarySend(Primary primary) {
		this.primary = primary;
		this.selectors = new ArrayList<Token>();
	}

	public boolean hasBlockWithAnswer() {
		return primary().hasBlockWithAnswer();
	}

	public String toString() {
		String result = primary.toString();
		for (Token selector : selectors)
			result = result + ' ' + selector.toString();
		return result;
	}

	public void add(Token selector) {
		selectors.add(selector);
	}

	public Primary primary() {
		return primary;
	}

	public List<Token> selectors() {
		return selectors;
	}

	public int lineNumber() {
		return primary.lineNumber();
	}
}
