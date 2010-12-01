package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class KeywordSend extends MessageSend {

	private final Token keyword;
	private final List<MessageSend> arguments;

	public KeywordSend(Token keyword) {
		this.keyword = keyword;
		this.arguments = new ArrayList<MessageSend>();
	}

	public boolean hasBlockWithAnswer() {
		for (MessageSend messageSend : arguments)
			if (messageSend.hasBlockWithAnswer())
				return true;
		return false;
	}

	public void add(MessageSend messageSend) {
		arguments.add(messageSend);
	}

	public Token keyword() {
		return keyword;
	}

	public List<MessageSend> arguments() {
		return arguments;
	}

	public Primary primary() {
		return arguments.get(0).primary();
	}
}
