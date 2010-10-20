package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class ClassDefinition {

	private final UnarySend unarySend;
	private final List<KeywordSend> keywordSends;

	public ClassDefinition(UnarySend unarySend) {
		this.unarySend = unarySend;
		this.keywordSends = new ArrayList<KeywordSend>();
	}

	public boolean isScript() {
		return false;
	}

	public void add(KeywordSend keywordSend) {
		keywordSends.add(keywordSend);
	}

	public Token rawInstanceVariableNames() {
		return keywordSends.get(1).arguments().get(0).primary().token();
	}

	public Token rawClassVariableNames() {
		return keywordSends.get(2).arguments().get(0).primary().token();
	}

	public Token rawPoolDictionaries() {
		return keywordSends.get(3).arguments().get(0).primary().token();
	}

	public String subclass() {
		return keywordSends.get(0).arguments().get(0).toString();
	}

	public int lineNumber() {
		return unarySend.lineNumber();
	}

	public String superclass() {
		return unarySend.primary().toString();
	}

	public String[] classVariableNames() {
		return split(rawClassVariableNames().toString());
	}

	public String[] instanceVariableNames() {
		return split(rawInstanceVariableNames().toString());
	}

	public String[] poolDictionaries() {
		return split(rawPoolDictionaries().toString());
	}

	public String category() {
		return keywordSends.get(4).arguments().get(0).primary().toString();
	}

	private String[] split(String string) {
		if (string.length() == 2)
			return new String[0];
		return string.substring(1, string.length() - 1).split(" ");
	}

	public UnarySend unarySend() {
		return unarySend;
	}

	public List<KeywordSend> keywordSends() {
		return keywordSends;
	}
}