package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class KeywordMessagePattern extends MessagePattern {

	private final List<KeywordPattern> keywordPatterns = new ArrayList<KeywordPattern>();

	public KeywordMessagePattern(Token keyword, Variable variable) {
		super(keyword);
		add(keyword, variable);
	}

	public void add(Token keyword, Variable variable) {
		keywordPatterns.add(new KeywordPattern(keyword, variable));
	}

	@Override
	int argumentCount() {
		return keywordPatterns.size();
	}

	@Override
	public String pattern() {
		String pattern = "";
		for (KeywordPattern keywordPattern : keywordPatterns)
			pattern += keywordPattern.keyword;
		return pattern;
	}

	public class KeywordPattern {

		private final Token keyword;
		private final Variable variable;

		public KeywordPattern(Token keyword, Variable variable) {
			this.keyword = keyword;
			this.variable = variable;
		}
	}
}
