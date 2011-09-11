/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class KeywordMessagePattern implements MessagePattern {

	private final List<VariableName> variableNames;
	private final StringBuffer keywords;
	private final int line;
	private int index = 0;

	public KeywordMessagePattern(KeywordAndVariableName keywordAndVariableName) {
		variableNames = new ArrayList<VariableName>();
		keywords = new StringBuffer();
		line = keywordAndVariableName.keyword.line;
		add(keywordAndVariableName);
	}

	public int line() {
		return line;
	}

	public void add(KeywordAndVariableName keywordAndVariableName) {
		keywords.append(keywordAndVariableName.keyword.value);
		keywordAndVariableName.variableName.index(index++);
		variableNames.add(keywordAndVariableName.variableName);
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, keywords.toString(), line, variableNames);
	}
}
