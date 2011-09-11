/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class KeywordExpression implements MessageExpression {

	private final List<BinaryObjectDescription> binaryObjectDescriptions;
	private final StringBuffer keywords;
	private int line = -1;

	public KeywordExpression() {
		binaryObjectDescriptions = new ArrayList<BinaryObjectDescription>();
		keywords = new StringBuffer();
	}

	public void add(Keyword keyword, BinaryObjectDescription binaryObjectDescription) {
		if (line == -1)
			line = keyword.line;
		keywords.append(keyword.value);
		binaryObjectDescriptions.add(binaryObjectDescription);
	}

	public void accept(NodeVisitor nodeVisitor) {
		String keywords = this.keywords.toString();
		nodeVisitor.visit(this, keywords, binaryObjectDescriptions.size(), line);
		for (BinaryObjectDescription binaryObjectDescription : binaryObjectDescriptions)
			binaryObjectDescription.accept(nodeVisitor);
		nodeVisitor.visitEnd(this, keywords, binaryObjectDescriptions.size(), line);
	}
}
