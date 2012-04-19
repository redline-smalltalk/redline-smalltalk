/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class KeywordExpression implements MessageExpression {

	private final List<BinaryObjectDescription> binaryObjectDescriptions;
	private final StringBuffer keywords;
	private int line;

	KeywordExpression() {
		binaryObjectDescriptions = new ArrayList<BinaryObjectDescription>();
		keywords = new StringBuffer();
		line = -1;
	}

	void add(String keyword, int line, BinaryObjectDescription binaryObjectDescription) {
		if (this.line == -1)
			this.line = line;
		keywords.append(keyword);
		binaryObjectDescriptions.add(binaryObjectDescription);
	}

	String keywords() {
		return keywords.toString();
	}

	List<BinaryObjectDescription> binaryObjectDescriptions() {
		return binaryObjectDescriptions;
	}

	public void accept(NodeVisitor nodeVisitor) {
		String keywords = keywords();
		nodeVisitor.visitBegin(this, keywords, binaryObjectDescriptions.size(), line);
		for (BinaryObjectDescription binaryObjectDescription : binaryObjectDescriptions)
			binaryObjectDescription.accept(nodeVisitor);
		nodeVisitor.visitEnd(this, keywords, binaryObjectDescriptions.size(), line);
	}

    public boolean hasBlockWithAnswerExpression() {
	    String selector = keywords();
        for (BinaryObjectDescription binaryObjectDescription : binaryObjectDescriptions)
            if (binaryObjectDescription.hasBlockWithAnswerExpression() && !selector.equals("atSelector:put:"))
                return true;
        return false;
    }
}
