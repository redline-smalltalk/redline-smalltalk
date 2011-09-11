/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class BinaryExpression implements MessageExpression {

	private final List<BinarySelectorUnaryObjectDescription> binarySelectorUnaryObjectDescriptions;
	private KeywordExpression keywordExpression;

	public BinaryExpression() {
		binarySelectorUnaryObjectDescriptions = new ArrayList<BinarySelectorUnaryObjectDescription>();
	}

	public void add(BinarySelector binarySelector, UnaryObjectDescription unaryObjectDescription) {
		binarySelectorUnaryObjectDescriptions.add(new BinarySelectorUnaryObjectDescription(binarySelector, unaryObjectDescription));
	}

	public void add(KeywordExpression keywordExpression) {
		this.keywordExpression = keywordExpression;
	}

	public void accept(NodeVisitor nodeVisitor) {
		nodeVisitor.visit(this);
		for (BinarySelectorUnaryObjectDescription binarySelectorUnaryObjectDescription : binarySelectorUnaryObjectDescriptions)
				binarySelectorUnaryObjectDescription.accept(nodeVisitor);
		// keywordExpression may be null.
		if (keywordExpression != null)
			keywordExpression.accept(nodeVisitor);
		nodeVisitor.visitEnd(this);
	}
}
