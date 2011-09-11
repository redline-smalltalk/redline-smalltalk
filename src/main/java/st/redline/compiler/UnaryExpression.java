/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class UnaryExpression implements MessageExpression {

	private final List<UnarySelector> unarySelectors;
	private BinaryExpression binaryExpression;
	private KeywordExpression keywordExpression;

	public UnaryExpression() {
		unarySelectors = new ArrayList<UnarySelector>();
	}

	public void add(UnarySelector unarySelector) {
		unarySelectors.add(unarySelector);
	}

	public void add(BinaryExpression binaryExpression) {
		this.binaryExpression = binaryExpression;
	}

	public void add(KeywordExpression keywordExpression) {
		this.keywordExpression = keywordExpression;
	}

	public void accept(NodeVisitor visitor) {
		if (binaryExpression != null && keywordExpression != null)
			throw new IllegalStateException("Unary expression should not have both a binary and keyword expression.");
		visitor.visit(this);
		for (UnarySelector unarySelector : unarySelectors)
			unarySelector.accept(visitor);
		if (binaryExpression != null)
			binaryExpression.accept(visitor);
		else if (keywordExpression != null)
			keywordExpression.accept(visitor);
		visitor.visitEnd(this);
	}
}
