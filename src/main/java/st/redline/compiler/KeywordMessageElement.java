/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class KeywordMessageElement implements MessageElement {

	private final List<BinaryObjectDescription> binaryObjectDescriptions;
	private final StringBuffer keywords;
	private final int line;

	KeywordMessageElement(String keyword, int line, BinaryObjectDescription binaryObjectDescription) {
		binaryObjectDescriptions = new ArrayList<BinaryObjectDescription>();
		keywords = new StringBuffer();
		this.line = line;
		add(keyword, line, binaryObjectDescription);
	}

	void add(String keyword, int line, BinaryObjectDescription binaryObjectDescription) {
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
	}
}
