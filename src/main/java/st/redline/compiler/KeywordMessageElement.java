/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class KeywordMessageElement implements MessageElement {

	private final StringBuffer keywords;
	private final int line;
	private final List<BinaryObjectDescription> binaryObjectDescriptions;

	public KeywordMessageElement(Keyword keyword, BinaryObjectDescription binaryObjectDescription) {
		binaryObjectDescriptions = new ArrayList<BinaryObjectDescription>();
		keywords = new StringBuffer();
		line = keyword.line;
		add(keyword, binaryObjectDescription);
	}

	public void add(Keyword keyword, BinaryObjectDescription binaryObjectDescription) {
		keywords.append(keyword.value);
		binaryObjectDescriptions.add(binaryObjectDescription);
	}

	public void accept(NodeVisitor visitor) {
		visitor.visit(this, keywords.toString(), line, binaryObjectDescriptions);
	}
}
