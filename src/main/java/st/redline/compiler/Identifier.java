/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.HashMap;
import java.util.Map;

class Identifier extends ValuePrimary {

	static final Map<String, ReservedWord> RESERVED_WORDS = new HashMap<String, ReservedWord>();

	static {
		RESERVED_WORDS.put("self", new Self());
		RESERVED_WORDS.put("super", new Super());
		RESERVED_WORDS.put("true", new True());
		RESERVED_WORDS.put("false", new False());
		RESERVED_WORDS.put("nil", new Nil());
		RESERVED_WORDS.put("JVM", new JVM());
	}

	Identifier(String value, int line) {
		super(value, line);
	}

	public void accept(NodeVisitor nodeVisitor) {
		ReservedWord reservedWord = RESERVED_WORDS.get(value());
		if (reservedWord != null)
			reservedWord.accept(nodeVisitor, line());
		else
			nodeVisitor.visit(this, value(), line());
	}
}
