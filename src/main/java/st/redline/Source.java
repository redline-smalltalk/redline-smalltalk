/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

public class Source {

	private final String source;
	private final SourceFile sourceFile;

	public Source(String source, SourceFile sourceFile) {
		this.source = source;
		this.sourceFile = sourceFile;
	}

	public String source() {
		return source;
	}
}
