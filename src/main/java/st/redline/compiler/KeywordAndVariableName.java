/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class KeywordAndVariableName {

	protected final Keyword keyword;
	protected final VariableName variableName;

	public KeywordAndVariableName(Keyword keyword, VariableName variableName) {
		this.keyword = keyword;
		this.variableName = variableName;
	}
}
