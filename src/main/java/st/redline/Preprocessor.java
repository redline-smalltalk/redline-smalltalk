/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import org.antlr.runtime.*;
import st.redline.compiler.*;

public class Preprocessor {

	public Source parse(SourceFile sourceFile) {
		String preprocessedSource = parseFrom(sourceFile.contents(), sourceFile.shortName());
//		System.out.println(preprocessedSource);
		return new Source(preprocessedSource, sourceFile);
	}

	private String parseFrom(String source, String name) {
		try {
			PreProcLexer lexer = new PreProcLexer();
			lexer.setCharStream(new ANTLRStringStream(source));
			lexer.initPreProc(name);
			new CommonTokenStream(lexer);
			while (lexer.nextToken().getType() != Token.EOF) {
			}
			return lexer.getOutput();
		} catch (Exception e) {
			System.err.println("Preprocessor threw an exception:\n\n");
			throw RedlineException.withCause(e);
		}
 	}
}
