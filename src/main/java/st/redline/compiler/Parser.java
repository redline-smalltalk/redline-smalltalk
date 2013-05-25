/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.compiler;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import st.redline.compiler.ast.Program;

public class Parser {

    private final boolean ignoreParseErrors;

    public Parser(boolean ignoreParseErrors) {
        this.ignoreParseErrors = ignoreParseErrors;
    }

    public Program parse(String preprocessedSource, String className) {
        SmalltalkLexer smalltalkLexer = lexorOn(preprocessedSource);
        SmalltalkParser smalltalkParser = parserUsing(smalltalkLexer);
        try {
            Program program = smalltalkParser.program();
            if (!ignoreParseErrors && smalltalkParser.getNumberOfSyntaxErrors() > 0)
                throw new RuntimeException("Smalltalk Syntax error(s) detected in '" + className + "'.");
            return program;
        } catch (RecognitionException e) {
            throw new RuntimeException(e);
        }
    }

    private SmalltalkParser parserUsing(SmalltalkLexer smalltalkLexer) {
        return new SmalltalkParser(new CommonTokenStream(smalltalkLexer));
    }

    private SmalltalkLexer lexorOn(String sourceCode) {
        return new SmalltalkLexer(new ANTLRStringStream(sourceCode));
    }
}
