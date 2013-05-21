/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.compiler;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.Token;

public class Preprocessor {

    private boolean trace;

    public Preprocessor(boolean trace) {
        this.trace = trace;
    }

    public String preprocess(String rawSource, String name) {
        try {
            PreProcLexer lexer = new PreProcLexer();
            lexer.setCharStream(new ANTLRStringStream(rawSource));
            lexer.initPreProc(name);
            new CommonTokenStream(lexer);
            while (lexer.nextToken().getType() != Token.EOF) {
            }
            String preprocessedSource = lexer.getOutput();
            if (trace)
                System.out.println("[Preprocessed source >>\n" + preprocessedSource + "\n<< Preprocessed source]");
            return preprocessedSource;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
