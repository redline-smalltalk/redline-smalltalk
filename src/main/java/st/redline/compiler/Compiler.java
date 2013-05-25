/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.compiler;

import st.redline.classloader.Source;
import st.redline.compiler.ast.Program;

public class Compiler {

    private final Preprocessor preprocessor;
    private final Parser parser;
    private final AnalyserFactory analyserFactory;

    public Compiler(Preprocessor preprocessor, Parser parser, AnalyserFactory analyserFactory) {
        this.preprocessor = preprocessor;
        this.parser = parser;
        this.analyserFactory = analyserFactory;
    }

    public byte[] compile(Source source) {
        String preprocessedSource = preprocess(source);
        Program rootAstNode = parse(preprocessedSource, source.name());
        Analyser analyser = createAnalyser(rootAstNode, source);
        return analyser.analyse();
    }

    private Analyser createAnalyser(Program rootAstNode, Source source) {
        return analyserFactory.createAnalyser(analyserFactory, rootAstNode, source);
    }

    private Program parse(String preprocessedSource, String name) {
        return parser.parse(preprocessedSource, name);
    }

    private String preprocess(Source source) {
        String className = source.className();
        String rawSource = source.contents();
        return preprocess(rawSource, className);
    }

    private String preprocess(String rawSource, String name) {
        return preprocessor.preprocess(rawSource, name);
    }
}
