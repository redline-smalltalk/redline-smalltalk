/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.compiler;

import st.redline.classloader.SmalltalkClassLoader;
import st.redline.classloader.Source;
import st.redline.compiler.ast.Block;
import st.redline.compiler.ast.Program;
import st.redline.compiler.ast.VisitableNode;

public class AnalyserFactory {

    private final BytecodeWriterFactory bytecodeWriterFactory;
    private final boolean verboseAnalysis;

    public AnalyserFactory(BytecodeWriterFactory bytecodeWriterFactory, boolean verboseAnalysis) {
        this.bytecodeWriterFactory = bytecodeWriterFactory;
        this.verboseAnalysis = verboseAnalysis;
    }

    public Analyser createAnalyser(AnalyserFactory analyserFactory, VisitableNode rootAstNode, Source source) {
        return new Analyser(analyserFactory, bytecodeWriterFactory, verboseAnalysis, rootAstNode, source);
    }

    public Analyser createAnalyser(VisitableNode rootAstNode, Source source) {
        return new Analyser(this, bytecodeWriterFactory, verboseAnalysis, rootAstNode, source);
    }

    public AnalyserDelegate createJVMAnalyser(Analyser analyser, ClassBytecodeWriter bytecodeWriter) {
        JVMAnalyser jvmAnalyser = new JVMAnalyser(analyser, bytecodeWriter);
        return verboseAnalysis ? analyser.tracingDelegate(jvmAnalyser) : jvmAnalyser;
    }

    public BlockAnalyser createBlockAnalyser(Analyser analyser, SmalltalkClassLoader smalltalkClassLoader,
                                             String blockClassName, Source source, Block block) {
        return new BlockAnalyser(analyser, smalltalkClassLoader, this, bytecodeWriterFactory, verboseAnalysis, block,
                                blockClassName, source);
    }
}
