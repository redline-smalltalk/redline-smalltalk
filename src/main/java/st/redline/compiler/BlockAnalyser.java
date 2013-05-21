/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import st.redline.classloader.SmalltalkClassLoader;
import st.redline.classloader.Source;
import st.redline.compiler.ast.*;
import st.redline.compiler.ast.Number;

public class BlockAnalyser extends ProgramAnalyser implements AnalyserDelegate {

    private final boolean verbose;
    private final Block thisBlock;
    private final String blockClassName;

    public BlockAnalyser(Analyser analyser, SmalltalkClassLoader smalltalkClassLoader, AnalyserFactory analyserFactory,
                         BytecodeWriterFactory bytecodeWriterFactory, boolean verbose, Block block, String blockClassName, Source source) {
        super(analyser, smalltalkClassLoader, analyserFactory, bytecodeWriterFactory, source, blockClassName);
        this.verbose = verbose;
        this.thisBlock = block;
        this.blockClassName = blockClassName;
    }

    protected ClassBytecodeWriter createClassBytecodeWriter(String className) {
        String packageName =  source.packageName();
        return bytecodeWriterFactory.createBlockBytecodeWriter(className, packageName);
    }

    public boolean skipBlockVisit(Block block) {
        return block != thisBlock;
    }

    public void visitBegin(Block block, int line) {
        if (block == thisBlock) {
            writer.openClass();
        } else
            super.visitBegin(block, line);
    }

    public void visitEnd(Block block, int line) {
        if (block != thisBlock)
            throw new IllegalStateException("Expected visitEnd of own block. Got " + block);
        writer.closeClass();
    }

    public void visit(Identifier identifier, String value, int line) {
        if (identifier.isOnLoadSideOfExpression()) {
            if (isOuterArgument(value))
                writer.pushOuterArgument(thisBlock.outerArgument(value));
            else if (isOuterTemporary(value))
                writer.pushOuterTemporary(thisBlock.outerTemporary(value));
            else
                super.visit(identifier, value, line);
        } else {
            if (isOuterArgument(value))
                throw new RuntimeException("Can't store into an argument, only temporaries and variables.");
            else if (isOuterTemporary(value))
                writer.storeOuterTemporary(thisBlock.outerTemporary(value));
            else
                super.visit(identifier, value, line);
        }
    }

    boolean isOuterTemporary(String name) {
        return thisBlock.isOuterTemporary(name);
    }

    boolean isOuterArgument(String name) {
        return thisBlock.isOuterArgument(name);
    }

    public void visit(JVM jvm, int line) {
        writer.visitLine(line);
        AnalyserDelegate jvmAnalyser = analyserFactory.createJVMAnalyser(analyser, writer);
        analyser.currentDelegate(jvmAnalyser);
    }

    public void visitEnd(AnswerStatement answerStatement) {
        writer.invokeBlockAnswer(thisBlock.blockReturnType().replaceAll("/", "."));
    }

    public void visit(Self self, int line) {
        writer.visitLine(line);
        writer.pushOuterReceiver();
    }
}
