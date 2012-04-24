/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import st.redline.RedlineException;

public class BlockAnalyser extends ProgramAnalyser implements AnalyserDelegate {

    private boolean verbose;
    private Block thisBlock;

    BlockAnalyser(Analyser analyser, String className, String packageName, boolean verbose, Block block) {
        this(analyser, new BlockBytecodeWriter(className, packageName, verbose), verbose, packageName, block);
    }

    BlockAnalyser(Analyser analyser, ClassBytecodeWriter classBytecodeWriter, boolean verbose, String packageName, Block block) {
        super(analyser, classBytecodeWriter, verbose, packageName);
        this.verbose = verbose;
        this.thisBlock = block;
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
                throw new RedlineException("Can't store into an argument, only temporaries and variables.");
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
        JVMAnalyser jvmAnalyser = new JVMAnalyser(analyser, writer, verbose);
        analyser.currentDelegate(verbose ? analyser.tracingDelegate(jvmAnalyser) : jvmAnalyser);
    }

    public void visitEnd(AnswerStatement answerStatement) {
        writer.invokeBlockAnswer(thisBlock.blockReturnType().replaceAll("/", "."));
    }

    String createBlockName() {
        BLOCK_NUMBER++;
        return analyser.className() + "$B" + BLOCK_NUMBER;
    }
}
