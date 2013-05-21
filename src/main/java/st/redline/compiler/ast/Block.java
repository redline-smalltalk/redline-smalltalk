/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

import st.redline.compiler.Analyser;

import java.util.Map;

public class Block extends Primary {

    private final int line;
    private final BlockArguments blockArguments;
    private final Temporaries temporaries;
    private final Statements statements;
    private Analyser analyser;
    private String blockReturnType;
    private Map<String, Integer> outerTemporariesRegistry;
    private Map<String, Integer> outerArgumentsRegistry;
    private boolean answerHandled = false;

    public Block(int line, BlockArguments blockArguments, Temporaries temporaries, Statements statements) {
        this.line = line;
        this.blockArguments = blockArguments;
        this.temporaries = temporaries;
        this.statements = statements;
    }

    public Statements statements() {
        return statements;
    }

    public int line() {
        return line;
    }

    public void analyser(Analyser analyser) {
        this.analyser = analyser;
    }

    public Analyser analyser() {
        return analyser;
    }

    public byte[] classBytes() {
        return analyser.classBytes();
    }

    public BlockArguments arguments() {
        return blockArguments;
    }

    public Temporaries temporaries() {
        return temporaries;
    }

    public boolean isBlockWithAnswerExpression() {
        if (answerHandled) {
            // We have handled the try/catch for the ^ in the block or sub-block.
            return false;
        }
        return (answerHandled = (statements != null && (statements.hasAnswerExpression() || statements.hasBlockWithAnswerExpression())));
    }

    public void accept(NodeVisitor nodeVisitor) {
        nodeVisitor.visitBegin(this, line);
        if (nodeVisitor.skipBlockVisit(this))
            return;
        if (blockArguments != null)
            blockArguments.accept(nodeVisitor);
        if (temporaries != null)
            temporaries.accept(nodeVisitor);
        if (statements != null)
            statements.accept(nodeVisitor);
        nodeVisitor.visitEnd(this, line);
    }

    public int temporariesCount() {
        if (temporaries != null)
            return temporaries.size();
        return 0;
    }

    public void blockReturnType(String blockReturnType) {
    //		System.out.println("blockReturnType: " + blockReturnType);
        this.blockReturnType = blockReturnType;
    }

    public String blockReturnType() {
        return blockReturnType;
    }

    public void outerTemporariesRegistry(Map<String, Integer> temporariesRegistry) {
        outerTemporariesRegistry = temporariesRegistry;
    }

    public boolean isOuterTemporary(String name) {
        return outerTemporariesRegistry != null && outerTemporariesRegistry.containsKey(name);
    }

    public int outerTemporary(String name) {
        return outerTemporariesRegistry.get(name);
    }

    public void outerArgumentsRegistry(Map<String, Integer> argumentsRegistry) {
        outerArgumentsRegistry = argumentsRegistry;
    }

    public boolean isOuterArgument(String name) {
        return outerArgumentsRegistry != null && outerArgumentsRegistry.containsKey(name);
    }

    public int outerArgument(String name) {
        return outerArgumentsRegistry.get(name);
    }
}
