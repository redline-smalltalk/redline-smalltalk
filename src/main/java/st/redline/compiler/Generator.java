/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.compiler;

import org.antlr.v4.runtime.tree.ParseTree;

public class Generator {

    private final ParseTree tree;
    private final SmalltalkVisitor visitor;

    public Generator(ParseTree tree, SmalltalkVisitor visitor) {
        this.tree = tree;
        this.visitor = visitor;
    }

    public byte[] generate() {
        visitor.visit(tree);
        return visitor.generatedClassBytes();
    }
}
