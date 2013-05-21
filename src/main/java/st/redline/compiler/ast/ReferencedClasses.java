/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler.ast;

import java.util.HashMap;
import java.util.Map;

public class ReferencedClasses implements VisitableNode {

    private final Map<String, ReferencedClass> referencedClasses;

    public ReferencedClasses() {
        this.referencedClasses = new HashMap<String, ReferencedClass>();
    }

    public void add(ReferencedClass referencedClass) {
        // Only add what looks like a class, ie: starts with uppercase character.
        if (Character.isUpperCase(referencedClass.value().charAt(0)))
            referencedClasses.put(referencedClass.value(), referencedClass);
    }

    public boolean isEmpty() {
        return referencedClasses.isEmpty();
    }

    public int size() {
        return referencedClasses != null ? referencedClasses.size() : 0;
    }

    public void accept(NodeVisitor nodeVisitor) {
        if (referencedClasses == null)
            return;
        nodeVisitor.visitBegin(this);
        for (ReferencedClass referencedClass : referencedClasses.values())
            referencedClass.accept(nodeVisitor);
        nodeVisitor.visitEnd(this);
    }
}
