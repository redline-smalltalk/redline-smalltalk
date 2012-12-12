/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.HashMap;
import java.util.Map;

class ReferencedClasses implements VisitableNode {

    private final Map<String, ReferencedClass> referencedClasses;

    ReferencedClasses() {
        this.referencedClasses = new HashMap<String, ReferencedClass>();
    }

    void add(ReferencedClass referencedClass) {
        // Only add what looks like a class, ie: starts with uppercase character.
        if (Character.isUpperCase(referencedClass.value().charAt(0)))
            referencedClasses.put(referencedClass.value(), referencedClass);
    }

    boolean isEmpty() {
        return referencedClasses.isEmpty();
    }

    int size() {
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
