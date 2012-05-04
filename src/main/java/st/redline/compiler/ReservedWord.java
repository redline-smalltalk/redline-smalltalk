/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

interface ReservedWord {
    void accept(NodeVisitor nodeVisitor, int line);
}
