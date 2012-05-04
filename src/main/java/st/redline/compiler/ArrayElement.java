/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

interface ArrayElement extends VisitableNode {
    String value();
    int line();
    void index(int index);
    int index();
    void insideArray();
    boolean isInsideArray();
}
