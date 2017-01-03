/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.core;

import java.util.*;

public class PrimClass extends PrimObject {

    private boolean meta;
    private String name;
    private PrimObject superclass;
    private Map<String, PrimObject> methods = new HashMap<String, PrimObject>();

    public PrimClass() {
        this("", false);
    }

    public PrimClass(boolean isMeta) {
        this("", isMeta);
    }

    public PrimClass(String name) {
        this(name, false);
    }

    public PrimClass(String name, boolean isMeta) {
        this.meta = isMeta;
        this.name = name;
    }

    public String toString() {
        if (isMeta())
            return name + " class";
        return name;
    }

    public boolean isMeta() {
        return meta;
    }

    public boolean includesSelector(String selector) {
        return methods.containsKey(selector);
    }

    public PrimObject methodFor(String selector) {
        return methods.get(selector);
    }

    public void superclass(PrimObject superclass) {
        this.superclass = superclass;
    }

    protected PrimObject superclass() {
        return superclass;
    }

    public void addMethod(String selector, PrimObject method) {
        methods.put(selector, method);
    }
}
