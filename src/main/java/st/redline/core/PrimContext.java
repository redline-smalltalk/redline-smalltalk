/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.core;

import st.redline.classloader.*;

public class PrimContext {

    private final PrimObject receiver;
    private final PrimObject lookupClass;
    private final String selector;
    private final PrimObject[] arguments;
    private PrimObject[] temporaries;
    private PrimContext homeContext;
    private PrimContext outerContext;

    public PrimContext(PrimObject receiver) {
        this(receiver, null, null, null);
    }

    public PrimContext(PrimObject receiver, PrimObject lookupClass, String selector, PrimObject[] arguments) {
        this.receiver = receiver;
        this.lookupClass = lookupClass;
        this.selector = selector;
        this.arguments = arguments;
    }

    public PrimObject receiver() {
        return receiver;
    }

    public String selector() {
        return selector;
    }

    public PrimObject[] arguments() {
        return arguments;
    }

    public void initTemporaries(int count) {
        temporaries = new PrimObject[count];
        PrimObject nil = nil();
        for (int i = 0; i < count; i++)
            temporaries[i] = nil;
    }

    private PrimObject nil() {
        return classLoader().nilInstance();
    }

    private SmalltalkClassLoader classLoader() {
        return (SmalltalkClassLoader) Thread.currentThread().getContextClassLoader();
    }

    public PrimObject[] selectorAndArguments() {
        PrimObject selectorObject = new PrimObject();
        selectorObject.javaValue(selector);
        PrimObject[] selectorAndArguments = new PrimObject[arguments.length + 1];
        selectorAndArguments[0] = selectorObject;
        System.arraycopy(arguments, 0, selectorAndArguments, 1, arguments.length);
        return selectorAndArguments;
    }

    public PrimObject argumentAt(int index) {
        return arguments[index];
    }

    public PrimObject outerArgumentAt(int index) {
        return outerContext.argumentAt(index);
    }

    public PrimObject homeArgumentAt(int index) {
        return homeContext.argumentAt(index);
    }

    public Object argumentJavaValueAt(int index) {
        return argumentAt(index).javaValue();
    }

    public PrimObject temporaryAt(int index) {
        return temporaries[index];
    }

    public PrimObject homeTemporaryAt(int index) {
        return homeContext.temporaryAt(index);
    }

    public void temporaryAtPut(int index, PrimObject object) {
        temporaries[index] = object;
    }

    public PrimObject instVarAt(String var) {
        throw new RuntimeException("TODO.JCL - get instVar: " + var);
    }

    public void instVarAtPut(String var, PrimObject object) {
        throw new RuntimeException("TODO.JCL - set instVar: " + var);
    }

    public static void temporaryPutAt(PrimObject object, int index, PrimContext context) {
        context.temporaryAtPut(index, object);
    }

    public static void homeTemporaryPutAt(PrimObject object, int index, PrimContext context) {
        context.homeContext().temporaryAtPut(index, object);
    }

    public static void instVarPutAt(PrimObject object, String var, PrimContext context) {
        context.instVarAtPut(var, object);
    }

    public void setupCallContext(PrimContext context) {
        homeContext = context.homeContext();
        outerContext = context;
    }

    public PrimContext outerContext() {
        return outerContext;
    }

    public PrimContext homeContext() {
        return homeContext == null ? this : homeContext;
    }
}
