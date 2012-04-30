/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

import java.math.BigDecimal;

public class PrimContext {

    public PrimObject receiver;
    public PrimObject lookupClass;
    public String selector;
    public PrimObject[] arguments;
    private PrimObject[] temporaries;

    public PrimContext(PrimObject receiver) {
        this.receiver = receiver;
    }

    public PrimContext(PrimObject receiver, PrimObject lookupClass, String selector, PrimObject ... arguments) {
        this.receiver = receiver;
        this.lookupClass = lookupClass;
        this.selector = selector;
        this.arguments = arguments;
    }

    PrimObject[] temporaries() {
        return temporaries;
    }

    public static void temporaryPutAtIn(PrimObject object, int index, PrimContext context) {
        context.temporaryAtPut(index, object);
    }

    public static void temporaryPutAtInFrom(PrimObject object, int index, PrimContext context, PrimObject receiver) {
        ((PrimObjectBlock) receiver).outerContext().temporaryAtPut(index, object);
    }

    public PrimObject outerReceiver(PrimObject receiver) {
        return ((PrimObjectBlock) receiver).outerContext().receiver;
    }

    public void temporaryAtPut(int index, PrimObject object) {
        temporaries[index] = object;
    }

    public PrimObject temporaryAt(int index) {
        return temporaries[index];
    }

    public PrimObject temporaryAtFrom(int index, PrimObject receiver) {
        return ((PrimObjectBlock) receiver).outerContext().temporaryAt(index);
    }

    public PrimObject argumentAt(int index) {
        return arguments[index];
    }

    public PrimObject argumentAtFrom(int index, PrimObject receiver) {
        return ((PrimObjectBlock) receiver).outerContext().argumentAt(index);
    }

    public int intArgumentAt(int index) {
        return ((BigDecimal) argumentAt(index).javaValue()).intValue();
    }

    public void temporariesInit(int size) {
        temporaries = new PrimObject[size];
        for (int i = 0; i < size; i++)
            temporaries[i] = PrimObject.NIL;
    }
}
