/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

import java.math.BigDecimal;
import java.util.List;

public class PrimContext {

    public final ProtoObject receiver;
    public final ProtoObject lookupClass;
    public final String selector;
    public final ProtoObject[] arguments;
    public ProtoObject[] temporaries;

    public PrimContext(ProtoObject receiver) {
        this(receiver, null, null, null, null);
    }

    public PrimContext(ProtoObject receiver, ProtoObject lookupClass, String selector, ProtoObject... arguments) {
        this.receiver = receiver;
        this.lookupClass = lookupClass;
        this.selector = selector;
        this.arguments = arguments;
    }

    public ProtoObject argumentAt(int index) {
        return arguments[index];
    }

    public static void temporaryPutAtIn(ProtoObject object, int index, PrimContext context) {
        context.temporaryAtPut(index, object);
    }

    public static void temporaryPutAtInFrom(ProtoObject object, int index, PrimContext context, ProtoObject receiver) {
        ((ProtoBlock) receiver).outerContext().temporaryAtPut(index, object);
    }

    public ProtoObject temporaryAt(int index) {
        return temporaries[index];
    }

    public void temporaryAtPut(int index, ProtoObject object) {
        temporaries[index] = object;
    }

    public ProtoObject temporaryAtFrom(int index, ProtoObject receiver) {
        return ((ProtoBlock) receiver).outerContext().temporaryAt(index);
    }

    public ProtoObject argumentAtFrom(int index, ProtoObject receiver) {
        return ((ProtoBlock) receiver).outerContext().argumentAt(index);
    }

    public int intArgumentAt(int index) {
        return ((BigDecimal) argumentAt(index).javaValue()).intValue();
    }

    public void temporariesInit(int size) {
        temporaries = new ProtoObject[size];
        ProtoObject nil = nil();
        for (int i = 0; i < size; i++)
            temporaries[i] = nil;
    }

    private ProtoObject nil() {
        return receiver.classLoader().NIL;
    }

    public ProtoObject[] arrayArgumentAt(int index) {
        List<ProtoObject> list = (List<ProtoObject>) argumentAt(index).javaValue();
        ProtoObject[] array = list.toArray(new ProtoObject[list.size()]);
        ProtoObject[] theArgs = new ProtoObject[array.length - 1];
        System.arraycopy(array, 1, theArgs, 0, array.length - 1);
        return theArgs;
    }
}
