/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

public class PrimNil extends ProtoObject {

    // PRIM_NIL is used as a NullObject pattern answering the doesNotUnderstand method,
    // which means we don't have to check when we read the end of a class or superclass chain.
    protected static final PrimNil PRIM_NIL = new PrimNil();

    public PrimNil() {
        name = "PrimNIL";
    }
}
