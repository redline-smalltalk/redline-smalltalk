/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

public class PrimClass extends ProtoObject {

    public PrimClass() {
        name = "PrimClass-block";
    }

    protected ProtoObject invoke(ProtoObject receiver, PrimContext context) {
        return receiver.selfclass;
    }
}
