/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

public class BlockReturn extends RuntimeException {

    private ProtoObject answer = PrimNil.PRIM_NIL;

    public BlockReturn(ProtoObject answer) {
        if (answer != null)
            this.answer = answer;
    }

    public ProtoObject answer() {
        return answer;
    }
}
