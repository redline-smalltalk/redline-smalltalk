/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

public class BlockReturn extends RuntimeException {

    private PrimObject answer = PrimObject.NIL;

    public BlockReturn(PrimObject answer) {
        if (answer != null)
            this.answer = answer;
    }

    public PrimObject answer() {
        return answer;
    }
}
