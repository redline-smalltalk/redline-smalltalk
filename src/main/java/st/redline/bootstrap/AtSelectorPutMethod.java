/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.core.PrimObjectClass;
import st.redline.core.PrimContext;
import st.redline.core.PrimObject;
import st.redline.core.PrimObjectBlock;

public class AtSelectorPutMethod extends PrimObject {

    public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
        String selector = selectorFrom(primContext);
        PrimObjectBlock block = blockFrom(primContext);
        block.markAsMethodBlock();
        ((PrimObjectClass) receiver).methods().put(selector, block);
        return receiver;
    }

    String selectorFrom(PrimContext primContext) {
        return (String) primContext.argumentAt(0).javaValue();
    }

    PrimObjectBlock blockFrom(PrimContext primContext) {
        return (PrimObjectBlock) primContext.argumentAt(1);
    }
}
