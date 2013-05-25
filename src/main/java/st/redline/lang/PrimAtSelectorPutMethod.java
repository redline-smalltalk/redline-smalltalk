/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

public class PrimAtSelectorPutMethod extends ProtoObject {

    public ProtoObject invoke(ProtoObject receiver, PrimContext primContext) {
        String selector = selectorFrom(primContext);
        ProtoBlock block = blockFrom(primContext);
        block.markAsMethodBlock();
        ((ProtoClass) receiver).methodAtPut(selector, block);
        return receiver;
    }

    private String selectorFrom(PrimContext primContext) {
        return (String) primContext.argumentAt(0).javaValue();
    }

    private ProtoBlock blockFrom(PrimContext primContext) {
        return (ProtoBlock) primContext.argumentAt(1);
    }
}
