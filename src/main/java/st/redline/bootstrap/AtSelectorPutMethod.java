package st.redline.bootstrap;

import st.redline.PrimContext;
import st.redline.PrimObject;
import st.redline.PrimObjectBlock;
import st.redline.PrimObjectClass;

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
