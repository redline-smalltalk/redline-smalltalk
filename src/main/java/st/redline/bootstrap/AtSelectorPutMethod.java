package st.redline.bootstrap;

import st.redline.PrimContext;
import st.redline.PrimObject;
import st.redline.PrimObjectClass;

public class AtSelectorPutMethod extends PrimObject {

	public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
		String selector = selectorFrom(primContext);
		PrimObject block = blockFrom(primContext);
		((PrimObjectClass) receiver).methods().put(selector, block);
		return receiver;
	}

	String selectorFrom(PrimContext primContext) {
		return (String) primContext.argumentAt(0).javaValue();
	}

	PrimObject blockFrom(PrimContext primContext) {
		return primContext.argumentAt(1);
	}
}
