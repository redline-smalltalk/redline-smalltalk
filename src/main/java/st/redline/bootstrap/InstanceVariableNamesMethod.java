package st.redline.bootstrap;

import st.redline.PrimContext;
import st.redline.PrimObject;
import st.redline.PrimObjectClass;

public class InstanceVariableNamesMethod extends PrimObject {
	public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
//		System.out.println("instanceVariableNames: " + names(primContext));
		PrimObjectClass aClass = (PrimObjectClass) receiver;
		for (String name : names(primContext).split(" "))
			aClass.addVariableNamed(name);
		return receiver;
	}

	String names(PrimContext primContext) {
		return (String) primContext.argumentAt(0).javaValue();
	}
}
