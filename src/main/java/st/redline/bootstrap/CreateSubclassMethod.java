package st.redline.bootstrap;

import st.redline.PrimContext;
import st.redline.PrimObject;
import st.redline.PrimObjectMetaclass;

public class CreateSubclassMethod extends PrimObject {

	public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
		System.out.println("createSubclass: " + receiver + " called: " + subclassNameFrom(primContext));
		String subclassName = subclassNameFrom(primContext);
		PrimObjectMetaclass subclassMetaClass = PrimObjectMetaclass.basicSubclassOf((PrimObjectMetaclass) receiver.cls());
		PrimObjectMetaclass subclassClass = subclassMetaClass.basicCreate(subclassName, receiver, "", "", "", "");
        System.out.println("Have " + subclassNameFrom(primContext) + " " + subclassClass + " cls " + subclassMetaClass);
		PrimObject.CLASSES.put(fullyQualifiedClassName(subclassName), subclassClass);
		return subclassClass;
	}

	String fullyQualifiedClassName(String name) {
		String packageName = PrimObject.PACKAGE_REGISTRY.get().isEmpty() ? null : PrimObject.PACKAGE_REGISTRY.get().peek();
		return packageName == null ? name : packageName + "." + name;
	}

	String subclassNameFrom(PrimContext primContext) {
		return (String) primContext.argumentAt(0).javaValue();
	}
}
