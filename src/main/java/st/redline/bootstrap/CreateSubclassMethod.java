/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.core.PrimContext;
import st.redline.core.PrimObject;
import st.redline.core.PrimObjectMetaclass;

public class CreateSubclassMethod extends PrimObject {

    public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
//   		System.out.println("createSubclass: " + receiver + " called: " + subclassNameFrom(primContext));
        String subclassName = subclassNameFrom(primContext);
        PrimObjectMetaclass subclassMetaClass = PrimObjectMetaclass.basicSubclassOf((PrimObjectMetaclass) receiver.cls());
        PrimObjectMetaclass subclassClass = subclassMetaClass.basicCreate(subclassName, receiver, "", "", "", "");
//        System.out.println("Have " + subclassNameFrom(primContext) + " " + subclassClass + " cls " + subclassMetaClass);
//        System.out.println(fullyQualifiedClassName(subclassName));
        String fqn = fullyQualifiedClassName(subclassName);
        subclassMetaClass.fqn(fqn);
        shareEigenClassImports(subclassClass, fqn);
        PrimObject.CLASSES.put(fqn, subclassClass);
        return subclassClass;
    }

    private void shareEigenClassImports(PrimObjectMetaclass subclassClass, String fqn) {
        PrimObjectMetaclass eigenClass = eigenClassFor(fqn);
        if (eigenClass != null && eigenClass.imports() != null)
            if (subclassClass.imports() == null)
                subclassClass.imports(eigenClass.imports());
            else
                throw new IllegalStateException("Replacing new subclass imports with eigenClass imports.");
    }

    private PrimObjectMetaclass eigenClassFor(String fqn) {
        return PrimObject.EIGENCLASS_REGISTRY.get().get(fqn);
    }

    String fullyQualifiedClassName(String name) {
        String packageName = PrimObject.PACKAGE_REGISTRY.get().isEmpty() ? null : PrimObject.PACKAGE_REGISTRY.get().peek();
        return packageName == null ? name : packageName + "." + name;
    }

    String subclassNameFrom(PrimContext primContext) {
        return (String) primContext.argumentAt(0).javaValue();
    }
}
