/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.core;

import st.redline.classloader.SmalltalkClassLoader;

public class PrimSubclass extends PrimObject {

    public static final PrimObject PRIM_SUBCLASS = new PrimSubclass();

    private PrimObject theMetaclass;

    protected PrimObject invoke(PrimObject receiver, PrimContext primContext) {
        System.out.println("PrimSubclass invoke: " + String.valueOf(primContext.argumentJavaValueAt(0)));
        assert receiver.equals(primContext.receiver());

        String subclassName = String.valueOf(primContext.argumentJavaValueAt(0));
        PrimObject superclass = primContext.receiver();
        PrimClass newClass;
        PrimClass newMeta;
        boolean bootstrapping = isBootstrapping();

        if (bootstrapping) {
            newClass = (PrimClass) superclass.resolveObject(subclassName);
            if (newClass == null)
                throw new RuntimeException("New class is unexpectedly null.");
        } else {
            newClass = new PrimClass(subclassName);
            newMeta = new PrimClass(subclassName, true);
            newClass.selfClass(newMeta);
            newClass.superclass(superclass);
            newMeta.superclass(superclass.selfClass());
            newMeta.selfClass(theMetaclass);
        }

        // TODO - Add other definitions to appropriate objects.
        //System.out.println("TODO - Add other definitions to appropriate objects.");

        if (!bootstrapping) {
            SmalltalkClassLoader classLoader = classLoader();
            String fullQualifiedName = makeFullyQualifiedName(classLoader, subclassName);
            classLoader.cacheObject(fullQualifiedName, newClass);
        }

        return newClass;
    }

    private String makeFullyQualifiedName(SmalltalkClassLoader classLoader, String name) {
        String instantiationName = classLoader.peekInstantiationName();
        if (instantiationName != null && instantiationName.endsWith(name))
            return instantiationName;
        throw new RuntimeException("Current instantiating class name not found.");
    }

    public void metaclass(PrimClass metaclass) {
        theMetaclass = metaclass;
    }
}
