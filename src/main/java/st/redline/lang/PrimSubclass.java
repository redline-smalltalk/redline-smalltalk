/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

import st.redline.classloader.SmalltalkClassLoader;

public class PrimSubclass extends ProtoObject {

    public PrimSubclass() {
        name = "PrimSubclass-block";
    }

    protected ProtoObject invoke(ProtoObject receiver, PrimContext context) {
        SmalltalkClassLoader smalltalkClassLoader = classLoader();
        String name = subclassNameFrom(context);
        ProtoObject sublcass = createSubclass(smalltalkClassLoader.METACLASS, (ProtoClass) receiver, name);
        registerClass(smalltalkClassLoader, receiver, name, sublcass);
        return sublcass;
    }

    private void registerClass(SmalltalkClassLoader smalltalkClassLoader, ProtoObject receiver, String name, ProtoObject subclass) {
        String fullyQualifiedName = smalltalkClassLoader.currentPackage() + "." + name;
        smalltalkClassLoader.registerSmalltalkClass(fullyQualifiedName, subclass);
    }

    private ProtoObject createSubclass(ProtoObject metaclass, ProtoClass receiver, String name) {
        ProtoClass newMetaclass = ((ProtoClass) receiver.selfclass).subclass();
        newMetaclass.selfclass = metaclass;
        ProtoClass newClass = newMetaclass.create(name);
        newClass.superclass = receiver;
        return newClass;
    }

    private String subclassNameFrom(PrimContext primContext) {
        return (String) primContext.argumentAt(0).javaValue();
    }
}
