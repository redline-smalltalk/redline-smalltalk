/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import st.redline.lang.*;

import java.io.File;

public class Bootstrapper {

    private final SourceFinder sourceFinder;

    public Bootstrapper(SourceFinder sourceFinder) {
        this.sourceFinder = sourceFinder;
    }

    public void bootstrap(SmalltalkClassLoader smalltalkClassLoader) throws Exception {
        bootstrapNil(smalltalkClassLoader);
        bootstrapMetaclass(smalltalkClassLoader);
        bootstrapProtoObject(smalltalkClassLoader);
        bootstrapSymbol(smalltalkClassLoader);
        bootstrapObject(smalltalkClassLoader);
        bootstrapMetaclassHierarchy(smalltalkClassLoader);
        smalltalkClassLoader.stoppedBootstrapping();
        bootstrapNilHierarchy(smalltalkClassLoader);
        bootstrapBooleans(smalltalkClassLoader);
    }

    private void bootstrapBooleans(SmalltalkClassLoader smalltalkClassLoader) throws Exception {
        ProtoObject Metaclass = smalltalkClassLoader.METACLASS;
        ProtoObject TrueClass = Metaclass.resolveObject("st.redline.kernel.True");
        smalltalkClassLoader.TRUE = TrueClass.perform("new");
        ProtoObject FalseClass = Metaclass.resolveObject("st.redline.kernel.False");
        smalltalkClassLoader.FALSE = FalseClass.perform("new");
    }

    private void bootstrapNilHierarchy(SmalltalkClassLoader smalltalkClassLoader) throws Exception {
        ProtoObject Metaclass = smalltalkClassLoader.METACLASS;
        smalltalkClassLoader.NIL.selfclass = Metaclass.resolveObject("st.redline.kernel.UndefinedObject");
    }

    private void bootstrapNil(SmalltalkClassLoader smalltalkClassLoader) {
        smalltalkClassLoader.NIL = new ProtoObject();
    }

    private void bootstrapMetaclassHierarchy(SmalltalkClassLoader smalltalkClassLoader) throws Exception {
        ProtoObject Metaclass = smalltalkClassLoader.METACLASS;
        ProtoObject classDescription = Metaclass.resolveObject("st.redline.kernel.ClassDescription");
        ((ProtoClass) Metaclass).superclass(classDescription);
    }

    private void bootstrapObject(SmalltalkClassLoader smalltalkClassLoader) throws Exception {
        ProtoObject Metaclass = smalltalkClassLoader.METACLASS;
        ProtoObject Object = Metaclass.resolveObject("st.redline.kernel.Object");
        ProtoObject Class = Metaclass.resolveObject("st.redline.kernel.Class");
        ((ProtoClass) Object.selfclass).superclass(Class);
    }

    private void bootstrapSymbol(SmalltalkClassLoader smalltalkClassLoader) throws ClassNotFoundException {
        smalltalkClassLoader.METACLASS.resolveObject("st.redline.kernel.Symbol");
    }

    private void bootstrapMetaclass(SmalltalkClassLoader smalltalkClassLoader) {
        ProtoClass Metaclass = new ProtoClass();
        ProtoClass metaclass = Metaclass.create("Metaclass");
        Metaclass.selfclass = metaclass;
        smalltalkClassLoader.METACLASS = metaclass;
        metaclass.addMethod("atSelector:put:", new PrimAtSelectorPutMethod());
    }

    private ProtoObject bootstrapProtoObject(SmalltalkClassLoader smalltalkClassLoader) {
        ProtoClass ProtoObject = makeProtoObject((ProtoClass) smalltalkClassLoader.METACLASS);
        smalltalkClassLoader.registerProtoObject(ProtoObject);
        addKernelImports(ProtoObject);
        return ProtoObject;
    }

    private void addKernelImports(ProtoClass protoObject) {
        for (Source source : sourceFinder.findIn("st.redline.kernel"))
            addKernelImport(protoObject, source);
    }

    private void addKernelImport(ProtoClass protoObject, Source source) {
        protoObject.importAtPut(source.className(), source.fullyQualifiedName());
    }

    private ProtoClass makeProtoObject(ProtoClass metaclass) {
        // Create ProtoObject class (metaclass)
        ProtoClass protoObjectClass = new ProtoClass();
        protoObjectClass.selfclass = metaclass;

        // Add methods needed during bootstrap.
        protoObjectClass.addMethod("subclass:", new PrimSubclass());
        protoObjectClass.addMethod("import:", new PrimImport());
        protoObjectClass.addMethod("class", new PrimClass());
        protoObjectClass.addMethod("atSelector:put:", new PrimAtSelectorPutMethod());

        // Create ProtoObject sole instance. ProtoObject is a class.
        ProtoClass protoObject = protoObjectClass.create("ProtoObject");
        return protoObject;
    }
}
