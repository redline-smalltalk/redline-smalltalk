package st.redline.classloader;

import st.redline.core.*;

public class Bootstrapper {

    public void bootstrap(SmalltalkClassLoader classLoader) {
        setContextClassLoader(classLoader);

        classLoader.beginBootstrapping();
        createPrimObject(classLoader);
        createKernelObjectsHierarchy(classLoader);
        loadKernelObjects(classLoader);
        classLoader.endBootstrapping();
    }

    private void createKernelObjectsHierarchy(SmalltalkClassLoader classLoader) {

        // Create Kernel Objects and Classes we need to start Runtime.
        PrimObject primObject = classLoader.cachedObject("st.redline.core.PrimObject");
//        classLoader.registerPackage("PrimObject", primObject);

        PrimClass object = createKernelObject("Object", primObject);
        PrimClass behavior = createKernelObject("Behavior", object);
        PrimClass classDescription = createKernelObject("ClassDescription", behavior);
        PrimClass klass = createKernelObject("Class", classDescription);
        PrimClass metaclass = createKernelObject("Metaclass", classDescription);
        PrimClass undefinedObject = createKernelObject("UndefinedObject", object);
        PrimClass blockClosure = createKernelObject("BlockClosure", object);
        PrimClass compiledMethod = createKernelObject("CompiledMethod", object);
        PrimClass booleanObject = createKernelObject("Boolean", object);
        PrimClass trueObject = createKernelObject("True", booleanObject);
        PrimClass falseObject = createKernelObject("False", booleanObject);
        PrimClass collection = createKernelObject("Collection", object);
        PrimClass sequenceableCollection = createKernelObject("SequenceableCollection", collection);
        PrimClass arrayedCollection = createKernelObject("ArrayedCollection", sequenceableCollection);
        PrimClass string = createKernelObject("String", arrayedCollection);
        PrimClass symbol = createKernelObject("Symbol", string);

        // Initialise special Smalltalk circular hierarchy.
        ((PrimClass) object.selfClass()).superclass(klass);

        // Add basicAddSelector:withMethod: to Behaviour
        ((PrimClass) behavior).addMethod("basicAddSelector:withMethod:", new PrimAddMethod());

        // Create special instances, referred to with pseudo variables.
        PrimObject nil = new PrimObject();
        nil.selfClass(undefinedObject);
        classLoader.nilInstance(nil);

        PrimObject trueInstance = new PrimObject();
        trueInstance.selfClass(trueObject);
        classLoader.trueInstance(trueInstance);

        PrimObject falseInstance = new PrimObject();
        falseInstance.selfClass(falseObject);
        classLoader.falseInstance(falseInstance);

        // Load the hierarchy which will attached their methods.
        classLoader.cacheObject("st.redline.core.Object", object);
        classLoader.cacheObject("st.redline.core.Behavior", behavior);
        classLoader.cacheObject("st.redline.core.ClassDescription", classDescription);
        classLoader.cacheObject("st.redline.core.Class", klass);
        classLoader.cacheObject("st.redline.core.Metaclass", metaclass);
        classLoader.cacheObject("st.redline.core.UndefinedObject", undefinedObject);
        classLoader.cacheObject("st.redline.core.BlockClosure", blockClosure);
        classLoader.cacheObject("st.redline.core.CompiledMethod", compiledMethod);
        classLoader.cacheObject("st.redline.core.Boolean", booleanObject);
        classLoader.cacheObject("st.redline.core.True", trueObject);
        classLoader.cacheObject("st.redline.core.False", falseObject);
        classLoader.cacheObject("st.redline.core.Collection", collection);
        classLoader.cacheObject("st.redline.core.SequenceableCollection", sequenceableCollection);
        classLoader.cacheObject("st.redline.core.ArrayedCollection", arrayedCollection);
        classLoader.cacheObject("st.redline.core.String", string);
        classLoader.cacheObject("st.redline.core.Symbol", symbol);
    }

    private PrimClass createKernelObject(String name, PrimObject superclass) {
        PrimClass primMeta = new PrimClass(name,true);
        primMeta.superclass(superclass.selfClass());
        PrimClass primClass = new PrimClass(name);
        primClass.superclass(superclass);
        primClass.selfClass(primMeta);
        return primClass;
    }

    private void setContextClassLoader(SmalltalkClassLoader classLoader) {
        Thread.currentThread().setContextClassLoader(classLoader);
    }

    private void loadKernelObjects(SmalltalkClassLoader classLoader) {
        loadObject(classLoader, "st.redline.core.Object");
        loadObject(classLoader, "st.redline.core.Behavior");
        loadObject(classLoader, "st.redline.core.ClassDescription");
        loadObject(classLoader, "st.redline.core.Class");
        loadObject(classLoader, "st.redline.core.Metaclass");
        loadObject(classLoader, "st.redline.core.UndefinedObject");
        loadObject(classLoader, "st.redline.core.BlockClosure");
        loadObject(classLoader, "st.redline.core.CompiledMethod");
        loadObject(classLoader, "st.redline.core.Boolean");
        loadObject(classLoader, "st.redline.core.True");
        loadObject(classLoader, "st.redline.core.False");
        loadObject(classLoader, "st.redline.core.Collection");
        loadObject(classLoader, "st.redline.core.SequenceableCollection");
        loadObject(classLoader, "st.redline.core.ArrayedCollection");
        loadObject(classLoader, "st.redline.core.String");
        loadObject(classLoader, "st.redline.core.Symbol");
    }

    private void createPrimObject(SmalltalkClassLoader classLoader) {
        PrimObject primObject = new PrimObject();
        primObject.selfClass(primObject);
        classLoader.cacheObject("st.redline.core.PrimObject", primObject);
    }

    private void loadObject(ClassLoader classLoader, String name) {
        try {
            // Loading and instantiating the class causes the 'sendMessages' java method
            // to be called which executes all the message sends of the Smalltalk source.
            classLoader.loadClass(name).newInstance();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
