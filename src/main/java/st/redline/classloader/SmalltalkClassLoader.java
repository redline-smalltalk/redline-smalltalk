package st.redline.classloader;

import st.redline.compiler.Compiler;
import st.redline.core.PrimObject;

import java.util.HashMap;
import java.util.Map;

public class SmalltalkClassLoader extends ClassLoader {

    // Special Object instance values set during bootstrapping.
    private static PrimObject NIL;
    private static PrimObject TRUE;
    private static PrimObject FALSE;

    private final SourceFinder sourceFinder;
    private final Map<String, Class> classCache;
    private final Map<String, PrimObject> objectCache;
    private final Map<Object, Map<String, String>> packageCache;
    private boolean bootstrapping;

    public SmalltalkClassLoader(ClassLoader classLoader, SourceFinder sourceFinder, Bootstrapper bootstrapper) {
        super(classLoader);
        this.sourceFinder = sourceFinder;
        this.classCache = new HashMap<String, Class>();
        this.objectCache = new HashMap<String, PrimObject>();
        this.packageCache = new HashMap<Object, Map<String, String>>();

        // initialize Object cache with bootstrapped objects.
        bootstrapper.bootstrap(this);
    }

    public void registerPackage(String name, PrimObject group) {
        System.out.println("** registerPackage: " +  name + " " + group);
        // NOTE: For some reason getClass().getPackage() returns NULL.
        String className = group.getClass().getName();
        String groupName = className.substring(0, className.lastIndexOf('.'));
        Map<String, String> imports = packageCache.getOrDefault(groupName, new HashMap<>());
        imports.put(name, groupName + '.' + name);
        packageCache.put(groupName, imports);
    }

    public String findPackage(PrimObject group, String name) {
        System.out.println("** findPackage: " + group + " " + name);
        String className = group.getClass().getName();
        String groupName = className.substring(0, className.lastIndexOf('.'));
        return packageCache.get(groupName).get(name);
    }

    public PrimObject findObject(String name) {
        System.out.println("** findObject " + name);
        PrimObject cls = cachedObject(name);
        if (cls != null)
            return cls;
        try {
            findClass(name);
            cls = cachedObject(name);
            if (cls != null)
                return cls;
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } catch (ObjectNotFoundException e) {
            e.printStackTrace();
        }
        throw new ObjectNotFoundException("Object '" + name + "' was not found.");
    }

    protected PrimObject cachedObject(String name) {
        System.out.println("** cachedObject " + name);
        return objectCache.get(name);
    }

    public void cacheObject(String name, PrimObject object) {
        System.out.println("** cacheObject " + object + " as " + name);
        objectCache.put(name, object);
    }

    public Class findClass(String name) throws ClassNotFoundException {
        System.out.println("** findClass " + name);
        Class cls = cachedClass(name);
        if (cls != null)
            return cls;
        byte[] classData = loadClassData(name);
        if (classData == null)
            return super.findClass(name);
        cls = defineClass(null, classData, 0, classData.length);
        cacheClass(cls, name);
        return cls;
    }

    private void cacheClass(Class cls, String name) {
        System.out.println("** cacheClass " + cls + " as " + name);
        classCache.put(name, cls);
    }

    private Class cachedClass(String name) {
        return classCache.get(name);
    }

    private byte[] loadClassData(String name) {
        return compile(findSource(name));
    }

    private Source findSource(String name) {
        return sourceFinder.find(name);
    }

    private byte[] compile(Source source) {
        return compiler(source).compile();
    }

    private Compiler compiler(Source source) {
        return new Compiler(source);
    }

    public void beginBootstrapping() {
        bootstrapping = true;
    }

    public void endBootstrapping() {
        bootstrapping = false;
    }

    public boolean isBootstrapping() {
        return bootstrapping;
    }

    public void nilInstance(PrimObject nil) {
        NIL = nil;
    }

    public PrimObject nilInstance() {
        return NIL;
    }

    public void falseInstance(PrimObject instance) {
        FALSE = instance;
    }

    public PrimObject falseInstance() {
        return FALSE;
    }

    public void trueInstance(PrimObject instance) {
        TRUE = instance;
    }

    public PrimObject trueInstance() {
        return TRUE;
    }
}
