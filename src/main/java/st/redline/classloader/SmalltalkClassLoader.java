/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

// ------------------------------------------------------------------
// This ClassLoader uses delegation to make resetting the ClassLoader
// easy, and to provide tracing when requested.

import st.redline.compiler.ast.Block;
import st.redline.lang.*;

import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.Stack;
import java.util.concurrent.ConcurrentHashMap;

public class SmalltalkClassLoader extends ClassLoader {

    public int BLOCK_NUMBER = 0;
    public ProtoObject METACLASS;
    public ProtoObject NIL;
    public ProtoObject TRUE;
    public ProtoObject FALSE;
    public final Map<String, Object> blocksToBeCompiled = new Hashtable<String, Object>();
    public final Map<String, ProtoObject> blocks = new Hashtable<String, ProtoObject>();
    public final Map<String, String> blockReturnTypes = new HashMap<String, String>();

    private final Stack<EigenClass> eigenClasses;
    private final Map<String, ProtoObject> symbols;
    private final DelegateClassLoader delegate;
    private final Bootstrapper bootstrapper;

    private boolean bootstrapping = true;
    private ProtoObject protoObject;

    public SmalltalkClassLoader(ClassBuilder classBuilder, Bootstrapper bootstrapper, boolean trace) {
        this.eigenClasses = new Stack<EigenClass>();
        this.symbols = new HashMap<String, ProtoObject>();
        this.bootstrapper = bootstrapper;
        delegate = trace ? tracingClassLoader(classBuilder) : simpleClassLoader(classBuilder);
    }

    public void bootstrap() throws Exception {
        Thread.currentThread().setContextClassLoader(this);
        bootstrapper.bootstrap(this);
    }

    public void stoppedBootstrapping() {
        bootstrapping = false;
        isBootstrapping();
    }

    public boolean isBootstrapping() {
        return bootstrapping;
    }

    public synchronized void currentEigenClass(EigenClass eigenClass) {
        eigenClasses.push(eigenClass);
    }

    public synchronized EigenClass currentEigenClass() {
        return eigenClasses.peek();
    }

    public synchronized void removeEigenClass() {
        eigenClasses.pop();
    }

    public synchronized String currentPackage() {
        return currentEigenClass().packageName();
    }

    public DelegateClassLoader simpleClassLoader(ClassBuilder classBuilder) {
        return new SimpleClassLoader(this, classBuilder);
    }

    public DelegateClassLoader tracingClassLoader(ClassBuilder classBuilder) {
        return new TracingClassLoader(this, classBuilder);
    }

    public Class loadClass(String name) throws ClassNotFoundException {
        return delegate.loadClass(name);
    }

    public Class defineClass(byte[] classBytes) {
        return defineClass(classBytes, null);
    }

    public Class defineClass(byte[] classBytes, String name) {
        return defineClass(name, classBytes, 0, classBytes.length);
    }

    public DelegateClassLoader delegate() {
        return delegate;
    }

    public void registerProtoObject(ProtoClass protoObject) {
        this.protoObject = protoObject;
        registerSmalltalkClass("ProtoObject", protoObject);
    }

    public void registerSmalltalkClass(String name, ProtoObject aClass) {
        delegate.registerSmalltalkClass(name, aClass);
    }

    public ProtoObject loadSmalltalkClass(String name, boolean resolve) throws ClassNotFoundException {
        return delegate.loadSmalltalkClass(name, resolve);
    }

    public ProtoObject loadSmalltalkClass(String name) throws ClassNotFoundException {
        return delegate.loadSmalltalkClass(name, true);
    }

    public boolean isInternedSymbol(String symbol) {
        return symbols.containsKey(symbol);
    }

    public ProtoObject internedSymbolAt(String symbol) {
        return symbols.get(symbol);
    }

    public void internSymbolAtPut(String symbol, ProtoObject symbolObject) {
        symbols.put(symbol, symbolObject);
    }

    public void registerBlockToBeCompiled(Block block, String fullBlockClassName) {
        if (blocksToBeCompiled.containsKey(fullBlockClassName))
            throw new IllegalStateException("Block to be compiled registered twice: " + fullBlockClassName);
        blocksToBeCompiled.put(fullBlockClassName, block);
    }

    public ProtoObject loadSmalltalkBlock(String name, PrimContext context) {
        if (blocks.containsKey(name))
            return createBlockInstance(blocks.get(name), context);
        Block block = (Block) blocksToBeCompiled.remove(name);
        if (block == null)
            throw new IllegalStateException("Block to be compiled '" + name + "' not found.");
        block.accept(block.analyser());
        try {
            ProtoObject newblock = (ProtoObject) defineClass(block.classBytes()).newInstance();
            blocks.put(name, newblock);
            return createBlockInstance(newblock, context);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    protected ProtoObject createBlockInstance(ProtoObject block, PrimContext context) {
        if (block.isMethodBlock())
            return block;
        try {
            Constructor constructor = block.getClass().getConstructor(PrimContext.class);
            return (ProtoBlock) constructor.newInstance(context);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public Class loadBlockReturn(String blockReturnType) throws ClassNotFoundException {
        return super.loadClass(blockReturnType);
    }

    public boolean hasBlockReturnType(String fullyQualifiedClassName) {
        return blockReturnTypes.containsKey(fullyQualifiedClassName);
    }

    public void addBlockReturnType(String blockReturnTypeName) {
        blockReturnTypes.put(blockReturnTypeName, blockReturnTypeName);
    }

    // ----------------------------

    interface DelegateClassLoader {
        Class loadClass(String name) throws ClassNotFoundException;
        void registerSmalltalkClass(String name, ProtoObject aClass);
        ProtoObject loadSmalltalkClass(String name, boolean resolve) throws ClassNotFoundException;
    }

    // ----------------------------------
    // A simple non-tracing class loader.

    class SimpleClassLoader implements DelegateClassLoader {

        private final SmalltalkClassLoader owner;
        protected final ClassBuilder classBuilder;
        protected final Map<String, ProtoObject> classes;

        public SimpleClassLoader(SmalltalkClassLoader owner, ClassBuilder classBuilder) {
            this.owner = owner;
            this.classBuilder = classBuilder;
            this.classes = new HashMap<String, ProtoObject>();
        }

        public Class loadClass(String name) throws ClassNotFoundException {
            return findClass(name);
        }

        protected Class findClass(String name) throws ClassNotFoundException {
            byte[] classBytes = buildClass(name);
            if (classBytes != null)
                return defineClass(classBytes);
            /* check if system class */
            try { return findSystemClass(name); } catch (Exception e) {}
            return null;
        }

        protected Class defineClass(byte[] classBytes) {
            return owner.defineClass(classBytes);
        }

        protected byte[] buildClass(String name) {
            return classBuilder.build(name);
        }

        public void registerSmalltalkClass(String name, ProtoObject aClass) {
            if (classes.containsKey(name))
                System.err.println("WARNING: '" + name + "' registered twice.");
            classes.put(name, aClass);
        }

        public ProtoObject loadSmalltalkClass(String name, boolean resolve) throws ClassNotFoundException {
            ProtoObject aClass = classes.get(name);
            if (aClass != null)
                return aClass;
            if (resolve)
                // NOTE: We do not currently cache classes when found.
                try {
                    Class javaClass = findClass(name);
                    if (javaClass != null)
                        return (ProtoObject) javaClass.newInstance();
                } catch (Exception e) {
                    e.printStackTrace();
                    throw new ClassNotFoundException(name + " " + e.getMessage());
                }
            return null;
        }
    }

    // -------------------------------------------------
    // A tracing class loader - turn on with command line option: -verbose:stclass

    class TracingClassLoader extends SimpleClassLoader {

        public TracingClassLoader(SmalltalkClassLoader owner, ClassBuilder classBuilder) {
            super(owner, classBuilder);
        }

        public Class loadClass(String name) throws ClassNotFoundException {
            System.out.println("[LoadClass " + name + "]");
            return findClass(name);
        }

        protected byte[] buildClass(String name) {
            StringBuilder path = new StringBuilder();
            byte[] bytes = classBuilder.build(name, path);
            if (path.length() > 0)
                System.out.println("[Loaded " + name + " from " + path + "]");
            return bytes;
        }

        public void registerSmalltalkClass(String name, ProtoObject aClass) {
            System.out.println("[Registering Smalltalk Class " + name + " as " + aClass + "]");
            super.registerSmalltalkClass(name, aClass);
        }

        public ProtoObject loadSmalltalkClass(String name, boolean resolve) throws ClassNotFoundException {
            System.out.println("[Smalltalk LoadClass " + name + "]");
            return super.loadSmalltalkClass(name, resolve);
        }
    }
}
