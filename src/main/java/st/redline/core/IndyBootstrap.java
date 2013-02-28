package st.redline.core;

import java.lang.invoke.CallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.MutableCallSite;
import st.redline.compiler.ClassBytecodeWriter;
import static st.redline.core.PrimObject.CLASS_INDEX;

/**
 * Boostrap and dispatch logic for invokedynamic call paths.
 */
public class IndyBootstrap {
    static class SmalltalkCallSite extends MutableCallSite {
        public final MethodHandles.Lookup lookup;
        public SmalltalkCallSite(MethodType methodType, MethodHandles.Lookup lookup) {
            super(methodType);
            this.lookup = lookup;
        }
    }
    
    public static CallSite performBootstrap(MethodHandles.Lookup lookup, String name, MethodType methodType) throws Throwable {
        SmalltalkCallSite site = new SmalltalkCallSite(methodType, lookup);
        
        MethodHandle target = lookup.findStatic(IndyBootstrap.class, name, methodType.insertParameterTypes(0, SmalltalkCallSite.class));
        target = target.bindTo(site);
        site.setTarget(target);
        
        return site;
    }
    
    private static final MethodHandle APPLY;
    private static final MethodHandle TEST;
    private static final MethodHandle[] PERFORMS;
    static {
        MethodHandle apply;
        MethodHandle test;
        MethodHandle[] performs = new MethodHandle[6];
        try {
            apply = MethodHandles.lookup().findVirtual(PrimObject.class, "apply", MethodType.methodType(PrimObject.class, PrimObject.class, PrimObject.class, String.class, PrimObject[].class));
            test = MethodHandles.lookup().findStatic(IndyBootstrap.class, "test", MethodType.methodType(boolean.class, PrimObject.class, Object.class, PrimObject.class));
            performs[0] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(PrimObject.class, SmalltalkCallSite.class, PrimObject.class, String.class));
            performs[1] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(PrimObject.class, SmalltalkCallSite.class, PrimObject.class, PrimObject.class, String.class));
            performs[2] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(PrimObject.class, SmalltalkCallSite.class, PrimObject.class, PrimObject.class, PrimObject.class, String.class));
            performs[3] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(PrimObject.class, SmalltalkCallSite.class, PrimObject.class, PrimObject.class, PrimObject.class, PrimObject.class, String.class));
            performs[4] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(PrimObject.class, SmalltalkCallSite.class, PrimObject.class, PrimObject.class, PrimObject.class, PrimObject.class, PrimObject.class, String.class));
            performs[5] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(PrimObject.class, SmalltalkCallSite.class, PrimObject.class, PrimObject.class, PrimObject.class, PrimObject.class, PrimObject.class, PrimObject.class, String.class));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        APPLY = apply;
        TEST = test;
        PERFORMS = performs;
    }
    
    private static PrimObject findMethod(PrimObject cls, String selector) {
        while (!cls.includesSelector(selector))
            cls = cls.superclass();
        PrimObject method = cls.methodFor(selector);
        
        return method;
    }
    
    private static boolean test(PrimObject self, Object classModifications, PrimObject expectedCls) {
        return PrimObjectClass.CLASS_MODIFICATIONS == classModifications && self.attributes[CLASS_INDEX] == expectedCls;
    }
    
    private static void bind(SmalltalkCallSite site, PrimObject method, Object classModifications, PrimObject cls, int args) {
        // bind method and class
        MethodHandle target = MethodHandles.insertArguments(APPLY, 1, method, cls);
        
        // reorder and group arguments
        target = target.asCollector(PrimObject[].class, args);
        switch (args) {
            case 0: break;
            case 1:
                target = MethodHandles.permuteArguments(target, site.type(), new int[]{0, 2, 1});
                break;
            case 2:
                target = MethodHandles.permuteArguments(target, site.type(), new int[]{0, 3, 1, 2});
                break;
            case 3:
                target = MethodHandles.permuteArguments(target, site.type(), new int[]{0, 4, 1, 2, 3});
                break;
            case 4:
                target = MethodHandles.permuteArguments(target, site.type(), new int[]{0, 5, 1, 2, 3, 4});
                break;
            case 5:
                target = MethodHandles.permuteArguments(target, site.type(), new int[]{0, 6, 1, 2, 3, 4, 5});
                break;
        }
        
        // add class modification and type guard
        MethodHandle test = MethodHandles.insertArguments(TEST, 1, classModifications, cls);
        test = MethodHandles.permuteArguments(test, site.type().changeReturnType(boolean.class), new int[]{0});
        MethodHandle fail = PERFORMS[args].bindTo(site);
        target = MethodHandles.guardWithTest(test, target, fail);
        
        site.setTarget(target);
    }
    
    public static PrimObject perform(SmalltalkCallSite site, PrimObject self, String selector) throws Throwable {
        PrimObject cls = self.attributes[CLASS_INDEX];
        Object classModifications = PrimObjectClass.CLASS_MODIFICATIONS;
        PrimObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 0);
        return self.apply(method, cls, selector);
    }
    
    public static PrimObject perform(SmalltalkCallSite site, PrimObject self, PrimObject arg1, String selector) {
        PrimObject cls = self.attributes[CLASS_INDEX];
        Object classModifications = PrimObjectClass.CLASS_MODIFICATIONS;
        PrimObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 1);
        return self.apply(method, cls, selector, arg1);
    }
    
    public static PrimObject perform(SmalltalkCallSite site, PrimObject self, PrimObject arg1, PrimObject arg2, String selector) {
        PrimObject cls = self.attributes[CLASS_INDEX];
        Object classModifications = PrimObjectClass.CLASS_MODIFICATIONS;
        PrimObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 2);
        return self.apply(method, cls, selector, arg1, arg2);
    }
    
    public static PrimObject perform(SmalltalkCallSite site, PrimObject self, PrimObject arg1, PrimObject arg2, PrimObject arg3, String selector) {
        PrimObject cls = self.attributes[CLASS_INDEX];
        Object classModifications = PrimObjectClass.CLASS_MODIFICATIONS;
        PrimObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 3);
        return self.apply(method, cls, selector, arg1, arg2, arg3);
    }
    
    public static PrimObject perform(SmalltalkCallSite site, PrimObject self, PrimObject arg1, PrimObject arg2, PrimObject arg3, PrimObject arg4, String selector) {
        PrimObject cls = self.attributes[CLASS_INDEX];
        Object classModifications = PrimObjectClass.CLASS_MODIFICATIONS;
        PrimObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 4);
        return self.apply(method, cls, selector, arg1, arg2, arg3, arg4);
    }
    
    public static PrimObject perform(SmalltalkCallSite site, PrimObject self, PrimObject arg1, PrimObject arg2, PrimObject arg3, PrimObject arg4, PrimObject arg5, String selector) {
        PrimObject cls = self.attributes[CLASS_INDEX];
        Object classModifications = PrimObjectClass.CLASS_MODIFICATIONS;
        PrimObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 5);
        return self.apply(method, cls, selector, arg1, arg2, arg3, arg4, arg5);
    }
}
