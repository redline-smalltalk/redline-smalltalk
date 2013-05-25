package st.redline.lang;

import java.lang.invoke.CallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.invoke.MutableCallSite;
import st.redline.compiler.ClassBytecodeWriter;

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
            apply = MethodHandles.lookup().findVirtual(ProtoObject.class, "apply", MethodType.methodType(ProtoObject.class, ProtoObject.class, ProtoObject.class, String.class, ProtoObject[].class));
            test = MethodHandles.lookup().findStatic(IndyBootstrap.class, "test", MethodType.methodType(boolean.class, ProtoObject.class, Object.class, ProtoObject.class));
            performs[0] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(ProtoObject.class, SmalltalkCallSite.class, ProtoObject.class, String.class));
            performs[1] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(ProtoObject.class, SmalltalkCallSite.class, ProtoObject.class, ProtoObject.class, String.class));
            performs[2] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(ProtoObject.class, SmalltalkCallSite.class, ProtoObject.class, ProtoObject.class, ProtoObject.class, String.class));
            performs[3] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(ProtoObject.class, SmalltalkCallSite.class, ProtoObject.class, ProtoObject.class, ProtoObject.class, ProtoObject.class, String.class));
            performs[4] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(ProtoObject.class, SmalltalkCallSite.class, ProtoObject.class, ProtoObject.class, ProtoObject.class, ProtoObject.class, ProtoObject.class, String.class));
            performs[5] = MethodHandles.lookup().findStatic(IndyBootstrap.class, "perform", MethodType.methodType(ProtoObject.class, SmalltalkCallSite.class, ProtoObject.class, ProtoObject.class, ProtoObject.class, ProtoObject.class, ProtoObject.class, ProtoObject.class, String.class));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        APPLY = apply;
        TEST = test;
        PERFORMS = performs;
    }

    private static ProtoObject findMethod(ProtoObject cls, String selector) {
        while (!cls.includesSelector(selector))
            cls = cls.superclass();
        ProtoObject method = cls.methodFor(selector);

        return method;
    }

    private static boolean test(ProtoObject self, Object classModifications, ProtoObject expectedCls) {
        return ProtoClass.CLASS_MODIFICATIONS == classModifications && self.selfclass == expectedCls;
    }

    private static void bind(SmalltalkCallSite site, ProtoObject method, Object classModifications, ProtoObject cls, int args) {
        // bind method and class
        MethodHandle target = MethodHandles.insertArguments(APPLY, 1, method, cls);

        // reorder and group arguments
        target = target.asCollector(ProtoObject[].class, args);
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

    public static ProtoObject perform(SmalltalkCallSite site, ProtoObject self, String selector) throws Throwable {
        ProtoObject cls = self.selfclass;
        Object classModifications = ProtoClass.CLASS_MODIFICATIONS;
        ProtoObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 0);
        return self.apply(method, cls, selector);
    }

    public static ProtoObject perform(SmalltalkCallSite site, ProtoObject self, ProtoObject arg1, String selector) {
        ProtoObject cls = self.selfclass;
        Object classModifications = ProtoClass.CLASS_MODIFICATIONS;
        ProtoObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 1);
        return self.apply(method, cls, selector, arg1);
    }

    public static ProtoObject perform(SmalltalkCallSite site, ProtoObject self, ProtoObject arg1, ProtoObject arg2, String selector) {
        ProtoObject cls = self.selfclass;
        Object classModifications = ProtoClass.CLASS_MODIFICATIONS;
        ProtoObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 2);
        return self.apply(method, cls, selector, arg1, arg2);
    }

    public static ProtoObject perform(SmalltalkCallSite site, ProtoObject self, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, String selector) {
        ProtoObject cls = self.selfclass;
        Object classModifications = ProtoClass.CLASS_MODIFICATIONS;
        ProtoObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 3);
        return self.apply(method, cls, selector, arg1, arg2, arg3);
    }

    public static ProtoObject perform(SmalltalkCallSite site, ProtoObject self, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, String selector) {
        ProtoObject cls = self.selfclass;
        Object classModifications = ProtoClass.CLASS_MODIFICATIONS;
        ProtoObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 4);
        return self.apply(method, cls, selector, arg1, arg2, arg3, arg4);
    }

    public static ProtoObject perform(SmalltalkCallSite site, ProtoObject self, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, String selector) {
        ProtoObject cls = self.selfclass;
        Object classModifications = ProtoClass.CLASS_MODIFICATIONS;
        ProtoObject method = findMethod(cls, selector);
        bind(site, method, classModifications, cls, 5);
        return self.apply(method, cls, selector, arg1, arg2, arg3, arg4, arg5);
    }
}
