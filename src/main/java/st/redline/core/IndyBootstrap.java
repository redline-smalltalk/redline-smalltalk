package st.redline.core;

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
    public static CallSite performBootstrap(MethodHandles.Lookup lookup, String name, MethodType methodType) throws Throwable {
        MutableCallSite mcs = new MutableCallSite(methodType);
        
        MethodHandle target = lookup.findStatic(IndyBootstrap.class, name, methodType.insertParameterTypes(0, MutableCallSite.class));
        target = target.bindTo(mcs);
        mcs.setTarget(target);
        
        return mcs;
    }
    
    public static PrimObject perform(MutableCallSite mcs, PrimObject self, String name) {
        return self.perform(name);
    }
    
    public static PrimObject perform(MutableCallSite mcs, PrimObject self, PrimObject arg1, String name) {
        return self.perform(arg1, name);
    }
    
    public static PrimObject perform(MutableCallSite mcs, PrimObject self, PrimObject arg1, PrimObject arg2, String name) {
        return self.perform(arg1, arg2, name);
    }
    
    public static PrimObject perform(MutableCallSite mcs, PrimObject self, PrimObject arg1, PrimObject arg2, PrimObject arg3, String name) {
        return self.perform(arg1, arg2, arg3, name);
    }
    
    public static PrimObject perform(MutableCallSite mcs, PrimObject self, PrimObject arg1, PrimObject arg2, PrimObject arg3, PrimObject arg4, String name) {
        return self.perform(arg1, arg2, arg3, arg4, name);
    }
    
    public static PrimObject perform(MutableCallSite mcs, PrimObject self, PrimObject arg1, PrimObject arg2, PrimObject arg3, PrimObject arg4, PrimObject arg5, String name) {
        return self.perform(arg1, arg2, arg3, arg4, arg5, name);
    }
}
