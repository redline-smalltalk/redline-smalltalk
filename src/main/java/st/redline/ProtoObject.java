package st.redline;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;

public class ProtoObject {

	private static final ProtoObject[] NO_ARGUMENTS = {};

	public Map<String, Method> _methods_;

	public ProtoObject() {
		System.out.println("*** PROTOOBJECT CREATED ***");
	}

	public ProtoObject prim$end(String selector) {
		System.out.println("prim$end(" + selector + ")");
		return tryInvokeMethod(selector, NO_ARGUMENTS);
	}

	protected Method findMethod(String selector) {
		System.out.println("findMethod reached in ProtoObject - " + selector + " " + getClass().getName());
		return null; //_class_.method$().get(selector);
	}

	protected ProtoObject tryInvokeMethod(String selector, Object[] arguments) {
		Method method = findMethod(selector);
		if (method != null)
			try {
				return (ProtoObject) method.invoke(this, arguments);
			} catch (IllegalAccessException e) {
				e.printStackTrace();
				return this;
			} catch (InvocationTargetException e) {
				e.printStackTrace();
				return this;
			}
		return sendDoesNotUnderstand(selector, arguments);
	}

	private ProtoObject sendDoesNotUnderstand(String selector, Object[] arguments) {
		System.out.println("sendDoesNotUnderstand(" + selector + "," + arguments + ")");
		// TODO.
		throw new IllegalStateException(this.getClass().getName() + " does not understand #" + selector);
	}

	public ProtoObject prim$end(String selector, ProtoObject arg1) {
		System.out.println("prim$end(" + selector + "," + arg1.toString() + ")");
		return tryInvokeMethod(selector, new ProtoObject[] { arg1 });
	}

	public ProtoObject prim$end(String selector, ProtoObject arg1, ProtoObject arg2) {
		System.out.println("prim$end(" + selector + "," + arg1.toString() + "," + arg2.toString() + ")");
		return tryInvokeMethod(selector, new ProtoObject[] { arg1, arg2 });
	}

	public ProtoObject prim$end(String selector, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3) {
		System.out.println("prim$end(" + selector + "," + arg1.toString() + "," + arg2.toString() + "," + arg3.toString() + ")");
		return tryInvokeMethod(selector, new ProtoObject[] { arg1, arg2, arg3 });
	}

	public ProtoObject prim$end(String selector, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4) {
		System.out.println("prim$end(" + selector + "," + arg1.toString() + "," + arg2.toString() + "," + arg3.toString() + "," + arg4.toString() + ")");
		return tryInvokeMethod(selector, new ProtoObject[] { arg1, arg2, arg3, arg4 });
	}

	public ProtoObject prim$end(String selector, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5) {
		System.out.println(this.getClass().getName() + " prim$end(" + selector + "," + arg1.toString() + "," + arg2.toString() + "," + arg3.toString() + "," + arg4.toString() + "," + arg5.toString() + ")");
		return tryInvokeMethod(selector, new ProtoObject[] { arg1, arg2, arg3, arg4, arg5 });
	}

	public static Map<String, Method> methodsFrom(Class aClass) {
		System.out.println("methodsFrom(" + aClass.getName() + ")");
		Map<String, Method> methods = new HashMap<String, Method>();
		for (Method method : aClass.getDeclaredMethods()) {
			if (Modifier.isPublic(method.getModifiers())
				&& !Modifier.isStatic(method.getModifiers())
				&& !method.getName().startsWith("prim$")) {
				System.out.println("Caching method: " + method);
				methods.put(method.getName(), method);
			}
		}
		return methods;
	}
}
