package st.redline;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;

public class ProtoObject {

	private static final ProtoObject[] NO_ARGUMENTS = {};

	private ProtoObject _class_;
	private ProtoObject[] _variables_;
	private Map<String, Method> _methods_;
	private Object _primitiveValue_;

	public static ProtoObject objectFromPrimitive(String value, boolean isSymbol) {
//		System.out.println("objectFromPrimitive(" + value + ", " + isSymbol + ") - TODO");
		ProtoObject object = Smalltalk.classNamed( isSymbol ? "Symbol" : "String");
		if (object == null)
			object = new ProtoObject();
		return object.primitiveValue(value);
	}

	public static ProtoObject specialInstance() {
		ProtoObject subclass = new ProtoObject();
		Map<String, Method> methods = new HashMap<String, Method>();
		methods.put("subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:", subclass.specialSubclassMethod());
		subclass.method$(methods);
		subclass.clas$(subclass);  // <- note cyclic reference.
		return subclass;
	}

	private Method specialSubclassMethod() {
		try {
			return ProtoObject.class.getDeclaredMethod("prim$ubclass",
					new Class[] { ProtoObject.class, ProtoObject.class, ProtoObject.class, ProtoObject.class, ProtoObject.class } );
		} catch (NoSuchMethodException e) {
			/* its our method so we dont expect this. */
		}
		return null;
	}

	public ProtoObject prim$ubclass(ProtoObject subclassName, ProtoObject instanceVariableNames, ProtoObject classVariableNames, ProtoObject poolDictionaries, ProtoObject category) {
		System.out.println(this.getClass().getName() + " prim$ubclass(" + subclassName + ", " + instanceVariableNames + ", " + classVariableNames + ", " + poolDictionaries + ", " + category + ")");
		ProtoObject subclass = Smalltalk.classNamed(subclassName.toString());
		if (subclass == null)
			throw new IllegalStateException("Expected '" + subclassName + "' to have been registered.");
		System.out.println("subclass of type: " + subclass.getClass().getName());
		subclass.clas$(Smalltalk.classNamed("ProtoObject"));
		System.out.println();
		if (subclassName.toString().equals("Object"))
			System.out.println("*** fixup required here now all Object dependencies are loaded ***\n");
		return subclass;
	}

	public ProtoObject prim$init(String className, Class aClass) {
		System.out.println("prim$init: '" + className + "', " + this.getClass().getName() + " and " + aClass);
		Smalltalk.register(className, this);
		return this;
	}

	public String toString() {
		if (_primitiveValue_ != null)
			return _primitiveValue_.toString();
		return getClass().toString();
	}

	protected ProtoObject primitiveValue(Object value) {
		_primitiveValue_ = value;
		return this;
	}

	protected Object primitiveValue() {
		return _primitiveValue_;
	}

	protected ProtoObject clas$() {
		return _class_;
	}

	protected ProtoObject clas$(ProtoObject aClass) {
		System.out.println("ProtoObject.clas$() set to: " + aClass.getClass().getName());
		_class_ = aClass;
		return this;
	}

	protected Map<String, Method> method$() {
		return _methods_;
	}

	protected ProtoObject method$(Map<String, Method> methods) {
		_methods_ = methods;
		return this;
	}

	protected ProtoObject[] variable$() {
		return _variables_;
	}

	public ProtoObject prim$end(String selector) {
		System.out.println("prim$end(" + selector + ")");
		return tryInvokeMethod(selector, NO_ARGUMENTS);
	}
	
	protected Method findMethod(String selector) {
		System.out.println("findMethod reached in ProtoObject - " + selector + " " + getClass().getName());
		return _class_.method$().get(selector);
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

	private static Map<String, Method> methodsFrom(ProtoObject aClass) {
		return methodsFrom(aClass.getClass());
	}

	private static Map<String, Method> methodsFrom(Class aClass) {
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