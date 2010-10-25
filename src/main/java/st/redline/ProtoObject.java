package st.redline;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;

public abstract class ProtoObject {

	protected Map<String, Method> methodDictionary;

	public abstract ProtoObject $class();

	public abstract ProtoObject $super();

	public ProtoObject() {
		System.out.println("constructing ProtoObject");
	}

	public ProtoObject $send(java.lang.String selector) {
		System.out.println("$send(" + selector + ") " + this + " " + $class() + " " + $class().methodDictionary);
		ProtoObject aClass = $class();
		java.lang.reflect.Method method = aClass.methodDictionary.get(selector);
		if (method == null) {
			aClass = aClass.$super();
			System.out.println(aClass);
			method = aClass.$class().methodDictionary.get(selector);
			System.out.println(method);
		}
		throw new RuntimeException("Don't have method " + selector);
	}

	protected Map<String, Method> methodsFrom(java.lang.Class primitiveClass) {
		System.out.println("methodsFrom(" + primitiveClass + ")");
		Map<String, Method> methods = new HashMap<String, Method>();
		for (Method method : primitiveClass.getDeclaredMethods()) {
			if (Modifier.isPublic(method.getModifiers())
				&& !Modifier.isStatic(method.getModifiers())
				&& !method.getName().startsWith("$")) {
				System.out.println("Caching method: " + method);
				methods.put(method.getName(), method);
			}
		}
		return methods;
	}
}