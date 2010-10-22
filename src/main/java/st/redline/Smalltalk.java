package st.redline;

import java.util.HashMap;
import java.util.Map;

public class Smalltalk {

	private static final Map<String, ProtoObject> classes;
	static {
		ProtoObject bootstrap = ProtoObject.specialInstance();
		classes = new HashMap<String, ProtoObject>();
		classes.put("ProtoObject", bootstrap);
		classes.put("Object", bootstrap);
	}

	public static ProtoObject register(String className, ProtoObject aClass) {
		System.out.println("register(" + className + ", " + aClass + ")");
		classes.put(className, aClass);
		return aClass;
	}

	public static ProtoObject classNamed(String className) {
//		System.out.println("classNamed(" + className + ") exists: " + classes.containsKey(className));
		return classes.get(className);
	}
}