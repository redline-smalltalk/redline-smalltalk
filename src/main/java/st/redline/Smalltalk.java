package st.redline;

import java.util.HashMap;
import java.util.Map;

public class Smalltalk {

	private static final Map<String, ProtoObject> classes;
	static {
		classes = new HashMap<String, ProtoObject>();
	}

	public static void register(String className, ProtoObject aClass) {
		classes.put(className, aClass);
	}

	public static ProtoObject classNamed(String className) {
		System.out.println("classNamed(" + className + ") exists: " + classes.containsKey(className));
		return classes.get(className);
	}
}