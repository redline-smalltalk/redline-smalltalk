package st.redline;

import java.util.HashMap;
import java.util.Map;

public class Smalltalk {

	private static Map<String, ProtoObject> classes = new HashMap<String, ProtoObject>();

	public static void register(java.lang.Class primitiveClass, ProtoObject objectClass) {
		String name = primitiveClass.getName().substring(primitiveClass.getName().lastIndexOf('.')+1);
		System.out.println("Registering " + name + " as " + objectClass);
		classes.put(name, objectClass);
	}

	public static st.redline.ProtoObject classNamed(String className) {
		return classes.get(className);
	}
}