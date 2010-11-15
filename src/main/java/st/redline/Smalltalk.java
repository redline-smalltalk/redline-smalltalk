package st.redline;

import java.util.HashMap;
import java.util.Map;

public class Smalltalk {

	private static Map<String, ProtoObject> classesByName = new HashMap<String, ProtoObject>();
	private static Map<java.lang.Class, ProtoObject> classesByPrimitiveClass = new HashMap<java.lang.Class, ProtoObject>();
	private static ProtoObject nil = null;

	// ensure Smalltalk classes we rely on are loaded.
	static {
		basicClassNamed("Object");
		basicClassNamed("String");
		basicClassNamed("Symbol");
	}

	public static void register(java.lang.Class primitiveClass, ProtoObject objectClass) {
		String name = primitiveClass.getName().substring(primitiveClass.getName().lastIndexOf('.')+1);
		System.out.println("Registering " + name + " as " + objectClass);
		classesByName.put(name, objectClass);
		classesByPrimitiveClass.put(primitiveClass, objectClass);
	}

	public static st.redline.ProtoObject classNamed(String className) {
		return classesByName.get(className);
	}

	public static st.redline.ProtoObject classForPrimitiveClass(java.lang.Class primitiveClass) {
		System.out.println("Getting class for the primitive Class: " + primitiveClass);
		return classesByPrimitiveClass.get(primitiveClass);
	}

	public static ProtoObject nil() {
		if (nil == null)
			nil = instanceOf("UndefinedObject");
		return nil;
	}

	private static ProtoObject instanceOf(String className) {
		try {
			return (ProtoObject) basicClassNamed("st.redline." + className).newInstance();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private static Class basicClassNamed(String className) {
		try {
			return Class.forName("st.redline." + className);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}