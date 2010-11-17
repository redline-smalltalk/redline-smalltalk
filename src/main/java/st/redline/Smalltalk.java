package st.redline;

import java.util.HashMap;
import java.util.Map;

public class Smalltalk {

	private static Map<String, ProtoObject> classesByName = new HashMap<String, ProtoObject>();
	private static Map<java.lang.Class, ProtoObject> classesByPrimitiveClass = new HashMap<java.lang.Class, ProtoObject>();
	private static ProtoObject _nil = null;
	private static ProtoObject _true = null;
	private static ProtoObject _false = null;

	public static void register(java.lang.Class primitiveClass, ProtoObject objectClass) {
		String name = primitiveClass.getName().substring(primitiveClass.getName().lastIndexOf('.')+1);
		System.out.println("Registering " + name + " as " + objectClass);
		classesByName.put(name, objectClass);
		classesByPrimitiveClass.put(primitiveClass, objectClass);
	}

	public static st.redline.ProtoObject classNamed(String className) {
		System.out.println("classNamed: " + className);
		ProtoObject aClass = classesByName.get(className);
		if (aClass == null)
			basicClassNamed(className);
		return classesByName.get(className);
	}

	public static st.redline.ProtoObject classForPrimitiveClass(java.lang.Class primitiveClass) {
		System.out.println("Getting class for the primitive Class: " + primitiveClass);
		return classesByPrimitiveClass.get(primitiveClass);
	}

	public static ProtoObject $nil() {
		if (_nil == null) {
			_nil = classNamed("UndefinedObject").$send("new");
		}
		return _nil;
	}

	public static ProtoObject $true() {
		if (_true == null) {
			_true = classNamed("True").$send("new");
		}
		return _true;
	}

	public static ProtoObject $false() {
		if (_false == null) {
			_false = classNamed("False").$send("new");
		}
		return _false;
	}

//	private static ProtoObject instanceOf(String className) {
//		try {
//			return (ProtoObject) basicClassNamed("st.redline." + className).newInstance();
//		} catch (Exception e) {
//			throw new RuntimeException(e);
//		}
//	}

	private static Class basicClassNamed(String className) {
		try {
			return Class.forName("st.redline." + className);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}