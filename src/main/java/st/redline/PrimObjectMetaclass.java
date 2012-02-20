/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

// Provides a way of constructing CLASSES. A Metaclass is a class who's instances are CLASSES of CLASSES.
// Each class is an instance of its own Metaclass.
// Call basicSubclassOf: to get a subclass of Metaclass,
// Call basicCreate:... to create the Metaclass' sole instance.

import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

public class PrimObjectMetaclass extends PrimObjectClass {

	static final Integer ZERO_SIZE = 0;
	static final int DEFAULT_ATTRIBUTE_COUNT = 2;  // for name, superclass etc etc
	static final int NAME_INDEX = PrimObjectClass.SUPERCLASS_INDEX + 1;
	static final int INSTANCE_SIZE_INDEX = NAME_INDEX + 1;
	static final PrimObjectMetaclass METACLASS;

	static {
		PrimObject.BOOTSTRAPPING = true;
		METACLASS = basicSubclassOf(null).basicCreate("Metaclass", PrimObject.PRIM_NIL, "", "", "", "");
	}
	static Map<String, String> IMPORTS = new Hashtable<String, String>();

	Map<String, String> imports;

	public static PrimObjectMetaclass basicSubclassOf(PrimObjectMetaclass superMeta) {
		PrimObjectMetaclass newMetaclass = new PrimObjectMetaclass();
		newMetaclass.superclass(superMeta);
		return newMetaclass;
	}

	public PrimObjectMetaclass basicCreate(String name, PrimObject superclass, String instVarNames, String classVarNames,
	                                       String poolDictionaries, String classInstVarNames) {
		return basicCreate(string(name), superclass, string(instVarNames), string(classVarNames),
				string(poolDictionaries), string(classInstVarNames));
	}

	public PrimObjectMetaclass basicCreate(PrimObject name, PrimObject superclass, PrimObject instVarNames, PrimObject classVarNames,
	                                       PrimObject poolDictionaries, PrimObject classInstVarNames) {
		PrimObjectMetaclass newClass = new PrimObjectMetaclass();
		newClass.cls(this);
		newClass.superclass(superclass);
		newClass.name(name);
		// TODO.jcl take other parameters into account.
		return newClass;
	}

	public String toString() {
		if (name().javaValue == null)
			return super.toString();
		return (String) name().javaValue;
	}

	PrimObject name() {
		return attributes[NAME_INDEX];
	}

	PrimObjectMetaclass name(String name) {
		return name(string(name));
	}

	PrimObjectMetaclass name(PrimObject name) {
		attributes[NAME_INDEX] = name;
		return this;
	}

	public PrimObjectMetaclass() {
		this(0);
	}

	PrimObjectMetaclass(int basicSize) {
		super(basicSize + DEFAULT_ATTRIBUTE_COUNT);
		cls(METACLASS);
	}

	void bootstrap() {
		new Bootstrapper(METACLASS).bootstrap();
	}

	public String packageFor(String name) {
		String packageName;
		if (imports != null && (packageName = imports.get(name)) != null)
			return packageName;
		if (superclass() != null && (packageName = superclass().packageFor(name)) != null)
			return packageName;
		return IMPORTS.get(name);
	}

	protected PrimObject _sendMessages_(PrimObject receiver, PrimContext context) {
		return this;
	}
}
