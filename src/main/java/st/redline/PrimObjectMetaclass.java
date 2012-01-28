/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

// Provides a way of constructing classes. A Metaclass is a class who's instances are classes of classes.
// Each class is an instance of its own Metaclass.
// Call basicSubclassOf: to get a subclass of Metaclass,
// Call basicCreate:... to create the Metaclass' sole instance.

public class PrimObjectMetaclass extends PrimObjectClass {

	static final PrimObjectMetaclass METACLASS = new PrimObjectMetaclass();
	static final int DEFAULT_ATTRIBUTE_COUNT = 1;  // for name, superclass etc etc
	static final int NAME_INDEX = PrimObjectClass.SUPERCLASS_INDEX + 1;

	static {
		METACLASS.cls(null);
	}

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
	}

	String packageFor(String name) {
		return null;
	}

	protected PrimObject _sendMessages_(PrimObject receiver, PrimContext context) {
		return this;
	}
}
