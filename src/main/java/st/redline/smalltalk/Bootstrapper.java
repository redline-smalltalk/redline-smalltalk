/*
Redline Smalltalk is licensed under the MIT License

Redline Smalltalk Copyright (c) 2010 James C. Ladd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
package st.redline.smalltalk;

public class Bootstrapper {

	private static final String SUBCLASSING_SELECTOR = "subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:";
	private static final String NEW_SELECTOR = "new";
	private static final String METACLASS_NAME = "Metaclass";
	private static final boolean BOOTSTRAPPED = true;
	private static final boolean NOT_BOOSTRAPPED = false;

	private final Smalltalk smalltalk;

	public Bootstrapper(Smalltalk smalltalk) {
		this.smalltalk = smalltalk;
	}

	public void bootstrap() {
		RObject metaclassClass = createBootstrappedClass(METACLASS_NAME);
		RObject protoObjectClass = createBootstrappedClass("ProtoObject", metaclassClass);
		RObject objectClass = createBootstrappedSubclass("Object", protoObjectClass, metaclassClass);
		RObject behaviorClass = createBootstrappedSubclass("Behavior", objectClass, metaclassClass);
		RObject classDescriptionClass = createBootstrappedSubclass("ClassDescription", behaviorClass, metaclassClass);
		RObject classClass = createBootstrappedSubclass("Class", classDescriptionClass, metaclassClass);
		RObject collectionClass = createBootstrappedSubclass("Collection", objectClass, metaclassClass);
		RObject sequenceableCollectionClass = createBootstrappedSubclass("SequenceableCollection", collectionClass, metaclassClass);
		RObject arrayedCollectionClass = createBootstrappedSubclass("ArrayedCollection", sequenceableCollectionClass, metaclassClass);
		RObject stringClass = createBootstrappedSubclass("String", arrayedCollectionClass, metaclassClass);
		RObject symbolClass = createBootstrappedSubclass("Symbol", stringClass, metaclassClass);
		RObject undefinedObjectClass = createBootstrappedSubclass("UndefinedObject", objectClass, metaclassClass);

		// fixup hierarchy.
		protoObjectClass.oop[RObject.CLASS_OFFSET].oop[RObject.SUPERCLASS_OFFSET] = classClass;
		metaclassClass.oop[RObject.SUPERCLASS_OFFSET] = classDescriptionClass;
		metaclassClass.oop[RObject.CLASS_OFFSET].oop[RObject.SUPERCLASS_OFFSET] = classDescriptionClass.oop[RObject.CLASS_OFFSET];

		// create and register 'nil' instance.
		RObject nil = RObject.instanceInstance();
		nil.oop[RObject.CLASS_OFFSET] = undefinedObjectClass;
		smalltalk.primitiveAtPut("nil", nil);

		// add bootstrapped methods.
		stringClass.oop[RObject.CLASS_OFFSET].data.methodAtPut(NEW_SELECTOR, new PrimitiveNewMethod());
		undefinedObjectClass.oop[RObject.CLASS_OFFSET].data.methodAtPut(NEW_SELECTOR, new PrimitiveNewNotAllowedMethod());
		classClass.data.methodAtPut(SUBCLASSING_SELECTOR, new PrimitiveSubclassMethod());
	}

	private RObject createBootstrappedSubclass(String name, RObject superclass, RObject metaclassClass) {
		return createSubclass(name, superclass, metaclassClass, BOOTSTRAPPED);
	}

	private RObject createSubclass(String name, RObject superclass, RObject metaclassClass, boolean bootstrapped) {
		RObject aClass = createClass(name, metaclassClass, bootstrapped);
		// set aClass' superclass and the superclass of its metaclass (mirrored hierarchy).
		aClass.oop[RObject.SUPERCLASS_OFFSET] = superclass;
		aClass.oop[RObject.CLASS_OFFSET].oop[RObject.SUPERCLASS_OFFSET] = superclass.oop[RObject.CLASS_OFFSET];
		return aClass;
	}

	private RObject createBootstrappedClass(String name) {
		return createClass(name, BOOTSTRAPPED);
	}

	private RObject createClass(String name, boolean bootstrapped) {
		RObject aClass = RObject.classInstance();
		RObject aMetaclass = RObject.classInstance();
		aClass.oop[RObject.CLASS_OFFSET] = aMetaclass;
		aClass.data.primitiveName(name);
		aClass.data.bootstrapped(bootstrapped);
		smalltalk.primitiveAtPut(name, aClass);
		return aClass;
	}

	private RObject createBootstrappedClass(String name, RObject metaclassClass) {
		return createClass(name, metaclassClass, BOOTSTRAPPED);
	}

	private RObject createClass(String name, RObject metaclassClass, boolean bootstrapped) {
		RObject aClass = createClass(name, bootstrapped);
		aClass.oop[RObject.CLASS_OFFSET].oop[RObject.CLASS_OFFSET] = metaclassClass;
		return aClass;
	}

	public class PrimitiveNewMethod extends RMethod {
		public RObject applyTo(RObject receiver) {
			RObject instance = RObject.primitiveInstance();
			instance.oop[RObject.CLASS_OFFSET] = receiver;
			return instance;
		}
	}

	public class PrimitiveNewNotAllowedMethod extends RMethod {
		public RObject applyTo(RObject receiver) {
			throw new IllegalStateException("Can't new an instance of UndefinedObject.");
		}
	}

	public class PrimitiveSubclassMethod extends RMethod {
		public RObject applyToWith(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5) {
			System.out.println("** HERE **");
			// There is more to do here just not yet.
			String name = arg1.data.primitiveValue().toString();
			RObject existing = smalltalk.cachedObject0(name);
			if (existing != null) {
				existing.data.bootstrapped(false);
				return existing;
			}
			RObject metaclassClass = smalltalk.primitiveAt(METACLASS_NAME);
			return createSubclass(name, receiver, metaclassClass, NOT_BOOSTRAPPED);
		}
	}
}
