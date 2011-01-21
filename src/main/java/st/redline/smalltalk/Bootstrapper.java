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

	private final Smalltalk smalltalk;

	public Bootstrapper(Smalltalk smalltalk) {
		this.smalltalk = smalltalk;
	}

	public void bootstrap() {
		bootstrapNil();
		bootstrapLiterals();
		bootstrapProtoObject();
	}

	private void bootstrapProtoObject() {
		RObject classClass = bootstrapClassClass();
		RObject protoObjectClassMetaclass = bootstrapProtoObjectClassMetaclass(classClass);
		RObject protoObjectClass = bootstrapProtoObjectClass(protoObjectClassMetaclass);
		smalltalk.basicAtPut("Class", classClass);
		smalltalk.basicAtPut("ProtoObject", protoObjectClass);
	}

	private void bootstrapLiterals() {
		bootstrapStringLiteral();
		bootstrapSymbolLiteral();
	}

	private void bootstrapSymbolLiteral() {
		RObject symbolSuperclass = RObject.classInstance();
		RObject symbolClass = bootstrapPrimitiveClass(new NewMethod());
		smalltalk.basicAtPut("Symbol", symbolClass);
		symbolClass.oop[RObject.SUPERCLASS_OFFSET] = symbolSuperclass;
	}

	private void bootstrapStringLiteral() {
		RObject stringSuperclass = RObject.classInstance();
		RObject stringClass = bootstrapPrimitiveClass(new NewMethod());
		stringClass.oop[RObject.SUPERCLASS_OFFSET] = stringSuperclass;
		smalltalk.basicAtPut("String", stringClass);
	}

	private void bootstrapNil() {
		RObject undefinedObjectClass = bootstrapPrimitiveClass(new IllegalToNewMethod());
		RObject undefinedObject = RObject.primitiveInstance();
		undefinedObject.oop[RObject.CLASS_OFFSET] = undefinedObjectClass;
		smalltalk.basicAtPut("nil", undefinedObject);
		smalltalk.basicAtPut("UndefinedObject", undefinedObjectClass);
	}

	private RObject bootstrapPrimitiveClass(RMethod newMethod) {
		RObject primClass = RObject.classInstance();
		RObject primMetaclass = RObject.classInstance();
		primClass.oop[RObject.CLASS_OFFSET] = primMetaclass;
		primMetaclass.data.methodAtPut(NEW_SELECTOR, newMethod);
		return primClass;
	}

	private RObject bootstrapClassClass() {
		// TODO.JCL initialize super and metaclass.
		RObject classClass = RObject.classInstance();
		classClass.oop[RObject.CLASS_OFFSET] = null;
		classClass.oop[RObject.SUPERCLASS_OFFSET] = null;
		classClass.data.methodAtPut(SUBCLASSING_SELECTOR, new SubclassMethod());
		return classClass;
	}

	private RObject bootstrapProtoObjectClassMetaclass(RObject superclass) {
		RObject protoObjectClassMetaclass = RObject.classInstance();
		protoObjectClassMetaclass.oop[RObject.CLASS_OFFSET] = null;
		protoObjectClassMetaclass.oop[RObject.SUPERCLASS_OFFSET] = superclass;
		return protoObjectClassMetaclass;
	}

	private RObject bootstrapProtoObjectClass(RObject metaclass) {
		RObject protoObjectClass = RObject.classInstance();
		protoObjectClass.oop[RObject.CLASS_OFFSET] = metaclass;
		protoObjectClass.oop[RObject.SUPERCLASS_OFFSET] = null;
		return protoObjectClass;
	}

	public class SubclassMethod extends RMethod {
		public RObject applyToWith(RObject receiver, RObject arg1, RObject arg2, RObject arg3, RObject arg4, RObject arg5) {
			String subclass = arg1.data.primitiveValue().toString();
			RObject newClass = findOrCreateClass(subclass);
			RObject metaclass = findOrCreateMetaclass(newClass);
			newClass.oop[RObject.CLASS_OFFSET] = metaclass;
			newClass.oop[RObject.SUPERCLASS_OFFSET] = receiver;
			metaclass.oop[RObject.SUPERCLASS_OFFSET] = receiver.oop[RObject.CLASS_OFFSET].oop[RObject.SUPERCLASS_OFFSET];
			Smalltalk.instance().basicAtPut(subclass, newClass);
			return newClass;
		}
	}

	private RObject findOrCreateMetaclass(RObject aClass) {
		if (aClass.oop[RObject.CLASS_OFFSET] != null)
			return aClass.oop[RObject.CLASS_OFFSET];
		return RObject.classInstance();
	}

	private RObject findOrCreateClass(String subclassName) {
		RObject subclass = smalltalk.cachedObject0(subclassName);
		if (subclass != null)
			return subclass;
		return RObject.classInstance();
	}

	public class NewMethod extends RMethod {
		public RObject applyTo(RObject receiver) {
			RObject instance = RObject.primitiveInstance();
			instance.oop[RObject.CLASS_OFFSET] = receiver;
			return instance;
		}
	}

	public class IllegalToNewMethod extends RMethod {
		public RObject applyTo(RObject receiver) {
			throw new IllegalStateException("Can't new an instance of UndefinedObject.");
		}
	}
}
