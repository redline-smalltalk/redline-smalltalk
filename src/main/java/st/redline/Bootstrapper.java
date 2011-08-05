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

Please see DEVELOPER-CERTIFICATE-OF-ORIGIN if you wish to contribute a patch to Redline Smalltalk.
*/
package st.redline;

import st.redline.bootstrap.ClassSubclassMethod;

import java.io.File;

public class Bootstrapper {

	private final ProtoObject metaClassMetaClass;
	private final ProtoObject metaClass;
	private final ProtoObject protoObject;
	private final ProtoObject protoObjectMetaClass;
	private final ProtoObject object;
	private final ProtoObject objectMetaClass;
	private final ProtoObject behavior;
	private final ProtoObject behaviorMetaClass;
	private final ProtoObject classDescription;
	private final ProtoObject classDescriptionMetaClass;
	private final ProtoObject cls;
	private final ProtoObject clsMetaClass;
	private final ProtoObject collection;
	private final ProtoObject collectionMetaClass;
	private final ProtoObject sequenceableCollection;
	private final ProtoObject sequenceableCollectionMetaClass;
	private final ProtoObject arrayedCollection;
	private final ProtoObject arrayedCollectionMetaClass;
	private final ProtoObject string;
	private final ProtoObject stringMetaClass;
	private final ProtoObject symbol;
	private final ProtoObject symbolMetaClass;
	private final ProtoObject undefinedObject;
	private final ProtoObject undefinedObjectMetaClass;
	private final ProtoObject bool;
	private final ProtoObject boolMetaClass;
	private final ProtoObject boolTrue;
	private final ProtoObject boolTrueMetaClass;
	private final ProtoObject boolFalse;
	private final ProtoObject boolFalseMetaClass;

	protected Bootstrapper(ProtoObject protoObject) {
		// TODO.JCL remove passing name of object - just for tracing reasons.
		this.protoObject = protoObject;
		this.protoObjectMetaClass = new ProtoObject("ProtoObjectMetaClass");
		this.object = new ProtoObject("Object");
		this.objectMetaClass = new ProtoObject("ObjectMetaClass");
		this.behavior = new ProtoObject("Behavior");
		this.behaviorMetaClass = new ProtoObject("BehaviorMetaClass");
		this.classDescription = new ProtoObject("ClassDescription");
		this.classDescriptionMetaClass = new ProtoObject("ClassDescriptionMetaClass");
		this.cls = new ProtoObject("Class");
		this.clsMetaClass = new ProtoObject("ClassMetaClass");
		this.collection = new ProtoObject("Collection");
		this.collectionMetaClass = new ProtoObject("CollectionMetaClass");
		this.sequenceableCollection = new ProtoObject("SequenceableCollection");
		this.sequenceableCollectionMetaClass = new ProtoObject("SequenceableCollectionMetaClass");
		this.arrayedCollection = new ProtoObject("ArrayedCollection");
		this.arrayedCollectionMetaClass = new ProtoObject("ArrayedCollectionMetaClass");
		this.string = new ProtoObject("String");
		this.stringMetaClass = new ProtoObject("StringMetaClass");
		this.symbol = new ProtoObject("Symbol");
		this.symbolMetaClass = new ProtoObject("SymbolMetaClass");
		this.metaClass = new ProtoObject("MetaClass");
		this.metaClassMetaClass = new ProtoObject("MetaClassMetaClass");
		this.undefinedObject = new ProtoObject("UndefinedObject");
		this.undefinedObjectMetaClass = new ProtoObject("UndefinedObjectMetaClass");
		this.bool = new ProtoObject("Boolean");
		this.boolMetaClass = new ProtoObject("BooleanMetaClass");
		this.boolTrue = new ProtoObject("True");
		this.boolTrueMetaClass = new ProtoObject("TrueMetaClass");
		this.boolFalse = new ProtoObject("False");
		this.boolFalseMetaClass = new ProtoObject("FalseMetaClass");
	}

	public void bootstrap() {
		bootstrapClasses();
		bootstrapMethods();
		loadBaseClasses();
	}

	private void loadBaseClasses() {
		SmalltalkClassLoader smalltalk = currentClassLoader();
		System.out.println("*********************");
		loadUsing("st.redline.Object", smalltalk);
		loadUsing("st.redline.Behavior", smalltalk);
	}

	private void loadUsing(String name, SmalltalkClassLoader smalltalk) {
		try {
			smalltalk.findClass(name).newInstance();
		} catch (Exception e) {
			throw new RedlineException(e);
		}
	}

	private SmalltalkClassLoader currentClassLoader() {
		return (SmalltalkClassLoader) Thread.currentThread().getContextClassLoader();
	}

	private void bootstrapMethods() {
		cls.methodAtPut("<", new ClassSubclassMethod());
	}

	private void bootstrapClasses() {
		markBootstrappedClasses();
		associateClasses();
		registerClasses();
		mapPackages();
		instantiateSingletons();
	}

	private void markBootstrappedClasses() {
		metaClassMetaClass.bootstrapped();
		metaClass.bootstrapped();
		object.bootstrapped();
		objectMetaClass.bootstrapped();
		behavior.bootstrapped();
		behaviorMetaClass.bootstrapped();
		classDescription.bootstrapped();
		classDescriptionMetaClass.bootstrapped();
		cls.bootstrapped();
		clsMetaClass.bootstrapped();
		collection.bootstrapped();
		collectionMetaClass.bootstrapped();
		sequenceableCollection.bootstrapped();
		sequenceableCollectionMetaClass.bootstrapped();
		arrayedCollection.bootstrapped();
		arrayedCollectionMetaClass.bootstrapped();
		string.bootstrapped();
		stringMetaClass.bootstrapped();
		symbol.bootstrapped();
		symbolMetaClass.bootstrapped();
		undefinedObject.bootstrapped();
		undefinedObjectMetaClass.bootstrapped();
		bool.bootstrapped();
		boolMetaClass.bootstrapped();
		boolTrue.bootstrapped();
		boolTrueMetaClass.bootstrapped();
		boolFalse.bootstrapped();
		boolFalseMetaClass.bootstrapped();
	}

	private void associateClasses() {
		protoObjectMetaClass.cls(metaClass);
		protoObjectMetaClass.superclass(cls);
		protoObject.cls(protoObjectMetaClass);

		objectMetaClass.cls(metaClass);
		objectMetaClass.superclass(protoObjectMetaClass);
		object.cls(objectMetaClass);
		object.superclass(protoObject);

		behaviorMetaClass.cls(metaClass);
		behaviorMetaClass.superclass(objectMetaClass);
		behavior.cls(behaviorMetaClass);
		behavior.superclass(object);

		classDescriptionMetaClass.cls(metaClass);
		classDescriptionMetaClass.superclass(behaviorMetaClass);
		classDescription.cls(classDescriptionMetaClass);
		classDescription.superclass(behavior);

		clsMetaClass.cls(metaClass);
		clsMetaClass.superclass(classDescriptionMetaClass);
		cls.cls(clsMetaClass);
		cls.superclass(classDescription);

		metaClassMetaClass.superclass(classDescriptionMetaClass);
		metaClass.cls(metaClassMetaClass);
		metaClass.superclass(classDescription);

		collectionMetaClass.cls(metaClass);
		collectionMetaClass.superclass(objectMetaClass);
		collection.cls(collectionMetaClass);
		collection.superclass(object);

		sequenceableCollectionMetaClass.cls(metaClass);
		sequenceableCollectionMetaClass.superclass(collectionMetaClass);
		sequenceableCollection.cls(sequenceableCollectionMetaClass);
		sequenceableCollection.superclass(collection);

		arrayedCollectionMetaClass.cls(metaClass);
		arrayedCollectionMetaClass.superclass(sequenceableCollectionMetaClass);
		arrayedCollection.cls(arrayedCollectionMetaClass);
		arrayedCollection.superclass(sequenceableCollection);

		stringMetaClass.cls(metaClass);
		stringMetaClass.superclass(arrayedCollectionMetaClass);
		string.cls(stringMetaClass);
		string.superclass(arrayedCollection);

		symbolMetaClass.cls(metaClass);
		symbolMetaClass.superclass(stringMetaClass);
		symbol.cls(symbolMetaClass);
		symbol.superclass(string);

		undefinedObjectMetaClass.cls(metaClass);
		undefinedObjectMetaClass.superclass(objectMetaClass);
		undefinedObject.cls(undefinedObjectMetaClass);
		undefinedObject.superclass(object);

		boolMetaClass.cls(metaClass);
		boolMetaClass.superclass(objectMetaClass);
		bool.cls(boolMetaClass);
		bool.superclass(object);

		boolTrueMetaClass.cls(metaClass);
		boolTrueMetaClass.superclass(boolMetaClass);
		boolTrue.cls(boolTrueMetaClass);
		boolTrue.superclass(bool);

		boolFalseMetaClass.cls(metaClass);
		boolFalseMetaClass.superclass(boolMetaClass);
		boolFalse.cls(boolFalseMetaClass);
		boolFalse.superclass(bool);
	}

	private void registerClasses() {
		ProtoObject.primitiveRegisterAs(protoObject, "st.redline.ProtoObject");
		ProtoObject.primitiveRegisterAs(object, "st.redline.Object");
		ProtoObject.primitiveRegisterAs(metaClass, "st.redline.MetaClass");
		ProtoObject.primitiveRegisterAs(behavior, "st.redline.Behavior");
		ProtoObject.primitiveRegisterAs(classDescription, "st.redline.ClassDescription");
		ProtoObject.primitiveRegisterAs(cls, "st.redline.Class");
		ProtoObject.primitiveRegisterAs(collection, "st.redline.Collection");
		ProtoObject.primitiveRegisterAs(sequenceableCollection, "st.redline.SequenceableCollection");
		ProtoObject.primitiveRegisterAs(arrayedCollection, "st.redline.ArrayedCollection");
		ProtoObject.primitiveRegisterAs(symbol, "st.redline.Symbol");
		ProtoObject.primitiveRegisterAs(string, "st.redline.String");
		ProtoObject.primitiveRegisterAs(undefinedObject, "st.redline.UndefinedObject");
		ProtoObject.primitiveRegisterAs(bool, "st.redline.Boolean");
		ProtoObject.primitiveRegisterAs(boolTrue, "st.redline.True");
		ProtoObject.primitiveRegisterAs(boolFalse, "st.redline.False");
	}

	private void instantiateSingletons() {
		ProtoObject nil = new ProtoObject(false);
		nil.cls(undefinedObject);
		ProtoObject.instanceOfUndefinedObject = nil;

		ProtoObject trueInstance = new ProtoObject(false);
		trueInstance.cls(boolTrue);
		ProtoObject.instanceOfTrue = trueInstance;

		ProtoObject falseInstance = new ProtoObject(false);
		falseInstance.cls(boolFalse);
		ProtoObject.instanceOfFalse = falseInstance;
	}

	private void mapPackages() {
		ProtoObject.packageMap.put("ProtoObject", "st.redline.ProtoObject");
		for (String sourceFile : SourceFileFinder.findIn("st/redline")) {
			String packageName = sourceFile.substring(0, sourceFile.lastIndexOf("/"));
			String name = sourceFile.substring(packageName.length() + 1, sourceFile.lastIndexOf("."));
//			System.out.println(packageName + " " + name + " " + packageName.replaceAll(File.separator, ".") + "." + name);
			ProtoObject.packageMap.put(name, packageName.replaceAll(File.separator, ".") + "." + name);
		}
	}
}
