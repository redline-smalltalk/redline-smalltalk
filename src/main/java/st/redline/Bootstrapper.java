/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import st.redline.bootstrap.*;

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
	private final ProtoObject array;
	private final ProtoObject arrayMetaClass;
	private final ProtoObject string;
	private final ProtoObject stringMetaClass;
	private final ProtoObject symbol;
	private final ProtoObject symbolMetaClass;
	private final ProtoObject undefinedObject;
	private final ProtoObject undefinedObjectMetaClass;
	private final ProtoObject smalltalkImage;
	private final ProtoObject smalltalkImageMetaClass;
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
		this.array = new ProtoObject("Array");
		this.arrayMetaClass = new ProtoObject("ArrayMetaClass");
		this.string = new ProtoObject("String");
		this.stringMetaClass = new ProtoObject("StringMetaClass");
		this.symbol = new ProtoObject("Symbol");
		this.symbolMetaClass = new ProtoObject("SymbolMetaClass");
		this.metaClass = new ProtoObject("MetaClass");
		this.metaClassMetaClass = new ProtoObject("MetaClassMetaClass");
		this.undefinedObject = new ProtoObject("UndefinedObject");
		this.undefinedObjectMetaClass = new ProtoObject("UndefinedObjectMetaClass");
		this.smalltalkImage = new ProtoObject("SmalltalkImage");
		this.smalltalkImageMetaClass = new ProtoObject("SmalltalkImageMetaClass");
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
		loadUsing("st.redline.Object", smalltalk);
		loadUsing("st.redline.Behavior", smalltalk);
		loadUsing("st.redline.ClassDescription", smalltalk);
		loadUsing("st.redline.Class", smalltalk);
		loadUsing("st.redline.MetaClass", smalltalk);
		loadUsing("st.redline.Boolean", smalltalk);
		loadUsing("st.redline.True", smalltalk);
		loadUsing("st.redline.False", smalltalk);
		loadUsing("st.redline.Collection", smalltalk);
		loadUsing("st.redline.SequenceableCollection", smalltalk);
		loadUsing("st.redline.ArrayedCollection", smalltalk);
		loadUsing("st.redline.String", smalltalk);
		loadUsing("st.redline.Symbol", smalltalk);
		loadUsing("st.redline.UndefinedObject", smalltalk);
		loadUsing("st.redline.SmalltalkImage", smalltalk);
		loadUsing("st.redline.Array", smalltalk);
	}

	private void loadUsing(String name, SmalltalkClassLoader smalltalk) {
		try {
			smalltalk.findClass(name).newInstance();
		} catch (Exception e) {
			throw RedlineException.withCauseAndMessage(
					String.format("Unable to load class %s", name), e);
		}
	}

	private SmalltalkClassLoader currentClassLoader() {
		return (SmalltalkClassLoader) Thread.currentThread().getContextClassLoader();
	}

	private void bootstrapMethods() {
		cls.methodAtPut("<", new ClassSubclassMethod());
		cls.methodAtPut("import:", new ImportMethod());
		cls.methodAtPut("subclass:instanceVariableNames:classVariableNames:classInstanceVariableNames:poolDictionaries:category:", new ClassSubclassWithVariablesMethod());
		cls.methodAtPut("instanceVariableNames:", new InstanceVariableNamesMethod());
		cls.methodAtPut("classVariableNames:", new ClassVariableNamesMethod());
		cls.methodAtPut("classInstanceVariableNames:", new ClassInstanceVariableNamesMethod());
		cls.methodAtPut("poolDictionaries:", new PoolDictionariesMethod());
		cls.methodAtPut("category:", new CategoryMethod());
	}

	private void bootstrapClasses() {
		associateClasses();
		registerClasses();
		mapPackages();
		instantiateSingletons();
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

		arrayMetaClass.cls(metaClass);
		arrayMetaClass.superclass(arrayedCollectionMetaClass);
		array.cls(arrayMetaClass);
		array.superclass(arrayedCollection);

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

		smalltalkImageMetaClass.cls(metaClass);
		smalltalkImageMetaClass.superclass(objectMetaClass);
		smalltalkImage.cls(smalltalkImageMetaClass);
		smalltalkImage.superclass(object);

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
		ProtoObject.primitiveRegisterAs(array, "st.redline.Array");
		ProtoObject.primitiveRegisterAs(symbol, "st.redline.Symbol");
		ProtoObject.primitiveRegisterAs(string, "st.redline.String");
		ProtoObject.primitiveRegisterAs(undefinedObject, "st.redline.UndefinedObject");
		ProtoObject.primitiveRegisterAs(smalltalkImage, "st.redline.SmalltalkImage");
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

		ProtoObject smalltalkInstance = new ProtoObject(false);
		smalltalkInstance.cls(smalltalkImage);
		ProtoObject.instanceOfSmalltalk = smalltalkInstance;
	}

	private void mapPackages() {
		ProtoObject.packageMap.put("ProtoObject", "st.redline.ProtoObject");
		for (String sourceFile : SourceFileFinder.findIn("st" + File.separator + "redline")) {
			String packageName = ClassPathUtilities.filenameWithExtensionToPackageName(sourceFile);
			String name = ClassPathUtilities.filenameToClassName(sourceFile);
			ProtoObject.packageMap.put(name, packageName + "." + name);
		}
	}
}
