/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import st.redline.bootstrap.*;
import st.redline.bootstrap.InitializeMethod;

import java.io.File;

public class Bootstrapper {

	private ProtoObject protoObject;

	protected Bootstrapper(ProtoObject protoObject) {
		this.protoObject = protoObject;
	}

	public void bootstrap() throws ClassNotFoundException {
		markBootstrapping(true);
		mapPackages();
		registerRootClasses();
		instantiateBootstrappedSingletons();
		createClasses();
		makeClassSuperclassOfObjectsClass();
		reclassBootstrappedSingletons();
		markBootstrapping(false);
		instantiateNonBootstrappedSingletons();
	}

	private void reclassBootstrappedSingletons() throws ClassNotFoundException {
		ProtoObject.NIL.cls(Primitives.resolveObject(protoObject, "UndefinedObject"));
	}

	private void makeClassSuperclassOfObjectsClass() throws ClassNotFoundException {
		ProtoObject cls = Primitives.resolveObject(protoObject, "Class");
		ProtoObject object = Primitives.resolveObject(protoObject, "Object");
		object.cls().superclass(cls);
	}

	private void tearDownProtoObject() {
		protoObject.cls(null);
	}

	private void setupProtoObject() {
		protoObject.cls(protoObject);
		protoObject.methodAtPut("<", new ClassSubclassMethod());
		protoObject.methodAtPut("instanceVariableNames:", new InstanceVariableNamesMethod());
		protoObject.methodAtPut("classVariableNames:", new ClassVariableNamesMethod());
		protoObject.methodAtPut("classInstanceVariableNames:", new ClassInstanceVariableNamesMethod());
		protoObject.methodAtPut("poolDictionaries:", new PoolDictionariesMethod());
		protoObject.methodAtPut("category:", new CategoryMethod());
		protoObject.methodAtPut("import:", new ImportMethod());
        protoObject.methodAtPut("initialize", new InitializeMethod());
	}

	private void markBootstrapping(boolean bootstrapping) {
		Primitives.bootstrapping = bootstrapping;
	}

	private void loadUsing(String name, SmalltalkClassLoader smalltalk) {
		try {
			smalltalk.findClass(name).newInstance();
		} catch (Exception e) {
			markBootstrapping(false);
			throw RedlineException.withCauseAndMessage(String.format("Unable to load class %s", name), e);
		}
	}

	private void instantiateBootstrappedSingletons() {
		ProtoObject.METACLASS_INSTANCE = createMetaclassInstance();
		ProtoObject.NIL = createUndefinedObjectInstance();
	}

	private void instantiateNonBootstrappedSingletons() throws ClassNotFoundException {
		ProtoObject trueClass = Primitives.resolveObject(protoObject, "st.redline.True");
		ProtoObject.TRUE = Primitives.send(trueClass, "new", null);
		ProtoObject falseClass = Primitives.resolveObject(protoObject, "st.redline.False");
		ProtoObject.FALSE = Primitives.send(falseClass, "new", null);
	}

	private ProtoObject createMetaclassInstance() {
		ProtoObject classClass = new ProtoObject();
		ProtoObject cls = new ProtoObject(classClass);
		cls.name("Metaclass");
		return cls;
	}

	private ProtoObject createUndefinedObjectInstance() {
		ProtoObject classClass = new ProtoObject();
		ProtoObject cls = new ProtoObject(classClass);
		cls.name("UndefinedObject");
		return new ProtoObject(cls);
	}

	private void createClasses() {
		SmalltalkClassLoader smalltalk = currentClassLoader();
		setupProtoObject();
		loadUsing("st.redline.Object", smalltalk);
		tearDownProtoObject();
		loadUsing("st.redline.UndefinedObject", smalltalk);
		loadUsing("st.redline.Symbol", smalltalk);
		loadUsing("st.redline.Class", smalltalk);
		loadUsing("st.redline.True", smalltalk);
		loadUsing("st.redline.False", smalltalk);
	}

	private SmalltalkClassLoader currentClassLoader() {
		return (SmalltalkClassLoader) Thread.currentThread().getContextClassLoader();
	}

	private void registerRootClasses() {
		Primitives.registerAs(protoObject, "st.redline.ProtoObject");
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
