/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import st.redline.bootstrap.ClassSubclassMethod;
import st.redline.bootstrap.InstanceVariableNamesMethod;

import java.io.File;

public class Bootstrapper {

	private ProtoObject protoObject;
	private ProtoObject object;
	private ProtoObject collection;
	private ProtoObject sequenceableCollection;
	private ProtoObject arrayedCollection;
	private ProtoObject string;
	private ProtoObject symbol;

	protected Bootstrapper(ProtoObject protoObject) {
		this.protoObject = protoObject;
	}

	public void bootstrap() throws ClassNotFoundException {
		markBootstrapping(true);
		mapPackages();
		registerRootClasses();
		instantiateSingletons();
		createClasses();
		makeClassSuperclassOfObjectsClass();
		reclassNil();
		markBootstrapping(false);
	}

	private void reclassNil() throws ClassNotFoundException {
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

	private void instantiateSingletons() {
		ProtoObject.METACLASS_INSTANCE = createMetaclassInstance();
		ProtoObject.NIL = createUndefinedObjectInstance();
		ProtoObject.TRUE = createTrueInstance();
		ProtoObject.FALSE = createFalseInstance();
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

	private ProtoObject createTrueInstance() {
		ProtoObject classClass = new ProtoObject();
		ProtoObject cls = new ProtoObject(classClass);
		cls.name("True");
		return new ProtoObject(cls);
	}

	private ProtoObject createFalseInstance() {
		ProtoObject classClass = new ProtoObject();
		ProtoObject cls = new ProtoObject(classClass);
		cls.name("False");
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
