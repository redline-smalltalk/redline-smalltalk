/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import st.redline.bootstrap.ClassSubclassMethod;

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

	public void bootstrap() {
		markBootstrapping(true);
		mapPackages();
		setupProtoObject();
		registerRootClasses();
		instantiateSingletons();
		createClasses();
		tearDownProtoObject();
		markBootstrapping(false);
	}

	private void tearDownProtoObject() {
		protoObject.cls(null);
	}

	private void setupProtoObject() {
		protoObject.cls(protoObject);
		protoObject.methodAtPut("<", new ClassSubclassMethod());
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
		ProtoObject.METACLASS_INSTANCE = createTemporaryMetaclassInstance();
	}

	private ProtoObject createTemporaryMetaclassInstance() {
		ProtoObject classClass = new ProtoObject(protoObject);
		ProtoObject cls = new ProtoObject(classClass);
		cls.name("Metaclass(Bootstrapped)");
		cls.superclass(protoObject);
		return Primitives.p70(cls, null, null, null, null, null, null, null, null);
	}

	private void createClasses() {
		SmalltalkClassLoader smalltalk = currentClassLoader();
		loadUsing("st.redline.Object", smalltalk);
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
