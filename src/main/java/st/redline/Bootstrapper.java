/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import st.redline.bootstrap.AccessClassMethod;
import st.redline.bootstrap.AtSelectorPutMethod;
import st.redline.bootstrap.CreateSubclassMethod;
import st.redline.bootstrap.InitializeMethod;

import java.io.File;
import java.util.Map;

public class Bootstrapper {

	PrimObjectMetaclass primObjectMetaclass;

	Bootstrapper(PrimObjectMetaclass primObjectMetaclass) {
		this.primObjectMetaclass = primObjectMetaclass;
	}

	public void bootstrap() {
		markBootstrapping(true);
		mapPackages(PrimObjectMetaclass.IMPORTS);
		createAndRegisterProtoObject();
		registerBootstrappedSingletons();
		makeClassSuperclassOfObjectsClass();
		makeClassDescriptionSuperclassOfMetaclassClass();
		markBootstrapping(false);
		instantiateNonBootstrappedSingletons();
	}

	void makeClassDescriptionSuperclassOfMetaclassClass() {
		primObjectMetaclass.superclass(primObjectMetaclass.resolveObject("st.redline.ClassDescription"));
	}

	void makeClassSuperclassOfObjectsClass() {
		PrimObjectClass objectClass = (PrimObjectClass) primObjectMetaclass.resolveObject("st.redline.Object").cls();
		objectClass.superclass(primObjectMetaclass.resolveObject("st.redline.Class"));
	}

	void instantiateNonBootstrappedSingletons() {
		PrimObject.NIL.cls(primObjectMetaclass.resolveObject("st.redline.UndefinedObject"));
		PrimObject.TRUE = primObjectMetaclass.resolveObject("st.redline.True").perform("new");
		PrimObject.FALSE = primObjectMetaclass.resolveObject("st.redline.False").perform("new");
	}

	void registerBootstrappedSingletons() {
		PrimObject.CLASSES.put("st.redline.Metaclass", primObjectMetaclass);
		primObjectMetaclass.methods().put("atSelector:put:", new AtSelectorPutMethod());
		PrimObjectMetaclass undefinedObjectMetaClass = PrimObjectMetaclass.basicSubclassOf(primObjectMetaclass);
		PrimObjectMetaclass undefinedObjectClass = undefinedObjectMetaClass.basicCreate("UndefinedObject", null, "", "", "", "");
		PrimObject.NIL = new PrimObject();
		PrimObject.NIL.cls(undefinedObjectClass);
	}

	void markBootstrapping(boolean bootstrapping) {
		PrimObject.BOOTSTRAPPING = bootstrapping;
	}

	void createAndRegisterProtoObject() {
		PrimObjectMetaclass protoObjectMetaclass = PrimObjectMetaclass.basicSubclassOf(primObjectMetaclass);
		protoObjectMetaclass.methods().put("<", new CreateSubclassMethod());
		protoObjectMetaclass.methods().put("atSelector:put:", new AtSelectorPutMethod());
		protoObjectMetaclass.methods().put("class", new AccessClassMethod());
		protoObjectMetaclass.methods().put("initialize", new InitializeMethod());
		PrimObjectMetaclass protoObjectClass = protoObjectMetaclass.basicCreate("ProtoObject", null, "", "", "", "");
		protoObjectClass.superclass(PrimObject.PRIM_NIL);
		PrimObject.CLASSES.put("st.redline.ProtoObject", protoObjectClass);
	}

	void mapPackages(Map<String, String> imports) {
		imports.put("ProtoObject", "st.redline.ProtoObject");
		for (String sourceFile : SourceFileFinder.findIn("st" + File.separator + "redline")) {
			String packageName = ClassPathUtilities.filenameWithExtensionToPackageName(sourceFile);
			String name = ClassPathUtilities.filenameToClassName(sourceFile);
			imports.put(name, packageName + "." + name);
		}
	}
}
