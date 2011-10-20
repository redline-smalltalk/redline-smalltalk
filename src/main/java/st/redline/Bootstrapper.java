/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

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
		createClasses();
		instantiateSingletons();
		registerClasses();
		mapPackages();
	}

	private void instantiateSingletons() {
//		ProtoObject.METACLASS_INSTANCE = special case.
	}

	private void createClasses() {
		object = Primitives.createSubclass(protoObject, "Object");
		collection = Primitives.createSubclass(object, "Collection");
		sequenceableCollection = Primitives.createSubclass(collection, "SequenceableCollection");
		arrayedCollection = Primitives.createSubclass(sequenceableCollection, "ArrayedCollection");
		string = Primitives.createSubclass(arrayedCollection, "String");
		symbol = Primitives.createSubclass(string, "Symbol");
	}

	private void registerClasses() {
		Primitives.registerAs(protoObject, "st.redline.ProtoObject");
		Primitives.registerAs(object, "st.redline.Object");
		Primitives.registerAs(collection, "st.redline.Collection");
		Primitives.registerAs(sequenceableCollection, "st.redline.SequenceableCollection");
		Primitives.registerAs(arrayedCollection, "st.redline.ArrayedCollection");
		Primitives.registerAs(string, "st.redline.String");
		Primitives.registerAs(symbol, "st.redline.Symbol");
//		Primitives.registerAs(ProtoObject.METACLASS_INSTANCE, "st.redline.MetaClass");
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
