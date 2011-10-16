/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import java.io.File;

public class Bootstrapper {

	private ProtoObject protoObject;

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
	}

	private void registerClasses() {
		Primitives.registerAs(protoObject, "st.redline.ProtoObject");
//		Primitives.registerAs(ProtoObject.METACLASS_INSTANCE, "st.redline.MetaClass");
	}

	private void mapPackages() {
		ProtoObject.packageMap.put("ProtoObject", "st.redline.ProtoObject");
		for (String sourceFile : SourceFileFinder.findIn("st" + File.separator + "redline")) {
			String packageName = ClassPathUtilities.filenameWithExtensionToPackageName(sourceFile);
			String name = ClassPathUtilities.filenameToClassName(sourceFile);
			System.out.println("packageMap.put() '" + name + "' " + packageName + "." + name);
			ProtoObject.packageMap.put(name, packageName + "." + name);
		}
	}
}
