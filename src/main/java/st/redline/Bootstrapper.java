/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import java.io.File;
import java.util.Map;

public class Bootstrapper {

	PrimObjectMetaclass primObjectMetaclass;

	Bootstrapper(PrimObjectMetaclass primObjectMetaclass) {
		this.primObjectMetaclass = primObjectMetaclass;
	}

	public void bootstrap() {
		mapPackages(PrimObjectMetaclass.IMPORTS);
	}

	private void mapPackages(Map<String, String> imports) {
		imports.put("ProtoObject", "st.redline.ProtoObject");
		for (String sourceFile : SourceFileFinder.findIn("st" + File.separator + "redline")) {
			String packageName = ClassPathUtilities.filenameWithExtensionToPackageName(sourceFile);
			String name = ClassPathUtilities.filenameToClassName(sourceFile);
			imports.put(name, packageName + "." + name);
		}
	}
}
