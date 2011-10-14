/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import st.redline.bootstrap.*;
import st.redline.compiler.Primitive;

import java.io.File;

public class Bootstrapper {

	protected Bootstrapper(ProtoObject protoObject) {
	}

	public void bootstrap() {
		mapPackages();
	}

	private void mapPackages() {
		Primitives.packageMap.put("ProtoObject", "st.redline.ProtoObject");
		for (String sourceFile : SourceFileFinder.findIn("st" + File.separator + "redline")) {
			String packageName = ClassPathUtilities.filenameWithExtensionToPackageName(sourceFile);
			String name = ClassPathUtilities.filenameToClassName(sourceFile);
			Primitives.packageMap.put(name, packageName + "." + name);
		}
	}
}
