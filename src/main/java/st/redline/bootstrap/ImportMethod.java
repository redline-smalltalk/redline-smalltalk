/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.*;

import java.io.File;
import java.util.List;

public class ImportMethod extends PrimObject {

    public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
//		System.out.println("import into " + receiver + " of " + primContext.argumentAt(0).javaValue());
        PrimObjectMetaclass metaclass = (PrimObjectMetaclass) receiver;
		SmalltalkClassLoader smalltalkClassLoader = smalltalkClassLoader();
		for (SourceFile sourceFile : findSources(smalltalkClassLoader, primContext.argumentAt(0).javaValue()))
			addAssociationBetweenObjectAndPackage(metaclass, sourceFile.shortName(), sourceFile.packageName());
		return receiver;
	}

	private void addAssociationBetweenObjectAndPackage(PrimObjectMetaclass metaclass, String className, String packageName) {
		metaclass.packageAtPut(className, makeFullyQualifiedPath(packageName, className));
	}

	public static String makeFullyQualifiedPath(String packageName, String className) {
		return packageName + "." + className;
	}

	private List<SourceFile> findSources(SmalltalkClassLoader smalltalkClassLoader, Object importPaths) {
		return smalltalkClassLoader.findSources(String.valueOf(importPaths));
	}

//	private SmalltalkClassLoader smalltalkClassLoader() {
//		return SmalltalkClassLoader.instance();
//	}
}
