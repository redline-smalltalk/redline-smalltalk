/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.ProtoMethod;
import st.redline.ProtoObject;
import st.redline.SmalltalkClassLoader;
import st.redline.SourceFile;

import java.io.File;
import java.util.List;

public class ImportMethod extends ProtoMethod {

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject classMethodWasFoundIn, ProtoObject argument) {
		SmalltalkClassLoader smalltalkClassLoader = smalltalkClassLoader();
		for (SourceFile sourceFile : findSources(smalltalkClassLoader, argument.javaValue()))
			addAssociationBetweenObjectAndPackage(receiver, sourceFile.shortName(), sourceFile.packageName());
		return receiver;
	}

	private void addAssociationBetweenObjectAndPackage(ProtoObject receiver, String className, String packageName) {
		ProtoObject.primitivePackageAtPut(receiver, className, makeFullyQualifiedPath(packageName, className));
	}

	public static String makeFullyQualifiedPath(String packageName, String className) {
		return packageName + "." + className;
	}

	private List<SourceFile> findSources(SmalltalkClassLoader smalltalkClassLoader, Object importPaths) {
		return smalltalkClassLoader.findSources(String.valueOf(importPaths));
	}

	private SmalltalkClassLoader smalltalkClassLoader() {
		return SmalltalkClassLoader.instance();
	}
}
