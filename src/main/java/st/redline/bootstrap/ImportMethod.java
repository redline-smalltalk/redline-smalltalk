/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.core.*;

import java.util.List;

public class ImportMethod extends PrimObject {

    public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
        PrimObjectMetaclass metaclass = (PrimObjectMetaclass) receiver;
        SmalltalkEnvironment smalltalkEnvironment = smalltalkEnvironment();
        for (SourceFile sourceFile : findSources(smalltalkEnvironment, primContext.argumentAt(0).javaValue()))
            addAssociationBetweenObjectAndPackage(metaclass, sourceFile.shortName(), sourceFile.packageName());
        return receiver;
    }

    private void addAssociationBetweenObjectAndPackage(PrimObjectMetaclass metaclass, String className, String packageName) {
        metaclass.packageAtPut(className, makeFullyQualifiedPath(packageName, className));
    }

    public static String makeFullyQualifiedPath(String packageName, String className) {
        return packageName + "." + className;
    }

    private List<SourceFile> findSources(SmalltalkEnvironment smalltalkEnvironment, Object importPaths) {
        return smalltalkEnvironment.findSources(String.valueOf(importPaths));
    }
}
