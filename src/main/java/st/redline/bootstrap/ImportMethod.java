/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.core.*;

import java.util.List;

public class ImportMethod extends PrimObject {

    public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
        PrimObjectMetaclass metaclass = (PrimObjectMetaclass) receiver;
        SmalltalkEnvironment smalltalkEnvironment = smalltalkEnvironment();
        for (SourceFile sourceFile : findSources(smalltalkEnvironment, primContext.argumentAt(0).javaValue(), null))
            addAssociationBetweenObjectAndPackage(metaclass, sourceFile.alias(), sourceFile.shortName(), sourceFile.packageName());
        return receiver;
    }

    protected void addAssociationBetweenObjectAndPackage(PrimObjectMetaclass metaclass, String alias, String className, String packageName) {
        metaclass.packageAtPut(alias, makeFullyQualifiedPath(packageName, className));
    }

    public static String makeFullyQualifiedPath(String packageName, String className) {
        return packageName + "." + className;
    }

    protected List<SourceFile> findSources(SmalltalkEnvironment smalltalkEnvironment, Object importPaths, Object alias) {
        return findSources(smalltalkEnvironment, String.valueOf(importPaths), alias == null ? null : String.valueOf(alias));
    }

    protected List<SourceFile> findSources(SmalltalkEnvironment smalltalkEnvironment, String importPaths, String alias) {
        return smalltalkEnvironment.findSources(importPaths);
    }
}
