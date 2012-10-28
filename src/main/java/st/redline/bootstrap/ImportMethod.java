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
        registerEigenClass(metaclass);
        return receiver;
    }

    protected void registerEigenClass(PrimObjectMetaclass metaclass) {
        // Register EigenClass so we can use its imports later when we create a subclass.
        String eigenClassName = metaclass.getClass().getName();
        PrimObject.EIGENCLASS_REGISTRY.get().put(eigenClassName, metaclass);
//        System.out.println("Registering eigenClass: " + eigenClassName);
    }

    protected void addAssociationBetweenObjectAndPackage(PrimObjectMetaclass metaclass, String alias, String className, String packageName) {
//        System.out.println("addAssociationBetweenObjectAndPackage: " + metaclass + " ( " + metaclass.getClass() + " ) " + alias);
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
