/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.core.*;

import java.io.File;
import java.util.List;

public class ImportAsMethod extends ImportMethod {

    public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
        PrimObjectMetaclass metaclass = (PrimObjectMetaclass) receiver;
        SmalltalkEnvironment smalltalkEnvironment = smalltalkEnvironment();
        for (SourceFile sourceFile : findSources(smalltalkEnvironment, primContext.argumentAt(0).javaValue(), primContext.argumentAt(1).javaValue()))
            addAssociationBetweenObjectAndPackage(metaclass, sourceFile.alias(), sourceFile.shortName(), sourceFile.packageName());
        registerEigenClass(metaclass);
        return receiver;
    }

    protected List<SourceFile> findSources(SmalltalkEnvironment smalltalkEnvironment, String importPaths, String alias) {
        List<SourceFile> sources = smalltalkEnvironment.findSources(importPaths, false);
        if (sources.size() > 1) {
            System.err.println("WARNING: Not expecting more than one file in '" + importPaths + "' to alias as '" + alias + "'.");
            return sources;
        }
        if (sources.size() == 1) {
            sources.get(0).alias(alias);
            return sources;
        }
        PretendSourceFile sourceFile = new PretendSourceFile(importPaths, alias);
        sources.add(sourceFile);
        return sources;
    }
}
