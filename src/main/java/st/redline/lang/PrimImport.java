/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.lang;

import st.redline.RedlineFile;

public class PrimImport extends ProtoObject {

    public PrimImport() {
        name = "PrimImport-block";
    }

    protected ProtoObject invoke(ProtoObject receiver, PrimContext context) {
        String fullyQualifiedName = fullyQualifiedName(context);
        String className = className(fullyQualifiedName);
//        System.out.println("primitive import: " + fullyQualifiedName + " for " + receiver + " as " + className);
        ((ProtoClass) receiver).importAtPut(className, fullyQualifiedName);
        return receiver;
    }

    private String className(String importName) {
        int index = importName.lastIndexOf(".");
        if (index == -1)
            return importName;
        return importName.substring(index + 1);
    }

    private String fullyQualifiedName(PrimContext context) {
        String fullyQualifiedName = (String) context.argumentAt(0).javaValue();
        return fullyQualifiedName.replace(RedlineFile.separator, ".");
    }
}
