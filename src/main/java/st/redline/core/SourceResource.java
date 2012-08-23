/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

import java.io.InputStream;

public class SourceResource extends SourceFile {

    private final InputStream stream;

    public SourceResource(InputStream stream, String filename) {
        super(filename);
        this.stream = stream;
    }

    public String contents() {
        return sourceFileReader().read(stream);
    }

//    public String alias() {
//        String alias = super.alias();
//        System.out.println("alias() " + alias);
//        return alias;
//    }
//
//    public String shortName() {
//        String name = name();
//        // assumes we have an file extension.
//        String sn = ClassPathUtilities.filenameToClassName(name);
//        System.out.println("shortName() " + sn);
//        return sn;
//    }
//
//    public String packageName() {
//        String name = name();
//        String pn = ClassPathUtilities.filenameToPackageName(name);
//        System.out.println("packageName() " + pn);
//        return pn;
//    }
//
//    private String name() {
//        String name = toString();
//        String userDir = userDir();
//        if (name.startsWith(userDir))
//            name = name.substring(userDir.length() + 1);
//        if (name.startsWith("rt"))
//            name = name.substring(3);
//        System.out.println("name() " + name);
//        return name;
//    }
}
