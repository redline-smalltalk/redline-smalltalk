/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

public class PretendSourceFile extends SourceFile {

    private String alias;
    private String name;

    public PretendSourceFile(String name, String alias) {
        super("");
        this.name = name;
        this.alias = alias;
    }

    public String alias() {
        return alias;
    }
    
    public String shortName() {
        return name.substring(name.lastIndexOf(".") + 1);
    }

    public String packageName() {
        return name.substring(0, name.lastIndexOf("."));
    }
}
