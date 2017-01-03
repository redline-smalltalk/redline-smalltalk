/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

public interface Source {

    boolean hasContent();
    String contents();
    String className();
    String fullClassName();
    String fileExtension();
    String packageName();
}
