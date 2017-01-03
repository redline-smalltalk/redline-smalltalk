/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

public interface LineTransformer {
    String transform(String line);
    String begin();
    String end();
}
