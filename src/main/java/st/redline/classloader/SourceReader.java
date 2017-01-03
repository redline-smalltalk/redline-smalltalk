/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

public interface SourceReader {

    String contents(LineTransformer lineTransformer);
}
