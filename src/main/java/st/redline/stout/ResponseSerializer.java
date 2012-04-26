/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.PrimObject;

import java.io.IOException;
import java.io.Writer;

public interface ResponseSerializer {
    void serializeOn(PrimObject object, Writer writer) throws Exception;
}
