/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.core.PrimObject;

public interface RouterFactory {
    Router create(String requestPathSpec, String type, PrimObject block);
}
