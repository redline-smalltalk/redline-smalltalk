/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.core.PrimObject;

public interface RouterRegistry {
    Router register(PrimObject spec, PrimObject type, PrimObject method, PrimObject block);
    Router register(String spec, String type, String method, PrimObject block);
    Router lookup(String path, String method, String type);
    Router lookup(PrimObject path, PrimObject method, PrimObject request);
}
