/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.ProtoBlock;
import st.redline.ProtoObject;

public interface RouterRegistry {
    Router register(ProtoObject spec, String type, String method, ProtoObject block);
    Router register(String spec, String type, String method, ProtoBlock block);
    Router lookup(String path, String method);
    Router lookup(ProtoObject path, ProtoObject method);
}
