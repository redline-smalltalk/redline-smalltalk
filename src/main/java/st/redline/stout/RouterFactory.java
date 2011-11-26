/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.ProtoBlock;

public interface RouterFactory {
    Router create(String requestPathSpec, String type, ProtoBlock block);
}
