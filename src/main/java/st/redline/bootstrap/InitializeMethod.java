/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.core.PrimContext;
import st.redline.core.PrimObject;

public class InitializeMethod extends PrimObject {
    public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
        return receiver;
    }
}
