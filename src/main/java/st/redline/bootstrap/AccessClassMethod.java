/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.PrimContext;
import st.redline.PrimObject;

public class AccessClassMethod extends PrimObject {
    public PrimObject invoke(PrimObject receiver, PrimContext primContext) {
        return receiver.cls();
    }
}
