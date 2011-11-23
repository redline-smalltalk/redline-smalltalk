/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import st.redline.ProtoMethod;
import st.redline.ProtoObject;
import st.redline.ThisContext;

/**
 * A simple abstract method for acting as a placeholder for the initialize method.
 *
 * Subclasses will override in Smalltalk.
 *
 * @author rroland
 * @since 11/22/11
 */
public class InitializeMethod extends ProtoMethod {
    @Override
    public ProtoObject applyTo(ProtoObject receiver, ThisContext context) {
        return receiver;
    }
}
