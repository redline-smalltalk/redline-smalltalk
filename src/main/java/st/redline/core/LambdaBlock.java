/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.core;

public interface LambdaBlock {

    PrimObject apply(PrimObject thiz, PrimObject receiver, PrimContext context);
}
