/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */

package st.redline.core;

public class DoesNotUnderstandException extends RuntimeException {
    public DoesNotUnderstandException(String message) {
        super(message);
    }
}