/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

public class RedlineException extends RuntimeException {

	public static RedlineException withCauseAndMessage(String message, Exception exception) {
		if (exception instanceof RedlineException)
			return (RedlineException) exception;
		return new RedlineException(message, exception);
	}
	
	public static RedlineException withCause(Exception exception) {
		if (exception instanceof RedlineException)
			return (RedlineException) exception;
		return new RedlineException(exception);
	}

	public static RedlineException withMessage(String message) {
		return new RedlineException(message);
	}

	private RedlineException(Exception exception) {
		super(exception);
	}

	private RedlineException(String message) {
		super(message);
	}

	private RedlineException(String message, Throwable cause) {
		super(message, cause);
	}
}
