/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

public class MethodFactory {
	public static Method create(String objectName, String cls, MessagePattern messagePattern, Primitive primitive, Temporaries temporaries, Statements statements) {
		if (cls == null)
			return new InstanceMethod(objectName, messagePattern, primitive, temporaries, statements);
		else
			return new ClassMethod(objectName, messagePattern, primitive, temporaries, statements);
	}
}
