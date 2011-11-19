/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.ProtoMethod;
import st.redline.ProtoObject;
import st.redline.ThisContext;

public class CategoryMethod extends ProtoMethod {
	public ProtoObject applyTo(ProtoObject receiver, ThisContext thisContext, ProtoObject argument) {
//		System.out.println("CategoryMethod() " + receiver + " " + String.valueOf(argument.javaValue()));
		receiver.category(String.valueOf(argument.javaValue()));
		return receiver;
	}
}
