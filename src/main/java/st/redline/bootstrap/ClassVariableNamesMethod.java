/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.ProtoMethod;
import st.redline.ProtoObject;

public class ClassVariableNamesMethod extends ProtoMethod {
	public ProtoObject applyTo(ProtoObject receiver, ProtoObject classMethodWasFoundIn, ProtoObject argument) {
		System.out.println("ClassVariableNamesMethod() " + receiver + " " + String.valueOf(argument.javaValue()));
		return receiver;
	}
}
