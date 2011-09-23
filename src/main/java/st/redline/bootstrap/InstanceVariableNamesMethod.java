/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.ProtoMethod;
import st.redline.ProtoObject;

public class InstanceVariableNamesMethod extends ProtoMethod {
	public ProtoObject applyTo(ProtoObject receiver, ProtoObject classMethodWasFoundIn, ProtoObject argument) {
//		System.out.println("*** InstanceVariableNamesMethod() " + receiver + " " + String.valueOf(argument.javaValue()));
		String[] variableNames = String.valueOf(argument.javaValue()).split(" ");
		for (String variableName : variableNames)
			receiver.addVariableNamed(variableName);
		return receiver;
	}
}
