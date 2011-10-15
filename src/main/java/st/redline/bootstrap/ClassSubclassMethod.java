/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.Primitives;
import st.redline.ProtoMethod;
import st.redline.ProtoObject;

public class ClassSubclassMethod extends ProtoMethod {

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject classMethodWasFoundIn, ProtoObject argument) {
		System.out.println("ClassSubclassMethod() " + receiver + " " + String.valueOf(argument.javaValue()));
		String name = fullyQualifiedClassName(String.valueOf(argument.javaValue()));
		ProtoObject subclass = Primitives.createSubclass(receiver, name);
		Primitives.registerAs(subclass, name);
		return subclass;
	}

	private String fullyQualifiedClassName(String name) {
		String packageName = Primitives.packageRegistryCurrent();
		return packageName == null ? name : packageName + "." + name;
	}
}
