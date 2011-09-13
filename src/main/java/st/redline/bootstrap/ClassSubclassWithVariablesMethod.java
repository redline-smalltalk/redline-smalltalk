/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.ProtoMethod;
import st.redline.ProtoObject;

public class ClassSubclassWithVariablesMethod extends ProtoMethod {

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject classMethodWasFoundIn, ProtoObject arg1, ProtoObject arg2, ProtoObject arg3, ProtoObject arg4, ProtoObject arg5, ProtoObject arg6) {
//		System.out.println("ClassSubclassMethod() " + receiver + " " + String.valueOf(argument.javaValue()));
		String name = fullyQualifiedClassName(String.valueOf(arg1.javaValue()));
		ProtoObject subclass = ProtoObject.primitiveCreateSubclass(receiver, name);
		ProtoObject.primitiveRegisterAs(subclass, name);
		return subclass;
	}

	private String fullyQualifiedClassName(String name) {
		String packageName = ProtoObject.primitivePackageRegistryCurrent();
		return packageName == null ? name : packageName + "." + name;
	}
}
