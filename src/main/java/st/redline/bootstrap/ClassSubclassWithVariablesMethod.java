/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.ProtoMethod;
import st.redline.ProtoObject;

public class ClassSubclassWithVariablesMethod extends ProtoMethod {

	public ProtoObject applyTo(ProtoObject receiver, ProtoObject classMethodWasFoundIn, ProtoObject className, ProtoObject instVars, ProtoObject classVars, ProtoObject classInstVars, ProtoObject poolDicts, ProtoObject category) {
//		System.out.println("ClassSubclassMethod() " + receiver + " " + String.valueOf(className.javaValue()) + " Inst: " + String.valueOf(instVars.javaValue()) + " " + String.valueOf(classVars.javaValue()) + " clsInstVars: " + String.valueOf(classInstVars.javaValue()));
		String name = fullyQualifiedClassName(String.valueOf(className.javaValue()));
		ProtoObject subclass = ProtoObject.primitiveCreateSubclass(receiver, name);
		ProtoObject.primitiveRegisterAs(subclass, name);
		addVariableNames(subclass, String.valueOf(instVars.javaValue()));
		addVariableNames(subclass.cls(), String.valueOf(classVars.javaValue()));
		subclass.category(String.valueOf(category.javaValue()));
		return subclass;
	}

	private String fullyQualifiedClassName(String name) {
		String packageName = ProtoObject.primitivePackageRegistryCurrent();
		return packageName == null ? name : packageName + "." + name;
	}
}
