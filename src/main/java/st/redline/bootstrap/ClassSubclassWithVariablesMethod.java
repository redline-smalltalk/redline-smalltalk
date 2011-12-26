/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.bootstrap;

import st.redline.Primitives;
import st.redline.ProtoMethod;
import st.redline.ProtoObject;
import st.redline.ThisContext;

public class ClassSubclassWithVariablesMethod extends ProtoMethod {

	public ProtoObject applyTo(ProtoObject receiver, ThisContext thisContext, ProtoObject className, ProtoObject instVars, ProtoObject classVars, ProtoObject classInstVars, ProtoObject poolDicts) {
//		subclass:instanceVariableNames:classVariableNames:poolDictionaries:
//		System.out.println("ClassSubclassMethod() " + receiver + " " + String.valueOf(className.javaValue()) + " Inst: " + String.valueOf(instVars.javaValue()) + " " + String.valueOf(classVars.javaValue()) + " clsInstVars: " + String.valueOf(classInstVars.javaValue()));
		String name = fullyQualifiedClassName(String.valueOf(className.javaValue()));
		ProtoObject subclass = Primitives.createSubclass(receiver, name);
		Primitives.registerAs(subclass, name);
		addVariableNames(subclass, String.valueOf(instVars.javaValue()));
		addVariableNames(subclass.cls(), String.valueOf(classVars.javaValue()));
		return subclass;
	}

	private String fullyQualifiedClassName(String name) {
		String packageName = Primitives.packageRegistryCurrent();
		return packageName == null ? name : packageName + "." + name;
	}
}
