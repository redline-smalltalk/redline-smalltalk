/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

public abstract class ProtoObjectData {

	private ProtoObject cls;
	private Map<String, ProtoObject> variables;

	abstract void javaValue(Object value);
	abstract Object javaValue();
	abstract void superclass(ProtoObject superclass);
	abstract ProtoObject superclass();
	abstract ProtoMethod methodAt(String selector);
	abstract void methodAtPut(String selector, ProtoMethod method);
	abstract boolean isClass();
	abstract String packageAt(String name);
	abstract void packageAtPut(String name, String packageName);

	public static ProtoObjectData classData() {
		return new ClassData();
	}

	public static ProtoObjectData instanceData() {
		return new InstanceData();
	}

	protected void cls(ProtoObject cls) {
		this.cls = cls;
	}

	protected ProtoObject cls() {
		return cls;
	}


	private static class InstanceData extends ProtoObjectData {

		private Object javaValue;

		protected void javaValue(Object value) {
			javaValue = value;
		}

		protected Object javaValue() {
			return javaValue;
		}

		protected boolean isClass() {
			return false;
		}

		protected void superclass(ProtoObject superclass) {
			throw new IllegalStateException("An instance can't have a superclass.");
		}

		protected ProtoObject superclass() {
			throw new IllegalStateException("An instance doesn't have a superclass.");
		}

		protected ProtoMethod methodAt(String selector) {
			throw new IllegalStateException("An instance doesn't have a method dictionary.");
		}

		protected void methodAtPut(String selector, ProtoMethod method) {
			throw new IllegalStateException("An instance can't have a method dictionary.");
		}

		protected String packageAt(String name) {
			throw new IllegalStateException("An instance can't have a packages.");
		}

		protected void packageAtPut(String name, String packageName) {
			throw new IllegalStateException("An instance can't have a packages.");
		}
	}

	private static class ClassData extends ProtoObjectData {

		private ProtoObject superclass;
		private Map<String, ProtoMethod> methods = new HashMap<String, ProtoMethod>();
		private Map<String, String> packages;

		protected void javaValue(Object value) {
			throw new IllegalStateException("A Class can't have a javaValue.");
		}

		protected Object javaValue() {
			throw new IllegalStateException("A Class doesn't have a javaValue.");
		}

		protected boolean isClass() {
			return true;
		}

		protected void superclass(ProtoObject superclass) {
			this.superclass = superclass;
		}

		protected ProtoObject superclass() {
			return superclass;
		}

		protected ProtoMethod methodAt(String selector) {
			return methods.get(selector);
		}

		protected void methodAtPut(String selector, ProtoMethod method) {
			// System.out.println("methodAtPut() " + selector + " " + method);
			methods.put(selector, method);
		}

		protected String packageAt(String name) {
			return packages != null ? packages.get(name) : null;
		}

		protected void packageAtPut(String name, String packageName) {
			if (packages == null)
				packages = new Hashtable<String, String>();
			packages.put(name, packageName);
		}
	}
}
