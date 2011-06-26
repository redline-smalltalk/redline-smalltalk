package st.redline;

import java.util.Map;

public interface ObjectData {
	RObject cls();
	void cls(RObject cls);
	RObject superclass();
	Map<String, RObject> variables();
	Map<String, RMethod> methodDictionary();
	RMethod methodAt(String selector);
	void methodAtPut(String selector, RMethod method);
	void bootstrapped(boolean bootstrapped);
	boolean isBootstrapped();
	boolean isClass();
	String primitiveName();
	void primitiveName(String primitiveName);
	String primitiveSourceFile();
	void primitiveSourceFile(String primitiveSourceFile);
	Object primitiveValue();
	void primitiveValue(Object primitiveValue);
	void primitiveCategory(RObject category);
	void primitiveAddPoolNamed(RObject variable);
	boolean primitiveHasPoolNamed(String variable);
	void primitiveAddClassInstanceVariableNamed(RObject variable);
	void primitiveAddClassVariableNamed(RObject variable);
	boolean primitiveHasClassVariableNamed(String variable);
	void primitiveAddInstanceVariableNamed(RObject variable);
	boolean primitiveHasInstanceVariableNamed(String variable);
}
