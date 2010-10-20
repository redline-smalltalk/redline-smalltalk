package st.redline.compiler;

import java.util.List;

public interface Translator {
	void visitClassDefinition(ClassDefinition classDefinition, ClassInstanceVariables classInstanceVariables);
	void visitMethods(List<Method> classMethods, List<Method> instanceMethods);
}