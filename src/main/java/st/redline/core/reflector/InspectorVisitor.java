package st.redline.core.reflector;

public interface InspectorVisitor {
    void visitBegin(String className);
    void visitEnd(String className);
    void visitConstructorBegin(String className, String constructorName, int parameterCount);
    void visitConstructorEnd(String className, String constructorName, int parameterCount);
    void visitParameterTypesBegin(int length);
    void visitParameterTypesEnd(int length);
    void visitParameterType(String parameterType, int index);
}
