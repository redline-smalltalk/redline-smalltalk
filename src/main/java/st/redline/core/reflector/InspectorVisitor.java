package st.redline.core.reflector;

public interface InspectorVisitor {
    void visitBegin(String suffix, String className);
    void visitEnd(String suffix, String className);
    void visitConstructorBegin(String suffix, String className, String constructorName, int parameterCount);
    void visitConstructorEnd(String suffix, String className, String constructorName, int parameterCount);
    void visitParameterTypesBegin(int length);
    void visitParameterTypesEnd(int length);
    void visitParameterType(String parameterType, int index);
}
