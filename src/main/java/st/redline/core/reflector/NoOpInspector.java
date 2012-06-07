package st.redline.core.reflector;

public class NoOpInspector implements InspectorVisitor {

    public void visitBegin(String className) {
        throw new IllegalStateException("This inspector should not be getting this.");
    }

    public void visitEnd(String className) {
        throw new IllegalStateException("This inspector should not be getting this.");
    }

    public void visitConstructorBegin(String className, String constructorName, int parameterCount) {
        throw new IllegalStateException("This inspector should not be getting this.");
    }

    public void visitConstructorEnd(String className, String constructorName, int parameterCount) {
        throw new IllegalStateException("This inspector should not be getting this.");
    }

    public void visitParameterTypesBegin(int length) {
        throw new IllegalStateException("This inspector should not be getting this.");
    }

    public void visitParameterTypesEnd(int length) {
        throw new IllegalStateException("This inspector should not be getting this.");
    }

    public void visitParameterType(String parameterType, int index) {
        throw new IllegalStateException("This inspector should not be getting this.");
    }
}
