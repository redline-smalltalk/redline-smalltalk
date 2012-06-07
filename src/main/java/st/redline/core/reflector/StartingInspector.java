package st.redline.core.reflector;

public class StartingInspector extends NoOpInspector {

    private final Reflector reflector;

    public StartingInspector(Reflector reflector) {
        this.reflector = reflector;
    }

    public void visitBegin(String suffix, String fullClassName) {
        reflector.append("\"@: " + fullClassName + suffix + "\"\n")
                 .append("Object < #" + className(fullClassName, suffix) + ".\n");
    }

    private String className(String fullclassName, String suffix) {
        return fullclassName.substring(fullclassName.lastIndexOf('.') + 1) + suffix;
    }

    public void visitEnd(String suffix, String fullClassName) {
        reflector.append("\n")
                 .append(className(fullClassName, suffix))
                 .append(" initialize.\n");
    }

    public void visitConstructorBegin(String suffix, String className, String constructorName, int parameterCount) {
        this.reflector.useConstructorVisitor();
        this.reflector.visitConstructorBegin(suffix, className, constructorName, parameterCount);
    }
}
