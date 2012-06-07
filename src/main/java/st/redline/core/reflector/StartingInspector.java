package st.redline.core.reflector;

public class StartingInspector extends NoOpInspector {

    private final Reflector reflector;

    public StartingInspector(Reflector reflector) {
        this.reflector = reflector;
    }

    public void visitBegin(String fullClassName) {
        reflector.append("\"@: " + fullClassName + "Adaptor\"\n")
                 .append("Object < #" + className(fullClassName) + ".\n");
    }

    private String className(String fullclassName) {
        return fullclassName.substring(fullclassName.lastIndexOf('.') + 1) + "Adaptor";
    }

    public void visitEnd(String fullClassName) {
        reflector.append("\n")
                 .append(className(fullClassName))
                 .append(" initialize.\n");
    }

    public void visitConstructorBegin(String className, String constructorName, int parameterCount) {
        this.reflector.useConstructorVisitor();
        this.reflector.visitConstructorBegin(className, constructorName, parameterCount);
    }
}
