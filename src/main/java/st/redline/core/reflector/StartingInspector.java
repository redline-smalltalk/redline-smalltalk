/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
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

    public void visitConstructorsBegin(String suffix, String className) {
        this.reflector.useConstructorVisitor();
        this.reflector.visitConstructorsBegin(suffix, className);
    }

    public void visitConstructorsEnd(String suffix, String className) {
        this.reflector.useConstructorVisitor();
        this.reflector.visitConstructorsEnd(suffix, className);
    }

    public void visitConstructorBegin(String suffix, String className, String constructorName, int parameterCount) {
        this.reflector.useConstructorVisitor();
        this.reflector.visitConstructorBegin(suffix, className, constructorName, parameterCount);
    }

    public void visitMethodsBegin(String suffix, String className) {
        this.reflector.useMethodVisitor();
        this.reflector.visitMethodsBegin(suffix, className);
    }

    public void visitMethodBegin(String suffix, String className, String constructorName, int parameterCount, String returnType) {
        this.reflector.visitMethodBegin(suffix, className, constructorName, parameterCount, returnType);
    }

    public void visitMethodsEnd(String suffix, String className) {
        this.reflector.useMethodVisitor();
        this.reflector.visitMethodsEnd(suffix, className);
    }
}
