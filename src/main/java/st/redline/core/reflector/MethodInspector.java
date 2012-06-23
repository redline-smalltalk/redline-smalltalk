/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core.reflector;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class MethodInspector extends ConstructorInspector {

    private boolean hasReturnType;
    private Map<String, Integer> methodNameDuplicates;

    public MethodInspector(Reflector reflector) {
        super(reflector);
    }

    public void visitMethodsBegin(String suffix, String name) {
        methodNameDuplicates = new HashMap<String, Integer>();
    }

    public void visitMethodsEnd(String suffix, String name) {
        reflector.usePreviousVisitor();
        for (Map.Entry<String, Integer> e : methodNameDuplicates.entrySet())
            System.out.println(e.getKey() + " occurred " + e.getValue());
    }

    public void visitMethodBegin(String suffix, String className, String methodName, int parameterCount, String returnType) {
        javaArgumentSignature = new StringBuilder();
        methodSymbol = new StringBuilder();

        System.out.println("visitMethodBegin: " + className + " " + methodName + " " + parameterCount + " " + returnType);
        this.className = className;
        this.classNameAdaptor = className.substring(className.lastIndexOf('.') + 1) + suffix;
        this.javaClassName = className.replace(".", "/");
        this.javaArgumentTypes = new String[parameterCount];
        this.hasReturnType = !returnType.equals("void");
        this.methodSymbol.append(methodName);
        reflector.append("\n")
                 .append(classNameAdaptor)
                 .append(" class atSelector: #");

        int count = 1;
        String methodNameForDuplicateTracking = methodName + (parameterCount == 0 ? "" : ":");
        if (methodNameDuplicates.containsKey(methodNameForDuplicateTracking))
            count = methodNameDuplicates.get(methodNameForDuplicateTracking) + 1;
        methodNameDuplicates.put(methodNameForDuplicateTracking, count);
    }

    public void visitMethodEnd(String suffix, String className, String methodName, int parameterCount, String returnType) {
        if (!hasReturnType)
            reflector.append("  ^ self.\n");
        reflector.append("].\n");
    }

    public void visitParameterTypesBegin(int length) {
    }

    public void visitParameterTypesEnd(int length) {
        reflector.append(methodSymbol.toString())
                .append(length == 0 ? "" : ":")
                .append(" put: [")
                .append(length == 0 ? " " : " :args |")
                .append("\n");
    }

    public void visitConstructorBegin(String suffix, String className, String constructorName, int parameterCount) {
        throw new IllegalStateException("This inspector should not be getting this.");
    }

    public void visitConstructorEnd(String suffix, String className, String constructorName, int parameterCount) {
        throw new IllegalStateException("This inspector should not be getting this.");
    }

    public void visitConstructorsBegin(String suffix, String className) {
        throw new IllegalStateException("This inspector should not be getting this.");
    }

    public void visitConstructorsEnd(String suffix, String className) {
        throw new IllegalStateException("This inspector should not be getting this.");
    }
}
