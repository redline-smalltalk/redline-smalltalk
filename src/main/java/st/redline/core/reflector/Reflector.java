package st.redline.core.reflector;

import java.util.Stack;

public class Reflector implements InspectorVisitor {

    private final Inspector inspector;
    private final StringBuilder result;
    private Stack<InspectorVisitor> reflectors;

    public static void main(String[] args) throws ClassNotFoundException {
        System.out.println(new Reflector(args[0], "Adaptor").reflect().result());
    }

    public Reflector(String className, String suffix) throws ClassNotFoundException {
        inspector = new Inspector(className, suffix);
        result = new StringBuilder(1024);
        initializeReflectors();
    }

    private void initializeReflectors() {
        reflectors = new Stack<InspectorVisitor>();
        reflectors.push(new StartingInspector(this));
    }

    public String result() {
        return result.toString();
    }

    public Reflector reflect() {
        inspector.inspectWith(this);
        return this;
    }

    public Reflector append(String result) {
        this.result.append(result);
        return this;
    }

    public Reflector append(int result) {
        this.result.append(result);
        return this;
    }

    public void visitBegin(String suffix, String className) {
        currentReflector().visitBegin(suffix, className);
    }

    public void visitEnd(String suffix, String className) {
        currentReflector().visitEnd(suffix, className);
    }

    public void visitConstructorBegin(String suffix, String className, String constructorName, int parameterCount) {
        currentReflector().visitConstructorBegin(suffix, className, constructorName, parameterCount);
    }

    private InspectorVisitor currentReflector() {
        return reflectors.peek();
    }

    public void visitConstructorEnd(String suffix, String className, String constructorName, int parameterCount) {
        currentReflector().visitConstructorEnd(suffix, className, constructorName, parameterCount);
    }

    public void visitParameterTypesBegin(int length) {
        currentReflector().visitParameterTypesBegin(length);
    }

    public void visitParameterTypesEnd(int length) {
        currentReflector().visitParameterTypesEnd(length);
    }

    public void visitParameterType(String parameterType, int index) {
        currentReflector().visitParameterType(parameterType, index);
    }

    public void useConstructorVisitor() {
        reflectors.push(new ConstructorInspector(this));
    }

    public void usePreviousVisitor() {
        reflectors.pop();
    }
}
