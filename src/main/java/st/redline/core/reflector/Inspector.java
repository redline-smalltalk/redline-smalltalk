package st.redline.core.reflector;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

public class Inspector {

    private final Class theClass;
    private final String suffix;

    public Inspector(String className, String suffix) throws ClassNotFoundException {
        this.suffix = suffix;
        theClass = loadClassToInspect(className);
    }

    private Class loadClassToInspect(String className) throws ClassNotFoundException {
        return Class.forName(className);
    }

    public void inspectWith(InspectorVisitor inspectorVisitor) {
        inspectorVisitor.visitBegin(suffix, theClass.getName());
        inspectConstructorsWith(inspectorVisitor);
        inspectorVisitor.visitEnd(suffix, theClass.getName());
    }

    private void inspectConstructorsWith(InspectorVisitor inspectorVisitor) {
        Constructor[] constructors = theClass.getDeclaredConstructors();
        for (int i = 0; i < constructors.length; i++)
            if (Modifier.isPublic(constructors[i].getModifiers()))
                inspectConstructor(constructors[i], inspectorVisitor);
    }

    private void inspectConstructor(Constructor constructor, InspectorVisitor inspectorVisitor) {
        Class[] parameterTypes = constructor.getParameterTypes();
        inspectorVisitor.visitConstructorBegin(suffix, theClass.getName(), constructor.getName(), parameterTypes.length);
        inspectParameterTypesWith(parameterTypes, inspectorVisitor);
        inspectorVisitor.visitConstructorEnd(suffix, theClass.getName(), constructor.getName(), parameterTypes.length);
    }

    private void inspectParameterTypesWith(Class[] parameterTypes, InspectorVisitor inspectorVisitor) {
        inspectorVisitor.visitParameterTypesBegin(parameterTypes.length);
        for (int i = 0; i < parameterTypes.length; i++)
            inspectParameterTypeWith(parameterTypes[i], i, inspectorVisitor);
        inspectorVisitor.visitParameterTypesEnd(parameterTypes.length);
    }

    private void inspectParameterTypeWith(Class parameterType, int index, InspectorVisitor inspectorVisitor) {
        inspectorVisitor.visitParameterType(parameterType.getName(), index);
    }
}
