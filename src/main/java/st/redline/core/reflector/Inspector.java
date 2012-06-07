package st.redline.core.reflector;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

public class Inspector {

    private Class theClass;

    public Inspector(String className) throws ClassNotFoundException {
        theClass = loadClassToInspect(className);
    }

    private Class loadClassToInspect(String className) throws ClassNotFoundException {
        return Class.forName(className);
    }

    public void inspectWith(InspectorVisitor inspectorVisitor) {
        inspectorVisitor.visitBegin(theClass.getName());
        inspectConstructorsWith(inspectorVisitor);
        inspectorVisitor.visitEnd(theClass.getName());
    }

    private void inspectConstructorsWith(InspectorVisitor inspectorVisitor) {
        Constructor[] constructors = theClass.getDeclaredConstructors();
        for (int i = 0; i < constructors.length; i++)
            if (Modifier.isPublic(constructors[i].getModifiers()))
                inspectConstructor(constructors[i], inspectorVisitor);
    }

    private void inspectConstructor(Constructor constructor, InspectorVisitor inspectorVisitor) {
        Class[] parameterTypes = constructor.getParameterTypes();
        inspectorVisitor.visitConstructorBegin(theClass.getName(), constructor.getName(), parameterTypes.length);
        inspectParameterTypesWith(parameterTypes, inspectorVisitor);
        inspectorVisitor.visitConstructorEnd(theClass.getName(), constructor.getName(), parameterTypes.length);
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
