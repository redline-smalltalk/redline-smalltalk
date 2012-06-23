/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core.reflector;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;

public class Inspector {

    private static final Map<String, String> IGNORED_METHODS = new HashMap<String, String>();
    static {
        IGNORED_METHODS.put("getClass", "getClass");
    }

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
        inspectMethodsConstructorsWith(inspectorVisitor);
        inspectorVisitor.visitEnd(suffix, theClass.getName());
    }

    private void inspectMethodsConstructorsWith(InspectorVisitor inspectorVisitor) {
        inspectMethodsBegin(inspectorVisitor);
        Method[] methods = theClass.getMethods();
        for (int i = 0; i < methods.length; i++)
            if (Modifier.isPublic(methods[i].getModifiers()))
                inspectMethod(methods[i], inspectorVisitor);
        inspectMethodsEnd(inspectorVisitor);
    }

    private void inspectMethod(Method method, InspectorVisitor inspectorVisitor) {
        String methodName = method.getName();
        if (IGNORED_METHODS.containsKey(methodName))
            return;
        Class[] parameterTypes = method.getParameterTypes();
        String returnType = method.getReturnType().getName();
        inspectorVisitor.visitMethodBegin(suffix, theClass.getName(), methodName, parameterTypes.length, returnType);
        inspectParameterTypesWith(parameterTypes, inspectorVisitor);
        inspectorVisitor.visitMethodEnd(suffix, theClass.getName(), methodName, parameterTypes.length, returnType);
    }

    private void inspectMethodsEnd(InspectorVisitor inspectorVisitor) {
        inspectorVisitor.visitMethodsEnd(suffix, theClass.getName());
    }

    private void inspectMethodsBegin(InspectorVisitor inspectorVisitor) {
        inspectorVisitor.visitMethodsBegin(suffix, theClass.getName());
    }

    private void inspectConstructorsWith(InspectorVisitor inspectorVisitor) {
        inspectConstructorsBegin(inspectorVisitor);
        Constructor[] constructors = theClass.getDeclaredConstructors();
        for (int i = 0; i < constructors.length; i++)
            if (Modifier.isPublic(constructors[i].getModifiers()))
                inspectConstructor(constructors[i], inspectorVisitor);
        inspectConstructorsEnd(inspectorVisitor);
    }

    private void inspectConstructorsBegin(InspectorVisitor inspectorVisitor) {
        inspectorVisitor.visitConstructorsBegin(suffix, theClass.getName());
    }

    private void inspectConstructorsEnd(InspectorVisitor inspectorVisitor) {
        inspectorVisitor.visitConstructorsEnd(suffix, theClass.getName());
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
