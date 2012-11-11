package st.redline.core;

import st.redline.compiler.SmalltalkGeneratorOfAdaptorOfAJavaClass;

public class DynamicJavaClassAdaptor {

    private final String className;

    DynamicJavaClassAdaptor(String className) {
        this.className = className;
    }

    PrimObject build() {
        String source = generateSmalltalkFor();
//        System.out.println(source);
        return Evaluator.evaluate(source);
    }

    private String generateSmalltalkFor() {
        return new SmalltalkGeneratorOfAdaptorOfAJavaClass(className).adaptorSource();
    }

    public static String fullyQualifiedClassNameForJavaClassWrapperClassNamed(String fullyQualifiedJavaClassName) { // remove when old code is gone - chad
        return "smalltalkClassesThatAdaptJavaClasses." + fullyQualifiedJavaClassName + "Adaptor";
    }
}
