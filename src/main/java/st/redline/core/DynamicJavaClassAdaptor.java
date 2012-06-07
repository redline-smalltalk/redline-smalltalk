package st.redline.core;

import st.redline.core.reflector.Reflector;

public class DynamicJavaClassAdaptor {

    private final String className;

    DynamicJavaClassAdaptor(String className) {
        this.className = className;
    }

    PrimObject build() {
        System.out.println("DynamicJavaClassAdaptor.build()");
        String source = generateSmalltalkFor(className);
        source = "\"@: st.redline.core.ArgThingAdaptor\"\n" +
                "Object < #ArgThingAdaptor.\n" +
                "\n" +
                "ArgThingAdaptor class atSelector: #with: put: [ :args || obj |\n" +
                "  ^ obj.\n" +
                "].\n" +
                "\n" +
                "ArgThingAdaptor initialize.\n\n";
        System.out.println(source);
        return Evaluator.evaluate(source);
    }

    private String generateSmalltalkFor(String className) {
        try {
            return new Reflector(className).reflect().result();
        } catch (ClassNotFoundException e) {
            throw new RedlineException(e);
        }
    }
}
