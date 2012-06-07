package st.redline.core;

import st.redline.core.reflector.Reflector;

public class DynamicJavaClassAdaptor {

    private final String className;
    private final String suffix;

    DynamicJavaClassAdaptor(String className, String suffix) {
        this.className = className;
        this.suffix = suffix;
    }

    PrimObject build() {
        System.out.println("DynamicJavaClassAdaptor.build()");
        String source = generateSmalltalkFor();
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

    private String generateSmalltalkFor() {
        try {
            return new Reflector(className, suffix).reflect().result();
        } catch (ClassNotFoundException e) {
            throw new RedlineException(e);
        }
    }
}
