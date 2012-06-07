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
        String source = generateSmalltalkFor();
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
