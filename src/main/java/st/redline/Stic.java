/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline;

public class Stic {

    private SticConfiguration configuration;

    public static void main(String[] args) {
        SticConfiguration configuration = configuration(args);
        try {
            new Stic(configuration).run();
        } catch (Exception e) {
            print(e.getCause() != null ? e.getCause() : e);
        }
    }

    private static void print(Throwable throwable) {
        throwable.printStackTrace();
    }

    private static SticConfiguration configuration(String[] args) {
        return new SticConfiguration(args);
    }

    public Stic(SticConfiguration configuration) {
        this.configuration = configuration;
    }

    public void run() throws Exception {
        runProgram(scriptName());
    }

    private void runProgram(String program) throws Exception {
        run(loadProgram(program));
    }

    private void run(Class aClass) throws Exception {
        if (aClass != null)
            aClass.newInstance();
    }

    public Class loadProgram(String program) throws Exception {
        ClassLoader classLoader = classLoader();
        return Class.forName(program, true, classLoader);
    }

    private String scriptName() {
        String scriptName = configuration.scriptName();
        return "".equals(scriptName) ? "st.redline.NoScript" : scriptName;
    }

    private ClassLoader classLoader() throws Exception {
        return configuration.classLoader();
    }
}
