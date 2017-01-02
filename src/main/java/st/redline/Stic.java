package st.redline;

import st.redline.classloader.*;

import java.io.*;

public class Stic {

    private final String[] args;

    public static void main(String[] args) throws Exception {
        new Stic(args).run();
    }

    public Stic(String[] args) {
        this.args = args;
    }

    private void run() throws ClassNotFoundException, InstantiationException, IllegalAccessException {
        run(loadScript(scriptName()));
    }

    private void run(Class cls) throws IllegalAccessException, InstantiationException {
        cls.newInstance();
    }

    private Class loadScript(String name) throws ClassNotFoundException {
        return classLoader().loadClass(name);
    }

    private ClassLoader classLoader() {
        return new SmalltalkClassLoader(currentClassLoader(), sourceFinder(), bootstrapper());
    }

    private Bootstrapper bootstrapper() {
        return new Bootstrapper();
    }

    private SourceFinder sourceFinder() {
        return new SmalltalkSourceFinder(sourceFactory(), classPaths());
    }

    private SourceFactory sourceFactory() {
        return new SourceFactory();
    }

    public String[] classPaths() {
        return classPath().split(File.pathSeparator);
    }

    private String classPath() {
        return System.getProperty("java.class.path");
    }

    private ClassLoader currentClassLoader() {
        return Thread.currentThread().getContextClassLoader();
    }

    private String scriptName() {
        return hasArguments() ? firstArgument() : defaultScriptName();
    }

    private String defaultScriptName() {
        return "st.redline.NoArguments";
    }

    private String firstArgument() {
        return args[0];
    }

    private boolean hasArguments() {
        return args.length > 0;
    }
}
