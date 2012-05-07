package st.redline.core;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

public class RedlineClassLoaderBuilder {
    private ClassLoaderCreator classLoaderCreator;

    public RedlineClassLoaderBuilder(ClassLoaderCreator classLoaderCreator) {
        this.classLoaderCreator = classLoaderCreator;
    }

    public ClassLoader build(ClassLoader parentClassLoader, CommandLine commandLine) throws MalformedURLException {
        return classLoaderCreator.createExposedClassLoader(
                classLoaderCreator.createUrlFileClassLoader(
                        classLoaderCreator.createSmalltalkClassLoader(parentClassLoader, commandLine),
                        getSourceJarUrls(commandLine)), commandLine);
    }

    private URL[] getSourceJarUrls(CommandLine commandLine) throws MalformedURLException {
        List<URL> urls = new ArrayList<URL>();
        for (File jarFile : commandLine.sourceJarFiles()) {
            System.out.println("Adding: " + jarFile.toURI().toURL());
            urls.add(jarFile.toURI().toURL());
        }
        return urls.toArray(new URL[urls.size()]);
    }
}
