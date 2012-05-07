package st.redline.core;

import java.net.URL;
import java.net.URLClassLoader;

public class ClassLoaderCreatorImpl implements ClassLoaderCreator {

    public ClassLoader createExposedClassLoader(ClassLoader parent, CommandLine commandLine) {
        return new ExposedClassLoader(parent, commandLine);
    }

    public ClassLoader createSmalltalkClassLoader(ClassLoader parent, CommandLine commandLine) {
        return new SmalltalkSourceClassLoader(parent, commandLine);
    }

    public ClassLoader createUrlFileClassLoader(ClassLoader parent, URL[] urls) {
        return new URLClassLoader(urls, parent);
    }
}
