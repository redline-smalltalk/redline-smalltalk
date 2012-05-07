package st.redline.core;

import java.net.URL;

public interface ClassLoaderCreator {

    ClassLoader createExposedClassLoader(ClassLoader parent, CommandLine commandLine);
    ClassLoader createSmalltalkClassLoader(ClassLoader parent, CommandLine commandLine);
    ClassLoader createUrlFileClassLoader(ClassLoader parent, URL[] urls);

}
