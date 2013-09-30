/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import st.redline.RedlineFile;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;

public class SourceFinder {

    public static final String WINDOWS_PATH_SEPARATOR = "\\";
    public static final String JAR_PATH_SEPARATOR = "/";

    private static final String SOURCE_EXTENSION = ".st";
    private SourceFactory sourceFactory;
    private final String[] classPaths;

    public SourceFinder(SourceFactory sourceFactory, String[] classPaths) {
        this.sourceFactory = sourceFactory;
        this.classPaths = classPaths;
    }

    public List<Source> findIn(String path) {
        String packagePath = path.replace(".", RedlineFile.separator);
        List<Source> sources = new ArrayList<Source>();
        for (String classPath : classPaths)
            sources.addAll(findIn(packagePath, classPath));
        return sources;
    }

    public List<Source> findIn(String packagePath, String classPath) {
        if (isJar(classPath))
            return findSourceInInJar(packagePath, classPath);
        else
            return findSourceInFile(packagePath, classPath);
    }

    private List<Source> findSourceInInJar(String packagePath, String classPath) {
        List<Source> sources = new ArrayList<Source>();
        JarFile jarFile = tryCreateJarFile(classPath);
        for (Enumeration em1 = jarFile.entries(); em1.hasMoreElements();) {
            String entry = em1.nextElement().toString();
            int lastSlash = entry.lastIndexOf('/');
            int pathLength = packagePath.length();
            if (entry.startsWith(packagePath) && pathLength == lastSlash && entry.endsWith(".st"))
                sources.add(sourceFactory.createFromJar(entry, classPath));
        }
        return sources;
    }

    private List<Source> findSourceInFile(String packagePath, String classPath) {
        File folder = new File(classPath + RedlineFile.separator + packagePath);
        if (!folder.isDirectory())
            return Collections.EMPTY_LIST;
        List<Source> sources = new ArrayList<Source>();
        for (File file : folder.listFiles())
            if (file.isFile() && file.getName().endsWith(".st"))
                sources.add(sourceFactory.createFromFile(packagePath + RedlineFile.separator + file.getName(), file.getAbsolutePath()));
        return sources;
    }

    public Source find(String name) {
        String sourceName = makeFilename(name);
        return findSource(sourceName);
    }

    public Source findSource(String sourceName) {
        Source source;
        for (String classPath : classPaths)
            if ((source = findSource(sourceName, classPath)) != null)
                return source;
        return null;
    }

    public Source findSource(String sourceName, String classPath) {
        if (isJar(classPath))
            return findSourceInJar(sourceName, classPath);
        else
            return findSourceFile(sourceName, classPath);
    }

    private Source findSourceInJar(String sourceName, String classPath) {
        JarFile jarFile = tryCreateJarFile(classPath);
        String sourceNameForJar = sourceName.replace(WINDOWS_PATH_SEPARATOR, JAR_PATH_SEPARATOR);
        ZipEntry entry = jarFile.getEntry(sourceNameForJar);
        if (entry != null)
            return sourceFactory.createFromJar(sourceNameForJar, classPath);
        return null;
    }

    private JarFile tryCreateJarFile(String classPath) {
        try {
            return createJarFile(classPath);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private boolean isJar(String classPath) {
        return classPath.endsWith(".jar") || classPath.endsWith(".JAR");
    }

    private Source findSourceFile(String sourceName, String classPath) {
        File file = createFile(sourceName, classPath);
        if (file.exists())
            return sourceFactory.createFromFile(sourceName, classPath);
        return null;
    }

    public File createFile(String sourceName, String classPath) {
        return new File(classPath + RedlineFile.separator + sourceName);
    }

    public JarFile createJarFile(String classPath) throws IOException {
        return new JarFile(classPath);
    }

    private String makeFilename(String name) {
        String filename = name.replace(".", RedlineFile.separator);
        return filename + SOURCE_EXTENSION;
    }
}
