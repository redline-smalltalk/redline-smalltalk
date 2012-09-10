/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.core;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.JarURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarFile;
import java.util.regex.Matcher;

public class SourceFileFinder {

    private static List<String> sourceFilePaths;

    private final String sourceFileName;
    private final String className;
    private final ClassLoader classLoader;

    public SourceFileFinder(String className, ClassLoader classLoader) {
        this.className = className;
        this.classLoader = classLoader;
        this.sourceFileName = makeSourceFileName(className);
    }

    public SourceFile findSourceFile() {
        return findSourceFile(className);
    }

    private String makeSourceFileName(String className) {
        return makePackageIntoPath(className) + ".st";
    }

    public static String makePackageIntoPath(String packageName) {
        return packageName.replaceAll("\\.", Matcher.quoteReplacement(File.separator));
    }

    public static List<String> findInPackage(String packageName) {
        return findIn(makePackageIntoPath(packageName));
    }

    private SourceFile findSourceFile(String sourceFileName) {
        SourceFile sourceFile;
        for (String sourceFilePath : sourceFilePaths()) {
            if ((sourceFile = findSourceFile(sourceFilePath, sourceFileName)) != null)
                return sourceFile;
        }
        return null;
    }

    private SourceFile findSourceFile(String sourceFilePath, String className) {
        String filename = ClassPathUtilities.classNameToFileName(sourceFilePath, className);
        File file = new File(filename);
        if (file.exists())
            return new SourceFile(file);
        InputStream inputStream = classLoader.getResourceAsStream(filename);
        if (inputStream != null)
            return new SourceResource(inputStream, filename);
        return null;
    }

    protected static List<String> findIn(String path) {
        // TODO.JCL - Maybe return a source file that knows it package and base etc so we don't have to play with strings.
        List<String> sourceFiles = new ArrayList<String>();
        for (String sourceFilePath : sourceFilePaths()) {
            File folder = new File(sourceFilePath + File.separator + path);
            System.out.println("findIn() " + folder.toString() + " isFolder: " + folder.isDirectory() + " Exists: " + folder.exists());
            if (folder.exists() && folder.isDirectory()) {
                for (File file : folder.listFiles())
                    if (file.isFile() && file.getName().endsWith(".st")) {
                        System.out.println("found: " + file.toString());
                        int index = file.getAbsolutePath().indexOf(sourceFilePath);
                        sourceFiles.add(file.getAbsolutePath().substring(index + sourceFilePath.length() + 1));
                        System.out.println("adding: " + file.getAbsolutePath().substring(index + sourceFilePath.length() + 1));
                    }
            } else {
                String resourcePath = folder.toString();
                ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
                try {
                    Enumeration<URL> enumeration = classLoader.getResources(folder.toString());
                    while (enumeration.hasMoreElements()) {
                        URL url = enumeration.nextElement();
                        String upath = url.toString();
                        if (upath.startsWith("jar:")) {
                            upath = upath.substring(upath.indexOf(":/") + 1, upath.lastIndexOf("!"));
                            JarFile jarFile = new JarFile(upath);
                            for (Enumeration em1 = jarFile.entries(); em1.hasMoreElements();) {
                                String entry = em1.nextElement().toString();
                                if (entry.startsWith(resourcePath) && entry.endsWith(".st")) {
                                    int index = entry.indexOf(sourceFilePath);
                                    sourceFiles.add(entry.substring(index + sourceFilePath.length() + 1));
                                }
                            }
                        }
                    }
                } catch (IOException e) {
                    throw new RedlineException(e);
                }
            }
        }
        return sourceFiles;
    }

    protected static List<String> sourceFilePaths() {
        if (sourceFilePaths == null) {
            sourceFilePaths = new ArrayList<String>();
            addDefaultPathsTo(sourceFilePaths);
            addUserDefinedPathsTo(sourceFilePaths);
        }
        return sourceFilePaths;
    }

    private static void addUserDefinedPathsTo(List<String> sourceFilePaths) {
        CommandLine commandLine = SmalltalkEnvironment.instance().commandLine();
        sourceFilePaths.addAll(commandLine.sourcePaths());
        List<String> runtimePaths = commandLine.runtimePaths();
        if (runtimePaths.isEmpty())
            System.out.println("Warning: no path to Redline Runtime specified.");
        else
            sourceFilePaths.addAll(commandLine.runtimePaths());
    }

    private static void addDefaultPathsTo(List<String> sourceFilePaths) {
        sourceFilePaths.add("src" + File.separator + "main" + File.separator + "smalltalk");
        sourceFilePaths.add("src" + File.separator + "test" + File.separator + "smalltalk");
        sourceFilePaths.add("rt");
    }
}
