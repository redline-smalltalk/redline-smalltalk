/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline.classloader;

import st.redline.compiler.Compiler;

public class ClassBuilder {

    private final SourceFinder sourceFinder;
    private final Compiler compiler;

    public ClassBuilder(SourceFinder sourceFinder, Compiler compiler) {
        this.sourceFinder = sourceFinder;
        this.compiler = compiler;
    }

    public byte[] build(String name, StringBuilder foundInPath) {
        Source source = findSource(name);
        if (source == null)
            return null;
        if (foundInPath != null)
            foundInPath.append(source.path());
        return compiler.compile(source);
    }

    public byte[] build(String name) {
        return build(name, null);
    }

    public Source findSource(String name) {
        return sourceFinder.find(name);
    }
}
