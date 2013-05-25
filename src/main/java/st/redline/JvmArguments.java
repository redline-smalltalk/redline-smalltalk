/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution. */
package st.redline;

import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.util.HashMap;
import java.util.List;

public class JvmArguments {

    private final HashMap<String, String> jvmArguments;

    public JvmArguments() {
        jvmArguments = new HashMap<String, String>();
        getJVMArguments();
    }

    private void getJVMArguments() {
        RuntimeMXBean bean = ManagementFactory.getRuntimeMXBean();
        List<String> arguments = bean.getInputArguments();
        for (String argument : arguments)
            storeJVMArgument(argument);
    }

    private void storeJVMArgument(String argument) {
        jvmArguments.put(argument.toLowerCase(), argument);
    }

    public boolean isTraceClassLoading() {
        return jvmArguments.containsKey("-verbose:stclass");
    }

    public boolean isTracePreprocessor() {
        return jvmArguments.containsKey("-verbose:stprep");
    }

    public boolean isTraceAnalysis() {
        return jvmArguments.containsKey("-verbose:stanal");
    }

    public boolean ignoreParseErrors() {
        return jvmArguments.containsKey("-verbose:stignoreparseerror");
    }

    public boolean isTraceBytecodeWriting() {
        return jvmArguments.containsKey("-verbose:stbyte");
    }
}
