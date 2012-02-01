/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

class BlockAnalyser extends ProgramAnalyser implements AnalyserDelegate {

	BlockAnalyser(Analyser analyser, String className, String packageName, boolean verbose) {
		this(analyser, new ClassBytecodeWriter(className, packageName, verbose), verbose);
	}

	BlockAnalyser(Analyser analyser, ClassBytecodeWriter classBytecodeWriter, boolean verbose) {
		super(analyser, classBytecodeWriter, verbose);
	}
}
