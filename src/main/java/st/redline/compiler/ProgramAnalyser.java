/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.compiler;

import java.util.HashMap;
import java.util.Map;

class ProgramAnalyser implements AnalyserDelegate {

	private final ClassBytecodeWriter writer;
	private Map<String, Integer> temporariesRegistry;
	private int temporariesIndex = 0;

	ProgramAnalyser(String className, String packageName, boolean verbose) {
		this(new ClassBytecodeWriter(className, packageName, verbose));
	}

	ProgramAnalyser(ClassBytecodeWriter classBytecodeWriter) {
		writer = classBytecodeWriter;
	}

	ClassBytecodeWriter classBytecodeWriter() {
		return writer;
	}

	int temporariesIndex() {
		return temporariesIndex;
	}

	Map<String, Integer> temporariesRegistry() {
		return temporariesRegistry;
	}

	public byte[] classBytes() {
		return writer.contents();
	}

	public void visitBegin(Program program) {
		writer.openClass();
	}

	public void visitEnd(Program program) {
		writer.closeClass();
	}

	public void visitBegin(Temporaries temporaries) {
		initializeTemporariesRegistration();
		writer.invokeContextTemporariesInit(temporaries.size());
	}

	void initializeTemporariesRegistration() {
		temporariesIndex = 0;
		temporariesRegistry = new HashMap<String, Integer>();
	}

	public void visitEnd(Temporaries temporaries) {
	}

	public void visitBegin(Statements statements) {
	}

	public void visitEnd(Statements statements) {
	}

	public void visit(Temporary temporary, String value, int line) {
		// todo.jcl - output a warning if registered twice?
		temporariesRegistry.put(value, temporariesIndex++);
	}
}
