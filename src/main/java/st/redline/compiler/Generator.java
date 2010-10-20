package st.redline.compiler;

import st.redline.ScriptListener;

import java.util.List;

public class Generator extends ClassLoader implements Executor, Translator {

	private final JavaBytecodeEncoder encoder;
	private final ParsedSource parsedSource;
	private final ScriptListener scriptListener;

	private String sourcePath;
	private int classDefinitionLineNumber;

	public Generator(ParsedSource parsedSource, ScriptListener scriptListener) {
		this.parsedSource = parsedSource;
		this.scriptListener = scriptListener;
		encoder = new JavaBytecodeEncoder();
	}

	public String outputPath() {
		return parsedSource.outputPath();
	}

	public void execute() {
		sourcePath = parsedSource.sourcePath();
		parsedSource.apply(this);
	}

	public String definedClassName() {
		return parsedSource.definedClassName();
	}

	public String definedPackageName() {
		return JavaBytecodeEncoder.DEFAULT_JAVA_PACKAGE;
	}

	public byte[][] definedClassBytes() {
		notifyClassGenerated();

		// to be removed, just a test.
//		byte[][] bytes = encoder.definedClassBytes();
//		try {
//			System.out.println(defineClass("st.redline.Object$mClass", bytes[1], 0, bytes[1].length).newInstance().getClass().getName());
//			System.out.println(defineClass("st.redline.Object", bytes[0], 0, bytes[0].length).newInstance().getClass().getName());
//		} catch (Exception e) { e.printStackTrace(); }
		// to be removed, just a test.
//		return bytes;
		return encoder.definedClassBytes();
	}

	private void notifyClassGenerated() {
		if (scriptListener.isEnabled())
			scriptListener.generated(definedPackageName() + definedClassName(), outputPath());
	}

	public void visitClassDefinition(ClassDefinition classDefinition, ClassInstanceVariables classInstanceVariables) {
		encoder.defineClass(classDefinition, sourcePath);
	}

	public void visitMethods(List<Method> classMethods, List<Method> instanceMethods) {
		encoder.defineMethods(sourcePath, classDefinitionLineNumber, classMethods, true);
		encoder.defineMethods(sourcePath, classDefinitionLineNumber, instanceMethods, false);
	}
}