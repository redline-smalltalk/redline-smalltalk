package st.redline.compiler;

import st.redline.Smalltalk;

public class ScriptExecutor implements Executor {

	private final Generator generator;

	public ScriptExecutor(Generator generator) {
		this.generator = generator;
	}

	public void execute() {
		generator.execute();
		invokeScript();
	}

	private void invokeScript() {
		String scriptName = generator.filenameWithoutExtension();
		try {
			Class.forName("st.redline." + scriptName);
			Smalltalk.classNamed(scriptName).prim$end("new").prim$end("doIt");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
