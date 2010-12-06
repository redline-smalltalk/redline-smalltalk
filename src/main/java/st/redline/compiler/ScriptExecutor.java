package st.redline.compiler;

import st.redline.ProtoObject;
import st.redline.Smalltalk;

public class ScriptExecutor implements Executor {

	private final Generator generator;

	public ScriptExecutor(Generator generator) {
		this.generator = generator;
	}

	public void execute() throws Throwable {
		generator.execute();
		invokeScript();
	}

	private void invokeScript() throws Throwable {
		String scriptName = generator.filenameWithoutExtension();
		tryInitializeScriptClass(scriptName);
		scriptInstance(scriptName).$send("doIt");
	}

	private ProtoObject scriptInstance(String scriptName) throws Throwable {
		return scriptClass(scriptName).$send("new");
	}

	private ProtoObject scriptClass(String scriptName) {
		return Smalltalk.classNamed(scriptName);
	}

	private void tryInitializeScriptClass(String scriptName) {
		try {
			Class.forName("st.redline." + scriptName);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}
