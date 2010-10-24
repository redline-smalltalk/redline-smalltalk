package st.redline.compiler;

import st.redline.ProtoObject;
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
			Class.forName("st.redline." + scriptName).newInstance();

			ProtoObject object = Smalltalk.classNamed("Object");
			dump(object);

			Smalltalk.classNamed(scriptName).$send("new").$send("doIt");

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void dump(ProtoObject object) {
		System.out.println();
		System.out.println("object      " + object);
		System.out.println("object type " + object.getClass().getName());
		System.out.println();
	}
}
