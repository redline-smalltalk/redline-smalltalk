/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import java.io.PrintWriter;

public class Stic {

	public static void main(String[] args) throws Exception {
		invokeWith(args);
	}

	public static ProtoObject invokeWith(String[] args) throws Exception {
		CommandLine commandLine = createCommandLineWith(args);
		if (commandLine.haveNoArguments()) {
			commandLine.printHelp(new PrintWriter(System.out));
			return null;
		}
		return new Stic(commandLine).invoke((String) commandLine.arguments().get(0));
	}

	public static CommandLine createCommandLineWith(String[] args) {
		return new CommandLine(args);
	}

	public Stic(CommandLine commandLine) {
		initializeClassLoader(commandLine);
		bootstrap();
	}

	private void bootstrap() {
		classLoader().bootstrap();
	}

	private void initializeClassLoader(CommandLine commandLine) {
		Thread.currentThread().setContextClassLoader(createClassLoader(commandLine));
	}

	private ClassLoader createClassLoader(CommandLine commandLine) {
		return new SmalltalkClassLoader(Thread.currentThread().getContextClassLoader(), commandLine);
	}

	private SmalltalkClassLoader classLoader() {
		return SmalltalkClassLoader.instance();
	}

	public ProtoObject invoke(String className) throws Exception {
		ProtoObject root = protoObjectInstance();
		root.name("<root>");
		return createClassInstance(root, className);
	}

	private ProtoObject createClassInstance(ProtoObject root, String className) throws Exception {
		return ProtoObject.primitiveResolveObject(root, className);
	}

	private ProtoObject protoObjectInstance() throws ClassNotFoundException, IllegalAccessException, InstantiationException {
		return (ProtoObject) classLoader().loadClass("st.redline.ProtoObject").newInstance();
	}
}
