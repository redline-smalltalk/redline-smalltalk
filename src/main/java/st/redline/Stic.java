/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
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
		String inputFilename = (String) commandLine.arguments().get(0);
		if (commandLine.executeNowRequested()) {
			inputFilename = writeInputCodeToTemporaryFile(commandLine).getName();
			inputFilename = inputFilename.substring(0, inputFilename.lastIndexOf("."));
		}
		return new Stic(commandLine).invoke(inputFilename);
	}

	private static File writeInputCodeToTemporaryFile(CommandLine commandLine) throws Exception {
		File input = File.createTempFile("Tmp" + commandLine.hashCode(), ".st", new File(commandLine.userPath()));
		input.deleteOnExit();
		BufferedWriter out = new BufferedWriter(new FileWriter(input));
		try {
			out.write(commandLine.input());
			out.write("\n\n");
			out.flush();
		} finally {
			out.close();
		}
		return input;
	}

	public static CommandLine createCommandLineWith(String[] args) {
		return new CommandLine(args);
	}

	public Stic(CommandLine commandLine) throws ClassNotFoundException {
		initializeClassLoader(commandLine);
		bootstrap();
	}

	private void bootstrap() throws ClassNotFoundException {
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
		return createClassInstance(protoObjectInstance(), className);
	}

	private ProtoObject createClassInstance(ProtoObject root, String className) throws Exception {
		return ProtoObject.resolveObject(root, className);
	}

	private ProtoObject protoObjectInstance() throws ClassNotFoundException, IllegalAccessException, InstantiationException {
		return (ProtoObject) classLoader().loadClass("st.redline.ProtoObject").newInstance();
	}
}
