/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;

public class Stic {

	public static void main(String[] args) throws Exception {
		invokeWith(Stic.class, args);
	}

	// NOTE: The object returned by this method can be an eigenClass.
	public static PrimObject invokeWith(Class aClass, String[] args) throws Exception {
		CommandLine commandLine = createCommandLineWith(args);
		if (commandLine.haveNoArguments()) {
			commandLine.printHelp(new PrintWriter(System.out));
			return null;
		}
		String inputFilename;
		if (commandLine.executeNowRequested()) {
			inputFilename = writeInputCodeToTemporaryFile(commandLine).getName();
			inputFilename = inputFilename.substring(0, inputFilename.lastIndexOf("."));
		} else {
			inputFilename = (String) commandLine.arguments().get(0);
		}
		return instanceOfWith(aClass, commandLine).invoke(inputFilename);
	}

	private static Stic instanceOfWith(Class aClass, CommandLine commandLine) throws Exception {
		Constructor constructor = aClass.getConstructor(CommandLine.class);
		return (Stic) constructor.newInstance(commandLine);
	}

	static File writeInputCodeToTemporaryFile(CommandLine commandLine) throws Exception {
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

	void bootstrap() throws ClassNotFoundException {
		classLoader().bootstrap();
	}

	void initializeClassLoader(CommandLine commandLine) {
		Thread.currentThread().setContextClassLoader(createClassLoader(commandLine));
	}

	ClassLoader createClassLoader(CommandLine commandLine) {
		return new SmalltalkClassLoader(Thread.currentThread().getContextClassLoader(), commandLine);
	}

	SmalltalkClassLoader classLoader() {
		return SmalltalkClassLoader.instance();
	}

	public PrimObject invoke(String className) throws Exception {
		return (PrimObject) classLoader().loadClass(className).newInstance();
	}
}
