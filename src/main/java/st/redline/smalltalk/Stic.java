/*
Redline Smalltalk is licensed under the MIT License

Redline Smalltalk Copyright (c) 2010 James C. Ladd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
package st.redline.smalltalk;

import java.io.File;
import java.io.PrintWriter;
import java.util.List;

/**
 * Invokes Smalltalk from the command line.
 */
public class Stic {

	private Smalltalk smalltalk;

	public static void main(String[] args) {
		PrintWriter standardOut = new PrintWriter(System.out);
		PrintWriter standardErr = new PrintWriter(System.err);
		CommandLine commandLine = createCommandLineWith(args);
		Environment environment = createEnvironmentFrom(commandLine, standardOut, standardErr);
		Smalltalk smalltalk = createSmalltalkOn(environment);
		new Stic(smalltalk).run();
	}

	public static Environment createEnvironmentFrom(CommandLine commandLine, PrintWriter output, PrintWriter error) {
		return Environment.from(commandLine, output, error);
	}

	public static Smalltalk createSmalltalkOn(Environment environment) {
		return Smalltalk.with(environment);
	}

	public static CommandLine createCommandLineWith(String[] arguments) {
		return new CommandLine(arguments);
	}

	public Stic(Smalltalk smalltalk) {
		this.smalltalk = smalltalk;
	}

	public Stic run() {
		if (helpRequested())
			return printHelp();
		if (haveFileNames())
			runSmalltalkScripts();
		return this;
	}

	private void runSmalltalkScripts() {
		for (Object fileName : fileNames())
			smalltalk.eval(new File(fileName.toString()));
	}

	private List fileNames() {
		return commandLine().arguments();
	}

	private boolean haveFileNames() {
		return !commandLine().haveNoArguments();
	}

	private Stic printHelp() {
		commandLine().printHelp(output());
		return this;
	}

	private PrintWriter output() {
		return smalltalk.output();
	}

	private boolean helpRequested() {
		return commandLine().helpRequested();
	}

	private CommandLine commandLine() {
		return smalltalk.commandLine();
	}
}
