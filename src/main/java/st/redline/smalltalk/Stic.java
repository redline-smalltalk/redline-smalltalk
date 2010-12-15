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

import java.io.PrintWriter;
import java.util.List;

/**
 * Invokes Smalltalk from the command line.
 */
public class Stic {

	private final PrintWriter outputWriter;
	private final PrintWriter errorWriter;
	private final SticCommandLine commandLine;

	public static void main(String[] args) {
		new Stic(args, new PrintWriter(System.out), new PrintWriter(System.err)).run();
	}

	public Stic(String[] arguments, PrintWriter outputWriter, PrintWriter errorWriter) {
		this.outputWriter = outputWriter;
		this.errorWriter = errorWriter;
		this.commandLine = commandLineFrom(arguments);
		parseCommandLine();
	}

	private SticCommandLine commandLineFrom(String[] arguments) {
		return new SticCommandLine(arguments);
	}

	private void parseCommandLine() {
		commandLine.tryParseArguments();
	}

	public Stic run() {
		if (helpRequested())
			return printHelp();
		if (haveFileNames())
			System.out.println(fileNames());
		return this;
	}

	private List fileNames() {
		return commandLine.arguments();
	}

	private boolean haveFileNames() {
		return !commandLine.haveNoArguments();
	}

	private Stic printHelp() {
		commandLine.printHelp(outputWriter);
		return this;
	}

	private boolean helpRequested() {
		return commandLine.helpRequested();
	}
}
