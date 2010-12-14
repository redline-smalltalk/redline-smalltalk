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

import java.io.PrintStream;

/**
 * Invokes Smalltalk from the command line.
 */
public class Stic {

	private final PrintStream outputStream;
	private final PrintStream errorStream;
	private final SticCommandLine commandLine;

	public static void main(String[] args) {
		new Stic(args, System.out, System.err).run();
	}

	public Stic(String[] arguments, PrintStream outputStream, PrintStream errorStream) {
		this.outputStream = outputStream;
		this.errorStream = errorStream;
		this.commandLine = new SticCommandLine(arguments);
		parseCommandLine();
	}

	private void parseCommandLine() {
		commandLine.tryParseArguments();
	}

	public Stic run() {
		if (helpRequested())
			return printHelp();
		return this;
	}

	private Stic printHelp() {
		return commandLine.printHelp();
	}

	private boolean helpRequested() {
		return commandLine.helpRequested();
	}
}
