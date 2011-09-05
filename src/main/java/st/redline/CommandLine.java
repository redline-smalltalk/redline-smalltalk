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

Please see DEVELOPER-CERTIFICATE-OF-ORIGIN if you wish to contribute a patch to Redline Smalltalk.
*/
package st.redline;

import org.apache.commons.cli.*;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

/**
 * Processes the raw arguments given in String[] format.
 *
 * Command line options are different to command line arguments.
 * Arguments is the list of <source files>
 */
public class CommandLine {

	private static final String SOURCEPATH_OPTION = "s";
	private static final String RUNTIMEPATH_OPTION = "r";
	private static final String REDLINE_HOME_ENVVAR = "REDLINE_HOME";

	private final String[] rawArguments;
	private org.apache.commons.cli.CommandLine commandLine;

	public CommandLine(String[] rawArguments) {
		this.rawArguments = rawArguments;
		tryParseArguments();
	}

	String userPath() {
		return System.getProperty("user.dir");
	}

	public void printHelp(PrintWriter printWriter) {
		helpFormatter().printHelp(printWriter, 80, "stic [options] <source files>", "", commandLineOptions(), 1, 4, "");
		printWriter.flush();
	}

	public List arguments() {
		return commandLine.getArgList();
	}

	public boolean verboseRequested() {
		return haveVerboseOption();
	}

	boolean helpRequested() {
		return haveHelpOption() || haveNoArguments();
	}

	public boolean haveNoArguments() {
		return commandLine.getArgList().isEmpty();
	}

	boolean haveHelpOption() {
		return commandLine.hasOption('?');
	}

	boolean haveVerboseOption() {
		return commandLine.hasOption('v');
	}

	void tryParseArguments() {
		try {
			commandLine = commandLineParser().parse(commandLineOptions(), rawArguments);
		} catch (ParseException e) {
			throw new IllegalStateException(e);
		}
	}

	Options commandLineOptions() {
		return new CommandLineOptions();
	}

	List<String> sourcePaths() {
		List<String> paths = pathsSpecifiedFor(SOURCEPATH_OPTION);
		paths.add(userPath());
		return paths;
	}

	List<String> runtimePaths() {
		List<String> paths = pathsSpecifiedFor(RUNTIMEPATH_OPTION);
		if (paths.isEmpty() && System.getenv(REDLINE_HOME_ENVVAR) != null) {
			String redlineHome = System.getenv(REDLINE_HOME_ENVVAR);
			if (!redlineHome.endsWith("rt"))
				redlineHome = redlineHome + File.separator + "rt";
			paths.add(redlineHome);
		}
		return paths;
	}

	private List<String> pathsSpecifiedFor(String commandLineOption) {
		List<String> paths = new ArrayList<String>();
		if (commandLine.hasOption(commandLineOption))
			for (String path : commandLine.getOptionValue(commandLineOption).split(File.pathSeparator))
				paths.add(path);
		return paths;
	}

	CommandLineParser commandLineParser() {
		return new GnuParser();
	}

	HelpFormatter helpFormatter() {
		return new HelpFormatter();
	}

	private class CommandLineOptions extends Options {

		public CommandLineOptions() {
			addOption(help());
			addOption(sourcePath());
			addOption(runtimePath());
			addOption(verbose());
		}

		private Option verbose() {
			return new Option("v", "verbose", false, "output messages about what Redline is doing.");
		}

		private Option help() {
			return new Option("?", "help", false, "print this message.");
		}

		private Option sourcePath() {
			return OptionBuilder.withArgName("paths")
								.hasArg()
								.withDescription("where to find input source files. Separate each path with " + File.pathSeparator + ". The paths src/main/smalltalk and src/test/smalltalk are included by default.")
								.create(SOURCEPATH_OPTION);
		}

		private Option runtimePath() {
			return OptionBuilder.withArgName("path")
								.hasArg()
								.withDescription("where to find Redline. Defaults to environment " + REDLINE_HOME_ENVVAR + ".")
								.create(RUNTIMEPATH_OPTION);
		}
	}
}
