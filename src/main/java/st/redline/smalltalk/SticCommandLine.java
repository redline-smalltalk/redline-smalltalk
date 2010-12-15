package st.redline.smalltalk;

import org.apache.commons.cli.*;

import java.util.List;

/**
 * Processes the rawArguments given to Stic.
 */
class SticCommandLine {

	private final String[] rawArguments;
	private CommandLine commandLine;

	public SticCommandLine(String[] rawArguments) {
		this.rawArguments = rawArguments;
	}

	Stic printHelp() {
		helpFormatter().printHelp("stic [options] <source files>", commandLineOptions());
		return null;
	}

	List arguments() {
		return commandLine.getArgList();
	}

	boolean helpRequested() {
		return haveHelpOption() || haveNoArguments();
	}

	boolean haveNoArguments() {
		return commandLine.getArgList().isEmpty();
	}

	boolean haveHelpOption() {
		return commandLine.hasOption('?');
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

	CommandLineParser commandLineParser() {
		return new GnuParser();
	}

	HelpFormatter helpFormatter() {
		return new HelpFormatter();
	}

	private class CommandLineOptions extends Options {

		public CommandLineOptions() {
			addOption(help());
		}

		private Option help() {
			return new Option("?", "help", false, "print this message.");
		}
	}
}