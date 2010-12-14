package st.redline.smalltalk;

import org.apache.commons.cli.*;

/**
 * Processes the arguments given to Stic.
 */
class SticCommandLine {

	private final String[] arguments;
	private CommandLine commandLine;

	public SticCommandLine(String[] arguments) {
		this.arguments = arguments;
	}

	Stic printHelp() {
		helpFormatter().printHelp("Stic", commandLineOptions());
		return null;
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
			commandLine = commandLineParser().parse(commandLineOptions(), arguments);
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
