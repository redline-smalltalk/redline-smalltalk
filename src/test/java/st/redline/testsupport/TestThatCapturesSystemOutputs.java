package st.redline.testsupport;

import java.io.PrintStream;

/**
 * A base for tests that need to capture System standard out and error.
 */
public class TestThatCapturesSystemOutputs {

	private PrintStream systemStandardOutput;
	private PrintStream systemErrorOutput;

	protected void dontCaptureSystemOutputs() {
		dontCaptureSystemStandardOutput();
		dontCaptureSystemErrorOutput();
	}

	protected void captureSystemOutputs() {
		captureSystemStandardOutput();
		captureSystemErrorOutput();
	}

	protected String capturedStandardOutput() {
		return ((StringPrintStream) System.out).contents();
	}

	protected String capturedErrorOutput() {
		return ((StringPrintStream) System.err).contents();
	}

	private void captureSystemErrorOutput() {
		systemErrorOutput = System.err;
		System.setErr(new StringPrintStream(systemErrorOutput));
	}

	private void captureSystemStandardOutput() {
		systemStandardOutput = System.out;
		System.setOut(new StringPrintStream(systemStandardOutput));
	}

	private void dontCaptureSystemErrorOutput() {
		System.setErr(systemErrorOutput);
	}

	private void dontCaptureSystemStandardOutput() {
		System.setOut(systemStandardOutput);
	}
}