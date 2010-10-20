package st.redline.compiler;

import java.io.*;

public class ScriptFile extends File {

	private static final int CAPACITY = 1024;

	private String line;
	private StringBuffer lineBuffer;
	private String contents;

	public ScriptFile(String scriptFilename) {
		super(scriptFilename);
	}

	public String contents() {
		initializeBuffer();
		readContents();
		resetBuffer();
		return contents;
	}

	private void resetBuffer() {
		lineBuffer = null;
	}

	private void initializeBuffer() {
		lineBuffer = new StringBuffer(CAPACITY);
	}

	private void readContents() {
		Reader reader = createReader();
		try {
			readContentsWith(reader);
		} finally {
			close(reader);
		}
	}

	private void readContentsWith(Reader reader) {
		BufferedReader lineReader = new BufferedReader(reader);
		try {
			while ((line = lineReader.readLine()) != null)
				lineBuffer.append(line).append('\n');
			contents = lineBuffer.toString();
		} catch (IOException e) {
			contents = "Error reading script: " + e.getMessage();
		}
	}

	private Reader createReader() {
		try {
			return new FileReader(this);
		} catch (FileNotFoundException e) {
			throw new IllegalStateException(e);
		}
	}

	private void close(Reader reader) {
		try {
			reader.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}