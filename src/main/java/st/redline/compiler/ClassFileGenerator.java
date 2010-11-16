package st.redline.compiler;

import st.redline.ScriptListener;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

public class ClassFileGenerator extends Generator {

	public ClassFileGenerator(ParsedSource parsedSource, ScriptListener scriptListener) {
		super(parsedSource, scriptListener);
	}

	public void execute() {
		super.execute();
		writeClassFile();
	}

	private void writeClassFile() {
		byte[][] bytes = definedClassBytes();
		writeClassFile(bytes[0], definedPackageName() + definedClassName());
		writeClassFile(bytes[1], definedPackageName() + definedClassName() + "$mClass");
		for (int i = 2; i < bytes.length; i++)
			writeClassFile(bytes[i], definedPackageName() + definedClassName() + "$block" + i);
	}

	private void writeClassFile(byte[] bytes, String filename) {
		String realFilename = makeRealfilename(filename);
		makeOutputPath();
		FileOutputStream fos = null;
		try {
			fos = new FileOutputStream(realFilename);
			fos.write(bytes);
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (fos != null) try { fos.close(); } catch(IOException e) { e.printStackTrace(); }
		}
	}

	private void makeOutputPath() {
		File folder = new File(outputPath());
		if (!folder.exists())
			folder.mkdirs();
	}

	private String makeRealfilename(String filename) {
		String outputPath = outputPath();
		if (!outputPath.endsWith(File.separator))
			outputPath = outputPath() + File.separator;
		return (outputPath + filename).replace(".", File.separator) + ".class";
	}
}