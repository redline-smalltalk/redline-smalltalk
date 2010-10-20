package st.redline.compiler;

public interface Parser {
	Object parse() throws Exception;
	void sourcePath(String sourcePath);
	void outputPath(String outputPath);
}