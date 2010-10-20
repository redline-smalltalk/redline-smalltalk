package st.redline;

public interface ScriptListener {

	boolean isEnabled();
	void parsing(String scriptPath);
	void generated(String fullClassName, String outputPath);
}