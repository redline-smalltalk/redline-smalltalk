package st.redline.compiler;

public abstract class MessageSend {
	abstract Primary primary();
	abstract boolean hasBlockWithAnswer();
}