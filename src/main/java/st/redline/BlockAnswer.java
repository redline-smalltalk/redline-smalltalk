package st.redline;

public class BlockAnswer extends Error {

	private ProtoObject answer;

	public BlockAnswer() {}

	public void answer(ProtoObject answer) {
		this.answer = answer;
	}
}