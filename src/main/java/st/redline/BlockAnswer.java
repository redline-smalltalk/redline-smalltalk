package st.redline;

public class BlockAnswer extends RuntimeException {

	private ProtoObject answer;

	public BlockAnswer() {}

	public void answer(ProtoObject answer) {
		System.out.println("BlockAnswer set: " + answer);
		this.answer = answer;
	}

	public ProtoObject answer() {
		System.out.println("BlockAnswer get: " + answer);
		return answer;
	}
}