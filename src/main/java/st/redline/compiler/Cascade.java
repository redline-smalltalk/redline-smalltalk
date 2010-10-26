package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class Cascade {

	private UnarySend unarySend;
	private List<BinarySend> binarySends = new ArrayList<BinarySend>();
	private List<KeywordSend> keywordSends = new ArrayList<KeywordSend>();
	private List<Object> cascadedSends = new ArrayList<Object>();

	public void add(UnarySend unarySend) {
		this.unarySend = unarySend;
	}

	public UnarySend unarySend() {
		return unarySend;
	}

	public void add(BinarySend binarySend) {
		binarySends.add(binarySend);
	}

	public boolean hasBinarySends() {
		return !binarySends.isEmpty();
	}

	public List<BinarySend> binarySends() {
		return binarySends;
	}

	public void add(KeywordSend keywordSend) {
		keywordSends.add(keywordSend);
	}

	public boolean hasKeywordSends() {
		return !keywordSends.isEmpty();
	}

	public List<KeywordSend> keywordSends() {
		return keywordSends;
	}

	public void addCascadeSend(Object send) {
		cascadedSends.add(send);
	}

	public boolean hasCascadedSends() {
		return !cascadedSends.isEmpty();
	}

	public List<Object> cascadedSends() {
		return cascadedSends;
	}
}