package st.redline.stout;

public interface RouterFactory {
    Router create(String requestPathSpec, String type, Block block);
}
