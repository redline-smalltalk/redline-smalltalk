package st.redline.stout;

public interface RouterRegistry {
    Router register(String spec, String type, String method, Block block);
    Router lookup(String path, String method);
}
