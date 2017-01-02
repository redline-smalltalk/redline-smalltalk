package st.redline.classloader;

public interface LineTransformer {
    String transform(String line);
    String begin();
    String end();
}
