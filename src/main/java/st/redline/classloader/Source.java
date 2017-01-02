package st.redline.classloader;

public interface Source {

    boolean hasContent();
    String contents();
    String className();
    String fullClassName();
    String fileExtension();
    String packageName();
}
