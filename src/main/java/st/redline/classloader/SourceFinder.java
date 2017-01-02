package st.redline.classloader;

import java.util.*;

public interface SourceFinder {

    Source find(String name);
    List<Source> findIn(String packageName);
}
