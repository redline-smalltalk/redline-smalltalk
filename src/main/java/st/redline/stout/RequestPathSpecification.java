package st.redline.stout;

import java.util.Map;

public interface RequestPathSpecification {
    Map<String, String> parseParameters(String requestPath);

    boolean isPathMatching(String requestPath);
}
