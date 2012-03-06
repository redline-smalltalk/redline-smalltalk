/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import java.util.Map;

public interface RequestPathSpecification {
    Map<String, String> parseParameters(String requestPath);

    boolean isPathMatching(String requestPath);
}
