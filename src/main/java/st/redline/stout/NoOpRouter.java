/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

class NoOpRouter implements Router {
    static final Router INSTANCE = new NoOpRouter();

    private NoOpRouter() {}

    public void dispatchToBlock(HttpServletResponse servletResponse, String requestPath) throws IOException {}

    public boolean canHandleRequest(String requestPath) {
        return true;
    }
}
