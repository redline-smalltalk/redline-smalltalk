/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.core.PrimObject;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

class NoOpRouter implements Router {

    static final Router INSTANCE = new NoOpRouter();

    private NoOpRouter() {}

    public void dispatchToBlock(PrimObject servletResponse, PrimObject requestPath) throws IOException { System.out.println("NOOP"); }
    public void dispatchToBlock(HttpServletResponse servletResponse, String requestPath) throws IOException { System.out.println("NOOP"); }

    public boolean canHandleRequest(String requestPath, String acceptType) {
        return true;
    }
}
