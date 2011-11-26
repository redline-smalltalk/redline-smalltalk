/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.ProtoObject;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public interface Router {
    void dispatchToBlock(HttpServletResponse servletResponse, String requestPath) throws IOException;
    void dispatchToBlock(ProtoObject servletResponse, ProtoObject requestPath) throws IOException;
    boolean canHandleRequest(String requestPath);
}
