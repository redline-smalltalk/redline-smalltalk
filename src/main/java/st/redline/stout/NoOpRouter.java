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
