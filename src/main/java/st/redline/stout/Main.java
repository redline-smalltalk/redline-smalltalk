/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.PrimContext;
import st.redline.PrimObject;
import st.redline.PrimObjectBlock;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;

public class Main {
    public static void main(String[] args) throws IOException {

        RouterRegistryImpl routerRegistry = createRouterRegistry();
        registerRouter(routerRegistry);
        serveRequest(routerRegistry);

    }

    private static void registerRouter(RouterRegistry routerRegistry) {
        String spec = "/user/:id";
        routerRegistry.register(spec, "JSON", "get", new PrimObjectBlock() {
            protected PrimObject invoke(PrimObject receiver, PrimContext context) {
                return this;
            }
        });
    }

    private static void serveRequest(RouterRegistryImpl routerRegistry) throws IOException {
        String path = "/user/344645";
        HttpServletResponse response = new MockServletResponse();
        routerRegistry.lookup(path, "get").dispatchToBlock(response, path);
    }

    private static RouterRegistryImpl createRouterRegistry() {
        return new RouterRegistryImpl(new RouterFactoryImpl(new RequestPathSpecificationFactoryImpl()));
    }

    private static class MockServletResponse implements HttpServletResponse {

        public void addCookie(Cookie cookie) {
        }

        public boolean containsHeader(String s) {
            return false;
        }

        public String encodeURL(String s) {
            return null;
        }

        public String encodeRedirectURL(String s) {
            return null;
        }

        public String encodeUrl(String s) {
            return null;
        }

        public String encodeRedirectUrl(String s) {
            return null;
        }

        public void sendError(int i, String s) throws IOException {
        }

        public void sendError(int i) throws IOException {
        }

        public void sendRedirect(String s) throws IOException {
        }

        public void setDateHeader(String s, long l) {
        }

        public void addDateHeader(String s, long l) {
        }

        public void setHeader(String s, String s1) {
        }

        public void addHeader(String s, String s1) {
        }

        public void setIntHeader(String s, int i) {
        }

        public void addIntHeader(String s, int i) {
        }

        public void setStatus(int i) {
        }

        public void setStatus(int i, String s) {
        }

        public String getCharacterEncoding() {
            return null;
        }

        public String getContentType() {
            return null;
        }

        public ServletOutputStream getOutputStream() throws IOException {
            return null;
        }

        public PrintWriter getWriter() throws IOException {
            return new PrintWriter(System.out);
        }

        public void setCharacterEncoding(String s) {
        }

        public void setContentLength(int i) {
        }

        public void setContentType(String s) {
        }

        public void setBufferSize(int i) {
        }

        public int getBufferSize() {
            return 0;
        }

        public void flushBuffer() throws IOException {
        }

        public void resetBuffer() {
        }

        public boolean isCommitted() {
            return false;
        }

        public void reset() {
        }

        public void setLocale(Locale locale) {
        }

        public Locale getLocale() {
            return null;
        }
    }
}
