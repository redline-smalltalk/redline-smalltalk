/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import org.mortbay.jetty.Handler;
import org.mortbay.jetty.Server;
import org.mortbay.jetty.handler.AbstractHandler;

import st.redline.core.CommandLine;
import st.redline.core.PrimObject;
import st.redline.core.Stic;
import st.redline.core.PrimObjectMetaclass;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class Run extends Stic {

    static Server server;
    static final PrimObject[] DISPATCH_SYMBOLS = new PrimObject[9];

    public static void main(String[] args) throws Exception {
        startServer(args);
    }

    public Run(CommandLine commandLine) throws Exception {
        super(commandLine);
    }

    static void startServer(String[] args) throws Exception {
        server = new Server(8080);
        server.setHandler(initialHandler(args));
        server.start();
        server.join();
    }

    static Handler initialHandler(final String[] args) throws Exception {
        return new AbstractHandler() {
            public void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch)
                    throws IOException, ServletException {
                Handler handler;
                try {
                    handler = continuingHandler(objectFrom(args));
                } catch (Exception e) {
                    throw new ServletException(e);
                }
                server.setHandler(handler);
                handler.handle(target, request, response, dispatch);
            }

            private PrimObject objectFrom(String[] args) throws Exception {
                PrimObjectMetaclass eigenClass = (PrimObjectMetaclass) invokeWith(Run.class, args);
                return PrimObject.CLASSES.get(eigenClass.getClass().getName()).perform("new");
            }
        };
    }

    static Handler continuingHandler(final PrimObject receiver) {
        return new AbstractHandler() {
            public void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch)
                    throws IOException, ServletException {
                try {
                    System.out.println("continuingHandler() " + receiver + " " + request.getMethod() + " " + target);
                    receiver.perform(
                            method(request.getMethod()),
                            string(target),
                            wrapper(request),
                            wrapper(response),
                            dispatch(dispatch),
                            "handle:on:with:and:and:");
                } catch (Exception e) {
                    throw new ServletException(e);
                }
            }
            PrimObject wrapper(Object value) {
                PrimObject object = new PrimObject();
                object.javaValue(value);
                return object;
            }
            PrimObject string(String value) {
                return PrimObject.string(value);
            }
            PrimObject dispatch(int dispatch) {
                if (DISPATCH_SYMBOLS[dispatch] == null)
                    initializeDispatchSymbol(dispatch);
                return DISPATCH_SYMBOLS[dispatch];
            }
            void initializeDispatchSymbol(int dispatch) {
                switch (dispatch) {
                    case 1:
                        DISPATCH_SYMBOLS[1] = PrimObject.symbol("Request");
                        break;
                    case 2:
                        DISPATCH_SYMBOLS[2] = PrimObject.symbol("Forward");
                        break;
                    case 4:
                        DISPATCH_SYMBOLS[4] = PrimObject.symbol("Include");
                        break;
                    case 8:
                        DISPATCH_SYMBOLS[8] = PrimObject.symbol("Error");
                        break;
                    default:
                        throw new IllegalStateException("Dispatch value of " + dispatch + " not understood.");
                }
            }
            PrimObject method(String method) {
                return PrimObject.symbol(method);
            }
        };
    }
}
