package st.redline;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.mortbay.jetty.Connector;
import org.mortbay.jetty.Handler;
import org.mortbay.jetty.HttpConnection;
import org.mortbay.jetty.Request;
import org.mortbay.jetty.Server;
import org.mortbay.jetty.bio.SocketConnector;
import org.mortbay.jetty.handler.AbstractHandler;

public class Serve {

	// App will be available on localhost:8080
	public static void main(String[] args) throws Exception {
		Server server = new Server();
		Connector connector=new SocketConnector();
		connector.setPort(8080);
		server.setConnectors(new Connector[]{connector});

		server.setHandler(handler());
		server.start();
		server.join();
	}

	private static Handler handler() {
		return new AbstractHandler() {
			public void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) throws IOException, ServletException {
				System.out.println(request);
				Request base_request = (request instanceof Request) ? (Request) request:HttpConnection.getCurrentConnection().getRequest();
				base_request.setHandled(true);

				response.setContentType("text/html");
				response.setStatus(HttpServletResponse.SC_OK);
				response.getWriter().println("<h1>Hello from Redline Smalltalk</h1>");
			}
		};
	}
}