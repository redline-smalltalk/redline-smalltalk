/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.ProtoObject;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.Writer;
import java.util.Map;

public class RouterImpl implements Router {
    private ResponseSerializer responseSerializer;
    private Block block;
    private RequestPathSpecification requestPathSpecification;

    public RouterImpl(ResponseSerializer responseSerializer, Block block,
                      RequestPathSpecification requestPathSpecification) {
        this.responseSerializer = responseSerializer;
        this.block = block;
        this.requestPathSpecification = requestPathSpecification;
    }

    public void dispatchToBlock(ProtoObject servletResponse, ProtoObject requestPath) throws IOException {
        System.out.println("dispatchToBlock()" + servletResponse + " " + requestPath);
    }

    public void dispatchToBlock(HttpServletResponse servletResponse, String requestPath) throws IOException {
        Map<String, String> parameters = retrieveParametersAccordingToSpecification(requestPath);
        Object responseValue = invokeTargetBlock(parameters);
        String response = serializeResponse(responseValue);
        sendClientResponse(servletResponse, response);
    }

    public boolean canHandleRequest(String requestPath) {
        return requestPathSpecification.isPathMatching(requestPath);
    }

    private void sendClientResponse(HttpServletResponse servletResponse, String response) throws IOException {
        Writer writer = servletResponse.getWriter();
        writer.write(response);
        writer.close();
    }

    private String serializeResponse(Object response) {
        return responseSerializer.serialize(response);
    }

    private Object invokeTargetBlock(Map<String, String> parameters) {
        return block.applyTo(parameters);
    }

    private Map<String, String> retrieveParametersAccordingToSpecification(String requestPath) {
        return requestPathSpecification.parseParameters(requestPath);
    }
}
