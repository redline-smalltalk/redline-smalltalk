/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
package st.redline.stout;

import st.redline.PrimObjectBlock;
import st.redline.PrimObject;

import javax.servlet.http.HttpServletRequest;
import java.util.*;

public class RouterRegistryImpl implements RouterRegistry {

    private RouterFactory routerFactory;
    private Map<String, List<Router>> routers = new HashMap<String, List<Router>>();

    public RouterRegistryImpl(RouterFactory routerFactory) {
        this.routerFactory = routerFactory;
    }

    public Router register(PrimObject spec, PrimObject type, PrimObject method, PrimObject block) {
        return register((String) spec.javaValue(), (String) type.javaValue(), (String) method.javaValue(), block);
    }

    public Router register(String spec, String type, String method, PrimObject block) {
        Router router = routerFactory.create(spec, type, block);
        getRoutersForMethod(method).add(router);
        return router;
    }

    private List<Router> getRoutersForMethod(String method) {
        if (!routers.containsKey(method)) {
            routers.put(method, new ArrayList<Router>());
        }
        return routers.get(method);
    }

    public Router lookup(PrimObject path, PrimObject method, PrimObject request) {
        String type = acceptHeaderFrom((HttpServletRequest) request.javaValue());
        return lookup((String) path.javaValue(), (String) method.javaValue(), type);
    }

    private String acceptHeaderFrom(HttpServletRequest httpServletRequest) {
        Object attribute = httpServletRequest.getHeader("Accept");
        return attribute != null ? attribute.toString() : "";
    }

    public Router lookup(String path, String method, String type) {
        for (Router router : getRoutersForMethod(method)) {
            if (router.canHandleRequest(path, type)) return router;
        }
        return NoOpRouter.INSTANCE;
    }
}
