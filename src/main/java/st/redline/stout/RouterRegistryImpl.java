package st.redline.stout;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class RouterRegistryImpl implements RouterRegistry {
    private RouterFactory routerFactory;
    private Map<String, List<Router>> routers = new HashMap<String, List<Router>>();

    public RouterRegistryImpl(RouterFactory routerFactory) {
        this.routerFactory = routerFactory;
    }

    public Router register(String spec, String type, String method, Block block) {
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

    public Router lookup(String path, String method) {
        for (Router router : getRoutersForMethod(method)) {
            if (router.canHandleRequest(path)) return router;
        }
        return NoOpRouter.INSTANCE;
    }
}
