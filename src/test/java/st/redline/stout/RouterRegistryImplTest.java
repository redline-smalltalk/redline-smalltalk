package st.redline.stout;

import org.easymock.EasyMockSupport;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import st.redline.stout.*;

import java.util.UUID;

import static org.easymock.EasyMock.eq;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.not;
import static org.junit.Assert.assertSame;

public class RouterRegistryImplTest extends EasyMockSupport {
    private RouterRegistryImpl routerRegistry;
    private RouterFactory routerFactory;

    @Test
    public void shouldBeAbleLookupRouterByDifferentPaths() {
        String spec1 = createRandomString();
        String expectedPathMatchesWithSpec1 = createRandomString();
        String spec2 = createRandomString();
        String expectedPathMatchesWithSpec2 = createRandomString();
        String type = createRandomString();
        String method = createRandomString();
        Block block1 = createMock(Block.class);
        Block block2 = createMock(Block.class);

        Router expectedRouterForSpec1 = expectCreateRouter(spec1, type, block1, expectedPathMatchesWithSpec1);
        Router expectedRouterForSpec2 = expectCreateRouter(spec2, type, block2, expectedPathMatchesWithSpec2);

        replayAll();

        assertSame(expectedRouterForSpec1, routerRegistry.register(spec1, type, method, block1));
        assertSame(expectedRouterForSpec1, routerRegistry.lookup(expectedPathMatchesWithSpec1, method));

        assertSame(expectedRouterForSpec2, routerRegistry.register(spec2, type, method, block2));
        assertSame(expectedRouterForSpec2, routerRegistry.lookup(expectedPathMatchesWithSpec2, method));

        verifyAll();
    }

    @Test
    public void shouldBeAbleToLookupRouterByDifferentMethod() {
        String spec1 = createRandomString();
        String spec2 = createRandomString();
        String expectedPathMatchesWithSpec = createRandomString();
        String type = createRandomString();
        String method1 = createRandomString();
        String method2 = createRandomString();
        Block block1 = createMock(Block.class);
        Block block2 = createMock(Block.class);

        Router expectedRouterForMethod1 = expectCreateRouter(spec1, type, block1, expectedPathMatchesWithSpec);
        Router expectedRouterForMethod2 = expectCreateRouter(spec2, type, block2, expectedPathMatchesWithSpec);

        replayAll();

        routerRegistry.register(spec1, type, method1, block1);
        assertSame(expectedRouterForMethod1, routerRegistry.lookup(expectedPathMatchesWithSpec, method1));

        routerRegistry.register(spec2, type, method2, block2);
        assertSame(expectedRouterForMethod2, routerRegistry.lookup(expectedPathMatchesWithSpec, method2));

        verifyAll();
    }

    @Test
    public void shouldHaveNoOpRouterForRequestThatDoNotHaveRouterRegistered() {
        assertSame(NoOpRouter.INSTANCE, routerRegistry.lookup(createRandomString(), createRandomString()));
    }

    private Router expectCreateRouter(String spec, String type, Block block, String expectedPathMatchesWithSpec) {
        Router router = createMock(Router.class);

        expect(routerFactory.create(spec, type, block)).andReturn(router);
        expect(router.canHandleRequest(expectedPathMatchesWithSpec)).andReturn(true).anyTimes();
        expect(router.canHandleRequest(not(eq(expectedPathMatchesWithSpec)))).andReturn(false).anyTimes();

        return router;
    }

    private String createRandomString() {
        return UUID.randomUUID().toString();
    }

    @Before
    public void setUp() {
        routerFactory = createMock(RouterFactory.class);
        routerRegistry = new RouterRegistryImpl(routerFactory);
    }

    @After
    public void tearDown() {
        routerRegistry = null;
    }
}
