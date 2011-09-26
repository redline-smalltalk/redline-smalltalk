package st.redline.stout;

/*
  I don't know what are the types (JSON, XML?) will be available. This implementation is just to illustrate how
  st.redline.stout.Router should be created. This class should be removed and implemented properly.
 */
public class RouterFactoryImpl implements RouterFactory {

    private RequestPathSpecificationFactory requestPathSpecificationFactory;

    private static final ResponseSerializer TO_STRING_SERIALIZER = new ResponseSerializer() {
        public String serialize(Object object) {
            return object.toString();
        }
    };

    public RouterFactoryImpl(RequestPathSpecificationFactory requestPathSpecificationFactory) {
        this.requestPathSpecificationFactory = requestPathSpecificationFactory;
    }

    /*
      type parameter should be used to determine which implementation of the st.redline.stout.ResponseSerializer to be used.
     */
    public Router create(String requestPathSpec, String type, Block block) {
        return new RouterImpl(TO_STRING_SERIALIZER, block, requestPathSpecificationFactory.create(requestPathSpec));
    }
}
