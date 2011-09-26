package st.redline.stout;

public class RequestPathSpecificationFactoryImpl implements RequestPathSpecificationFactory {
    public RequestPathSpecification create(String specification) {
        return new RequestPathSpecificationImpl(specification);
    }
}
