package st.redline.stout;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RequestPathSpecificationImpl implements RequestPathSpecification {
    private static final String PARAM_NAME_REGEX = "[\\p{Alnum}_]+";
    private static final String PARAM_VALUE_REGEX = "[^/]+";
    private static final String ID_REGEX = ":" + PARAM_NAME_REGEX;
    private String specification;

    public RequestPathSpecificationImpl(String specification) {
        this.specification = specification;
    }

    public boolean isPathMatching(String requestPath) {
        return requestPath.matches(regexEscapedSpecificationWithIdPatternReplacedWith(PARAM_VALUE_REGEX));
    }

    public Map<String, String> parseParameters(String requestPath) {
        List<String> fieldNames = getFieldNames();
        List<String> parameterValues = getFieldValues(requestPath);
        return mergeFieldWithValues(fieldNames, parameterValues);
    }

    private Map<String, String> mergeFieldWithValues(List<String> fieldNames, List<String> parameterValues) {
        Map<String, String> parameters = new HashMap<String, String>();
        for (int i = 0; i < fieldNames.size(); i++) {
            parameters.put(fieldNames.get(i), parameterValues.get(i));
        }
        return parameters;
    }

    private List<String> getFieldValues(String requestPath) {
        return extractValuesFromString(requestPath, String.format("(%s)", PARAM_VALUE_REGEX));
    }

    private List<String> getFieldNames() {
        return extractValuesFromString(specification, String.format(":(%s)", regexEscape(PARAM_NAME_REGEX)));
    }

    private List<String> extractValuesFromString(String string, String valueGroupRegexPattern) {
        List<String> values = new ArrayList<String>();
        Pattern pattern = Pattern.compile(regexEscapedSpecificationWithIdPatternReplacedWith(valueGroupRegexPattern));
        Matcher matcher = pattern.matcher(string);
        matcher.find();
        for (int i = 1; i <= matcher.groupCount(); i++) {
            values.add(matcher.group(i));
        }
        return values;
    }

    private String regexEscapedSpecification() {
        return regexEscape(specification);
    }

    private String regexEscape(String value) {
        return value.replace("\\", "\\\\");
    }

    private String regexEscapedSpecificationWithIdPatternReplacedWith(String value) {
        return regexEscapedSpecification().replaceAll(ID_REGEX, value);
    }
}
