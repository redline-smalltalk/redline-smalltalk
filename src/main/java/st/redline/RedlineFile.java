package st.redline;

public class RedlineFile {

    /**
     * The File.separator is replaced by system specific file separator, e.g. on Linux it is "/" and on Windows
     * it is "\". This can't be used, however, inside jar files and the source class generator get messed if the
     * Windows file separator is used. So, instead of File.separator, using simply the "/" separator is the best option
     * since it is recognized by the java io API as an interoperable separator, i.e., it changes to "\" on Windows
     * systems when appropriated; it works for jar files and the source class generator works as expected.
     */
    public static final String separator = "/";
}
