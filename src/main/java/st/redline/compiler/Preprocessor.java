package st.redline.compiler;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Preprocessor {

	private static final String PATTERN_CLASSCOMMENT_START = "category:[^!]+";

	private final String contents;
	private static final String TERMINATOR = "! !";

	public Preprocessor(String contents) {
		this.contents = terminate(contents);
	}

	private String terminate(String contents) {
		if (!contents.trim().endsWith(TERMINATOR))
			return contents + TERMINATOR;
		return contents;
	}

	// The preprocessor looks for the start of the class comment which is any
	// sequence of characters terminated with a "!".
	// I have had trouble getting the parser to handle these "chunks" so Im
	// removing them.
	// I'd like to avoid this but will need to revisit later.
	public String preprocess() {
		Pattern commentStartHeader = Pattern.compile(PATTERN_CLASSCOMMENT_START);
		Matcher matcher = commentStartHeader.matcher(contents);
		if (matcher.find())
			return contentsWithTokenAtStartOfClassComment(matcher.end());
		return contents;
	}

	private String contentsWithTokenAtStartOfClassComment(int start) {
		int index = start;
		// skip any whitespace we may have.
		while (Character.isWhitespace(contents.charAt(index))) {
			index++;
		}
		if (contents.charAt(index) == '!') {
			index = skipCommentStampChunk(index);
		}

		// if we dont have a commentStamp, we dont need to continue.
		if (index == -1)
			return contents;

		// skip any whitespace we may have.
		while (Character.isWhitespace(contents.charAt(index))) {
			index++;
		}

		// Do we have a class comment?
		if (contents.charAt(index) != '!') {
			StringBuffer newSource = new StringBuffer(contents.substring(0, index));
			// skip comment, but count new lines.
			int lines = 0;
			while (contents.charAt(index) != '!') {
				if (contents.charAt(index) == '\n')
					lines++;
				index++;
			}
			index++;
			// add new lines back so source line numbers match.
			for (int l = 0; l < lines; l++)
				newSource.append("\n");
			newSource.append(contents.substring(index));
			return newSource.toString();
		}
		return contents;
	}

	private int skipCommentStampChunk(int index) {
		int mark = index;
		index++;
		// find start of "commentStamp:" comment.
		while (contents.charAt(index) != '!') {
			index++;
		}
		index++;
		while (contents.charAt(index) != '!') {
			index++;
		}
		// check we just skipped the right thing.
		if (contents.substring(mark, index).indexOf("commentStamp:") == -1)
			return -1;
		index++;
		return index;
	}
}
