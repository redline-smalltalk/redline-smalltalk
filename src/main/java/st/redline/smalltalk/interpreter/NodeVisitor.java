package st.redline.smalltalk.interpreter;

public interface NodeVisitor {
	void visit(Program program);
	void visit(Sequence sequence);
	void visit(Statements statements);
	void visit(StatementList statementList);
	void visit(Expression expression);
	void visit(Cascade cascade);
	void visit(MessageSend messageSend);
	void visit(UnaryMessageSend unaryMessageSend);
	void visit(KeywordMessageSend keywordMessageSend);
	void visit(KeywordMessage keywordMessage);
	void visit(KeywordMessagePart keywordMessagePart);
	void visit(KeywordArgument keywordArgument);
	void visit(Variable variable);
	void visit(UnaryMessage unaryMessage);
}
