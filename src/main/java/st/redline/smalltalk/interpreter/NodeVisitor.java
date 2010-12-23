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
	void visit(Primary primary);
	void visit(UnaryMessage unaryMessage);
}
