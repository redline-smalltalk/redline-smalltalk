package st.redline.compiler;

import java.util.ArrayList;
import java.util.List;

public class Statements {

	private final List<Expression> expressions;

	public Statements() {
		expressions = new ArrayList<Expression>();
	}

	public void add(Expression expression) {
		expressions.add(expression);
	}

	public void add(Statements statements) {
		for (Expression expression : statements.expressions)
			add(expression);
	}

	public List<Expression> expressions() {
		return expressions;
	}

	public boolean hasAnsweredExpression() {
		for (Expression expression : expressions)
			if (expression.isAnswered())
				return true;
		return false;
	}
}
