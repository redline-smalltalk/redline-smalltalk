// Generated from Smalltalk.g4 by ANTLR 4.1
package st.redline.compiler;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link SmalltalkParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface SmalltalkVisitor<T> extends ParseTreeVisitor<T> {
	byte[] generatedClassBytes();
	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#symbol}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSymbol(@NotNull SmalltalkParser.SymbolContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#unaryTail}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryTail(@NotNull SmalltalkParser.UnaryTailContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#blockParamList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlockParamList(@NotNull SmalltalkParser.BlockParamListContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#keywords}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitKeywords(@NotNull SmalltalkParser.KeywordsContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#keywordSend}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitKeywordSend(@NotNull SmalltalkParser.KeywordSendContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#unaryMessage}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnaryMessage(@NotNull SmalltalkParser.UnaryMessageContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#temps}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTemps(@NotNull SmalltalkParser.TempsContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#stFloat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStFloat(@NotNull SmalltalkParser.StFloatContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#reference}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReference(@NotNull SmalltalkParser.ReferenceContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#number}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumber(@NotNull SmalltalkParser.NumberContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#dynamicDictionary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDynamicDictionary(@NotNull SmalltalkParser.DynamicDictionaryContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#block}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBlock(@NotNull SmalltalkParser.BlockContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#hex}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitHex(@NotNull SmalltalkParser.HexContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#StatementExpressions}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatementExpressions(@NotNull SmalltalkParser.StatementExpressionsContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#literalArrayRest}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteralArrayRest(@NotNull SmalltalkParser.LiteralArrayRestContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#cascade}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCascade(@NotNull SmalltalkParser.CascadeContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#stInteger}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStInteger(@NotNull SmalltalkParser.StIntegerContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#binarySend}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBinarySend(@NotNull SmalltalkParser.BinarySendContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#expressions}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpressions(@NotNull SmalltalkParser.ExpressionsContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#script}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitScript(@NotNull SmalltalkParser.ScriptContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#StatementExpressionsAnswer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatementExpressionsAnswer(@NotNull SmalltalkParser.StatementExpressionsAnswerContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#sequence}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSequence(@NotNull SmalltalkParser.SequenceContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#expressionList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpressionList(@NotNull SmalltalkParser.ExpressionListContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#numberExp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumberExp(@NotNull SmalltalkParser.NumberExpContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#string}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitString(@NotNull SmalltalkParser.StringContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#primitive}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPrimitive(@NotNull SmalltalkParser.PrimitiveContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#unarySend}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnarySend(@NotNull SmalltalkParser.UnarySendContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#keywordMessage}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitKeywordMessage(@NotNull SmalltalkParser.KeywordMessageContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#subexpression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSubexpression(@NotNull SmalltalkParser.SubexpressionContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteral(@NotNull SmalltalkParser.LiteralContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#bareSymbol}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBareSymbol(@NotNull SmalltalkParser.BareSymbolContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#dynamicArray}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDynamicArray(@NotNull SmalltalkParser.DynamicArrayContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#ws}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWs(@NotNull SmalltalkParser.WsContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#pseudoVariable}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPseudoVariable(@NotNull SmalltalkParser.PseudoVariableContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpression(@NotNull SmalltalkParser.ExpressionContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#StatementAnswer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStatementAnswer(@NotNull SmalltalkParser.StatementAnswerContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#bareLiteralArray}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBareLiteralArray(@NotNull SmalltalkParser.BareLiteralArrayContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#unarySelector}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnarySelector(@NotNull SmalltalkParser.UnarySelectorContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#assignment}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAssignment(@NotNull SmalltalkParser.AssignmentContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#message}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMessage(@NotNull SmalltalkParser.MessageContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#binaryMessage}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBinaryMessage(@NotNull SmalltalkParser.BinaryMessageContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#charConstant}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCharConstant(@NotNull SmalltalkParser.CharConstantContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#parsetimeLiteral}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParsetimeLiteral(@NotNull SmalltalkParser.ParsetimeLiteralContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#answer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnswer(@NotNull SmalltalkParser.AnswerContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#keywordPair}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitKeywordPair(@NotNull SmalltalkParser.KeywordPairContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#binaryTail}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBinaryTail(@NotNull SmalltalkParser.BinaryTailContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#literalArray}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteralArray(@NotNull SmalltalkParser.LiteralArrayContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#variable}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVariable(@NotNull SmalltalkParser.VariableContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#runtimeLiteral}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRuntimeLiteral(@NotNull SmalltalkParser.RuntimeLiteralContext ctx);

	/**
	 * Visit a parse tree produced by {@link SmalltalkParser#operand}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOperand(@NotNull SmalltalkParser.OperandContext ctx);
}