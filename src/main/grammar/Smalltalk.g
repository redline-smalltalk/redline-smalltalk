/*
Redline Smalltalk is licensed under the MIT License

Redline Smalltalk Copyright (c) 2010 James C. Ladd

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
grammar Smalltalk;

options {
	language = Java;
}

@header {
	package st.redline.smalltalk.interpreter;
}

@lexer::header {
	package st.redline.smalltalk.interpreter;
}

program returns [Program n] 
	:	temporaries? statements methods {$n = new Program($temporaries.n, $statements.n, $methods.n);}
	;

methods returns [Methods n]
	:	{$n = new Methods();}
		( method {$n.add($method.n);} )*
	;
	
method returns [Method n]
	: 	(i = 'def' | c = 'cdef') messagePattern temporaries? statements {$n = MethodFactory.create($i.text, $c.text, $messagePattern.n, $temporaries.n, $statements.n);}
	;

messagePattern returns [MessagePattern n]
	:	unarySelector {$n = new UnarySelectorMessagePattern($unarySelector.n);}
	|	binarySelector variableName {$n = new BinarySelectorMessagePattern($binarySelector.n, $variableName.n);}
	|	( keyword variableName {if ($n != null) ((KeywordMessagePattern)$n).add(new KeywordAndVariableName($keyword.n, $variableName.n)); else $n = new KeywordMessagePattern(new KeywordAndVariableName($keyword.n, $variableName.n));} )+ 
	;

temporaries	 returns [Temporaries n]
	:	{$n = new Temporaries();}
		'|' ( variableName {$n.add(new Temporary($variableName.n));} )* '|'
	;

statements returns [Statements n]
	: 	( nonEmptyStatements {$n = $nonEmptyStatements.n;} )?
	;

nonEmptyStatements returns [Statements n]
	:	'^' expression {$n = new Statements(new AnswerExpression($expression.n));} '.'?
	|	expression ( '.' statements )? {$n = new Statements($expression.n, $statements.n);}
	;

expression returns [Expression n]
	:	variableName (':=' | '_') e = expression {$n = new AssignmentExpression($variableName.n, $e.n);}
	|	simpleExpression {$n = $simpleExpression.n;}
	;

simpleExpression returns [SimpleExpression n]
	:	{$n = new SimpleExpression();}
		primary {$n.add($primary.n);} ( messageExpression {$n.add($messageExpression.n);} ( ';' messageElement {$n.add($messageElement.n);} )* )?
	;

messageElement returns [MessageElement n]
	:	unarySelector {$n = new UnarySelectorMessageElement($unarySelector.n);}
	|	binarySelector unaryObjectDescription {$n = new BinarySelectorMessageElement($binarySelector.n, $unaryObjectDescription.n);}
	|	( keyword binaryObjectDescription {if ($n != null) ((KeywordMessageElement)$n).add($keyword.n, $binaryObjectDescription.n); else $n = new KeywordMessageElement($keyword.n, $binaryObjectDescription.n);} )+
	;

messageExpression returns [MessageExpression n]
	:	unaryExpression {$n = $unaryExpression.n;}
	|	binaryExpression {$n = $binaryExpression.n;}
	|	keywordExpression {$n = $keywordExpression.n;}
	;

unaryExpression returns [UnaryExpression n]
	:	{$n = new UnaryExpression();}
	    ( unarySelector {$n.add($unarySelector.n);} )+ (binaryExpression {$n.add($binaryExpression.n);} | keywordExpression {$n.add($keywordExpression.n);})?
	;

binaryExpression returns [BinaryExpression n]
	:	{$n = new BinaryExpression();}
	    ( binarySelector unaryObjectDescription {$n.add($binarySelector.n, $unaryObjectDescription.n);} )+ (keywordExpression {$n.add($keywordExpression.n);})?
	;

keywordExpression returns [KeywordExpression n]
	:	{$n = new KeywordExpression();}
	    ( keyword binaryObjectDescription {$n.add($keyword.n, $binaryObjectDescription.n);} )+
	;

unaryObjectDescription returns [UnaryObjectDescription n]
	:	primary {$n = new UnaryObjectDescription($primary.n);} ( unarySelector {$n.add($unarySelector.n);} )*
	;

binaryObjectDescription returns [BinaryObjectDescription n]
	:	primary {$n = new BinaryObjectDescription($primary.n);} ( unarySelector {$n.add($unarySelector.n);} )* ( binarySelector unaryObjectDescription {$n.add($binarySelector.n, $unaryObjectDescription.n);} )*
	;

primary returns [Primary n]
	:	literal {$n = $literal.n;}
	|	variableName {$n = $variableName.n;}
	|	block {$n = $block.n;}
	|	'(' expression {$n = new PrimaryExpression($expression.n);} ')'
	|	'{' statements {$n = new PrimaryStatements($statements.n);} '}'
	;

literal returns [Literal n]
	:	numberConstant {$n = new LiteralNumber($numberConstant.n);}
	|	characterConstant {$n = new LiteralCharacter($characterConstant.n);}
	|	stringConstant {$n = new LiteralString($stringConstant.n);}
	|	'#' ( symbol {$n = new LiteralSymbol($symbol.n);} | array {$n = new LiteralArray($array.n);} )
	;

block returns [Block n]
	:	{$n = new Block();}
		'[' ((':' variableName {$n.add($variableName.n);})+ '|')? ( temporaries {$n.add($temporaries.n);} )? statements {$n.add($statements.n);} ']'
	;

array returns [Array n]
	:	{$n = new Array();}
		'(' ( arrayConstantElement {$n.add($arrayConstantElement.n);})* ')'
	;

arrayConstantElement returns [VisitableNode n]
 	:	numberConstant {$n = $numberConstant.n;}
	|	characterConstant {$n = $characterConstant.n;}
	|	stringConstant {$n = $stringConstant.n;}
	|	symbol {$n = $symbol.n;}
	|	array {$n = $array.n;}
	;

symbol returns [Symbol n]
	:	identifier {$n = new Symbol($identifier.n);}
	|	binarySelector {$n = new Symbol($binarySelector.n);}
	|	keyword {$n = new Symbol($keyword.n);}
	;

unarySelector returns [UnarySelector n] 
	:	NAME {$n = new UnarySelector($NAME.text, $NAME.line);}
	;

binarySelector returns [BinarySelector n]
	:	BINARY_SYMBOL {$n = new BinarySelector($BINARY_SYMBOL.text, $BINARY_SYMBOL.line);}
	;

variableName returns [VariableName n]
	:	NAME {$n = new VariableName($NAME.text, $NAME.line);}
	;

keyword returns [Keyword n]
	:	KEYWORD {$n = new Keyword($KEYWORD.text, $KEYWORD.line);}
	;

identifier returns [Identifier n]
	:	NAME {$n = new Identifier($NAME.text, $NAME.line);}
	;

characterConstant returns [CharacterConstant n]
	:	CHARACTER {$n = new CharacterConstant($CHARACTER.text, $CHARACTER.line);}
	;

stringConstant returns [StringConstant n]
	:	STRING_LITERAL {$n = new StringConstant($STRING_LITERAL.text, $STRING_LITERAL.line);}
	;

numberConstant returns [NumberConstant n]
	:	DIGITS {$n = new NumberConstant($DIGITS.text, $DIGITS.line);}
	;

DIGITS: ('0'..'9')+;
NAME: ('a'..'z' | 'A'..'Z')('a'..'z' | 'A'..'Z' | '0'..'9')*;
KEYWORD: NAME ':';
STRING_LITERAL: '\'' .* '\'';
BINARY_SYMBOL: ('~'|'!'|'@'|'%'|'&'|'*'|'-'|'+'|'='|'\\'|'|'|'?'|'/'|'>'|'<'|',') ('~'|'!'|'@'|'%'|'&'|'*'|'-'|'+'|'='|'\\'|'|'|'?'|'/'|'>'|'<'|',')*;
CHARACTER: '$' . ;
COMMENT: '"' .* '"' {$channel = HIDDEN;};
WHITESPACE: (' ' | '\t' | '\n' | '\r' | '\f' )+ {$channel = HIDDEN;};

