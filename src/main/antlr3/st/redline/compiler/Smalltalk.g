/* Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution */
grammar Smalltalk;

options {
  language = Java;
  backtrack = false;
}
@header {
  package st.redline.compiler;
  import org.antlr.runtime.Parser;
  import st.redline.compiler.ast.*;
  import st.redline.compiler.ast.Number;
}
@lexer::header {
  package st.redline.compiler;
}
@lexer::members {
  List<RecognitionException> exceptions = new ArrayList<RecognitionException>();
  public List<RecognitionException> getExceptions() { return exceptions; }
  public void reportError(RecognitionException e) { super.reportError(e); exceptions.add(e); }
}
@parser::members {
  ReferencedClasses referencedClasses = new ReferencedClasses();
  public ReferencedClasses getReferencedClasses() { return referencedClasses; }
  public void addReferencedClass(String cls) { referencedClasses.add(new ReferencedClass(cls)); }
}

program returns [Program program]
  : temporaries? statements WHITESPACE?  EOF {program = new Program(new Temporaries($temporaries.temporaries), $statements.statements, getReferencedClasses());}
  ;	

primary returns [Primary primary]
  : WHITESPACE? 
    ( IDENTIFIER {primary = new Identifier($IDENTIFIER.text, $IDENTIFIER.line); addReferencedClass($IDENTIFIER.text); }
    | number {primary = $number.number;}
    | symbol_constant {primary = $symbol_constant.symbolConstant;}
    | CHARACTER_CONSTANT {primary = new CharacterConstant($CHARACTER_CONSTANT.text.substring(1), $CHARACTER_CONSTANT.line);}
    | STRING {primary = new StringConstant($STRING.text, $STRING.line);}
    | array_constant {primary = $array_constant.arrayConstant; }
    | block {primary = $block.block;}
    | '(' expression WHITESPACE? ')' {primary = new PrimaryExpression($expression.expression);}
    | '{' statements WHITESPACE? '}' {primary = new PrimaryStatements($statements.statements);}
    | '<' KEYWORD WHITESPACE DIGITS '>' {primary = new Primitive($KEYWORD.text, $KEYWORD.line, $DIGITS.text);}
    )
  ;

statements returns [Statements statements]
  : non_empty_statements? { statements = $non_empty_statements.statements; }
  ;

non_empty_statements returns [Statements statements]
  : WHITESPACE? a='^'  expression '.'? {statements = new AnswerStatement($a.line, $expression.expression);}
  | expression ( '.' WHITESPACE statements )? {statements = new Statements($expression.expression, $statements.statements);}
  ;

expression returns [Expression expression]
  :  WHITESPACE? IDENTIFIER WHITESPACE? ':=' e=expression {expression = new AssignmentExpression(new Identifier($IDENTIFIER.text, $IDENTIFIER.line), $e.expression);}
  | simple_expression {expression = $simple_expression.simpleExpression;}
  ;

simple_expression returns [SimpleExpression simpleExpression]
  @init { simpleExpression = new SimpleExpression(); }
  : primary {simpleExpression.add($primary.primary);} ( message_expression {$simpleExpression.add($message_expression.messageExpression);} 
    ( ';' WHITESPACE message_element {$simpleExpression.add($message_element.messageElement);} )* )?
  ;

message_element returns [MessageElement messageElement]
  : IDENTIFIER {$messageElement = new UnarySelectorMessageElement(new UnarySelector($IDENTIFIER.text, $IDENTIFIER.line));}
    | binary_selector unary_object_description {$messageElement = new BinarySelectorMessageElement($binary_selector.binarySelector, $unary_object_description.unaryObjectDescription);}
    | (KEYWORD binary_object_description  {
           if ($messageElement != null) { 
               ((KeywordMessageElement)$messageElement).add($KEYWORD.text, $KEYWORD.line, $binary_object_description.binaryObjectDescription);
          }  else { 
              $messageElement = new KeywordMessageElement($KEYWORD.text, $KEYWORD.line, $binary_object_description.binaryObjectDescription);
          }
      } WHITESPACE? )+
  ;

message_expression returns [MessageExpression messageExpression]
  : unary_expression {$messageExpression = $unary_expression.unaryExpression;}
  | binary_expression {$messageExpression = $binary_expression.binaryExpression;}
  | keyword_expression {$messageExpression = $keyword_expression.keywordExpression;}
  ;

unary_expression returns [UnaryExpression unaryExpression]
  @init { unaryExpression = new UnaryExpression(); }
  : (WHITESPACE IDENTIFIER {$unaryExpression.add(new UnarySelector($IDENTIFIER.text, $IDENTIFIER.line));} )+
    (binary_expression {$unaryExpression.add($binary_expression.binaryExpression);} | keyword_expression {$unaryExpression.add($keyword_expression.keywordExpression);})?
  ;

binary_expression returns [BinaryExpression binaryExpression]
  @init { binaryExpression = new BinaryExpression(); }
  : (WHITESPACE binary_selector unary_object_description {$binaryExpression.add($binary_selector.binarySelector, $unary_object_description.unaryObjectDescription);} )+ 
    (keyword_expression {$binaryExpression.add($keyword_expression.keywordExpression);})?
  ;

keyword_expression returns [KeywordExpression keywordExpression]
  @init { keywordExpression = new KeywordExpression(); }
  : (WHITESPACE KEYWORD binary_object_description {$keywordExpression.add($KEYWORD.text, $KEYWORD.line, $binary_object_description.binaryObjectDescription);} )+
  ;

unary_object_description returns [UnaryObjectDescription unaryObjectDescription]
  : primary {$unaryObjectDescription = new UnaryObjectDescription($primary.primary);} (WHITESPACE IDENTIFIER {$unaryObjectDescription.add(new UnarySelector($IDENTIFIER.text, $IDENTIFIER.line));})*
  ;

binary_object_description returns [BinaryObjectDescription binaryObjectDescription]
: primary {$binaryObjectDescription = new BinaryObjectDescription($primary.primary);} (WHITESPACE IDENTIFIER {$binaryObjectDescription.add(new UnarySelector($IDENTIFIER.text, $IDENTIFIER.line));})*
 (WHITESPACE binary_selector unary_object_description {$binaryObjectDescription.add($binary_selector.binarySelector, $unary_object_description.unaryObjectDescription);})*
;

block returns [Block block]
  : o= '[' block_arguments? temporaries? statements WHITESPACE? ']' {block = new Block($o.line, new BlockArguments($block_arguments.blockArguments), new Temporaries($temporaries.temporaries), $statements.statements);}
  ;

temporaries returns [List<Temporary> temporaries]
  @init { temporaries = new ArrayList<Temporary>(); }
  :  WHITESPACE? '|'  (WHITESPACE? IDENTIFIER {temporaries.add(new Temporary($IDENTIFIER.text, $IDENTIFIER.line));})+ WHITESPACE? '|'
  ;

block_arguments returns [List<BlockArgument> blockArguments]
  @init { blockArguments = new ArrayList<BlockArgument>(); }
  : WHITESPACE? (':' IDENTIFIER WHITESPACE? {blockArguments.add(new BlockArgument($IDENTIFIER.text, $IDENTIFIER.line));})+  '|'
  ;

array_constant returns [ArrayConstant arrayConstant]
  : h='#' array {arrayConstant = new ArrayConstant($array.array, $h.line);}
  ;

array returns [Array array]
  @init { array = new Array(); }
  : '(' (WHITESPACE? array_element {array.add($array_element.arrayElement);})* WHITESPACE? ')'
  ;

array_element returns [ArrayElement arrayElement]
  : number {arrayElement = $number.number;}
  | symbol {arrayElement = $symbol.symbol;}
  | STRING {arrayElement = new StringConstant($STRING.text, $STRING.line);}
  | CHARACTER_CONSTANT {arrayElement = new CharacterConstant($CHARACTER_CONSTANT.text.substring(1), $CHARACTER_CONSTANT.line);}
  | array {arrayElement = $array.array;}
  ;

symbol_constant returns [SymbolConstant symbolConstant]
  : '#' symbol {symbolConstant = new SymbolConstant($symbol.symbol.value(), $symbol.symbol.line());}
  ;

symbol returns [Symbol symbol]
  @init { symbol = new Symbol(); }
  :  binary_selector {symbol.valueAndLine($binary_selector.binarySelector.value(), $binary_selector.binarySelector.line());}
  | IDENTIFIER {symbol.valueAndLine($IDENTIFIER.text, $IDENTIFIER.line); }
  | (KEYWORD {symbol.addValueAndLine($KEYWORD.text, $KEYWORD.line);} )+    // Decision can match input such as "KEYWORD" using multiple alternatives: 1, 2
  ;

binary_selector returns [BinarySelector binarySelector]
  @init { binarySelector = new BinarySelector(); }
  : (s=('-'|'+'|'/'|'\\'|'*'|'~'|'<'|'>'|'='|'@'|'%'|'|'|'&'|'?'|'!'|',')  {binarySelector.add($s.text,$s.line);} )+
  ;

number returns [Number number]
  : (d1=DIGITS r='r')? (m1='-')? d2=DIGITS ('.' d3=DIGITS)? (e='e' (m2='-')? d4=DIGITS)? {number = new Number($d1.text, $r.text, $m1.text, $d2.text, $d2.line,$d3.text, $e.text, $m2.text, $d4.text);}
  ;

WHITESPACE:		(' '|'\t'|'\r'|'\n')+;
COMMENT:		'"' .* '"' WHITESPACE {$channel = HIDDEN;};
KEYWORD:		IDENTIFIER ':';
IDENTIFIER:		LETTER (LETTER | DIGIT | '_')*;
CHARACTER_CONSTANT:	'$' ('\'' | '"' | SPECIAL_CHAR | NORMAL_CHAR | DIGIT | LETTER);
STRING:		'\'' (~'\''|'\'\'')* '\'';
DIGITS:		DIGIT+;

fragment LETTER:		('a'..'z' | 'A'..'Z');
fragment DIGIT:		'0'..'9';
fragment SPECIAL_CHAR:		'+'|'/'|'\\'|'*'|'~'|'<'|'>'|'='|'@'|'%'|'|'|'&'|'?'|'!'|',';
fragment NORMAL_CHAR:		'['|']'|'{'|'}'|'('|')'|'^'|'_'|';'|'$'|'#'|':'|'.'|'\'';
