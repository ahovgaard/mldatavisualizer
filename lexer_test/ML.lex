datatype lexresult =
    EOF
  | ID of string
  | DATATYPE
  | VAL
  | EQUAL
  | ASTERISK
  | BAR
  | LPAREN
  | RPAREN
  | COMMA

val eof = fn () => EOF

%%
ws    = [\ \t\n];
alpha = [A-Za-z];
digit = [0-9];

%%
{ws}+       => (lex());
"datatype"  => (DATATYPE);
"val"       => (VAL);
"="         => (EQUAL);
"*"         => (ASTERISK);
"|"         => (BAR);
"("         => (LPAREN);
")"         => (RPAREN);
","         => (COMMA);
[A-Za-z]+[A-Za-z0-9_]* => (ID yytext);
