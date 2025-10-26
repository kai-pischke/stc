%{
open Ast
%}

%token <string> IDENT
%token <int> EINT
%token END REC MAIN IF THEN ELSE
%token ARROW QUEST BANG
%token LBRACK RBRACK
%token LBRACE RBRACE COLON COLONCOLON COMMA BAR DOT SEMI
%token LPAREN RPAREN
%token ETRUE EFALSE NOT OR AND
%token PLUS MINUS TIMES DIV MOD
%token LT GT LE GE EQ
%token EOF

/* Entry points that must hit end-of-file */
%start <string Ast.global> gfile
%start <string Ast.local > lfile
%start <string Ast.processes > pfile
%start <string Ast.program> program_file

/* Operator precedence and associativity for expressions */
%left OR
%left AND
%left EQ
%left LT GT LE GE
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT

%%

ident:
  IDENT                                   { $1 }

/* ───── global types ───── */
gfile:
  global_type EOF                         { $1 }

global_type:
  END                                     { GEnd Loc.dummy }
| ident                                   { GVar ($1, Loc.dummy) }
| REC ident DOT global_type               { GRec ($2, $4, Loc.dummy) }
| ident ARROW ident LBRACE g_lab RBRACE   { GBra ($1, $3, $5, Loc.dummy) }
| global_type BAR global_type             { GPar ($1, $3, Loc.dummy) }
| ident ARROW ident COLON LBRACK ident RBRACK SEMI global_type { GMsg ($1, $3, $6, $9, Loc.dummy) }
| LPAREN global_type RPAREN               { $2 }

g_lab:
  separated_nonempty_list(COMMA, g_pair)  { $1 }

g_pair:
  ident COLON global_type                 { ($1, $3) }

/* ───── local types ───── */
lfile:
  local_type EOF                          { $1 }

local_type:
  END                                     { LEnd Loc.dummy }
| ident                                   { LVar ($1, Loc.dummy) }
| REC ident DOT local_type                { LRec ($2, $4, Loc.dummy) }
| ident QUEST LBRACE l_lab RBRACE         { LExt ($1, $4, Loc.dummy) }
| ident BANG  LBRACE l_lab RBRACE         { LInt ($1, $4, Loc.dummy) }
| ident QUEST LBRACK ident RBRACK SEMI local_type { LRecv ($1, $4, $7, Loc.dummy) }
| ident BANG  LBRACK ident RBRACK SEMI local_type { LSend ($1, $4, $7, Loc.dummy) }
| LPAREN local_type RPAREN                { $2 }

l_lab:
  separated_nonempty_list(COMMA, l_pair)  { $1 }

l_pair:
  ident COLON local_type                  { ($1, $3) }

/* ───── processes ───── */
pfile:
  processes EOF                           { $1 }

expression:
  | ETRUE                       { ETrue Loc.dummy }
  | EFALSE                      { EFalse Loc.dummy }
  | EINT                        { EInt ($1, Loc.dummy) }
  | ident                       { EVar ($1, Loc.dummy) }
  | NOT expression              { ENot ($2, Loc.dummy) }
  | expression OR expression    { EOr ($1, $3, Loc.dummy) }
  | expression AND expression   { EAnd ($1, $3, Loc.dummy) }
  | expression PLUS expression  { EPlus ($1, $3, Loc.dummy) }
  | expression MINUS expression { EMinus ($1, $3, Loc.dummy) }
  | expression TIMES expression { ETimes ($1, $3, Loc.dummy) }
  | expression DIV expression   { EDiv ($1, $3, Loc.dummy) }
  | expression MOD expression   { EMod ($1, $3, Loc.dummy) }
  | expression LT expression    { ELt ($1, $3, Loc.dummy) }
  | expression GT expression    { EGt ($1, $3, Loc.dummy) }
  | expression LE expression    { ELe ($1, $3, Loc.dummy) }
  | expression GE expression    { EGe ($1, $3, Loc.dummy) }
  | expression EQ expression    { EEq ($1, $3, Loc.dummy) }
  | LPAREN expression RPAREN    { $2 }

processes:
  | EINT                                          { if $1 = 0 then PInact Loc.dummy 
                                                     else failwith "Invalid process literal" }
  | ident                                         { PVar ($1, Loc.dummy) }
  | REC ident DOT processes                       { PRec ($2, $4, Loc.dummy) }
  | ident BANG LBRACE p_lab RBRACE                { PInt ($1, $4, Loc.dummy) }
  | ident QUEST LBRACE p_lab RBRACE               { PExt ($1, $4, Loc.dummy) }
  | ident BANG LBRACK expression RBRACK DOT processes  { PSend ($1, $4, $7, Loc.dummy) }
  | ident QUEST LPAREN ident RPAREN DOT processes      { PRecv ($1, $4, $7, Loc.dummy) }
  | IF expression THEN processes ELSE processes   { PIfThenElse ($2, $4, $6, Loc.dummy) }
  | LPAREN processes RPAREN                       { $2 }

p_lab:
  separated_nonempty_list(COMMA, p_pair)  { $1 }

p_pair:
  ident COLON processes                   { ($1, $3) }

/* ───── programs with definitions ───── */
program_file:
  process_defs main_def EOF               { { definitions = $1; main = $2; loc = Loc.dummy } }

process_defs:
  /* empty */                             { [] }
| process_def process_defs                { $1 :: $2 }

process_def:
  ident EQ processes                      { { name = $1; body = $3; loc = Loc.dummy } }

main_def:
  MAIN EQ tagged_comp                     { $3 }

tagged_comp:
  separated_nonempty_list(BAR, tagged_proc) { $1 }

tagged_proc:
  ident COLONCOLON ident                  { { participant = $1; process_ref = $3; loc = Loc.dummy } }
