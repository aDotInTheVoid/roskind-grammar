%{

    /* Copyright (C) 1989,1990 James A. Roskind, All rights reserved.
    This grammar was developed  and  written  by  James  A.  Roskind. 
    Copying  of  this  grammar  description, as a whole, is permitted 
    providing this notice is intact and applicable  in  all  complete 
    copies.   Translations as a whole to other parser generator input 
    languages  (or  grammar  description  languages)   is   permitted 
    provided  that  this  notice is intact and applicable in all such 
    copies,  along  with  a  disclaimer  that  the  contents  are   a 
    translation.   The reproduction of derived text, such as modified 
    versions of this grammar, or the output of parser generators,  is 
    permitted,  provided  the  resulting  work includes the copyright 
    notice "Portions Copyright (c)  1989,  1990  James  A.  Roskind". 
    Derived products, such as compilers, translators, browsers, etc., 
    that  use  this  grammar,  must also provide the notice "Portions 
    Copyright  (c)  1989,  1990  James  A.  Roskind"  in   a   manner 
    appropriate  to  the  utility,  and in keeping with copyright law 
    (e.g.: EITHER displayed when first invoked/executed; OR displayed 
    continuously on display terminal; OR via placement in the  object 
    code  in  form  readable in a printout, with or near the title of 
    the work, or at the end of the file).  No royalties, licenses  or 
    commissions  of  any  kind are required to copy this grammar, its 
    translations, or derivative products, when the copies are made in 
    compliance with this notice. Persons or corporations that do make 
    copies in compliance with this notice may charge  whatever  price 
    is  agreeable  to  a  buyer, for such copies or derivative works. 
    THIS GRAMMAR IS PROVIDED ``AS IS'' AND  WITHOUT  ANY  EXPRESS  OR 
    IMPLIED  WARRANTIES,  INCLUDING,  WITHOUT LIMITATION, THE IMPLIED 
    WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR 
    PURPOSE.

    James A. Roskind
    Independent Consultant
    516 Latania Palm Drive
    Indialantic FL, 32903
    (407)729-4348
    jar@ileaf.com
    or ...!uunet!leafusa!jar


    ---end of copyright notice---


This file is a companion file to a C++ grammar description file.

*/


/* FILENAME: C.Y */

/*  This  is a grammar file for the dpANSI C language.  This file was 
last modified by J. Roskind on 3/7/90. Version 1.00 */




/* ACKNOWLEDGMENT:

Without the effort expended by the ANSI C standardizing committee,  I 
would  have been lost.  Although the ANSI C standard does not include 
a fully disambiguated syntax description, the committee has at  least 
provided most of the disambiguating rules in narratives.

Several  reviewers  have also recently critiqued this grammar, and/or 
assisted in discussions during it's preparation.  These reviewers are 
certainly not responsible for the errors I have committed  here,  but 
they  are responsible for allowing me to provide fewer errors.  These 
colleagues include: Bruce Blodgett, and Mark Langley. */


%}

/* This refined grammar resolves several typedef ambiguities  in  the 
draft  proposed  ANSI  C  standard  syntax  down  to  1  shift/reduce 
conflict, as reported by a YACC process.  Note  that  the  one  shift 
reduce  conflicts  is the traditional if-if-else conflict that is not 
resolved by the grammar.  This ambiguity can  be  removed  using  the 
method  described in the Dragon Book (2nd edition), but this does not 
appear worth the effort.

There was quite a bit of effort made to reduce the conflicts to  this 
level,  and  an  additional effort was made to make the grammar quite 
similar to the C++ grammar being developed in  parallel.   Note  that 
this grammar resolves the following ANSI C ambiguity as follows:

ANSI  C  section  3.5.6,  "If  the [typedef name] is redeclared at an 
inner scope, the type specifiers shall not be omitted  in  the  inner 
declaration".   Supplying type specifiers prevents consideration of T 
as a typedef name in this grammar.  Failure to supply type specifiers 
forced the use of the TYPEDEFname as a type specifier.
              
ANSI C section 3.5.4.3, "In a parameter declaration, a single typedef 
name in parentheses is  taken  to  be  an  abstract  declarator  that 
specifies  a  function  with  a  single  parameter,  not as redundant 
parentheses around the identifier".  This is extended  to  cover  the 
following cases:

typedef float T;
int noo(const (T[5]));
int moo(const (T(int)));
...

Where  again the '(' immediately to the left of 'T' is interpreted as 
being the start of a parameter type list,  and  not  as  a  redundant 
paren around a redeclaration of T.  Hence an equivalent code fragment 
is:

typedef float T;
int noo(const int identifier1 (T identifier2 [5]));
int moo(const int identifier1 (T identifier2 (int identifier3)));
...

*/




/* Define terminal tokens */


/* keywords */
%token AUTO            DOUBLE          INT             STRUCT
%token BREAK           ELSE            LONG            SWITCH
%token CASE            ENUM            REGISTER        TYPEDEF
%token CHAR            EXTERN          RETURN          UNION
%token CONST           FLOAT           SHORT           UNSIGNED
%token CONTINUE        FOR             SIGNED          VOID
%token DEFAULT         GOTO            SIZEOF          VOLATILE
%token DO              IF              STATIC          WHILE

/* ANSI Grammar suggestions */
%token IDENTIFIER              STRINGliteral
%token FLOATINGconstant        INTEGERconstant        CHARACTERconstant
%token OCTALconstant           HEXconstant

/* New Lexical element, whereas ANSI suggested non-terminal */

%token TYPEDEFname /* Lexer will tell the difference between this and 
    an  identifier!   An  identifier  that is CURRENTLY in scope as a 
    typedef name is provided to the parser as a TYPEDEFname.*/

/* Multi-Character operators */
%token  ARROW            /*    ->                              */
%token  ICR DECR         /*    ++      --                      */
%token  LS RS            /*    <<      >>                      */
%token  LE GE EQ NE      /*    <=      >=      ==      !=      */
%token  ANDAND OROR      /*    &&      ||                      */
%token  ELLIPSIS         /*    ...                             */

/* modifying assignment operators */
%token MULTassign  DIVassign    MODassign   /*   *=      /=      %=      */
%token PLUSassign  MINUSassign              /*   +=      -=              */
%token LSassign    RSassign                 /*   <<=     >>=             */
%token ANDassign   ERassign     ORassign    /*   &=      ^=      |=      */

%start prog.start

%%
prog.start:
        translation.unit
        ;

/* CONSTANTS */
constant:
        FLOATINGconstant
        | INTEGERconstant
        /* We are not including ENUMERATIONconstant here  because  we 
        are  treating  it like a variable with a type of "enumeration 
        constant".  */
        | OCTALconstant
        | HEXconstant
        | CHARACTERconstant
        ;

/* STRING LITERALS */
string.literal.list:
                STRINGliteral
                | string.literal.list STRINGliteral
                ;


/* EXPRESSIONS */
primary.expression:
        IDENTIFIER  /* We cannot use a typedef name as a variable */
        | constant
        | string.literal.list
        | '(' expression ')'
        ;

postfix.expression:
        primary.expression
        | postfix.expression '[' expression ']'
        | postfix.expression '(' ')'
        | postfix.expression '(' argument.expression.list ')'
        | postfix.expression '.' identifier.or.typedef.name
        | postfix.expression ARROW identifier.or.typedef.name
        | postfix.expression ICR
        | postfix.expression DECR
        ;

argument.expression.list:
        assignment.expression
        | argument.expression.list ',' assignment.expression
        ;

unary.expression:
        postfix.expression
        | ICR unary.expression
        | DECR unary.expression
        | unary.operator cast.expression
        | SIZEOF unary.expression
        | SIZEOF '(' type.name ')'
        ;

unary.operator:
        '&'
        | '*'
        | '+'
        | '-'
        | '~'
        | '!'
        ;

cast.expression:
        unary.expression
        | '(' type.name ')' cast.expression
        ;

multiplicative.expression:
        cast.expression
        | multiplicative.expression '*' cast.expression
        | multiplicative.expression '/' cast.expression
        | multiplicative.expression '%' cast.expression
        ;

additive.expression:
        multiplicative.expression
        | additive.expression '+' multiplicative.expression
        | additive.expression '-' multiplicative.expression
        ;

shift.expression:
        additive.expression
        | shift.expression LS additive.expression
        | shift.expression RS additive.expression
        ;

relational.expression:
        shift.expression
        | relational.expression '<' shift.expression
        | relational.expression '>' shift.expression
        | relational.expression LE shift.expression
        | relational.expression GE shift.expression
        ;

equality.expression:
        relational.expression
        | equality.expression EQ relational.expression
        | equality.expression NE relational.expression
        ;

AND.expression:
        equality.expression
        | AND.expression '&' equality.expression
        ;

exclusive.OR.expression:
        AND.expression
        | exclusive.OR.expression '^' AND.expression
        ;

inclusive.OR.expression:
        exclusive.OR.expression
        | inclusive.OR.expression '|' exclusive.OR.expression
        ;

logical.AND.expression:
        inclusive.OR.expression
        | logical.AND.expression ANDAND inclusive.OR.expression
        ;

logical.OR.expression:
        logical.AND.expression
        | logical.OR.expression OROR logical.AND.expression
        ;

conditional.expression:
        logical.OR.expression
        | logical.OR.expression '?' expression ':'
                conditional.expression
        ;

assignment.expression:
        conditional.expression
        | unary.expression assignment.operator assignment.expression
        ;

assignment.operator:
        '='
        | MULTassign
        | DIVassign
        | MODassign
        | PLUSassign
        | MINUSassign
        | LSassign
        | RSassign
        | ANDassign
        | ERassign
        | ORassign
        ;

expression:
        assignment.expression
        | expression ',' assignment.expression
        ;

constant.expression:
        conditional.expression
        ;

    /* The following was used for clarity */
expression.opt:
        /* Nothing */
        | expression
        ;



/* DECLARATIONS */

    /* The following is different from the ANSI C specified  grammar.  
    The  changes  were  made  to  disambiguate  typedef's presence in 
    declaration.specifiers (vs.  in the declarator for redefinition); 
    to allow struct/union/enum tag declarations without  declarators, 
    and  to  better  reflect the parsing of declarations (declarators 
    must be combined with declaration.specifiers ASAP  so  that  they 
    are visible in scope).

    Example  of  typedef  use  as either a declaration.specifier or a 
    declarator:

      typedef int T;
      struct S { T T;}; /* redefinition of T as member name * /

    Example of legal and illegal statements detected by this grammar:

      int; /* syntax error: vacuous declaration * /
      struct S;  /* no error: tag is defined or elaborated * /

    Example of result of proper declaration binding:
        
        int a=sizeof(a); /* note that "a" is declared with a type  in 
            the name space BEFORE parsing the initializer * /

        int b, c[sizeof(b)]; /* Note that the first declarator "b" is 
             declared  with  a  type  BEFORE the second declarator is 
             parsed * /

    */

declaration:
        sue.declaration.specifier ';'
        | sue.type.specifier ';'
        | declaring.list ';'
        | default.declaring.list ';'
        ;

    /* Note that if a typedef were  redeclared,  then  a  declaration 
    specifier must be supplied */

default.declaring.list:  /* Can't  redeclare typedef names */
        declaration.qualifier.list identifier.declarator {} initializer.opt
        | type.qualifier.list identifier.declarator {} initializer.opt
        | default.declaring.list ',' identifier.declarator {} initializer.opt
        ;

declaring.list:
        declaration.specifier declarator {} initializer.opt
        | type.specifier declarator {} initializer.opt
        | declaring.list ',' declarator {} initializer.opt
        ;

declaration.specifier:
        basic.declaration.specifier        /* Arithmetic or void */
        | sue.declaration.specifier          /* struct/union/enum */
        | typedef.declaration.specifier      /* typedef*/
        ;

type.specifier:
        basic.type.specifier                 /* Arithmetic or void */
        | sue.type.specifier                 /* Struct/Union/Enum */
        | typedef.type.specifier             /* Typedef */
        ;


declaration.qualifier.list:  /* const/volatile, AND storage class */
        storage.class
        | type.qualifier.list storage.class
        | declaration.qualifier.list declaration.qualifier
        ;

type.qualifier.list:
        type.qualifier
        | type.qualifier.list type.qualifier
        ;

declaration.qualifier:
        type.qualifier                  /* const or volatile */
        | storage.class
        ;

type.qualifier:
        CONST
        | VOLATILE
        ;

basic.declaration.specifier:      /*StorageClass+Arithmetic or void*/
        basic.type.specifier  storage.class
        | declaration.qualifier.list basic.type.name
        | basic.declaration.specifier declaration.qualifier
        | basic.declaration.specifier basic.type.name
        ;

basic.type.specifier:
        basic.type.name            /* Arithmetic or void */
        | type.qualifier.list basic.type.name
        | basic.type.specifier type.qualifier
        | basic.type.specifier basic.type.name
        ;

sue.declaration.specifier:          /* StorageClass + struct/union/enum */
        sue.type.specifier storage.class
        | declaration.qualifier.list elaborated.type.name
        | sue.declaration.specifier declaration.qualifier
        ;

sue.type.specifier:
        elaborated.type.name              /* struct/union/enum */
        | type.qualifier.list elaborated.type.name
        | sue.type.specifier type.qualifier
        ;


typedef.declaration.specifier:       /*Storage Class + typedef types */
        typedef.type.specifier storage.class
        | declaration.qualifier.list TYPEDEFname
        | typedef.declaration.specifier declaration.qualifier
        ;

typedef.type.specifier:              /* typedef types */
        TYPEDEFname
        | type.qualifier.list TYPEDEFname
        | typedef.type.specifier type.qualifier
        ;

storage.class:
        TYPEDEF
        | EXTERN
        | STATIC
        | AUTO
        | REGISTER
        ;

basic.type.name:
        VOID
        | CHAR
        | SHORT
        | INT
        | LONG
        | FLOAT
        | DOUBLE
        | SIGNED
        | UNSIGNED
        ;

elaborated.type.name:
        struct.or.union.specifier
        | enum.specifier
        ;

struct.or.union.specifier:
        struct.or.union '{' struct.declaration.list '}'
        | struct.or.union identifier.or.typedef.name
                '{' struct.declaration.list '}'
        | struct.or.union identifier.or.typedef.name
        ;

struct.or.union:
        STRUCT
        | UNION
        ;

struct.declaration.list:
        struct.declaration
        | struct.declaration.list struct.declaration
        ;

struct.declaration:
        struct.declaring.list ';'
        | struct.default.declaring.list ';'
        ;

struct.default.declaring.list:        /* doesn't redeclare typedef*/
        type.qualifier.list struct.identifier.declarator
        | struct.default.declaring.list ',' struct.identifier.declarator
        ;

struct.declaring.list:        
        type.specifier struct.declarator
        | struct.declaring.list ',' struct.declarator
        ;


struct.declarator:
        declarator bit.field.size.opt
        | bit.field.size
        ;

struct.identifier.declarator:
        identifier.declarator bit.field.size.opt
        | bit.field.size
        ;

bit.field.size.opt:
        /* nothing */
        | bit.field.size
        ;

bit.field.size:
        ':' constant.expression
        ;

enum.specifier:
        ENUM '{' enumerator.list '}'
        | ENUM identifier.or.typedef.name '{' enumerator.list '}'
        | ENUM identifier.or.typedef.name
        ;



enumerator.list:
        identifier.or.typedef.name enumerator.value.opt
        | enumerator.list ',' identifier.or.typedef.name enumerator.value.opt
        ;

enumerator.value.opt:
        /* Nothing */
        | '=' constant.expression
        ;

parameter.type.list:
        parameter.list
        | parameter.list ',' ELLIPSIS
        ;

parameter.list:
        parameter.declaration
        | parameter.list ',' parameter.declaration
        ;

parameter.declaration:
        declaration.specifier
        | declaration.specifier abstract.declarator
        | declaration.specifier identifier.declarator
        | declaration.specifier parameter.typedef.declarator
        | declaration.qualifier.list 
        | declaration.qualifier.list abstract.declarator
        | declaration.qualifier.list identifier.declarator
        | type.specifier
        | type.specifier abstract.declarator
        | type.specifier identifier.declarator
        | type.specifier parameter.typedef.declarator
        | type.qualifier.list 
        | type.qualifier.list abstract.declarator
        | type.qualifier.list identifier.declarator
        ;

    /*  ANSI  C  section  3.7.1  states  "An identifier declared as a 
    typedef name shall not be redeclared as a parameter".  Hence  the 
    following is based only on IDENTIFIERs */

identifier.list:
        IDENTIFIER
        | identifier.list ',' IDENTIFIER
        ;

identifier.or.typedef.name:
        IDENTIFIER
        | TYPEDEFname
        ;

type.name:
        type.specifier
        | type.specifier abstract.declarator
        | type.qualifier.list 
        | type.qualifier.list abstract.declarator
        ;

initializer.opt:
        /* nothing */
        | '=' initializer
        ;

initializer:
        '{' initializer.list '}'
        | '{' initializer.list ',' '}'
        | assignment.expression
        ;

initializer.list:
        initializer
        | initializer.list ',' initializer
        ;


/* STATEMENTS */
statement:
        labeled.statement
        | compound.statement
        | expression.statement
        | selection.statement
        | iteration.statement
        | jump.statement
        ;

labeled.statement:
        identifier.or.typedef.name ':' statement
        | CASE constant.expression ':' statement
        | DEFAULT ':' statement
        ;

compound.statement:
        '{' '}'
        | '{' declaration.list '}'
        | '{' statement.list '}'
        | '{' declaration.list statement.list '}'
        ;

declaration.list:
        declaration
        | declaration.list declaration
        ;

statement.list:
        statement
        | statement.list statement
        ;

expression.statement:
        expression.opt ';'
        ;

selection.statement:
        IF '(' expression ')' statement
        | IF '(' expression ')' statement ELSE statement
        | SWITCH '(' expression ')' statement
        ;

iteration.statement:
        WHILE '(' expression ')' statement
        | DO statement WHILE '(' expression ')' ';'
        | FOR '(' expression.opt ';' expression.opt ';'
                expression.opt ')' statement
        ;

jump.statement:
        GOTO identifier.or.typedef.name ';'
        | CONTINUE ';'
        | BREAK ';'
        | RETURN expression.opt ';'
        ;


/* EXTERNAL DEFINITIONS */

translation.unit:
        external.definition
        | translation.unit external.definition
        ;

external.definition:
        function.definition
        | declaration
        ;

function.definition:
                                     identifier.declarator compound.statement
        | declaration.specifier      identifier.declarator compound.statement
        | type.specifier             identifier.declarator compound.statement
        | declaration.qualifier.list identifier.declarator compound.statement
        | type.qualifier.list        identifier.declarator compound.statement

        |                            old.function.declarator compound.statement 
        | declaration.specifier      old.function.declarator compound.statement
        | type.specifier             old.function.declarator compound.statement
        | declaration.qualifier.list old.function.declarator compound.statement
        | type.qualifier.list        old.function.declarator compound.statement

        |                            old.function.declarator declaration.list 
                compound.statement
        | declaration.specifier      old.function.declarator declaration.list
                compound.statement
        | type.specifier             old.function.declarator declaration.list
                compound.statement
        | declaration.qualifier.list old.function.declarator declaration.list
                compound.statement
        | type.qualifier.list        old.function.declarator declaration.list
                compound.statement
        ;

declarator:
        typedef.declarator
        | identifier.declarator
        ;

typedef.declarator:
        paren.typedef.declarator  /* would be ambiguous as parameter*/
        | parameter.typedef.declarator   /* not ambiguous as param*/
        ;

parameter.typedef.declarator:
        TYPEDEFname 
        | TYPEDEFname postfixing.abstract.declarator
        | clean.typedef.declarator
        ;

    /*  The  following have at least one '*'. There is no (redundant) 
    '(' between the '*' and the TYPEDEFname. */

clean.typedef.declarator:
        clean.postfix.typedef.declarator
        | '*' parameter.typedef.declarator
        | '*' type.qualifier.list parameter.typedef.declarator  
        ;

clean.postfix.typedef.declarator:
        '(' clean.typedef.declarator ')'
        | '(' clean.typedef.declarator ')' postfixing.abstract.declarator
        ;

    /* The following have a redundant '(' placed immediately  to  the 
    left of the TYPEDEFname */

paren.typedef.declarator:
        paren.postfix.typedef.declarator
        | '*' '(' simple.paren.typedef.declarator ')' /* redundant paren */
        | '*' type.qualifier.list  
                '(' simple.paren.typedef.declarator ')' /* redundant paren */
        | '*' paren.typedef.declarator
        | '*' type.qualifier.list paren.typedef.declarator
        ;
        
paren.postfix.typedef.declarator: /* redundant paren to left of tname*/
        '(' paren.typedef.declarator ')'
        | '(' simple.paren.typedef.declarator postfixing.abstract.declarator ')' /* redundant paren */
        | '(' paren.typedef.declarator ')' postfixing.abstract.declarator
        ;

simple.paren.typedef.declarator:
        TYPEDEFname
        | '(' simple.paren.typedef.declarator ')'
        ;

identifier.declarator:
        unary.identifier.declarator
        | paren.identifier.declarator
        ;

unary.identifier.declarator:
        postfix.identifier.declarator
        | '*' identifier.declarator
        | '*' type.qualifier.list identifier.declarator
        ;
        
postfix.identifier.declarator:
        paren.identifier.declarator postfixing.abstract.declarator
        | '(' unary.identifier.declarator ')'
        | '(' unary.identifier.declarator ')' postfixing.abstract.declarator
        ;

paren.identifier.declarator:
        IDENTIFIER
        | '(' paren.identifier.declarator ')'
        ;

old.function.declarator:
        postfix.old.function.declarator
        | '*' old.function.declarator
        | '*' type.qualifier.list old.function.declarator
        ;

postfix.old.function.declarator:
        paren.identifier.declarator '(' identifier.list ')'
        | '(' old.function.declarator ')'
        | '(' old.function.declarator ')' postfixing.abstract.declarator
        ;

abstract.declarator:
        unary.abstract.declarator
        | postfix.abstract.declarator
        | postfixing.abstract.declarator
        ;

postfixing.abstract.declarator:
        array.abstract.declarator
        | '(' ')'
        | '(' parameter.type.list ')'
        ;

array.abstract.declarator:
        '[' ']'
        | '[' constant.expression ']'
        | array.abstract.declarator '[' constant.expression ']'
        ;

unary.abstract.declarator:
        '*' 
        | '*' type.qualifier.list 
        | '*' abstract.declarator
        | '*' type.qualifier.list abstract.declarator
        ;

postfix.abstract.declarator:
        '(' unary.abstract.declarator ')'
        | '(' postfix.abstract.declarator ')'
        | '(' postfixing.abstract.declarator ')'
        | '(' unary.abstract.declarator ')' postfixing.abstract.declarator
        ;

%%
/* ----end of grammar----*/


