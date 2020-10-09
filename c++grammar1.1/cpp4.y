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

MOTIVATION-

My  goal  is  to  see  software  developers  adopt  this grammar as a
standard until such time as a better  standard  is  accessible.   The
only  way  to  get it to become a standard, is to be sure that people
know that derivations are based on a specific work.   The  intent  of
releasing  this  grammar is to provide a publicly accessible standard
grammar for C++.  The intent of the  copyright  notice  is  to  allow
arbitrary  commercial  and non-commercial use of the grammar, as long
as reference is given to the original standard.  Without reference to
a specific standard, many alternative  grammars  would  develop.   By
referring  to  the  standard,  this grammar is given publicity, which
should lead to further use in compatible products and  systems.   The
benefits  of  such  a  standard  to  commercial  products  (browsers,
beautifiers, translators, compilers, ...) should be  obvious  to  the
developers,  in  that  other compatible products will emerge, and the
value of all conforming products  will  rise.   Most  developers  are
aware  of  the  value  of  acquiring  a fairly complete grammar for a
language, and the copyright notice  (and  the  resulting  affiliation
with my work) should not be too high a price to pay.  By copyrighting
this  grammar,  I have some minor control over what this standard is,
and I can (hopefully) keep it from degrading without my approval.   I
will  consistently  attempt  to  provide  upgraded  grammars that are
compliant  with  the  current  art,  and  the  ANSI   C++   Committee
recommendation  in  particular.   A developer is never prevented from
modifying the grammar to improve it in  whatever  way  is  seen  fit.
There  is  also  no  restriction on the sale of copies, or derivative
works, providing the request in the copyright notice are satisfied.

If you are not "copying" my work, but  are  rather  only  abstracting
some  of  the  standard,  an acknowledgment with references to such a
standard would be appreciated.  Specifically,  agreements  with  this
standard  as  to  the  resolution  of otherwise ambiguous constructs,
should be noted.

Simply put: "make whatever use you would like  of  the  grammar,  but
include  the  ``portions  Copyright  ...''  as  a  reference  to this
standard."


*/

/* File CPP4.Y is translated by YACC to y.tab.c
                                Version 1.1, last modified 5/25/90 */

/* ACKNOWLEDGMENT: Without Bjarne Stroustrup and his many  co-workers 
at  Bell  Labs, there would be no C++ Language for which to provide a 
syntax description. Bjarne has also been especially helpful and  open 
in  discussions,  and  by  permitting me to review his texts prior to 
their publication, allowed me a wonderful vantage point of clarity.

Without the effort expended by the ANSI C standardizing committee,  I 
would  have been lost.  Although the ANSI C standard does not include 
a fully disambiguated syntax description, the committee has at  least 
provided  most  of  the disambiguating rules in narratives.  This C++ 
grammar is intended to be a superset of an ANSI C compatible  grammar 
that is provided in an related file.

Several  reviewers  have  also  recently  critiqued this grammar, the 
related C  grammar,  and  or  assisted  in  discussions  during  it's 
preparation.   These  reviewers are certainly not responsible for the 
errors I have committed here, but they are responsible  for  allowing 
me   to  provide  fewer  errors.   These  colleagues  include:  Bruce 
Blodgett, Mark Langley, Joe Fialli, Greg Perkins, and Ron Guilmette.
*/ 

%}

/* Interesting ambiguity:
Usually
        typename ( typename2 ) ...
or
        typename ( typename2 [4] ) ...
etc.
is a redeclaration of typename2.

Inside a structure elaboration, it is sometimes the declaration of  a 
constructor!   Note,  this  only  counts  if  typename IS the current 
containing class name. (Note this can't conflict with ANSI C  because 
ANSI  C  would  call  it a redefinition, but claim it is semantically 
illegal because you can't have a member declared the same type as the 
containing struct!) Since the ambiguity is only reached when a ';' is 
found,  there  is  no  problem  with  the  fact  that  the   semantic 
interpretation  is  providing  the  true  resolution.   As  currently 
implemented, the constructor semantic actions must be able to process 
an ordinary declaration.  I may reverse this in the future,  to  ease 
semantic implementation. 
*/



/*

INTRO TO ANSI C GRAMMAR (provided in a separate file):

The refined grammar resolves several typedef ambiguities in the draft 
proposed  ANSI  C standard syntax down to 1 shift/reduce conflict, as 
reported by a YACC process.  Note that the one shift reduce conflicts 
is the traditional if-if-else conflict that is not  resolved  by  the 
grammar.  This ambiguity can be removed using the method described in 
the  Dragon  Book  (2nd  edition), but this does not appear worth the 
effort.

There was quite a bit of effort made to reduce the conflicts to  this 
level,  and  an  additional effort was made to make the grammar quite 
similar to the C++ grammar being developed in  parallel.   Note  that 
this grammar resolves the following ANSI C ambiguities:

ANSI  C  section  3.5.6,  "If  the [typedef name] is redeclared at an 
inner scope, the type specifiers shall not be omitted  in  the  inner 
declaration".   Supplying type specifiers prevents consideration of T 
as a typedef name in this grammar.  Failure to supply type specifiers 
forced the use of the TYPEDEFname as a type specifier.  This is taken 
to an (unnecessary) extreme by this implementation.  The ambiguity is 
only a problem with the first declarator in  a  declaration,  but  we 
restrict   ALL   declarators  whenever  the  users  fails  to  use  a 
type.specifier.
              
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


%{
/*************** Includes and Defines *****************************/
#define YYDEBUG_LEXER_TEXT (yylval) /* our lexer loads this up each time */
#define YYDEBUG 1        /* Force the pretty debugging code to compile*/
#define YYSTYPE  char *  /* interface with flex: should be in header file */
/*************** Standard ytab.c continues here *********************/
%}

/*************************************************************************/



%token AUTO            DOUBLE          INT             STRUCT
%token BREAK           ELSE            LONG            SWITCH
%token CASE            ENUM            REGISTER        TYPEDEF
%token CHAR            EXTERN          RETURN          UNION
%token CONST           FLOAT           SHORT           UNSIGNED
%token CONTINUE        FOR             SIGNED          VOID
%token DEFAULT         GOTO            SIZEOF          VOLATILE
%token DO              IF              STATIC          WHILE

/* The following are used in C++ only.  ANSI C would call these IDENTIFIERs */
%token NEW             DELETE
%token THIS            
%token OPERATOR
%token CLASS           
%token PUBLIC          PROTECTED       PRIVATE
%token VIRTUAL         FRIEND
%token INLINE          OVERLOAD        

/* ANSI C Grammar suggestions */
%token IDENTIFIER              STRINGliteral
%token FLOATINGconstant        INTEGERconstant        CHARACTERconstant
%token OCTALconstant           HEXconstant

/* New Lexical element, whereas ANSI C suggested non-terminal */
%token TYPEDEFname 

/* Multi-Character operators */
%token  ARROW            /*    ->                              */
%token  ICR DECR         /*    ++      --                      */
%token  LS RS            /*    <<      >>                      */
%token  LE GE EQ NE      /*    <=      >=      ==      !=      */
%token  ANDAND OROR      /*    &&      ||                      */
%token  ELLIPSIS         /*    ...                             */
                 /* Following are used in C++, not ANSI C        */
%token  CLCL             /*    ::                              */
%token  DOTstar ARROWstar/*    .*       ->*                    */
 
/* modifying assignment operators */
%token MULTassign  DIVassign    MODassign   /*   *=      /=      %=      */
%token PLUSassign  MINUSassign              /*   +=      -=              */
%token LSassign    RSassign                 /*   <<=     >>=             */
%token ANDassign   ERassign     ORassign    /*   &=      ^=      |=      */


/*************************************************************************/

%start prog.start

/*************************************************************************/

%%
prog.start:
        /*nothing*/
        | translation.unit
        ;

/* CONSTANTS */
constant:
        FLOATINGconstant
        | INTEGERconstant
        /*  We are not including ENUMERATIONconstant here because we
          are treating it like a variable with a type of "enumeration
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

    /* The following can be used in an identifier  based  declarator.  
    (Declarators   that  redefine  an  existing  TYPEDEFname  require 
    special handling, and are not included here).  In  addition,  the 
    following are all valid "identifiers" in an expression, whereas a 
    TYPEDEFname is NOT.*/
    
rescoped.identifier:
        IDENTIFIER  /* ANSI C: We cannot use a TYPEDEFname as a variable */
        | operator.function.name /* C++, not ANSI C*/
        | class.rescoped.identifier
        ;


    /* When nested types are supported, an action must be taken prior 
    to  the  CLCL  in each of the following productions to notify the 
    lexer of a temporary change in scope. This change will allow  the 
    lexer to deduce whether the name that follows the CLCL is that of 
    an  IDENTIFIER  (member),  or  of a TYPEDEFname (locally declared 
    type).  This lexical distinction is critical to continued  syntax 
    analysis. */

class.rescoped.identifier:
        TYPEDEFname CLCL identifier.or.typedef.name /* C++, not ANSI C */
        | TYPEDEFname CLCL operator.function.name /* C++, not ANSI C */
        | TYPEDEFname CLCL '~' TYPEDEFname /* C++, not ANSI C */
        | TYPEDEFname CLCL class.rescoped.identifier /* C++, not ANSI C */
        ;

    /* Note that the following MUST come before primary.expression so 
    that  the  reduce/reduce conflict is properly resolved (according 
    to Stroustrup: "If it can be a declaration, then it  should  be".  
    I  just  give  up  and  go  in  that  direction  after  I see the 
    declarator, whereas cfront tries to lex  ahead  to  disambiguate.  
    My reduce-reduce conflict disambiguates this situation at a point 
    where human readers cannot generally disambiguate (mentally). */
    
paren.identifier.declarator:
        rescoped.identifier
        | '(' paren.identifier.declarator ')'
        ;

    /*  Note that CLCL IDENTIFIER is NOT part of rescoped.identifier. 
    It is ONLY valid for referring to an identifier,  and  NOT  valid 
    for  declaring  (or  importing  an  external  declaration  of) an 
    identifier.  This disambiguates the following code,  which  would 
    otherwise be syntactically and semantically ambiguous:

            class base {
                static int i; // element i;
                float member_funct(void);
                };
            base i; // global i
            float base::member_function(void) {
                i; // refers to static int element "i" of base
                ::i; // refers to global "i", with type "base"
                    {
                    base :: i; // import of global "i", like "base (::i);"?
                                // OR reference to global??
                    }
                }
        */
primary.expression:
        CLCL identifier.or.typedef.name  /* C++, not ANSI C */
        | CLCL operator.function.name /* C++, not ANSI C */
        | THIS   /* C++, not ANSI C */
        | rescoped.identifier
        | constant
        | string.literal.list
        | '(' expression ')'
        ;

    /*  The  following  introduces  MANY  conflicts.   Requiring  and 
    allowing '(' ')' around the `type' when the type is complex would 
    help a lot. */

operator.function.name:
        OPERATOR any.operator
        | OPERATOR type.specifier.or.name  operator.function.ptr.opt
        | OPERATOR type.qualifier.list     operator.function.ptr.opt
        ;

    /* The following causes several ambiguities on *  and  &.   These 
    conflicts  would also be removed if parens around the `type' were 
    required in the derivations for operator.function.name */

    /*  Interesting  aside:  The  use  of  right  recursion  in   the 
    production  for  operator.function.ptr.opt gives both the correct 
    parsing, AND removes a conflict!   Right  recursion  permits  the 
    parser   to   defer   reductions  (aka:  delay  resolution),  and 
    effectively make a second pass! */
        
operator.function.ptr.opt:
        /* nothing */
        | pointer.operator operator.function.ptr.opt
        | indirect.or.reference operator.function.ptr.opt
        ;

any.operator:
        '+'
        | '-'
        | '*'
        | '/'
        | '%'
        | '^'
        | '&'
        | '|'
        | '~'
        | '!'
        | '<'
        | '>'
        | LS
        | RS
        | ANDAND
        | OROR
        | ARROW
        | ARROWstar
        | '.' /* can we overload this? */
        | DOTstar
        | ICR
        | DECR
        | LE
        | GE
        | EQ
        | NE
        | assignment.operator
        | '(' ')'
        | '[' ']'
        | NEW
        | DELETE
        | ','
        ;

    /* The following production for type.qualifier.list was specially 
    placed BEFORE the definition of postfix.expression to  resolve  a 
    reduce-reduce conflict correctly */        

type.qualifier.list.opt:
        /* Nothing */
        | type.qualifier.list
        ;

    /*  Note  that  the next set of productions in this grammar gives 
    post-increment a higher precedence that pre-increment.   This  is 
    not  clearly  stated  in  the  C++  Reference manual, and is only 
    implied by the grammar in the ANSI C Standard. */

postfix.expression:
        primary.expression
        | postfix.expression '[' expression ']'
        | postfix.expression '(' ')'
        | postfix.expression '(' argument.expression.list ')'
        | postfix.expression '.' rescoped.identifier.or.typedef.name
        | postfix.expression ARROW rescoped.identifier.or.typedef.name
        | postfix.expression ICR
        | postfix.expression DECR

                /* The next 4 rules are the source of cast ambiguity */
        | TYPEDEFname '(' ')'
        | TYPEDEFname '(' argument.expression.list ')'
        | basic.type.name '(' assignment.expression ')'
                /* If the following rule is added to the grammar, there will
                be 3 additional reduce-reduce conflicts.  They will all be
                resolved in favor of NOT using the following rule, so no harm
                will be done.  However, since the rule is semantically
                illegal we will omit it until we are enhancing the grammar
                for error recovery */
/*      | basic.type.name '(' ')'  Illegal: no such constructor*/
        ;

rescoped.identifier.or.typedef.name:
        TYPEDEFname 
        | rescoped.identifier
        ;

argument.expression.list:
        assignment.expression
        | argument.expression.list ',' assignment.expression
        ;

unary.expression:
        postfix.expression
        | ICR unary.expression
        | DECR unary.expression
        | indirect.or.reference cast.expression
        | '-' cast.expression
        | '+' cast.expression
        | '~' cast.expression
        | '!' cast.expression
        | SIZEOF unary.expression
        | SIZEOF '(' type.name ')'
        | allocation.expression
        ;


    /* Note that I could have moved the  newstore  productions  to  a 
    lower  precedence  level  than  multiplication  (binary '*'), and 
    lower than bitwise AND (binary '&').  These moves  are  the  nice 
    way  to  disambiguate a trailing unary '*' or '&' at the end of a 
    freestore expression.  Since the freestore expression (with  such 
    a  grammar  and  hence  precedence  given)  can never be the left 
    operand of a binary '*' or '&', the ambiguity would  be  removed.  
    These  problems  really  surface when the binary operators '*' or 
    '&' are overloaded, but this must be syntactically  disambiguated 
    before the semantic checking is performed...  Unfortunately, I am 
    not  creating  the language, only writing a grammar that reflects 
    its specification, and  hence  I  cannot  change  its  precedence 
    assignments.   If  I  had  my  druthers,  I would probably prefer 
    surrounding the type with parens all the time, and  avoiding  the 
    dangling * and & problem all together.*/

       /* Following are C++, not ANSI C */
allocation.expression:
        operator.new                                  '(' type.name ')'
                operator.new.initializer.opt
        | operator.new '(' argument.expression.list ')' '(' type.name ')'
                operator.new.initializer.opt
                /* next two rules are the source of * and & ambiguities */
        | operator.new                                  operator.new.type
        | operator.new '(' argument.expression.list ')' operator.new.type
        ;

operator.new:
        NEW
        | CLCL NEW
        ;

    /*  Note:  the  otherwise  "silly" inline expansion in the next 4 
    productions is necessary to allow such expressions as:

      new const T :: * ;
      new const T ;

    to be parsed correctly.  */

operator.new.type:
        type.qualifier.list      operator.new.declarator.opt
                        operator.new.initializer.opt
        | type.specifier.or.name operator.new.declarator.opt
                        operator.new.initializer.opt
        ;

    /* Perhaps I should move  the  following  to  a  more  consistent 
    position, but for now I will just leave it here. */

    /*  Note that the following does not include type.qualifier.list. 
    Hence, whenever type.specifier.or.name is used, an adjacent  rule 
    is  supplied containing type.qualifier.list.  It is not generally 
    possible    to    know    immediately    (i.e.,     reduce)     a 
    type.qualifier.list,  as  a TYPEDEFname that follows might not be 
    part of a type specifier, but might instead  be  "TYPEDEFname  :: 
    *". */

type.specifier.or.name:
        type.specifier
        | basic.type.name
        | TYPEDEFname
        ;

    /*  Right  recursion  is critical in the following productions to 
    avoid a conflict on TYPEDEFname */

operator.new.declarator.opt:
        /* Nothing */
        | operator.new.array.declarator
        | indirect.or.reference operator.new.declarator.opt
        | pointer.operator      operator.new.declarator.opt
        ;

operator.new.array.declarator:
        '[' ']'
        | '[' expression ']'
        | operator.new.array.declarator '[' expression ']'
        ;

operator.new.initializer.opt:
        /* Nothing */
        | '(' ')'
        | '(' argument.expression.list ')'
        ;

cast.expression:
        unary.expression
        | '(' type.name ')' cast.expression
        ;

    /* Following are C++, not ANSI C */
deallocation.expression:
        cast.expression
        | clcl.opt.delete deallocation.expression
        | clcl.opt.delete '[' expression ']' deallocation.expression  /* archaic */
        | clcl.opt.delete '[' ']' deallocation.expression
        ;

    /* Following are C++, not ANSI C */
clcl.opt.delete:
        DELETE
        | CLCL DELETE
        ;

    /* Following are C++, not ANSI C */
point.member.expression:
        deallocation.expression
        | point.member.expression DOTstar  deallocation.expression
        | point.member.expression ARROWstar  deallocation.expression
        ;
        
multiplicative.expression:
        point.member.expression
        | multiplicative.expression '*' point.member.expression
        | multiplicative.expression '/' point.member.expression
        | multiplicative.expression '%' point.member.expression
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

    /* The following are notably different from the ANSI  C  Standard 
    specified  grammar,  but  are  present  in  my  ANSI C compatible 
    grammar.  The changes were made to disambiguate typedefs presence 
    in   declaration.specifiers   (vs.    in   the   declarator   for 
    redefinition);  to allow struct/union/enum/class tag declarations 
    without  declarators,  and  to  better  reflect  the  parsing  of 
    declarations     (declarators     must     be    combined    with 
    declaration.specifiers ASAP, so that they can immediately  become 
    visible in the current scope). */

declaration:
        sue.declaration.specifier ';' { /* this is semantic error, as it
                                        includes a storage class!?!*/ }
        | sue.type.specifier ';'
        | declaring.list ';'
        | default.declaring.list ';'
        ;

    /*  Note  that  if  a typedef were redeclared, then a declaration 
    specifier must be supplied (re: ANSI C spec).  The following  are 
    declarations  wherein  no  declaration.specifier is supplied, and 
    hence the 'default' must be used.  An example of this is

        const a;

    which by default, is the same as:

        const int a;

    `a' must NOT be a typedef in the above example. */


    /* The presence of `{}' in the following rules  indicates  points 
    at  which  the symbol table MUST be updated so that the tokenizer 
    can IMMEDIATELY  continue  to  maintain  the  proper  distinction 
    between a TYPEDEFname and an IDENTIFIER. */
    

default.declaring.list:  /* Can't  redeclare typedef names */
        declaration.qualifier.list   identifier.declarator {} initializer.opt
        | type.qualifier.list        identifier.declarator {} initializer.opt
        | default.declaring.list ',' identifier.declarator {} initializer.opt

        | declaration.qualifier.list constructed.identifier.declarator
        | type.qualifier.list        constructed.identifier.declarator
        | default.declaring.list ',' constructed.identifier.declarator
        ;

    /*  Note  how  type.qualifier.list  is  NOT used in the following 
    productions.   Qualifiers  are   NOT   sufficient   to   redefine 
    typedef-names (as prescribed by the ANSI C standard).*/
    
declaring.list:
        declaration.specifier  declarator {} initializer.opt
        | type.specifier       declarator {} initializer.opt
        | basic.type.name      declarator {} initializer.opt
        | TYPEDEFname          declarator {} initializer.opt
        | declaring.list ','   declarator {} initializer.opt

        | declaration.specifier constructed.declarator
        | type.specifier        constructed.declarator
        | basic.type.name       constructed.declarator
        | TYPEDEFname           constructed.declarator
        | declaring.list ','    constructed.declarator
        ;

    /*  Declarators  with  parenthesized  initializers  present a big 
    problem.  Typically a declarator that looks  like:  "*a(...)"  is 
    supposed  to bind FIRST to the "(...)", and then to the "*". This 
    binding presumes that the "(...)" stuff  is  a  prototype  scope.  
    With  constructed  declarators,  we  must (officially) finish the 
    binding to the "*" (finishing forming a good declarator) and THEN 
    connect with the argument list.  Unfortunately, by  the  time  we 
    realize  it  is  an  argument  list (and not a prototype) we have 
    pushed the separate declarator tokens "*a" onto  the  yacc  stack 
    WITHOUT combining them. The solution is to use odd productions to 
    carry   the   incomplete  declarator  along  with  the  "argument 
    expression list" back up the yacc stack.  We would then  actually 
    instantiate  the  symbol  table after we have fully decorated the 
    symbol with all the leading "*" stuff. Actually, since  we  don't 
    have  all  the  type  information in one spot till we reduce to a 
    declaring.list, this delay is not a problem.  Note that  ordinary 
    initializers  REQUIRE (ANSI C Standard) that the symbol be placed 
    into the symbol table BEFORE its initializer is read, but in  the 
    case  of  parenthesized  initializers,  this  is not possible (we 
    don't even know we have  an  initializer  till  have  passed  the 
    opening "(". )*/

constructed.declarator:
        nonunary.constructed.identifier.declarator
        | constructed.paren.typedef.declarator 
        | simple.paren.typedef.declarator '(' argument.expression.list ')'  
        | simple.paren.typedef.declarator postfixing.abstract.declarator 
                                          '(' argument.expression.list ')'  /* semantic error*/
        | constructed.parameter.typedef.declarator 
        | indirect.or.reference constructed.declarator
        | pointer.operator      constructed.declarator
        ;

constructed.paren.typedef.declarator:
        '(' paren.typedef.declarator ')'                                         
                    '(' argument.expression.list ')'  
        | '(' paren.typedef.declarator ')' postfixing.abstract.declarator        
                   '(' argument.expression.list ')'  
        | '(' simple.paren.typedef.declarator postfixing.abstract.declarator ')' 
                   '(' argument.expression.list ')'  
        | '(' TYPEDEFname postfixing.abstract.declarator ')'                     
                   '(' argument.expression.list ')'  
        ;

constructed.parameter.typedef.declarator:
        TYPEDEFname    '(' argument.expression.list ')' 
        | TYPEDEFname postfixing.abstract.declarator                       
                       '(' argument.expression.list ')'  /* semantic error */
        | '(' clean.typedef.declarator ')'                                 
                       '(' argument.expression.list ')'
        | '(' clean.typedef.declarator ')'  postfixing.abstract.declarator
                       '(' argument.expression.list ')'
        ;



constructed.identifier.declarator:
        nonunary.constructed.identifier.declarator
        | indirect.or.reference constructed.identifier.declarator
        | pointer.operator      constructed.identifier.declarator
        ;

    /*  The  following  are  restricted to NOT begin with any pointer
    operators.  This includes both "*" and "T::*"  modifiers.   Aside 
    from   this   restriction,   the   following   would  have  been: 
    identifier.declarator '(' argument.expression.list ')' */

nonunary.constructed.identifier.declarator:
        paren.identifier.declarator   '(' argument.expression.list ')'
        | paren.identifier.declarator postfixing.abstract.declarator   
                       '(' argument.expression.list ')'  /* semantic error*/
        | '(' unary.identifier.declarator ')' 
                       '(' argument.expression.list ')'
        | '(' unary.identifier.declarator ')' postfixing.abstract.declarator  
                       '(' argument.expression.list ')'
        ;


declaration.specifier:
        basic.declaration.specifier        /* Arithmetic or void */
        | sue.declaration.specifier          /* struct/union/enum/class */
        | typedef.declaration.specifier      /* typedef*/
        ;

type.specifier:
        basic.type.specifier                 /* Arithmetic or void */
        | sue.type.specifier                 /* Struct/Union/Enum/Class */
        | typedef.type.specifier             /* Typedef */
        ;


declaration.qualifier.list:  /* storage class and optional const/volatile */
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

basic.declaration.specifier:      /*Storage Class+Arithmetic or void*/
        basic.type.specifier  storage.class
        | basic.type.name     storage.class
        | declaration.qualifier.list basic.type.name
        | basic.declaration.specifier declaration.qualifier
        | basic.declaration.specifier basic.type.name
        ;

basic.type.specifier:
        basic.type.name        basic.type.name /* Arithmetic or void */
        | basic.type.name      type.qualifier
        | type.qualifier.list  basic.type.name
        | basic.type.specifier type.qualifier
        | basic.type.specifier basic.type.name
        ;

sue.declaration.specifier:          /* Storage Class + struct/union/enum/class */
        sue.type.specifier storage.class
        | declaration.qualifier.list elaborated.type.name
        | sue.declaration.specifier declaration.qualifier
        ;

sue.type.specifier:
        elaborated.type.name              /* struct/union/enum/class */
        | type.qualifier.list elaborated.type.name
        | sue.type.specifier type.qualifier
        ;


typedef.declaration.specifier:       /*Storage Class + typedef types */
        typedef.type.specifier storage.class
        | TYPEDEFname          storage.class
        | declaration.qualifier.list TYPEDEFname
        | typedef.declaration.specifier declaration.qualifier
        ;

typedef.type.specifier:              /* typedef types */
        TYPEDEFname              type.qualifier
        | type.qualifier.list    TYPEDEFname
        | typedef.type.specifier type.qualifier
        ;

    /* There are really several  distinct  sets  of  storage.classes.  
    The  sets  vary  depending  on whether the declaration is at file 
    scope, is a  declaration  within  a  struct/class,  is  within  a 
    function body, or in a function declaration/definition (prototype 
    parameter  declarations).  They  are grouped here to simplify the 
    grammar,  and  can  be  semantically  checked.   Note  that  this 
    approach  tends to ease the syntactic restrictions in the grammar 
    slightly, but allows for future language development,  and  tends 
    to  provide  superior  diagnostics  and  error  recovery (i.e.: a 
    syntax error does not disrupt the parse).


                File    File    Member  Member  Local   Local  Formal
                Var     Funct   Var     Funct   Var     Funct  Params
TYPEDEF         x       x       x       x       x       x
EXTERN          x       x                       x       x
STATIC          x       x       x       x       x
AUTO                                            x              x
REGISTER                                        x              x
FRIEND                                  x       
OVERLOAD                x               x               x
INLINE                  x               x               x
VIRTUAL                                 x               x
*/

storage.class:
        TYPEDEF 
        | EXTERN
        | STATIC 
        | AUTO
        | REGISTER
        | FRIEND   /* C++, not ANSI C */
        | OVERLOAD /* C++, not ANSI C */
        | INLINE   /* C++, not ANSI C */
        | VIRTUAL  /* C++, not ANSI C */
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
        aggregate.name
        | enum.name
        ;


    /* Since the expression "new type.name" MIGHT use  an  elaborated 
    type  and a derivation, it MIGHT have a ':'.  This fact conflicts 
    with the requirement that a new expression can be placed  between 
    a '?' and a ':' in a conditional expression (at least it confuses 
    most LR(1) parsers). */

    /*  The derivation.opt reduction was placed DELIBERATELY in front 
    of aggregate.name to resolve  a  reduce-reduce  on  '{'  conflict 
    properly */
    
derivation.opt:
        /* nothing */
        | ':' derivation.list
        ;

    /*  The  intermediate  actions  {}  represent points at which the 
    database of typedef names  must  be  updated  in  C++.   This  is 
    critical to the lexer, which must begin to tokenize based on this 
    new information. */

aggregate.name:
        aggregate.key                             derivation.opt {}
                '{' member.declaration.list.opt '}'
        | aggregate.key identifier.or.typedef.name derivation.opt  {}
                '{' member.declaration.list.opt '}'
        | aggregate.key identifier.or.typedef.name                 {}
        ;

derivation.list:
        parent.class
        | derivation.list ',' parent.class
        ;

parent.class:
        TYPEDEFname
        | VIRTUAL access.specifier.opt TYPEDEFname
        | access.specifier virtual.opt TYPEDEFname
        ;

virtual.opt:
        /* nothing */
        | VIRTUAL
        ;

access.specifier.opt:
        /* nothing */
        | access.specifier
        ;

access.specifier:
        PUBLIC
        | PRIVATE
        | PROTECTED /* not syntactically valid, but nice for error reporting*/
        ;

aggregate.key:
        STRUCT
        | UNION
        | CLASS /* C++, not ANSI C */
        ;

    /* Note that an empty list is ONLY allowed under C++. The grammar 
    can  be modified so that this stands out.  The trick is to define 
    member.declaration.list, and have that referenced for non-trivial 
    lists. */

member.declaration.list.opt:
        /* nothing */ 
        | member.declaration.list.opt member.declaration
        ;

member.declaration:
        member.declaring.list ';'
        | member.default.declaring.list ';'
        | PUBLIC ':'                      /* C++, not ANSI C */
        | PRIVATE ':'                     /* C++, not ANSI C */
        | PROTECTED ':'                   /* C++, not ANSI C */
        | new.function.definition         /* C++, not ANSI C */
        | constructor.function.in.class   /* C++, not ANSI C */
        | destructor.function             /* C++, not ANSI C */
        | sue.type.specifier ';'       /* nested tag elaborations*/ /* C++, not ANSI C */
        | class.rescoped.identifier ';' /*access modification*/     /* C++, not ANSI C */
        | typedef.declaration.specifier ';' /* friend T */   /* C++, not ANSI C */
        | sue.declaration.specifier ';'  /* friend class C*/  /* C++, not ANSI C */
        ;

member.default.declaring.list:        /* doesn't redeclare typedef*/
        type.qualifier.list          
                identifier.declarator member.pure.opt
        | declaration.qualifier.list 
                identifier.declarator member.pure.opt /* C++, not ANSI C */
        | member.default.declaring.list ',' 
                identifier.declarator member.pure.opt

        | type.qualifier.list           bit.field.identifier.declarator 
        | declaration.qualifier.list    bit.field.identifier.declarator /* C++, not ANSI C */
        | member.default.declaring.list ','  bit.field.identifier.declarator 
        ;

member.declaring.list:        /* Can possibly redeclare typedefs */
        type.specifier     declarator member.pure.opt
        | basic.type.name  declarator member.pure.opt
        | member.conflict.declaring.item

        | member.declaring.list ',' declarator member.pure.opt

        | type.specifier        bit.field.declarator 
        | basic.type.name       bit.field.declarator 
        | TYPEDEFname           bit.field.declarator 
        | declaration.specifier bit.field.declarator /* C++? not ANSI C */
        | member.declaring.list ',' bit.field.declarator 
        ;

    /* The following conflict with constructors-
      member.conflict.declaring.item:
        TYPEDEFname           declarator member.pure.opt
        | declaration.specifier declarator member.pure.opt /* C++, not ANSI C * /
        ;
    so we inline expand declarator to get the following productions...
    */
member.conflict.declaring.item:
        TYPEDEFname             identifier.declarator            member.pure.opt
        | TYPEDEFname           parameter.typedef.declarator     member.pure.opt
        | declaration.specifier identifier.declarator            member.pure.opt
        | declaration.specifier parameter.typedef.declarator     member.pure.opt
        | TYPEDEFname           simple.paren.typedef.declarator  member.pure.opt
        | declaration.specifier simple.paren.typedef.declarator  member.pure.opt

        | member.conflict.paren.declaring.item
        ;

    /* The following still conflicts with constructors-
      member.conflict.paren.declaring.item:
        TYPEDEFname           paren.typedef.declarator     member.pure.opt
        | declaration.specifier paren.typedef.declarator     member.pure.opt
        ;
    so paren.typedef.declarator is expanded inline to get...*/

member.conflict.paren.declaring.item:
        TYPEDEFname   indirect.or.reference 
                '(' simple.paren.typedef.declarator ')' member.pure.opt
        | TYPEDEFname pointer.operator
                '(' simple.paren.typedef.declarator ')' member.pure.opt
        | TYPEDEFname indirect.or.reference
                '(' TYPEDEFname ')'                     member.pure.opt
        | TYPEDEFname pointer.operator
                '(' TYPEDEFname ')'                     member.pure.opt
        | TYPEDEFname indirect.or.reference
                 paren.typedef.declarator               member.pure.opt
        | TYPEDEFname pointer.operator
                 paren.typedef.declarator               member.pure.opt
        | declaration.specifier indirect.or.reference
                '(' simple.paren.typedef.declarator ')' member.pure.opt
        | declaration.specifier pointer.operator
                '(' simple.paren.typedef.declarator ')' member.pure.opt
        | declaration.specifier indirect.or.reference
                '(' TYPEDEFname ')'                     member.pure.opt
        | declaration.specifier pointer.operator
                '(' TYPEDEFname ')'                     member.pure.opt
        | declaration.specifier indirect.or.reference
                paren.typedef.declarator                member.pure.opt
        | declaration.specifier pointer.operator
                paren.typedef.declarator                member.pure.opt

        | member.conflict.paren.postfix.declaring.item
        ;

    /* but we still have the following conflicts with constructors-
    member.conflict.paren.postfix.declaring.item:
      TYPEDEFname           paren.postfix.typedef.declarator member.pure.opt
      | declaration.specifier paren.postfix.typedef.declarator member.pure.opt
      ;
    so we expand paren.postfix.typedef inline and get...*/

member.conflict.paren.postfix.declaring.item:
        TYPEDEFname     '(' paren.typedef.declarator ')'
                                                           member.pure.opt
        | TYPEDEFname   '(' simple.paren.typedef.declarator
                        postfixing.abstract.declarator ')' member.pure.opt
        | TYPEDEFname   '(' TYPEDEFname
                        postfixing.abstract.declarator ')' member.pure.opt
        | TYPEDEFname   '(' paren.typedef.declarator ')'
                        postfixing.abstract.declarator     member.pure.opt

        | declaration.specifier '(' paren.typedef.declarator ')' 
                                                           member.pure.opt
        | declaration.specifier '(' simple.paren.typedef.declarator
                        postfixing.abstract.declarator ')' member.pure.opt
        | declaration.specifier '(' TYPEDEFname
                        postfixing.abstract.declarator ')' member.pure.opt
        | declaration.specifier '(' paren.typedef.declarator ')' 
                        postfixing.abstract.declarator     member.pure.opt
        ;


member.pure.opt:
        /* nothing */
        | '=' OCTALconstant /* C++, not ANSI C */ /* Pure function*/
        ;

    /* Note that bit field names cannot be parenthesized in C++  (due 
    to  ambiguities),  and  hence this part of the grammar is simpler 
    than ANSI C. :-) */

bit.field.declarator:
        bit.field.identifier.declarator
        | TYPEDEFname {} ':' constant.expression
        ;

    /* The actions taken in the "{}" above and below are probably not 
    really required (re: putting the symbol into the symbol table  as 
    part  of  the  struct), as there is (I think) no way to reference 
    them until the struct is complete.  Hence there is no  real  risk 
    that  the constant.expression would make reference to them (e.g., 
    "name : sizeof  name").   For  cleanliness,  we  will  leave  the 
    actions in this form. */

bit.field.identifier.declarator:
        ':' constant.expression
        | IDENTIFIER {} ':' constant.expression
        ;

enum.name:
        ENUM '{' enumerator.list '}'
        | ENUM identifier.or.typedef.name '{' enumerator.list '}'
        | ENUM identifier.or.typedef.name
        ;

enumerator.list:
        enumerator.list.no.trailing.comma
        | enumerator.list.no.trailing.comma ',' /* C++, not ANSI C */
        ;

    /*  Note  that we do not need to rush to add an enumerator to the 
    symbol table until *AFTER* the  enumerator.value.opt  is  parsed.  
    The  enumerated  value  is  only in scope AFTER its definition is 
    complete.  Hence the following is legal: "enum {a, b=a+10};"  but 
    the  following is (assuming no external matching of names) is not 
    legal: "enum {c, d=sizeof(d)};" ("d" not defined when sizeof  was 
    applied.)  This  is  notably  contrasted  with declarators, which 
    enter scope as soon as the declarator is complete. */

enumerator.list.no.trailing.comma:
        identifier.or.typedef.name enumerator.value.opt
        | enumerator.list.no.trailing.comma ',' 
               identifier.or.typedef.name enumerator.value.opt
        ;

enumerator.value.opt:
        /* Nothing */
        | '=' constant.expression
        ;

    /* We special case the lone type.name which has no storage  class 
    (even  though  it should be an example of a parameter.type.list). 
    This helped to disambiguate type-names in parenthetical casts.*/

parameter.type.list: 
        '(' ')'                             type.qualifier.list.opt
        | '(' type.name ')'                 type.qualifier.list.opt
        | '(' type.name initializer ')'     type.qualifier.list.opt /* C++, not ANSI C */
        | '(' named.parameter.type.list ')' type.qualifier.list.opt
        ;
    
    /* The following are used in old style function definitions, when 
    a complex return type includes the "function returning" modifier.  
    Note the  subtle  distinction  from  parameter.type.list.   These 
    parameters are NOT the parameters for the function being defined, 
    but are simply part of the type definition.  An example would be:

        int(*f(   a  ))(float) long a; {...}

    which is equivalent to the full new style definition:

        int(*f(long a))(float) {...}

    The    type    list    `(float)'    is    an    example   of   an 
    old.parameter.type.list.  The bizarre point here is that  an  old 
    function  definition  declarator  can be followed by a type list, 
    which can start with a qualifier `const'.   This  conflicts  with 
    the  new  syntactic  construct for const member functions!?! As a 
    result, an old style function definition cannot be  used  in  all 
    cases for a member function.  */

old.parameter.type.list: 
        '(' ')' 
        | '(' type.name ')' 
        | '(' type.name initializer ')'  /* C++, not ANSI C */
        | '(' named.parameter.type.list ')' 
        ;

named.parameter.type.list:  /* WARNING: excludes lone type.name*/
        parameter.list
        | parameter.list comma.opt.ellipsis
        | type.name comma.opt.ellipsis
        | type.name initializer comma.opt.ellipsis  /* C++, not ANSI C */
        | ELLIPSIS /* C++, not ANSI C */
        ;


comma.opt.ellipsis:
        ELLIPSIS       /* C++, not ANSI C */
        | ',' ELLIPSIS
        ;

parameter.list: 
        named.parameter.declaration
        | named.parameter.declaration initializer /* C++, not ANSI C */
        | type.name             ',' parameter.declaration
        | type.name initializer ',' parameter.declaration  /* C++, not ANSI C */
        | parameter.list ',' parameter.declaration
        ;

    /*  There  is  some very subtle disambiguation going on here.  Do 
    not be tempted to make further use of the following production in 
    parameter.list, or else the conflict count will grow  noticeably.  
    Specifically,  the  next  set  of  rules  has already been inline 
    expanded for the first parameter in a parameter.list to support a 
    deferred disambiguation. */
     
parameter.declaration:
        type.name
        | type.name initializer  /* C++, not ANSI C */
        | named.parameter.declaration
        | named.parameter.declaration initializer /* C++, not ANSI C */
        ;

    /* One big thing implemented here is that a TYPEDEFname CANNOT be 
    redeclared when we don't have declaration.specifiers! Notice that 
    when we do use a TYPEDEFname based declarator, only the "special" 
    (non-ambiguous  in  this  context)  typedef.declarator  is  used. 
    Everything else that is "missing" shows up as a type.name. */

named.parameter.declaration: /*have names or storage classes */
        declaration.specifier
        | declaration.specifier abstract.declarator
        | declaration.specifier identifier.declarator
        | declaration.specifier parameter.typedef.declarator

        | declaration.qualifier.list 
        | declaration.qualifier.list abstract.declarator
        | declaration.qualifier.list identifier.declarator
        | type.specifier identifier.declarator
        | type.specifier parameter.typedef.declarator

        | basic.type.name identifier.declarator
        | basic.type.name parameter.typedef.declarator

        | TYPEDEFname identifier.declarator
        | TYPEDEFname parameter.typedef.declarator

        | type.qualifier.list identifier.declarator
        ;
    
identifier.or.typedef.name:
        IDENTIFIER
        | TYPEDEFname 
        ;

type.name:
        type.specifier
        | basic.type.name 
        | TYPEDEFname 
        | type.qualifier.list 

        | type.specifier abstract.declarator
        | basic.type.name abstract.declarator
        | TYPEDEFname abstract.declarator
        | type.qualifier.list abstract.declarator
        ;

initializer.opt:
        /* nothing */
        | initializer
        ;

initializer:
        '=' initializer.group
        ;

initializer.group:        
        '{' initializer.list '}'
        | '{' initializer.list ',' '}'
        | assignment.expression
        ;

initializer.list:
        initializer.group
        | initializer.list ',' initializer.group
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

    /*  I sneak declarations into statement.list to support C++.  The 
    grammar is a little clumsy this  way,  but  the  violation  of  C 
    syntax is heavily localized */

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
        | statement.list declaration /* C++, not ANSI C */
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
        | FOR '(' declaration        expression.opt ';'
                expression.opt ')' statement  /* C++, not ANSI C */
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
        function.declaration
        | function.definition
        | declaration
        | linkage.specifier function.declaration     /* C++, not ANSI C*/
        | linkage.specifier function.definition      /* C++, not ANSI C*/
        | linkage.specifier declaration              /* C++, not ANSI C*/
        | linkage.specifier '{' translation.unit '}' /* C++, not ANSI C*/
        ;

linkage.specifier:
        EXTERN STRINGliteral
        ;

    /* Note that declaration.specifiers are left out of the following 
    function declarations.  This is something of an anomaly in C, but 
    an  unfortunately  common  coding practice.  It is also sometimes 
    necessary in C++, in instances where no  return  type  should  be 
    specified (e.g., a conversion operator).*/

function.declaration:
        identifier.declarator ';' /* semantically verify it is a function */
        ;
        
function.definition:
        new.function.definition
        | old.function.definition
        | constructor.function.definition
        ;

new.function.definition:
                                     identifier.declarator compound.statement
        | declaration.specifier      identifier.declarator compound.statement
        | type.specifier             identifier.declarator compound.statement
        | basic.type.name            identifier.declarator compound.statement
        | TYPEDEFname                identifier.declarator compound.statement
        | declaration.qualifier.list identifier.declarator compound.statement
        | type.qualifier.list        identifier.declarator compound.statement
        ;

old.function.definition:
                                     old.function.declarator compound.statement 
        | declaration.specifier      old.function.declarator compound.statement
        | type.specifier             old.function.declarator compound.statement
        | basic.type.name            old.function.declarator compound.statement
        | TYPEDEFname                old.function.declarator compound.statement
        | declaration.qualifier.list old.function.declarator compound.statement
        | type.qualifier.list        old.function.declarator compound.statement

        |                            old.function.declarator declaration.list 
                compound.statement
        | declaration.specifier      old.function.declarator declaration.list
                compound.statement
        | type.specifier             old.function.declarator declaration.list
                compound.statement
        | basic.type.name            old.function.declarator declaration.list
                compound.statement
        | TYPEDEFname                old.function.declarator declaration.list
                compound.statement
        | declaration.qualifier.list old.function.declarator declaration.list
                compound.statement
        | type.qualifier.list        old.function.declarator declaration.list
                compound.statement
        ;
    
    /* I needed to add some storage class options to the constructors 
    and  destructors  (e.g.: virtual, inline, ...  ) These were added 
    using declaration.qualifier.list, but they should be semantically 
    verified to be inline or virtual ONLY. If I don't have  to  allow 
    BOTH,  then  I  COULD be explicit (and not increase ambiguities). 
    With this LR(1) grammar, I have to reduce the  list  to  standard 
    declaration.qualifier.list ASAP.*/

destructor.function:
        declaration.qualifier.list '~' TYPEDEFname void.opt.param.list
                        destructor.function.body
        | '~' TYPEDEFname void.opt.param.list destructor.function.body
        ;

void.opt.param.list:
        '('        ')' type.qualifier.list.opt
        | '(' VOID ')' type.qualifier.list.opt
        ;

destructor.function.body:
        compound.statement  /* an actual destructor definition */
        | ';'               /* only a destructor declaration */
        ;

    /*  Semantically  verify that the following identifier.declarator 
    are:
    
        TYPEDEFname :: TYPEDEFname (parameter.type.list).  

    We use the more general form to prevent a clash  with  a  typical 
    function  definition  (which  won't have a constructor.init.list) 
    The ONLY valid declaration qualifier is INLINE. */

constructor.function.definition:
                                     identifier.declarator  
                                constructor.init.list compound.statement
        | declaration.qualifier.list identifier.declarator  
                                constructor.init.list compound.statement
        ;

    /* The following use of declaration.specifiers are made to  allow 
    for  a TYPEDEFname preceded by an INLINE modifier. This fact must 
    be verified semantically.  It should also be  verified  that  the 
    TYPEDEFname  is  ACTUALLY  the  class name being elaborated. Note 
    that we could break out typedef.declaration.specifier from within 
    declaration.specifier, and we  might  narrow  down  the  conflict 
    region a bit. A second alternative (to what is done) for cleaning 
    up  this  stuff  is  to  let the tokenizer specially identify the 
    current class being elaborated as a special token, and not just a 
    typedef.name. Unfortunately, things would get very confusing  for 
    the  lexer,  as  we may pop into enclosed tag elaboration scopes; 
    into function definitions; or into both recursively! */
    
constructor.function.in.class:
        declaration.specifier   constructor.parameter.list.and.body
        | TYPEDEFname           constructor.parameter.list.and.body
        ;

    /* The following conflicts with member declarations-
    constructor.parameter.list.and.body:
          parameter.type.list ';'
          | parameter.type.list constructor.init.list.opt compound.statement
          ;
    so parameter.type.list was expanded inline to get */

constructor.parameter.list.and.body:
          '('                           ')' type.qualifier.list.opt ';'
        | '(' type.name initializer     ')' type.qualifier.list.opt ';' /* C++, not ANSI C */
        | '(' named.parameter.type.list ')' type.qualifier.list.opt ';'
        | '('                           ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement
        | '(' type.name initializer     ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement  /* C++, not ANSI C */
        | '(' named.parameter.type.list ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement
        | constructor.conflicting.parameter.list.and.body
        ;

    /* The following conflicted with member declaration-
    constructor.conflicting.parameter.list.and.body:
        '(' type.name ')'                 type.qualifier.list.opt ';'
        | '(' type.name ')'                 type.qualifier.list.opt
                constructor.init.list.opt compound.statement
        ;
    so type.name was inline expanded to get the following... */

constructor.conflicting.parameter.list.and.body:
        '(' type.specifier ')' type.qualifier.list.opt
                ';'
        | '(' basic.type.name  ')' type.qualifier.list.opt
                ';'
        | '(' TYPEDEFname  ')' 
                ';'
        | '(' TYPEDEFname  ')' type.qualifier.list
                ';'

        | '(' type.qualifier.list  ')' type.qualifier.list.opt
                ';'
        | '(' type.specifier abstract.declarator ')' type.qualifier.list.opt
                ';'
        | '(' basic.type.name abstract.declarator ')' type.qualifier.list.opt
                ';'
        | '(' type.qualifier.list abstract.declarator ')' type.qualifier.list.opt
                ';'

        | '(' type.specifier ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement
        | '(' basic.type.name  ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement
        | '(' TYPEDEFname  ')' 
                constructor.init.list.opt compound.statement
        | '(' TYPEDEFname  ')' type.qualifier.list
                constructor.init.list.opt compound.statement

        | '(' type.qualifier.list  ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement
        | '(' type.specifier abstract.declarator ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement
        | '(' basic.type.name abstract.declarator ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement
        | '(' type.qualifier.list abstract.declarator ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement
        | constructor.conflicting.typedef.declarator
        ;


    /* The following have ambiguities with member declarations-
    constructor.conflicting.typedef.declarator:
      '(' TYPEDEFname abstract.declarator ')' type.qualifier.list.opt
                ';'
      |  '(' TYPEDEFname abstract.declarator ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement
      ;
    which can be deferred by expanding abstract.declarator, and in two 
    cases parameter.qualifier.list, resulting in ...*/

constructor.conflicting.typedef.declarator:
        '(' TYPEDEFname unary.abstract.declarator ')' type.qualifier.list.opt
                ';'
        | '(' TYPEDEFname unary.abstract.declarator ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement

        | '(' TYPEDEFname postfix.abstract.declarator ')' type.qualifier.list.opt
                ';'
        | '(' TYPEDEFname postfix.abstract.declarator ')' type.qualifier.list.opt
                constructor.init.list.opt compound.statement

        | '(' TYPEDEFname postfixing.abstract.declarator ')' type.qualifier.list
                ';'
        | '(' TYPEDEFname postfixing.abstract.declarator ')' type.qualifier.list
                constructor.init.list.opt compound.statement

        | '(' TYPEDEFname postfixing.abstract.declarator ')' 
                ';'
        | '(' TYPEDEFname postfixing.abstract.declarator ')' 
                constructor.init.list.opt compound.statement
        ;


constructor.init.list.opt:
        /* nothing */
        | constructor.init.list
        ;

constructor.init.list:
        ':' constructor.init
        | constructor.init.list ',' constructor.init
        ;

constructor.init:
        identifier.or.typedef.name   '(' argument.expression.list ')'
        | identifier.or.typedef.name '('                          ')'
        | '(' argument.expression.list ')' /* Single inheritance ONLY*/
        | '(' ')' /* Is this legal? It might be default! */
        ;        

declarator:
        typedef.declarator
        | identifier.declarator
        ;
        
typedef.declarator:
        paren.typedef.declarator  /* would be ambiguous as parameter*/
        | simple.paren.typedef.declarator /* also ambiguous */
        | parameter.typedef.declarator   /* not ambiguous as param*/
        ;

parameter.typedef.declarator:
        TYPEDEFname
        | TYPEDEFname postfixing.abstract.declarator
        | clean.typedef.declarator
        ;

    /* The following have at  least  one  '*'or  '&'.   There  is  no 
    (redundant) '(' between the '*'/'&' and the TYPEDEFname. */
    
clean.typedef.declarator:
        clean.postfix.typedef.declarator
        | indirect.or.reference parameter.typedef.declarator
        | pointer.operator      parameter.typedef.declarator  
        ;

clean.postfix.typedef.declarator:
        '(' clean.typedef.declarator ')'
        | '(' clean.typedef.declarator ')' postfixing.abstract.declarator
        ;

    /*  The  following have a redundant '(' placed immediately to the 
    left of the TYPEDEFname */
    
paren.typedef.declarator:
        paren.postfix.typedef.declarator
        | indirect.or.reference '(' simple.paren.typedef.declarator ')'
        | pointer.operator      '(' simple.paren.typedef.declarator ')'
        | indirect.or.reference '(' TYPEDEFname ')' /* redundant paren */
        | pointer.operator      '(' TYPEDEFname ')' /* redundant paren */
        | indirect.or.reference paren.typedef.declarator
        | pointer.operator      paren.typedef.declarator
        ;
        
paren.postfix.typedef.declarator:
        '(' paren.typedef.declarator ')'
        | '(' simple.paren.typedef.declarator postfixing.abstract.declarator ')' 
        | '(' TYPEDEFname postfixing.abstract.declarator ')' /* redundant paren */
        | '(' paren.typedef.declarator ')' postfixing.abstract.declarator
        ;

    /* The following excludes lone TYPEDEFname to help in a  conflict 
    resolution.   We  have  special cased lone TYPEDEFname along side 
    all uses */
    
simple.paren.typedef.declarator:
        '(' TYPEDEFname ')'
        | '(' simple.paren.typedef.declarator ')'
        ;

identifier.declarator:
        unary.identifier.declarator  
        | paren.identifier.declarator
        ;

    /* The following allows "function return array  of"  as  well  as 
    "array  of  function  returning".  It COULD be cleaned up the way 
    abstract declarators have been.  This change might make  it  hard 
    to  recover from user's syntax errors, whereas now they appear as 
    simple semantic errors. */

unary.identifier.declarator:
        postfix.identifier.declarator
        | indirect.or.reference identifier.declarator
        | pointer.operator      identifier.declarator
        ;

postfix.identifier.declarator:
        paren.identifier.declarator postfixing.abstract.declarator
        | '(' unary.identifier.declarator ')'
        | '(' unary.identifier.declarator ')' postfixing.abstract.declarator
        ;

old.function.declarator:
        postfix.old.function.declarator
        | indirect.or.reference old.function.declarator
        | pointer.operator      old.function.declarator
        ;

    /* ANSI C section 3.7.1  states  "An  identifier  declared  as  a 
    typedef  name shall not be redeclared as a parameter".  Hence the 
    following is based only on IDENTIFIERs.
    
    Instead of identifier.lists, an argument.expression.list is  used 
    in   old   style   function   definitions.   The  ambiguity  with 
    constructors required the use of argument lists, with a  semantic 
    verification   of   the   list  (e.g.:  check  to  see  that  the 
    "expressions" consisted of lone identifiers).  
    
    An interesting ambiguity appeared:
        const constant=5;
        int foo(constant) ...

    Is this an old function definition or constructor?  The  decision 
    is made later by THIS grammar based on trailing context :-). This 
    ambiguity  is probably what caused many parsers to give up on old 
    style function definitions. */

postfix.old.function.declarator:
        paren.identifier.declarator '(' argument.expression.list ')'
        | '(' old.function.declarator ')'
        | '(' old.function.declarator ')' old.postfixing.abstract.declarator
        ;

old.postfixing.abstract.declarator:
        array.abstract.declarator /* array modifiers */
        | old.parameter.type.list  /* function returning modifiers */
        ;

abstract.declarator:
        unary.abstract.declarator
        | postfix.abstract.declarator
        | postfixing.abstract.declarator
        ;

postfixing.abstract.declarator:
        array.abstract.declarator
        | parameter.type.list
        ;

array.abstract.declarator:
        '[' ']'
        | '[' constant.expression ']'
        | array.abstract.declarator '[' constant.expression ']'
        ;

unary.abstract.declarator:
        indirect.or.reference 
        | pointer.operator 
        | indirect.or.reference abstract.declarator
        | pointer.operator      abstract.declarator
        ;

postfix.abstract.declarator:
        '(' unary.abstract.declarator ')'
        | '(' postfix.abstract.declarator ')'
        | '(' postfixing.abstract.declarator ')'
        | '(' unary.abstract.declarator ')' postfixing.abstract.declarator
        ;

indirect.or.reference:
        '*'
        | '&'
        ;

pointer.operator:
        TYPEDEFname CLCL '*' type.qualifier.list.opt
        | indirect.or.reference type.qualifier.list
        ;
%%
yyerror(string)
char*string;
{
    printf("parser error: %s\n", string);
}


main()
{
    yyparse();
}
