# ACIF-Cheatsheet

## Type declaration
| meaning | acif name |
| - | - |
| var | Lord |
|    | Lady |
| const           | Knight  |
| pointerVar | Wildling |
| type | of House | 
| constValue | hosts a feast for |
| beginAlias | House |
| strongAlias | comes from the old lineage of |
| weakAlias | are the dogs of |

##  Datatypes
| meaning | acif name |
| - | - |
| int | Lanninteger |
| float | Freyt|
| bool | Boolton |
| char | Starkhar|
| atom | Barathom |
| void | No One|

##  Literals
| meaning | acif name |
| - | - |
| true | True Heir |
| false | Usurper |
| intLit | [\-]{0,1}$digits+ soldiers |
| floatLit | [\-]{0,1}$digits+\.$digits+ descendants|
| charLit | Hodor \'(@scapedchars)\' |
| | Hodor \'$printable\' |
| '{{' | army formation of |
| '}}' | aligned together |
| '[[' | The dead bodies of |
| ']]' | coming from beyond the wall|
| null | Rickon|
| naturalLit | [0-9]+|
| stringLit | Maester reading \" @scpaedchars $printable \"|

                                     
##  Composite types
| meaning | acif name |
| - | - |
| beginCompTypeId | Former |
| endCompTypeId | now |
| beginArray | Lord Commander of |
| endArray | bannermen                                  |
| string | Hand of the King|
| beginSz | leading |
| endSz | to their deaths|
| beginStruct | King to whom |
| endStruct | have bent their knees|
| beginUnion | Faceless Man who stole |
| endUnion | their faces|
| pointerType | Spearwife of |
| beginTuple | White Walker possesing |
| endTuple | wights |


##  Type conversion
| meaning | acif name |
| - | - |
| cast | adopted by House |

##  Operators
| meaning | acif name |
| - | - |
| ':=' | takes |
| ':==' | fight against |
| '+' | joined by |
| '-' | left by |
| '*' | combined forces with |
| '/' | cut into pieces by |
| '~' | turncloak                                               |
| '%' | stripped of his dignity by |
| and | and |
| or | or |
| '=' | similar to |
| '!=' | different from |
| '<' | bested by |
| '>' | defeating |
| '<=' | almost bested by |
| '>=' | almost defeating |

##  Composite type operators
| meaning | acif name |
| - | - |
| '<-' | subject of |
| '?' | looking in the mirror at |
| '->' | acting as |
| '[' | Soldier acquainted with |
| ']' | under command of |
| '[(' | Wight |
| new | marries|
| deref | Spouse of |
| free | becomes widowed|
 
## Exit statement
| meaning | acif name |
| - | - |
| beginExit | The book |
| endExit | has reached an unexpected end|

##  IO 
| meaning | acif name |
| - | - |
| read | A raven has come for |
| print | We must send a raven with everything we know of |

##  Empty Statement
| meaning | acif name |
| - | - |
| pass | The Three\-Eyed Raven watches from afar|

##  Procedures Definition
| meaning | acif name |
| - | - |
| beginFuncDec | Table\ of\ Contents\:|
| item | \-|
| globalDec | Prologue                                   |
| main | Epilogue                                   |
| beginFuncParams | watches                                    |
| endFuncParams | approach from a distance\; |
| beginReturnVals | I must warn you($white*)\, |
| endReturnVals | is coming\.|
| returnOpen | Dracarys                                   |
| returnClose | !|
| valueArg | Valued                                     |
| refArg | Honorable                                  |

##  Blocks
| meaning | acif name |
| - | - |
| '{' | Valar Morghulis\.|
| '}' | Valar Dohaeris\.|

##  Procedure Call
| meaning | acif name |
| - | - |
| '((' | traveling |
| procCallArgs | alongside |
| '))' | with caution |

##  Determinate repetition
| meaning | acif name |
| - | - |
| for | The things I do for |
| forLB | I would kill from |
| forUB | up to |
| endFor | That\, and much more I would do to get her love|

##  Undeterminate repetition
| meaning | acif name |
| - | - |
| while | While |
| whileDec | reigns truly upon the land|
| endWhile | Only for as long as the sovereign lives|

##  Non Structured Flow
| meaning | acif name |
| - | - |
| continue | What is dead may never die|
| break | This is the doom of Valyria|

##  Simple selection 
| meaning | acif name |
| - | - |
| if | If|
| then | may be the True King of the Seven Kingdoms\, then |
| else | Otherwise\,|
| endif | And so our fate rests upon this decision|

##  Multiple Selection
| meaning | acif name |
| - | - |
| switch | You will be betrayed by |
| switchDec | several times|
| case | Once for |
| endSwitch | So the prophecy says|

##  Identifiers
| meaning | acif name |
| - | - |
| id | [A-Z]([\']?[a-z]+)+|
| argNumber | "valid roman numeral"

##  Atoms
| meaning | acif name |
| - | - |
| nothing | nothing      |
| atomLit | [a-z]+|

##  Appendix
| meaning | acif name |
| - | - |
| aliasDec | Appendix\:|

##  Dot Comma
| meaning | acif name |
| - | - |
| ',' | \, |
| '.' | \. |

##  Expressions
| meaning | acif name |
| - | - |
| '(' | \<\<|
|      | «  |
| ')' | \>\>|
| |»|

##  Comments
| meaning | acif name |
| - | - |
| comment | Suddenly\,  .. \.|
| |                In the midst of  .. \.|
| |                Therefore  .. \.|

--- 

# Syntax reference

## Program

The phrase "No One" can appear in function invocation, instead of function argument list and return list.

```
A Song of Ice and Fire: -- <título> --
Table of Contents:
- Prologue
- <Foo_1> <#_args_1+1>
...
- <Foo_n> <#_args_n+1>
- Epilogue

Prologue
    Valar Morghulis.
    Valar Dohaeris.

<Foo_1>
    watches
        <Valued|Honorable> <Id_1> of House <type_1>,
        ...
        <Valued|Honorable> <Id_1> of House <type_n>
        |
        No one
    approach from a distance;
    I must warn you,
        <ret_type_1>,
        ...
        <ret_type_m>
        |
        No one
    is coming.
Valar Morghulis.
    <code>
    IGNORAR RETORNO INVOCACION: No One fight against <invocación> 
    Dracarys [<val_1>, ... <val_n>]!
Valar Dohaeris.

..

Epilogue
    Valar Morghulis.
        <foo_1>
    Valar Dohaeris.

Appendix:
House <alias_id_1> are the dogs of <alias_type_1>
..
House <alias_id_n> are the dogs of <alias_type_n>
```

## Functions

This:
```
<Foo_1>
    watches
        <Valued|Honorable> <Id_1> of House <type_1>,
        ...
        <Valued|Honorable> <Id_1> of House <type_n>
    approach from a distance;
    I must warn you,
        <ret_type_1>,
        ...
        <ret_type_m>
    is coming.
Valar Morghulis.
    <code>
    Dracarys [<val_1>, ... <val_n>]!
Valar Dohaeris.


```
Equals to this pseudo c code:
```
(<ret_type_1>,..,<ret_type_n>) foo_1([const] type_1 arg_1,..,[const] type_1 arg_n) {
    <code>
    return (val_1,..,val_n)
}
```

### Function invocation

This:
```
<id> takes <foo_id> traveling alongside <arg_1>, ... <arg_n> with caution    
```
Equals this c function call:
```
id = foo_id(arg_1,..,arg_n);
```

And this

```
No One fight against <foo_id> traveling alongside <arg_1>, ... <arg_n> with caution 
```
Equals to this c function call:
```
foo_id(arg_1,..,arg_n);
```



## Constant declaration

### Simple type
```
Knight <id> of House <tipo> hosts a feast for <value>.
```

### Composite type
Impossible for composite types without literals: structs, unions
```
Former Knight <id> now <tipo> hosts a feast for <value>.
```

## Variable Declaration

### Simple type
```
<Lord|Lady> <id> of House <tipo>.
```
### Composite type
```
Former <Lord|Lady> <id> now <tipo>.
Former Wildling <id> now <tipo>.
```

## Arrays

### Declaration

This
```
Lord Commander of <n_dimentions> <type> bannermen.

Lord Commander of <dimentions> <type> bannermen leading <tam_dim_1>,..,<tam_dim_n> to their deaths.
```
Equals this c array declaration:
```
type x[tam_dim_1]..[tam_dim_n];
```

### Literals

This:
```
army formation of
    army formation of <a_value_1>,..,<a_value_n> aligned together,
    ...
    army formation of <z_value_1>,..,<z_value_n> aligned together
aligned together
```

Equals this c array literal:
```
{{a_value_1,..,a_value_n}, .., {z_value_1,..,z_value_n}}
```


### Indexado

This:
```
Soldier acquainted with
    <index_1>,
    ...
    <index_n>
under command of <array_name>
```

Equals this c array indexation:
```
array_name[index_1]..[index_n];
```

## String

### Declaration
This:
```
Former <Lord|Lady> <id> now Hand of the king leading <size> soldiers to their death
```

Equals this c array declaration:
```
char <id>[<size>];
```

### Literal
```
Maester reading ".."
```

## Structs

There are no literals.

### Declaration
This:
```
Former <Lord|Lady> <id> now King to whom
    <decl_1>,
    ...
    <decl_n>
have bent their knees
```
Equals this c struct declaration:
```
struct <id> { 
        <decl_1>;
        ...
        <decl_n>;
}
```

### Field access
This:
```
<field_id> subject of <struct_id>
```
equals this c struct field reference:
```
<struct_id>.<field_id>;
```

## Unions

This has no literals.

### Declaration

This
```
Former <Lord|Lady> <id> now Faceless Man who stole <decl_1>, ...  <decl_n> their faces.
```
Approximates to this Haskell data declaration:
```
data <id> = <decl_1> | .. | <decl_n> 
```

### Field access

This
```
<union> acting as <id>
<union> looking in the mirror at <id>
```
Represents an approximation to Haskell pattern matching and a query of the current struct field respectively.

## Tuples

They have anonymous fields.

### Definition

```
White Walker possessing
    <type_1>,
    ...
    <type_n>
wights
```

### Literals

This:
```
The dead bodies of <val_1>, ..., <val_n> coming from beyond the Wall
```
Represents this haskell tuple literal
```
(a,b,c) = (<val_1>,..,<val_n>)
```

### Indexing

```
Wight <index> under command of <tuple_id>
```

## Pointers

### Declaration
This:
```
Former Wildling <id> now Spearwife of <type>
```
Equals to this c pointer declaration:
```
<type> *<id>; 
```

### Dereference
This
```
Spouse of <pointer_id>
```
Equals to this c statement:
```
*<pointer_id>
```

### Memory allocation
This:
```
<pointer_id> marries.
```
Equals this c malloc:
```
<pointer_id> = (pointer_id_type) malloc(sizeof(pointer_id_type)) ; 
```

### Memory deallocation

This:
```
<pointer_id> becomes widowed.
```
Equals this c deallocation:
```
free(<pointer_id>)
```

### Literals

`Rickon` is the same as `nullptr` in c

## Alias

### Weak Declaration
This
```
House <id> are the dogs of <type>.
```
Equals this type synonim of haskell:
```
type <id> = <type>
```

### Strong Declaration
This:
```
House <id> comes from the old lineage of <type>
```
Equals this newtype declaration in Haskell:
```
newtype <id> = <type> 
```

### Casting

This
```
<value> adopted by House <type>
```
Equals this c statement:
```
(<type>) <value>;
```
And is only allow between:
* Values with primitive types and primitive types
* Values whose type is equivalent to a strong alias given as a type. 

## Instructions

### Assignment

```
<l-value> takes <r-value>.
```

### Multiple Assignment

```
<l-value_1>, ... , <l-value_n> fight against <r-value>.
```

### No operation

```
The Three-Eyed Raven watches from afar.
```

### Abort Program

```
The book -- <título> -- has reached an unexpected end.
```

### IO Read

```
The book -- <título> -- has reached an unexpected end.
```

### IO Write

```
We must send a raven with everything we know of <r-value>.
```

### Simple selection

This:
```
If <cond> may be the True King of the Seven Kingdoms, then
    <code>
    [Otherwise, <code>]
And so our fate rests upon this decision.
```
Equals this c code:
```
If (<cond>) { 
    <code> 
} else { 
    <code>
}
```

### Multiple Selection

This: 
```
You will be betrayed by <expr> several times.
Once for <atom_1>. <code_1>
Once for <atom_2>. <code_2>
...
Once for <atom_n>. <code_n>
So the prophecy says.
```

Equals this Haskell case statement:;
```
case <expr> of 
    <atom_1> -> <code_1>
    ...
    <atom_n> -> <code_n>
```

### Determinate repetition
This
```
The things I do for <id> of House Lanninteger.
I would kill from <initial> up to <final>.
    What is dead, may never die.
    This is the Doom of Valyria.
    <code>
That, and much more I would to get her love.
```
Equals this c code:
```
for <id> = <initial> ; <id> < <final> ; <id>++) { 
    continue;
    break;
    <code>
}
```

### Undeterminate repetition

This:
```
While <cond> reigns truly upon the land.
    What is dead, may never die.
    This is the Doom of Valyria.
    <code>
Only for as long as the sovereign lives.
```
Equals this c code:
```
while (<cond>) {
    continue; 
    break;
    <code> 
}
```