# Instrucciones

## Asignación

Se asigna a una expresión con l-value una expresión con r-value.
La sintaxis es la siguiente:
```
<l-value> takes <r-value>.
```
donde *l-value* es una expresión con l-value y *r-value*
es una expresión con *r-value*.

## Asignación Múltiple

Se le asigna a una lista de expresiones con l-value los valores
de una expresión de tipo tupla con r-value.
```
<l-value_1>, ... , <l-value_n> fight against <r-value>.
```
donde *l-value_i* son expresiones con l-value y *r-value*
es una expresion de tipo tupla con r-value. Los tipos deben
coincidir, es decir, el tipo de *l-value_i* debe coincidir
con el tipo del i-ésimo campo de la tupla.

## No Operation

Se puede tener una instrucción vacía que no realiza ninguna
operación. La sintaxis de dicha instrucción es:

```
The Three-Eyed Raven watches from afar.
```

## Abortar Programa

Para abortar un programa, se tiene la siguiente instrucción

```
The book -- <título> -- has reached an unexpected end.
```
donde *título* es el nombre del programa asignado al inicio
del mismo. Al ser ejecutada esta instrucción se finaliza
la ejecución del programa.

## IO

### Lectura

Para leer valores del standard input se utiliza la siguiente
sintaxis:
```
A raven has come for <l-value>.
```
donde *l-value* es una expresión con l-value.

### Escritura

Para imprimir valores al standard output se utiliza la siguiente
sintaxis:

```
We must send a raven with everything we know of <r-value>.
```
donde *r-value* es una expresión con r-value.

## Selección

### Simple

Para tener selección simple (if-else) se utiliza la siguiente
sintáxis:
```
If <cond> may be the True King of the Seven Kingdoms, then
    <if-block>
[Otherwise, <else-block>]
And so our fate rests upon this decision.
```
donde *cond* es una expresión booleana con r-value, e *if-block*
y *else-block* son bloques con 1 o más instrucciones.
*if-block* es el bloque a ejecutar en caso de que *cond*
evalue a verdadero, y *else-block* se ejecuta en caso de
que evalue a falso. El bloque de *else* es opcional y se
puede omitir, en caso de ser omitido, no se tomará ninguna
acción en caso de que sea falso.

### Múltiple

La selección múltiple se lleva a cabo con el tipo átomo.
Se compara una expresión de tipo átomo con varios literales
y se ejecuta el bloque de código correspondiente. Solo
se ejecuta el primer bloque de código cuyo valor coincida.
Su sintaxis es la siguiente:

```
You will be betrayed by <expr> several times.
Once for <atom_1>. <code_1>
Once for <atom_2>. <code_2>
...
Once for <atom_n>. <code_n>
So the prophecy says.
```
donde *expr* es una expresión de tipo átomo,
*atom_i* es un átomo literal y *code_i* son 1 o más
instrucciones.

La palabra clave especial `nothing` sirve para hacer match
con cualquier átomo. Es decir, una vez qeu se llegue al
bloque `Once for nothing.` se ejecutará dicho bloque. Se
recomienda ponerlo al final.

## Memoria

### Alocación

Para reservar un espacio de memoria se debe tener
un apuntador, y se reservará la cantidad de memoria
que ocupa el tipo apuntado. La sintaxis de la
instrucción de alocación es la siguiente

```
<pointer> marries.
```
donde *pointer* es una expresión de tipo apuntador
con l-value. En caso de que el apuntador ya apunte
a algo antes de llamar esta instrucción, entonces
es equivialente a un No-Op.

### Liberación

La sintaxis para liberar memoria es la siguiente:
```
<pointer> becomes widowed.
```
donde *pointer* es una expresión de tipo apuntador
con l-value. Despues de la operación *pointer* tomará
el valor del apuntador nulo. Si ya evaluaba al
apuntador nulo, es equivalente a un No-Op.

## Repetición

### Determinada

La repetición determinada es un bucle for que itera desde
un valor entero hasta otro valor en incrementos de 1.
Se itera desde el valor inicial (inclusivo) hasta el valor
final (exclusivo). La variable de iteración NO tiene l-value,
no puede ser modificada ya que esto haría que la repetición
fuera indeterminada. La sintaxis de la repeticion determinada
es:

```
The things I do for <id> of House Lanninteger.
I would kill from <inicial> up to <final>.
    <code>
That, and much more I would to get her love.
```
donde *id* es el identificador de la variable de iteración,
*inicial* y *final* son los valores iniciales y finales
respectivamente y *code* es el bloque de código a
ser repetido en el bucle.

### Indeterminada

La repetición indeterminada es un bucle while que itera
mientras se cumpla una condición booleana. La sintaxis
es la siguiente:

```
While <cond> reigns truly upon the land.
    <code>
Only for as long as the sovereign lives.
```
donde *cond* es una condición booleana y *code* es el
bloque de codigo a ser ejecutado en el bucle.

## Instrucciones especiales

### Continue

Esta instrucción se utiliza para omitir el resto de la iteración
actual en medio de un bucle (determinado o indeterminado).
Su sintaxis es:
```
What is dead, may never die.
```

### Break

Esta instrucción se utiliza a salir de algun bucle. Se cancela
la iteración actual y no se itera más. La sintaxis para
esta instrucción es:
```
This is the Doom of Valyria.
```

### Return

Esta instrucción se utiliza en las subrutinas para retornar
los valores deseados. Su sintaxis es la siguietne:
```
Dracarys <val_1>, ... <val_n>!
```
donde *val_i* es una expresión con r-value del tipo correcto.
En caso de no retornar ningun valor se puede utlizar
únicamente
```
Dracarys!
```

Si una subrutina que retorne valores
no encuentra una instrucción de retorno en su ejecución
se arrojará un error y terminará la ejecución del programa.
