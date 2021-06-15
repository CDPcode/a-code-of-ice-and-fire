# Expresiones

En esta sección se listan todas las operaciones
que se pueden realizar sobre los distintos tipos
de datos y su comportamiento esperado.

## Operadores aritméticos

Para enteros y flotantes se soporta nativamente la
suma, resta, multiplicación, división y negación.
Las palabras clave para cada uno de estos operadores
son

* `joined by (+)`
* `left by (-)`
* `? (*)`
* `cut into pieces by (/)`
* `turncloak (- unario)`

Todo se comporta como es esperado, la única
particularidad es que negación es un operador postfijo.
La negación tiene mayor precedencia que todos los demás
operadores. La multiplicación y la división tienen la
misma precedencia y asocian a izquierda.
La suma y la resta tienen la misma precedencia
y asocian a izquierda. La multiplicación y división
tienen mayor precedencia que la suma y la resta.

Además para los enteros existe un operador para hallar el
módulo (es decir, el resto de una división). El módulo
tiene la misma precedencia que la multiplicación y la división
y también asocia a izquierda.

* `stripped of his dignity by (%)`

La división entre enteros es división entera. No se
pueden operar valores enteros con valores flotantes.

## Operadores de comparación

Los operadores de comparación retornan un booleano
y deben ser usados entre expresiones del mismo tipo.
Solo se soporta comparar enteros, booleanos, flotantes
y caracteres. Los operadores de comparación son los siguientes:

* `similar to (==)`
* `different from (/=)`
* `bested by (<)`
* `defeating (>)`
* `almost bested by (<=)`
* `almost defeating (>=)`

Los operadores de comparación tienen menor precedencia que
los operadores aritméticos, todos tienen la misma
precedencia entre sí y no asocian entre ellos.

## Operadores lógicos

Los operadores lógicos soportados son el Y-lógico y el O-lógico.
Son operadores con cortocircuito.

* `and (&&)`
* `or (||)`

## Operadores de indexación

### Sobre arreglos y strings

La sintaxis para indexar arreglos es la siguiente:

```
Soldier acquainted with
    <index_1>,
    ...
    <index_n>
under command of <arreglo>
```
donde *index_i* es una expresión de tipo entero y *arreglo*
es una expresión de tipo arreglo o string. Si la cantidad
de dimensiones del arreglo es *m*, si *n < m*, se retorna
un arreglo de *n - m* dimensiones, si *n = m*, se retorna
el valor correspondiente a la posicion especificada y
si *n > m* se arroja un error.

### Sobre structs

La sintaxis para indexar (acceder a los campos de) structs es

```
<id> subject of <struct>
```
donde *id* es el identificador del campo y *struct* es una
expresión de tipo struct. Este operador asocia a derecha.

### Sobre uniones

La sintaxis para acceder a un campo de una unión es
```
<union> acting as <id>
```
donde *union* es una expresión del tipo unión y
*id* es el identificador del campo al que se quiere acceder.
Se arroja un error si *id* no es el campo activo de la unión.
Este operador asocia a izquierda.

Además se provee el siguiente operador para verificar cuál
es el campo activo de una unión:
```
<union> looking in the mirror at <id>
```
donde *union* e *id* representan lo mismo que en el operador
anterior. Esta operación retorna verdadero si *id* es el
campo activo de *union* o falso de otra forma.

### Sobre tuplas

Para indexar sobre tuplas se utiliza la siguiente sintaxis
```
Wight <número> under command of <tupla>
```
donde *número* es un entero positivo que cumple con la
regex `[0-9]+` y *tupla* es una expresión de tipo tupla.

## Operadores de conversión de tipos

Se puede convertir una expresión de un tipo
a otro utilizando el siguiente operador

```
<valor> adopted by House <tipo>
```
donde *valor* es una expresión y *tipo* es el tipo al
que se desea transformar. Esta operación solo se puede
realizar en dos casos:

* *valor* es de un tipo primitivo y *tipo* es un tipo primitivo.
* *tipo* es un alias fuerte equivalente al tipo de *valor*.

## Evaluación de funciones

Ver sección de subrutinas para ver la sintaxis de invocación
de funciones.
