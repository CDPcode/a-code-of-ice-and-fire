# A-code-of-ice-and-fire


Lenguaje de programación basado en las novelas *"A song of Ice and Fire"*, para la cadena de Lenguajes de Programación (Lenguajes II, Lenguajes III)

### Integrantes

César Alfonso Rosario Escobar (15-11295)

Marcos José Lerones Ramírez (15-10778)

Jesús Raúl de Aguiar de Brito (15-10360)

### Overview

**A Code of Ice and Fire** es un lenguaje esotérico inspirado en la saga de libros escrita 
por George RR Martin *A Song of Ice and Fire*.

De manera similar al [lenguaje de programacion Shakespeare](https://en.wikipedia.org/wiki/Shakespeare_Programming_Language) 
se espera que los *desarrolladores de hielo y fuego* tengan presente que además de programar, 
están contando una historia. Como no todas las historias son buenas, habrán reglas para 
orientar al programador en su camino del héroe y con suerte convertirlo en el monarca de los 7 reinos.

---

# Especificacion

*A work in progress*

## Programa

Todo programa escrito en **A Code of Ice and Fire** debe comenzar con una línea de la 
siguiente forma: 

```
A Song of Ice and Fire: <nombre del programa>
```

Dicho nombre de programa debe cumplir con que es palabras que cumplan con la siguiente
regex `[A-Z][a-z]*` separadas por espacios. Dicho nombre no tiene ningún efecto en el 
programa y será ignorado durante la ejecución. 

## Identificadores

Los identificadores válidos de hielo y fuego son aquellos que comienzan con una letra 
mayúscula, y esta formado posteriormente por letras minúsculas o comillas simples (no continuas) y
que no formen parte de las palabras reservadas del lenguaje o algun alias de tipo.
La intención de estos identificadores es que sean nombres propios de la saga *A Song of Ice and Fire*.

Formalmente, toda palabra que cumpla con la siguiente expresión `[A-Z]([']{0,1}[a-z]+)+`
y que no sea una palabra reservada del lenguaje.

## Declaradores

Existen tres declaradores para los tipos de datos:
* `Lord` y `Lady` para declarar identificadores cuyos valores que no pueden ser reasignados.
* `Knight` para declarar identificadores cuyos valores pueden ser reasignados

La sintaxis para declaración es:

```
<Lord|Lady|Knight> <identificador> of House <declarador de tipo>
```

## Statements

Todas las instrucciones deben ser terminadas con un punto `.`

### Asignación

Para asignar un valor a una variable se debe usar la palabra
clave `take` o `takes`. La sintaxis es la siguiente 
(dependiendo del numero de asignaciones simultaneas):

```
<id> takes <expresion>
```

```
<lista de ids> take <lista de expresiones> respectively.
```


Por ejemplo: 
```
Cersei Lanninteger takes 7 golden dragons. 
Tyrion Lanninteger, Jamie Lanninteger take 5 golden dragons, Cersei Lanninteger respectively.
```

*Sintaxis por definir*

## Sistema de tipos

A code of Ice and Fire es fuertemente tipado. Incorpora características como
* Alias fuertes y débiles
* Soporte para definición de constantes y variables

### Basicos

Maneja 5 tipos basicos a saber:

* Enteros (tamaño de la palabra del sistema): 
 La palabra clave para declarar un entero es `Lanninteger`.
 Para usar enteros literales se debe escribir el número seguido de `golden dragons`
referenciando la moneda de mayor valor en Westeros. Por ejemplo `10 golden dragons`.

* Numeros de coma flotante (IEEE 754 - 2019):
    *por definir*

* Trilleanos: El nombre de este tipo es `Boolton`
y puede tomar únicamente 3 valores, `love`, `gold`,
`blood`, que representan valores de verdad, 
neutralidad y falsedad respectivamente (*propenso a cambio*).

* Caracteres (UTF - 8):
El nombre de este tipo es `Starkhar` y para escribir
un caracter literal se debe escribir la palabra
`rune` seguida del caracter deseado. Por ejemplo:
`rune λ`.

*Sintaxis e implementación por definir*

### Compuestos

Maneja 6 tipos compuestos a saber

* Registros

* Uniones: 
 Las uniones representan un tipo que puede tomar
 distintos tipos, pero uno a la vez. La sintaxis 
 para declararlos es 
 `Faceless Man with faces of: <lista de declaraciones serparadas por comas>` 

* Arreglos:
 Los arreglos serán de tamaño constante y serán 
 declarados con la sintaxis 
 `Arrayn with [1-9][0-9]+ <tipo> bannermen`.
 Por ejemplo:
 ```
 Knigth Jon Arrayn with 42 Starkhar bannermen
 ```

* Strings

* Apuntadores (solamente al heap): 
 Los apuntadores solamente pueden apuntar a espacios
 de memoria en el heap y deben ser reservados y
 liberados explícitamente. La sintaxis para 
 declararlos es `Spearwife of <tipo al que apunta>`

* Tuplas

*Sintaxis e implementación por definir*

### Operadores

Los operadores que se manejaran sobre los tipos de datos existentes seran: .. 

## Selección

La selección se realiza con los valores de tipo
`Boolton` y su sintaxis es la siguiente. 

```
You will be betrayed by <Boolton expression> three times.
Once for love:
<bloque de código>
Once for gold:
<bloque de código>
Once for blood:
<bloque de código>
```

Se ejecutará el bloque de código correspondiente al valor de
la expresión `Boolton`

*Sintaxis e implementación por definir*

## Repetición

*Sintaxis e implementación por definir*

### Determinada

*Sintaxis e implementación por definir*

### Indeterminada

*Sintaxis e implementacion por definir*

### Subrutinas

Las subrutinas se declaran al comienzo del programa con la siguiente sintaxis: 
```
Table of Contents:
- Prologue
- <nombre de la subrutina> <número de argumentos que recibe + 1>
- Epilogue
```

Las subrutina `Prologue` y `Epilogue` corresponden 
la función `main` del programa. Se ejecuta primero 
`Prologue` y luego `Epilogue`. 

El nombre de la subrutina debe ser un identificador
válido.
El número de argumentos que recibe (+1) debe ser
escrito en notación romana. 
Se puede tener subrutinas con el mismo nombre
siempre que reciban una cantidad distinta de 
argumentos y en la tabla de contenidos se encuentren
ordenados (`Arya IV` no puede estar antes de 
`Arya II`).

Por ejemplo:
```
Table of Contents:
- Prologue
- Arya I
- Jon III
- Arya II
- Jamie V
- Jon IV
- Epilogue
```

Dado que se manejan tuplas, se cuenta con retorno multivalor.

Para ejecutar las subrutinas se sigue la siguiente sintaxis
```
<lista de identificadores> fight against <nombre subrutina> [with arg1, arg2, ...]
```

Las variables a la izquierda del `fight against` reciben los 
valores retornados por la subrutina.

En caso de que la subrutina no tenga ningun valor de retorno
se debe usar `Nobody` en lugar de la lista de
identificadores. Si se retorna 0 ó 1 valor, se debe usar la
palabra `fights` en lugar de `fight`.

*Sintaxis e implementación por definir*
