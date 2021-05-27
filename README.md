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
<Lord|Lady|Knight> <identificador> of House <declarador de tipo>.
```

## Statements

Todas las instrucciones deben ser terminadas con un punto `.`

### Asignación

Para asignar un valor a una variable se debe usar la palabra
clave `take` o `takes`. La sintaxis es la siguiente 
(dependiendo del numero de asignaciones simultaneas):

```
<id> takes <expresion>.
```

```
<lista de ids> take <lista de expresiones> respectively.
```

Así mismo, para inicializar un identificador se utiliza:

```
<Lord|Lady|Knight> <identificador> of house <declarador de tipo> takes <expresion>.
```

Y de forma análoga, la sintaxis se extiende para asignación multiple

```
<lista de declaraciones> take <lista de expresiones> respectively.
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

* Registros:
  Estos representan un tipo que contiene a varios tipos a la vez, como
  un rey gobierna sobre distintas personas:
 `former <Lady|Lord|Knight> now <id> King of <lista de declaraciones>`
 
  Para declararlos se hace *Por definir*
 
* Uniones: 
 Las uniones representan un tipo que puede tomar
 distintos tipos, pero uno a la vez. La sintaxis 
 para declararlos es: 
 `former <Lady|Lord|Knight> now Faceless Man with faces of: <lista de declaraciones serparadas por comas>.` 
 
 Para declararlos se hace *Por definir*

* Arreglos:
 Los arreglos serán de tamaño constante y serán 
 declarados con la sintaxis: 
 `former <Lord|Lady|Knight> now lord commander with [1-9][0-9]+ <tipo> bannermen.`
 Por ejemplo:
 ```
 former Lord now lord commander Jon Arrayn with 42 Starkhar bannermen.
 ```

 Para declararlos se hace: *Por definir*
 
* Strings: 
Serian arreglos de caracteres con sintaxis glorificada:
`former <Lord|Lady|Knight> now hand of the king with [1-9][0-9]+ <tipo> servants.`

 Para declararlos se hace: 
`hand of the king with [1-9][0-9]+ <tipo> servants checks scroll <string>`
 
 
* Apuntadores (solamente al heap): 
 Los apuntadores solamente pueden apuntar a espacios
 de memoria en el heap y deben ser reservados y
 liberados explícitamente. La sintaxis para 
 declararlos es `former <Lady> now Spearwife of <tipo al que apunta>`

* Tuplas:
  Corresponden a Registros sin nombre


### Operadores

Los operadores que se manejaran sobre los tipos de datos existentes seran

#### Sobre enteros

La mayoria de los operadores cuentan con la siguiente firma `Lanninteger -> Lanninteger -> Lanninteger`

+ with :(+)
+ without: (-)
+ times the power of: (*)
+ divided by: (/)
+ picking what remains of: (%)
+ negated : (-)
+ is as powerfull as : (==)
+ not merely powerfull as : (/=)
+ is weaker than : (<)
+ is stronger than : (>) 
+ is almost as weaker than : (<=)
+ is almost as stronger than : (>=) 


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
 So the prophecy says
```

Se ejecutará el bloque de código correspondiente al valor de
la expresión `Boolton`

## Repeticion determinada

La repetición imita un *loop* `for` tradicional, con ciertas sutilezas:

```
the things I do for <declaracion de tipo Lanninteger> from <expresion de tipo Lanninteger> until <expresion de tipo Lanninteger>
  Valar Morghules
    ..
    Code
    ..
  Valar Dohaeris
```

## Repetición indeterminada

```
while <expresion de tipo Boolton> flowed down the river
  Valar Morghules
    ..
    Code
    ..
  Valar Dohaeris
```

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

La declaración de las subrutinas se dará de la siguiente manera:

```
<Nombre de la subrutina> 
  Hereby I introduce the honorable
    <Identificador> of House <Tipo>,
      ... ,
    <Identificador> of House <Tipo>
  I must warn you, <Tipo de retorno> is coming
  Valar Morghulis
    ..
    Code
    ..
  Valar Dohaeris
```
Para funciones, y
```
<Nombre de la subrutina> 
  Hereby I introduce the honorable
    <Identificador> of House <Tipo> ,
      ... ,
    <Identificador> of House <Tipo>
  What do we say to the lord of death? not today
  Valar Morghulis
    ..
    Code
    ..
  Valar Dohaeris
```
Para procedimientos


Para ejecutar las subrutinas se sigue la siguiente sintaxis
```
<lista de identificadores> fight against <nombre subrutina> [alongside arg1, arg2, ...]
```

Las variables a la izquierda del `fight against` reciben los 
valores retornados por la subrutina.

En caso de que la subrutina no tenga ningun valor de retorno
se debe usar `Nobody` en lugar de la lista de
identificadores. Si se retorna 0 ó 1 valor, se debe usar la
palabra `fights` en lugar de `fight`.

Dado que se manejan tuplas, se cuenta con retorno multivalor.


### Flujo no estructurado


#### Return

```
<Nombre de la subrutina> 
  Hereby I introduce the honorable
    <Identificador> of House <Tipo>,
      ... ,
    <Identificador> of House <Tipo>
  I must warn you, <Tipo de retorno> is coming
  Valar Morghulis
    ..
    Code
    ..
    Dracarys <valor de retorno>
  Valar Dohaeris
```

#### Continue

```
the things I do for <declaracion de tipo Lanninteger> from <expresion de tipo Lanninteger> until <expresion de tipo Lanninteger>
  Valar Morghules
    ..
    Code
    ..
    What is dead may never die.
    ..
    Code
    ..
  Valar Dohaeris
```

#### Break

```
while <expresion de tipo Boolton> flowed down the river
  Valar Morghules
    ..
    Code
    ..
    This is the doom of Valyria
    ..
    Code 
    ..
  Valar Dohaeris
```

--- 

Ejemplo de programa:


### Programa para determinar un numero de la suerte
```
A Song of Ice and Fire: Lucky number

Table of Contents:
- Prologue
- Arya III
- Epilogue

Arya III 
  Hereby I introduce the honorable
    Ramsay of house Boolton,
    Jamie of house Lanninteger
  I must warn you, Lanninteger is coming
  Valar Morghulis
  
    You will be betrayed by Ramsey three times.
      Once for love:
        Dracarys Jamie!.
      Once for gold:
        Dracarys 42!.
      Once for blood:
        Dracarys 69!.
      So the prophecy Says
        
  Valar Dohaeris

Prologue
  Valar Morghulis
    Knigth Tyrion of house Lanninteger takes 42 golden dragon.
    Lady Cersei of house Lanninteger.
    Loord Roose of house Boolton takes love.

    Cersei Lanninteger fights against Arya alongside Tyrion and Roose.
  Valar Dohaeris
```

### Fibonacci

```
A song of ice and fire: Fibonacci

Table of contents:
  - Prologue
  - Fibonacci I
  - Epilogue
  
Fibonaci I
  Hereby I introduce the honorable
    Jamie of house Lanninteger
  I must warn you, Lanninteger is coming
  Valar Morghules
    
    You will be betrayed by Jamie is as powerfull as 1 three times.
      Once for love:
        Dracarys 1!.
      Once for gold:
        Dracarys Jamie with fights against Fibonaci alongside (Jamie without 1) !.
      Once for blood:
        <undefined>
      So the prophecy Says
        
  Valar Dohaeris
  
  Prologue
    Valar Morghules
      Lord Tyrion of house Lanninteger takes 3 golden dragons.
      Lord Tywin of house Lanninteger fights against Fibonnaci alongside Tyrion.
    Valar Dohaeris
  
```
