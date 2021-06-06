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
A Song of Ice and Fire: "<nombre del programa>"
```

Dicho nombre de programa debe cumplir con que es palabras que cumplan con la siguiente
regex `[A-Z][a-z]*` separadas por espacios. Dicho nombre del programa podrá ser referenciado
luego para abortar la ejecución del mismo.  

## Comentarios

Al escribir alguna de las siguientes expresiones se inicia un comentario y se ignorarán todos
los caracteres hasta el siguiente punto `.`:

+ `Suddenly,`
+ `In the midst of`
+ `Therefore`

Los comentarios no se anidan. No puede haber comentarios en medio de una instrucción. Solo
se permite introducir comentarios entre distintas instrucciones. La idea de estas reglas
es poder rellenar los espacios entre las instrucciones con partes de la historia para 
hacerla más interesante.

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
Cersei Lanninteger takes 7 soldiers. 
Tyrion Lanninteger, Jamie Lanninteger take 5 soldiers, Cersei Lanninteger respectively.
``` 

### Asignacion desenvolviendo tuplas

Tambien se puede asignar la siguiente sintaxis para extraer los valores de una tupla:

```
<lista de ids> fight against <expresion de tipo tupla>
```

El tamaño de la lista de ids debe ser igual al tamaño de la tupla y los tipos deben 
corresponder. Esta sintaxis es particularmente util cuando se trata con funciones.

### Abortar programa

En cualquier punto del programa se puede abortar con la siguiente instrucción. 
```
The book <nombre del programa> has reaced an unexpected end.
```

### IO 

Para leer valores del standard input se usa la siguiente sintaxis. 
```
A raven has come for <l-value>.
```

Para imprimir valores al standard output se usa la siguiente sintaxis.
```
We must send a raven with everything we know of <r-value>. 
```

### Instrucción vacía

Podemos escribir una instrucción vacía que no hace nada (similar a un `pass` en Python).
Para esto podemos usar la siguiente sintaxis: 
```
The three-eyed raven watches from afar. 
```

*Sintaxis por definir*

## Sistema de tipos

A code of Ice and Fire es fuertemente tipado. Incorpora características como
* Alias fuertes y débiles
* Soporte para definición de constantes y variables

### Basicos

Maneja 5 tipos basicos a saber:

* Enteros (tamaño de la palabra del sistema): Se usan enteros complemento a 2.
 La palabra clave para declarar un entero es `Lanninteger`.
 Para usar enteros literales se debe escribir el número seguido de `soldiers`
referenciando la moneda de mayor valor en Westeros. Por ejemplo `10 soldiers`.

* Numeros de coma flotante (IEEE 754 - 2019):
 El nombre de este tipo `Freyt`. Para usar flotantes literales
 se debe escribir el número seguido de `descendants`. Por
 ejemplo `3.1416 descendants`.

* Trilleanos: El nombre de este tipo es `Boolton`
y puede tomar únicamente 3 valores, `blood`, `gold`,
`love`, que representan valores de verdad, 
neutralidad y falsedad respectivamente.

* Caracteres (UTF - 8):
El nombre de este tipo es `Starkhar` y para escribir
un caracter literal se debe escribir la palabra
`Hodor` seguida del caracter deseado encerrado entre comillas simples `'`. Por ejemplo:
`Hodor 'λ'`.

*Sintaxis e implementación por definir*

### Compuestos

Maneja 6 tipos compuestos a saber

* Registros:
  Estos representan un tipo que contiene a varios tipos a la vez, como
  un rey gobierna sobre distintas personas:
 ```
 Former <Lady|Lord|Knight> <id> now King of <lista de declaraciones>
 ```
 
 
* Uniones: 
 Las uniones representan un tipo que puede tomar
 distintos tipos, pero uno a la vez. La sintaxis 
 para declararlos es: 
 ```
 Former <Lady|Lord|Knight> <id> now Faceless Man holding faces of <lista de declaraciones serparadas por comas>.
 ```
 

* Arreglos:
 Los arreglos serán de tamaño constante y serán 
 declarados con la sintaxis: 
 ```
 Former <Lord|Lady|Knight> <id> now Lord Commander of <tipo> bannermen holding <expresion entera>.
 ```
 Por ejemplo:
 ```
 Former Knight Jon now Lord Commander of Starkhar bannermen holding 42 soliders.
 ```

 Tambien se puede usar la siguiente sintaxis si no se desea especificar el tamaño del arreglo y
 se le asigna un valor al ser declarado
```
Former <Lord|Lady|Knight> <id> now Lord Commander of <tipo> bannermen holding: <lista de valores>
```

Por ejemplo:
```
Former Knight Barristan now Lord Commander of Lanninteger bannermen holding: Tyrion, 3 soldiers, Jamie with Cersei.
```
 
* Strings: 
Serian arreglos de caracteres con sintaxis glorificada:
```
Former <Lord|Lady|Knight> <id> now Hand of the King ruling over <expresion entera>.
```

Por ejemplo:
```
Former Lord Eddard now Hand of the King ruling over 7 soldiers.
```

 Tambien se puede usar la siguiente sintaxis si no se desea especificar el tamaño del string y
 se le asigna un valor al ser declarado

```
Former <Lord|Lady|Knight> <id> now Hand of the King ruling with Grand Maester reading "<string>"
```

Por ejemplo:
```
Former Lord Jon now Hand of the King ruling with Grand Maester reading "The Book of Ancient Kings and Queens of Westeros". 
```

Para declarar strings literales debemos usar la siguiente 
sintaxis `Maester reading "<string>"`. 
 
* Apuntadores (solamente al heap): 
 Los apuntadores solamente pueden apuntar a espacios
 de memoria en el heap y deben ser reservados y
 liberados explícitamente. La sintaxis para 
 declararlos es 
 ```
 Former <Lady> <id> now Spearwife of <tipo al que apunta>
 ```

* Tuplas:
  Corresponden a Registros sin nombre. Se declaran con la siguiente sintaxis
  ```
  Former <Lord|Lady|Knight> <id> now White Walker with deads from Houses <lista de tipos>
  ``` 

Por ejemplo: 
```
Former Lord Tywin now White Walker with wights from Houses Lanninteger, Starkhar, Spearwife of Lanninteger.
```

### Aliases Fuertes

Para declarar un alias fuerte (equivalente al `newtype` en Haskell) se debe usar la siguiente
sintaxis:
```
House <id de tipo nuevo> comes from the old lineage of <tipo>.
```

### Aliases Débiles

Para declarar un alias débil (equivalente al `type` en Haskell) se debe usar la siguiente sintaxis:
```
House <id de tipo nuevo> are the dogs of <tipo>
```

### Operadores

Los operadores que se manejaran sobre los tipos de datos existentes seran

#### Sobre enteros

+ with :(+)
+ without: (-)
+ times the power of: (*)
+ divided by: (/)
+ picking what remains of: (%)
+ bastard : (-)
+ is as powerful as : (==)
+ not merely powerful as : (/=)
+ is weaker than : (<)
+ is stronger than : (>) 
+ is almost as weak as : (<=)
+ is almost as strong as : (>=) 

#### Sobre flotantes

+ with :(+)
+ without: (-)
+ times the power of: (*)
+ divided by: (/)
+ bastard : (-)
+ is as powerful as : (==)
+ not merely powerful as : (/=)
+ is weaker than : (<)
+ is stronger than : (>) 
+ is almost as weak as : (<=)
+ is almost as strong as : (>=) 

#### Sobre trileanos

Se manejan los operadores `flayed`, `and`, `or`, `equals`, `differentiates`.

##### Tabla de Valores para el Operador *flayed*

| `flayed` |         |
|---------|---------|
| **blood**     | love   |
| **gold** | gold |
| **love**   | blood     |

##### Tabla de Valores para el Operador *and*

| `and` | blood     | gold | love   |
|---------|---------|---------|---------|
| **blood**     | blood     | gold   | love   |
| **gold** | gold   | gold     | gold   |
| **love**  | love   | gold   | love     |

##### Tabla de Valores para el Operador *or*

| `or`  | blood     | gold | love   |
|---------|---------|---------|---------|
| **blood**   | blood     | blood   | blood   |
| **gold** | blood   | gold     | gold   |
| **love**   | blood   | gold   | love     |

##### Tabla de Valores para el Operador *equals*

| `eq`  | blood     | gold | love   |
|---------|---------|---------|---------|
| **blood**   | blood | gold  | love  |
| **gold**    | gold  | blood | gold  |
| **love**    | love  | gold  | blood |

##### Tabla de Valores para el Operador *differentiates*

| `differentiates` | blood     | gold | love   |
|---------|---------|---------|---------|
| **blood**     | love   | gold  | blood |
| **gold**      | gold   | love  | gold  |
| **love**      | blood  | gold  | love  |

#### Sobre registros

Para acceder a un campo de un registro se utiliza la siguiente 
sintaxis: 
```
<id del campo> subject of <id del registro>
```

#### Sobre uniones

Para verificar cual campo de la union está activo en este momento
se usa la siguiente sintaxis: 
```
Is <id de la union> using the face of <id del campo>?
```

Esto retorna un valor de tipo `Boolton` que sería `blood` en caso de ser
verdad, `love` en caso de ser falso y `gold` si no ha sido inicializado.

Para acceder a un campo de una union se utiliza la siguiente sintaxis:
```
<id de la unión> acting as <id del campo>
```

#### Sobre arreglos 

Para indexar un arreglo se utiliza la siguiente sintaxis:
```
Soldier acquainted with <expresion numerica> under command of <id del arreglo>
```

#### Sobre strings 

Para indexar un string se utiliza la siguiente sintaxis:
```
Soldier acquainted with <expresion numerica> under command of <id del string>
```

#### Sobre tuplas 

Para indexar una tupla se utiliza la siguiente sintaxis:
```
Wight following <literal entero> under command of <id de la tupla>
```

#### Sobre apuntadores

Para crear un apuntador a algun tipo se debe usar la siguiente sintaxis:
```
<id de apuntador> marries a <tipo al que apunta>
```

Para dereferenciar un apuntador se utiliza la siguiente sintaxis:
```
Spouse of <id del apuntador>
```

Para liberar un espacio de memoria apuntado se utiliza la siguiente sintaxis:
```
<id del apuntador> forsakes marriage
```

## Selección

La selección se realiza con los valores de tipo
`Boolton` y su sintaxis es la siguiente. 

```
You will be betrayed by <Boolton expression> three times.
 Once for blood:
   [bloque de código]
 Once for gold:
   [bloque de código]
 Once for love:
   [bloque de código]
 So the prophecy says
```

Se ejecutará el bloque de código correspondiente al valor de
la expresión `Boolton`. El bloque de codigo es opcional. 

## Repeticion determinada

La repetición imita un *loop* `for` tradicional, con ciertas sutilezas:

```
The things I do for <declaracion de tipo Lanninteger> from <expresion de tipo Lanninteger> until <expresion de tipo Lanninteger>
  Valar Morghules
    ..
    Code
    ..
  Valar Dohaeris
```

## Repetición indeterminada

```
While <expresion de tipo Boolton> flows down the river
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
  Hereby I introduce the
    <Valued|Honorable> <Identificador> of House <Tipo>,
      ... ,
    <Valued|Honorable> <Identificador> of House <Tipo>
  I must warn you, <Lista de tipos de retorno> is coming
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
    <Valued|Honorable> <Identificador> of House <Tipo> ,
      ... ,
    <Valued|Honorable> <Identificador> of House <Tipo>
  I must warn you, Nobody is coming
  Valar Morghulis
    ..
    Code
    ..
  Valar Dohaeris
```
Para procedimientos

La palabra clave `Valued` se utliza para pasar un parametro por valor, mientras que la palabra clave `Honorable` se utiliza para pasar un parametro por referencia.

Para ejecutar las subrutinas se sigue la siguiente sintaxis
```
<nombre subrutina> traveling [alongside arg1, arg2, ...] with caution
```

Todas las funciones retornan una tupla con los valores especificados en el 
retorno. 


### Flujo no estructurado


#### Return

```
<Nombre de la subrutina> 
  Hereby I introduce the honorable
    <Identificador> of House <Tipo>,
      ... ,
    <Identificador> of House <Tipo>
  I must warn you, <Lista de tipos de retorno> is coming
  Valar Morghulis
    ..
    Code
    ..
    Dracarys <lista de valores de retorno>
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
*los siguientes programas estan desactualizados*


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
      Once for blood:
        Dracarys 69!.
      Once for gold:
        Dracarys 42!.
      Once for love:
        Dracarys Jamie!.
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
    Jamie of House Lanninteger
  I must warn you, Lanninteger is coming
  Valar Morghules
    
    You will be betrayed by Jamie is as powerfull as 1 three times.
      Once for blood:
        Dracarys 1!.
      Once for gold:
        The Three-Eyed raven watches from afar.
      Once for love:
        Lord Tyrion of House Lanninteger fights against Fibonaci alongside Jamie without 1 golden dragon.
        Lady Cersei of House Lanninteger fights against Fibonaci alongside Jamie without 2 golden dragon.
        Dracarys Tyrion with Cersei.
      So the prophecy Says
        
  Valar Dohaeris
  
  Prologue
    Valar Morghules
      Lord Tyrion of house Lanninteger takes 3 soldiers.
      Lord Tywin of house Lanninteger fights against Fibonnaci alongside Tyrion.
    Valar Dohaeris
  
```
