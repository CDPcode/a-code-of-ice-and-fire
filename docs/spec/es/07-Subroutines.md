# Subrutinas

Las subrutinas (funciones y procedimientos) son de
tercera clase, es decir, solo pueden ser invocadas,
no pueden ser retornadas ni pasadas como parámetros.

Las subrutinas cuentan con retorno multivalor, en caso
de retornar más de un valor, se tomará como azúcar sintáctica
para expresar que se esta retornando una tupla con los valores
especificados.

La palabra clave `No One` se utiliza como place-holder
para especificar subrutinas que no reciben argumentos,
no retornan valores, o para hacer llamadas a estas.

## Declaración

Toda subrutina debe ser declarada en la sección
`Table of Contents` de la forma especificada
en la sección de *Programas*.

## Definición

Para definir una subrutina se utiliza la siguiente sintáxis
```
<id>
    watches
        <Valued|Honorable> <id_1> <tipo_1>,
        ...
        <Valued|Honorable> <id_n> <tipo_n>
    approach from a distance;
    I must warn you,
        <tipo_ret_1>,
        ...
        <tipo_ret_m>
    is coming.
Valar Morghules.
    <code>
Valar Dohaeres.
```

donde *id* es un identificador válido y representa el nombre
de la subrutina, *id_i* son identificadores válidos
y representan los nombres de los argumentos que recibe la
subrutina, *tipo_i* son los tipos de los argumentos
que recibe la función, *tipo_ret_i* son los tipos
que retorna la función y *code* representa una serie
de instrucciones que se ejecuntan al invocar a la
subrutina. Las palabras clave `Honorable` y `Valued`
representan si el argumento es pasado por referencia
o por valor respectivamente.

En caso de no recibir ningún argumento se coloca `No One`
en lugar de la lista de argumentos y en caso de no retornar
ningun valor se coloca `No One` en lugar de la lista
de tipos de retorno.

## Invocación

Para invocar subrutinas se utiliza la siguiente sintáxis:
```
<id> traveling alongside <arg_1>, ... <arg_n> with caution
```
donde *id* es el identificador de la subrutina y
*arg_i* son expresiones que evalúan al tipo requerido
por el i-ésimo argumento que espera la subrutina.
En caso de no recibir argumentos, se utiliza `No One`
en lugar de la lista de argumentos.

El retorno de una subrutina es un r-value y puede ser asignado
a variables o utilizado en alguna expresión. En caso de que
se desee ignorar el retorno de una subrutina (o que la misma
no tenga valor de retorno) se puede utilizar la siguiente
sintáxis
```
No One fights against <invocación>
```
donde *invocación* es una invocacón a la subrutina.

## Subrutinas especiales

El lenguaje posee dos subrutinas especiales, `Prologue` y `Epilogue`.

### Prologue 

Esta es un espacio reservado para realizar únicamente declaraciones de 
alcance global en el programa. Cualquiera de las constantes o variables 
declaradas en este scope podrán ser utilizadas en cualquier otro lugar del 
código. 

No debe recibir ningún parámetro ni retornar ningún valor. De la misma
manera, la subrutina no pude ser llamada en ningún espacio del programa. Las
declaraciones se ejecutarán antes del epilogo y justo después de las
declaraciones de aliaces de tipos realizadas en el apéndice. Puede consistir
en un bloque vacío.

### Epilogue

Esta es la función principal de cada programa. Consite en el punto donde se inicia
la ejecución del programa al terminar las declaraciones del prólogo y la declaración 
de aliases de tipo en el apéndice (en caso de existir alguna). No debe recibir
argumentos y tampoco debe retornar valores.
