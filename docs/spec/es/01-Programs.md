# Estructura de los programas

Todos los *programas de hielo y fuego* deben contar con la
siguiente estructura:

```
[Declaración del programa]

[Declaración de las subrutinas]

[Definición de las subrutinas]

[Declaración de alias de tipo]
```

## Declaración del programa

La declaración del programa consta de la siguiente
sintaxis:

```
A Song of Ice and Fire: -- <título> --
```

Donde *título* debe constar de palabras capitalizadas
separadas por exactamente un espacio simple.
Es decir, *título* debe cumplir con la siguiente
expresión regular `[A-Z][a-z]*([ ][A-Z][a-z]*)*`.

Dicho *título* representa el título del libro que
se está escribiendo y puede ser utilizado para
finalizar la ejecución del programa en algún momento
deseado con la instrucción:

```
The book -- <título> -- has reached an unxepected end.
```

Para más información, ver la sección de instrucciónes.

## Declaración de subrutinas

En esta sección se debe utilizar la siguiente sintaxis
para declarar subrutinas:

```
Table of Contents:
- Prologue
- <id_1> <args_1>
- <id_2> <args_2>
...
- <id_n> <args_n>
- Epilogue
```

donde `id_i` debe ser un identificador válido
(más información en sección de identificadores) y `args_i`
debe ser un número escrito en números romanos que representa
la cantidad de argumentos (+1) que recibe la subrutina.
Es importante notar que para declarar una subrutina que
no recibe argumentos se debe usar `I` como número de
argumentos, si se quiere que se reciba 1 argumento,
se debe usar `II`, y así sucesivamente.

Las subrutinas con el mismo identificador deben estar ordenadas
por el número de argumentos que reciben. Por ejemplo, la
subrutina `Arya IV` no puede ser declarada antes que la
subrutina `Arya II` si ambas existen.

Las subrutinas especiales `Prologue` y `Epilogue` se explican
más a profundidad en la sección de subrutinas. `Prologue`
corresponde a un bloque para declarar variables en el alcance
global, mientras que `Epilogue` contiene la función `main` del
programa.

## Definición de subrutinas

En esta sección se define el comportamiento de cada una de
las subrutinas declaradas anteriormente. Ver la sección
de subrutinas para más información sobre la sintáxis.

## Declaración de alias de tipo

En esta sección se declaran y definen cero o más aliases
de tipos que serán usados en el programa. No se puede
declarar aliases de tipo fuera de esta sección. La sintaxis
es la siguiente:

```
Appendix:
* <alias_1>
* <alias_2>
...
* <alias_n>
```

donde `alias_i` es una declaración de alias válida. Para más
información ver la sección de alias.
