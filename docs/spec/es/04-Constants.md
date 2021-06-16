# Constantes

En el lenguaje se pueden declarar constantes con las
palabras clave `Lady` y `Lord`. Es indistinto cuál
de las dos palabras se use, pero se busca que el
desarrollador haga coincidir la palabra clave
con el género del nombre propio usado como
identificador. A las constantes se les debe
asignar un valor al momento de ser declarardas.

La sintaxis para declarar constantes es la siguiente
```
<Lord|Lady> <id> of House <tipo> hosts a feast for <valor>.
```
donde *id* es un identificador válido, *tipo* es un tipo
primitivo o un alias válido y *valor* es una expresión
que evalúa al tipo *tipo*.

Para los tipos compuestos, la sintaxis es ligeramente
distinta:
```
Former <Lord|Lady> <id> now <tipo> hosts a feast for <valor>.
```
donde *tipo* es un tipo compuesto (struct, arreglo, union,
string o tupla).

A las constantes no se les puede asignar un valor despues de
declaradas. En caso de los tipos compuestos, se arrojará
un error si se intenta modificar el valor de alguno de
sus campos.

El único tipo del que no se pueden declarar constantes son
los apuntadores, ya que no tiene sentido dado la forma
de utilizarlos.
