# Variables

En el lenguaje se pueden declarar variables utilizando
la palabra clave `Knight`. Al declarar una variable
se le asigna el valor por defecto para la misma (0 en
caso de los enteros, 0.0 en caso de los flotantes,
etc).

La sintaxis para declarar variables es la siguiente:
```
Knight <id> of House <tipo>.
```
siendo *id* un identificador válido y *tipo* un alias
o tipo primitivo.

Para tipos compuestos se utiliza la siguiente sintaxis:
```
Former Knight <id> now <tipo>.
```
siendo *tipo* un tipo compuesto (struct, union, arreglo,
tupla o string). En el caso especial de los apuntadores
se utiliza la palabar clave `Wildling` en lugar de
`Knight`.

Los arreglos, los strings, los aliases o apuntadores
de alguno de estos tipos deben cumplir con que al
declararse se especifique el tamaño de los mismos
o reciban un valor adecuado.  Ver sección de strings
o sección de arreglos para más información.

Ejemplos:
* `Knight Jamie of House Lanninteger.`
* `Knight Jon of House Snow.` *(asumiendo que Snow es un alias de tipo)*
* `Former Knight Jeor now Lord Commander of 1 Starkhar bannermen
leading 42 soldiers to their deaths.`
* `Former Wildling Ygrette now Spearwife of Starkhar.`
* `Former Wildling Shae now Spearwife of Lord Commander of
1 Lanninteger bannermen leading 777 soldiers to their deaths.`
