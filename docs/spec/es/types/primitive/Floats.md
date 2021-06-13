# Números de punto flotante

Para los números de punto flotante se utiliza el estandar
IEEE 754 - 2019. Su tamaño será de 32 bits (presicion simple).
El nombre que recibe el tipo flotante en el lenguaje es
`Freyt`.

## Literales

Los flotantes literales consisten de un signo menos `-` opcional
seguido de 1 o más dígitos, seguido de un punto `.`, seguido de
1 o más dígitos, seguido de la palabra clave `descendants`. Para
ser más claros, deben cumplir con la siguiente expresión regular
`[\-]?[0-9]+\.[0-9]+ descendants`. Nótese que SIEMPRE se debe
incluir el punto y algún dígito antes y después del mismo.

Por ejemplo:
* `3.1416 descendants`
* `-1.0 descendants`
* `0.42 descendants`
