# Strings

Los strings o cadenas de caracteres representan
arreglos de caracteres de una dimensión. El tipo
string se representa en el lenguaje
como `Hand of the King`. Para especificar
el tamaño del string se utiliza la misma
sintaxis que para los arreglos, es decir
`leading <tam> to their deaths`, donde
*tam* es una expresión entera.

## Literales

Los strings literales comienzan con las palabras clave
`Maester reading` seguidos de la candena de caracteres
encerrada entre comillas dobles `"`. Las comillas dobles
pueden contenter cualquier caracter imprimible menos
saltos de línea, tabulaciones, backslashes, comillas
simples o comillas dobles. Dichos caracteres se deben
represnetar con una secuencia de escape:

* `\n`: Para el salto de línea
* `\t`: Para la tabulación
* `\\`: Para el backslash `\`
* `\'`: Para la comilla simple
* `\"`: para la comilla doble

Por ejemplo:
* `Maester reading "The Book of Ancient Kings and Queens of Westeros"`
* `Maester reading "Hodor\n"`
