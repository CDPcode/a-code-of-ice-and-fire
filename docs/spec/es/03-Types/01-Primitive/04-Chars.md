# Caracteres

Se utilizan caracteres Unicode codificados con el estandar
UTF-8. El tipo caracter recibe el nombre de `Starkhar`
en el lenguaje.

## Literales

Los caracteres literales son represenados por la palabra
clave `Hodor` seguida del caracter encerrado entre comillas
simples `'`. Entre las comillas simples debe haber exactamente
un (1) caracter, a menos que se trate de un caracter especial
que requiera ser representado con una secuencia de escape.
Los caracteres que requieren tener una secuencia de escape son
los siguientes:

* `\n`: Para el salto de línea
* `\t`: Para la tabulación
* `\\`: Para el backslash `\`
* `\'`: Para la comilla simple
* `\"`: para la comilla doble

Ejemplos:
* `Hodor 'a'`
* `Hodor 'λ'`
* `Hodor '\n'`
