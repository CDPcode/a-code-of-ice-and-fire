# Identificadores

Un identificador válido debe comenzar por una letra latina
mayúscula y seguir con una o más letras latinas minúsculas
o comillas simples `'`, siempre que no se encuentren
dos comillas simples consecutivas y la comilla simple
no sea el ultimo caracter del identificador. Además se debe
cumplir la restricción de que el id no sea ninguna palabra
reservada del lenguaje. La idea es que un
identificador represente un nombre propio de una persona o
casa. Para ser más específicos, un identificador válido
debe cumplir con la siguiente expresión regular:
`[A-Z]([']?[a-z]+)+`.

Ejemplos de identificadores válidos son:
* `Arya`
* `Jamie`
* `Lannister`
* `Hizdar'zo'loraq`
* `Jaquen'hagar`

Ejemplos de identificadores inválidos son:
* `ned`: No comienza con mayúscula
* `KhalDrogo`: Contiene una mayúscula en medio
* `Stannis'`: Termina con `'`
* `Dae''nerys`: Contiene dos comillas simples consecutivas
* `Ty.rion`: Contiene caracters que no son letras ni
comillas simples
