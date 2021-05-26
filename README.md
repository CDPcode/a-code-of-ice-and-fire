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
<Lord|Lady|Knight> <identificador> of House <declarador de tipo>
```

## Statements

*Sintaxis por definir*

## Sistema de tipos

A code of Ice and Fire es fuertemente tipado. Incorpora características como
* Alias fuertes y débiles
* Soporte para definición de constantes y variables

### Basicos

Maneja 5 tipos basicos a saber:
* Enteros (tamaño por definir)
* Numeros de coma flotante (IEEE 754 - 2019)
* Trilleanos **En discusion**
* Caracteres (UTF - 8) **En discusion**

*Sintaxis e implementación por definir*

### Compuestos

Maneja 6 tipos compuestos a saber

* Registros
* Registros variantes
* Arreglos
* Strings
* Apuntadores (solamente al heap)
* Tuplas

*Sintaxis e implementación por definir*

### Operadores

Los operadores que se manejaran sobre los tipos de datos existentes seran: .. 

## Selección

*Sintaxis e implementación por definir*

## Repetición

*Sintaxis e implementación por definir*

### Determinada

*Sintaxis e implementación por definir*

### Indeterminada

*Sintaxis e implementacion por definir*

### Subrutinas

Las subrutinas se declaran al comienzo del programa con las reglas ..

Dado que se manejan tuplas, se cuenta con retorno multivalor.

*Sintaxis e implementación por definir*
