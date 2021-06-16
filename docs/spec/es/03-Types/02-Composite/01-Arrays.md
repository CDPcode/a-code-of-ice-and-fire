# Arreglos

Los arreglos tienen tamaño constante de elaboración
y pueden contener cualquier tipo de datos excepto
otros arreglos o strings. Al declarar un arreglo
se debe especificar la cantidad de dimensiones
del mismo y esta información formará parte del tipo.
Se puede declarar el tamaño de las dimensiones
con cualquier expresión entera pero se debe
hacer en el momento de declarar la variable.
El nombre que recibe el tipo arreglo es el
siguiente: `Lord Commander of <dimensiones> <tipo> bannermen`
donde *dimensiones* es un número natural `[0-9]+`
representando el número de dimensiones del arreglo y
*tipo* es un identificador de tipo válido.

Para especificar los tipos de las dimensiones se utiliza
la siguiente sintaxis
`leading <tam_1>, ..., <tam_n> to their deaths`
donde *tam_i* son expresiones enteras especificando el
tamaño de cada una de las dimensiones.

Los arreglos son almacenados con el formato row-major.

## Literales

Los arreglos literales comienzan con la palabra clave
`army formation of` y culminan con la palabra clave
`aligned together`. En medio se encuentran los valores
que forman parte del arreglo separados por comas.
Dichos valores son expresiones (r-values) que evaluen al tipo
requerido.

Por ejemplo:
```
army formation of Jamie, 3 soldiers, Lancel aligned together
```

En el caso de que se tenga más de una dimensión se utiliza la
siguiente sintaxis:
```
army formation of
    army formation of
        army formation of <values> aligned together,
        army formation of <values> aligned together,
        ...
        army formation of <values> aligned together
    aligned together,
    ...
    army formation of
        army formation of <values> aligned together,
        army formation of <values> aligned together,
        ...
        army formation of <values> aligned together
    aligned together
aligned together
```

*(la identación es para agregar claridad pero no es
obligatoria)*
