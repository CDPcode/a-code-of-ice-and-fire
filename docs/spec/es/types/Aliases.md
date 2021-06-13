# Aliases

Se pueden declarar aliases de tipo débiles o fuertes.
Esto solo se puede realizar en el bloque del programa
destinado a ello (el bloque `Appendix`).

## Aliases Débiles

Los aliases débiles representan el mismo tipo del que
son un alias. Es decir, representan solo azúcar sintáctica
para que el código sea más legible. Su comportamiento
es como el de la palabra clave `type` del lenguaje Haskell.

La sintaxis para declarar un alias débil es:
```
House <id> are the dogs of <tipo>.
```

donde *id* es el identificador del nuevo tipo y debe ser
un identificador válido y *tipo* es un tipo válido.

## Aliases Fuertes

Los aliases fuertes representan un tipo nuevo que será
distinto al tipo del que son un alias. Es decir, no se
podrá recibir un valor del nuevo tipo donde se espera
un valor del tipo viejo o viceversa.

La sintaxis para declarar un alias fuerte es:
```
House <id> comes from the old lineage of <tipo>.
```

donde *id* es el identificador del nuevo tipo y debe ser
un identificador válido y *tipo* es un tipo válido.
