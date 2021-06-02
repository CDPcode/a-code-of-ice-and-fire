# Referencia Sintaxis

Por simplicidad en algunos casos se omite señalar el *whitespace* de forma explicita en las regex.

## Tipos de datos

### Simples
* enteros:  Lanninteger
* booleanos: Boolton
* caracteres: Starkhar
* float

### Compuestos
* arreglos: Lord commander
* uniones: faceless man
* registros: king
* string: hand of the king
* tuplas: registro sin nombre

## Programa
```
A song of ice and fire: <program name>
  Prolog
  [$fname]+
  Epilog
```

## Statements

```
valid acoiaf code.
```

## Algunos macros

`ws       = [ \t\n\b\r]*`

`name     = [A-Z]([']{0,1}[a-z]+)+`

`title    = [Lord|Lady|Knight]`

`house    = [Lanninteger|Boolton|Starkhar]`

`intVal   = -{0,1}[0-9]*`

`intExpr  = undefined`

`charVal  = *UTF-8 Character*` 

`charExpr = rune$ws*$charVal`

`boolVal  = [love|gold|blood]`

`boolExpr = undefined`

`validSt = "valid program statement"`

## Declaración

```
$title $name of house $house
```

## Asignación

### Tipos simples

```
$name $house takes #respExpr
```
con *respExpr* la expresion ajustada al tipo de datos. Las excepciones notables son:

#### Enteros
``` 
$name $house takes $intExpr
```

#### Caracteres
```
$name $house takes $charExpr
```

### Tipos compuestos



## Control de flujo

### Seleccion simple

```
You will be betrayed by $boolExpr three times.
 Once for love:
   [$validSt$ws*]*
 Once for gold:
   [$validSt$ws*]*
 Once for blood:
   [$validSt$ws*]*
 So the prophecy says
```

### Seleccion multiple

### Repeticion determinada

```
the things I do for <declaracion Lanninteger> from $intExpr until $intExpr 
  Valar Morghules
    [$validSt$ws*]*
  Valar Dohaeris
```

### Repeticion indeterminada

```
while $boolExpr flowed down the river
  Valar Morghules
    [$validSt$ws*]*
  Valar Dohaeris
```

### Subrutinas (declaración)

#### Funciones

```
$name
  Hereby I introduce the honorable
    [$name of house $house,]+$ws*[$name of house $house]
  I must warn you, $house is coming
  Valar Morghulis
    [$validSt$ws*]*
  Valar Dohaeris
```

### Procedimientos

```
$name
  Hereby I introduce the honorable
    [$name of house $house,]+$ws*[$name of house $house]
  what do we say to the lord of death ? not today
  Valar Morghulis
    [$validSt$ws*]*
  Valar Dohaeris
```

### Subrutinas (llamada)

#### Asignacion tras llamada
```
[$name$ws+]* fight against $name with [$name,]*[$name] 
```

#### simple llamada

```
Nobody fight against $name with [$name,]*[$name] 
```

## Flujo no estructurado

### Continue

```
while $boolExpr flowed down the river
  Valar Morghules
    [$validSt$ws*]*
    What is dead may never die.
    [$validSt$ws*]*
  Valar Dohaeris

```

### Break

```
the things I do for <declaracion Lanninteger> from $intExpr until $intExpr 
  Valar Morghules
    [$validSt$ws*]*
    This is the Doom of Valyria.
    [$validSt$ws*]*
  Valar Dohaeris
```

### Return

```
$name
  Hereby I introduce the honorable
    [$name of house $house,]+$ws*[$name of house $house]
  I must warn you, $house is coming
  Valar Morghulis
    [$validSt$ws*]*
    Dracarys $name !
  Valar Dohaeris

```
