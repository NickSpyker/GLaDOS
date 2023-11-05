# Langage GλaDOS

## Bases

### Types
- `int`: pour les nombres entiers.
- `float`: pour les nombres à virgule.
- `char`: pour les caractères et caractères spéciaux.
- `string`: pour les chaînes de caractères, exactement équivalent à `char[]`.
- `bool`: pour les valeurs booléennes.
- `void`: pour signifier qu'il n'a pas de type.

Créez des tableaux avec `type[]` (interdit avec `void`).
- `int[]`
- `float[]`
- `char[]`
- `string[]`
- `bool[]`

## Liaison

### Fonction
Une fonction se déclare avec le mot-clé `fun`, suivi du nom de la fonction. Ensuite, entre parenthèses, les arguments ayant la syntaxe `nom: type` séparés par des virgules s'il y a plus d'un argument (il peut n'y avoir aucun argument). Une fonction retourne toujours un type, s'il ne retourne rien, mettre `-> void`.
```js
fun factorial(nbr: int) -> int
{
    if (nbr == 0) {
        return 1;
    }

    return nbr * factorial(nbr - 1);
}
```

### Fonction spécial
La fonction `main` est le point d'entrée du programme et est obligatoire. Elle doit être définie dans tout programme GλaDOS.
```js
fun main() -> int
{
    return 0;
}
```


### Variable
Une variable est déclarée avec le mot-clé let, suivi du nom, du type, puis de la valeur de cette variable. Une variable doit toujours avoir une valeur et un type à la déclaration.
```js
let nbr: int = 0;

let c: char = 'R';
let nl: char = '\n';

let str: string = "Hello, World!";

let b: bool = false;
```

### Type
Créez un nouveau type à partir de types existants avec le mot-clé type.
```js
type strArray = string[];
```

### Enum
Créez un enum, chaque élément n'est pas assignable à un type ou une valeur.
```js
enum Stats {
    Init,
    Running,
    Stop
}
```

### Struct
Créez une structure avec une liste de variables, dans ce cas, les variables ne peuvent pas avoir de type par défaut.
```js
struct Position {
    x: float,
    y: float
}

```

## Contrôle

Voici la liste des éléments de logique et de contrôle :
```js
if (...) {
    // ...
}

if (...) {
    // ...
} else {
    // ...
}

while (...) {
    // ...
    continue;
    // ...
    break;
    // ...
}

return ...;
```
