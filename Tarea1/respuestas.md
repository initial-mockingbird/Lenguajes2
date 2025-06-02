# Daniel Pinto 15-11139

## Pregunta 1

```C
0 a := b + c
1 b := a / 2
2 b := d - c
3 a := a + d
4 c := b * a
```

Iteramos desde la ultima instruccion hasta la primera:

| i | Instruccion | Vivas   | Usos  |
|---|-------------|---------|-------|
| 4 | c := b * a  | b,d     | a = ? |
|   |             |         | b = ? |
|   |             |         | c = ? |
|   |             |         | d = ? |
|   |             |         |   _   |
| 3 | a := a + d  | a,b,d   | a = 4 |
|   |             |         | b = 4 |
|   |             |         | c = ? |
|   |             |         | d = ? |
|   |             |         |   _   |
| 2 | b := d - c  | a,b,d   | a = 3 |
|   |             |         | b = 4 |
|   |             |         | c = ? |
|   |             |         | d = 3 |
|   |             |         |   _   |
| 1 | b := a / 2  | a,d,c   | a = 3 |
|   |             |         | b = ? |
|   |             |         | c = 2 |
|   |             |         | d = 3 |
|   |             |         |   _   |
| 0 | a := b + c  | a,d,c   | a = 1 |
|   |             |         | b = ? |
|   |             |         | c = 2 |
|   |             |         | d = 3 |
|   |             |         |   _   |
| I |             | b,c,d   | a = ? |
|   |             |         | b = 0 |
|   |             |         | c = 0 |
|   |             |         | d = 3 |
|   |             |         |   _   |

## Pregunta 2


### Pregunta 2.a

De manera mas general, sea `ns = [n_1,n_2,...,n_m]` una lista de nodos ordenados
decrecientemente por su label. Entonces `label(ns)` se puede calcular como:

```haskell
label (c_max:cs)) = cmax + snd (foldl f (c_max,0) cs)
    where
      f (n,adicionales) c
            | n == c    = (n,adicionales+1)
            | otherwise = (n-1,adicionales)
```

Aqui la magia lo hace el ordenamiento y el parametro `p`: el cual cuenta cuantos registros
adicionales debemos usar para almacenar los resultados. Veamos un ejemplo:

Supongamos que tenemos los siguientes labels: `9 : 9 9 8 8 5 5 5`, entonces, la manera en que iteramos es:

```
# Necesito 9 registros, tengo 8 registros operativos ya que
necesito 1 para almacenar la cabeza, entonces necesito de disponer de 1 registro adicional.
Asi vuelvo a tener 8 libres
(9,0) 9 -> (9,1)
# Ahora necesito 9 registros, tengo 8 registros libres ya que
# necesito 1 para almacenar la cabeza, entonces necesito de disponer de 1 registro adicional.
Asi vuelvo a tener 8 libres
(9,1) 9 -> (9,2)
# Ahora necesito 8 registros, y tengo 8 libres! no es necesario disponer de registros adicionales
(9,2) 8 -> (8,2)
# Ahora necesito 8 registros, y tengo 7 libres! (porque necesite almacenar el ultimo resultado),
asi que necesito disponer de 1 registro adicional
(8,2) 8 -> (8,3)
# Ahora necesito 5 registros, y tengo 7 libres! no es necesario disponer de registros adicionales
(8,3) 5 -> (7,3)
# Ahora necesito 5 registros, y tengo 6 libres! no es necesario disponer de registros adicionales
(7,3) 5 -> (6,3)
# Ahora necesito 5 registros, y tengo 5 libres! no es necesario disponer de registros adicionales
(6,3) 5 -> (5,3)
```

Finalmente, necesitamos 9 registros (para hacer la operacion mas costosa) + 3 registros adicionales.

## Pregunta 2.b

El proceso de generacion de codigo es en esencia el mismo, visitamos los nodos en orden decreciente de label
almacenando un registro adicional segun el esquema descrito en la pregunta 2.a.

## Pregunta 3

```C
we[have] = to[go [deep] + er]
```

### Pregunta 3.a

```C
{ we,have,to,go,deep,er # previously known vars      }
{ WWE # WWE is a MACRO for the "we" type width       }
{ WI  # WI is a MACRO for the width of integer type  }
:
:
:
t1       := deep * WI
t2       := go[t1]
t3       := t2 + er
t4       := t3 * WWE
t5       := to[t4]
we[have] := t5
```

Explicacion:

```C
# `deep`` esta calculado, sabemos que `go[deep]` debe ser un
# entero ya que es usado para indexar a `to`
# asi que utilizamos el tamano de los enteros para calcular t1
t1 := deep * WI
# ya teniendo el salto calculado, indexamos a `go`
t2 := go[t1]
# ya teniendo `g[deep]` calculado, sumamos con `er`
t3 := t2 + er
# ahora usamos t3 para calcular el salto. Aqui NO sabemos que tipo
# tiene `to`, asi que asumiremos que es WWE.
t4 := t3 * WWE
# ya teniendo el salto, indexamos a `to`
t5 := to[t4]
# finalmente, asignamos el resultado a `we[have]`
we[have] := t5
```

## Pregunta 3.b

Inicialmente, empezamos con los siguientes descriptores:

| R1 | R2 | R3 | t1 | t2 | t3 | t4 | t5 | we | have | to | go | deep | er |
|----|----|----|----|----|----|----|----|----|------|----|----|------|----|
|    |    |    |    |    |    |    |    | we | have | to | go | deep | er |

Luego, para la instruccion: `t1 := deep * WI`:

```
# Ningun registro esta lleno, asi que usamos todos los registros
# WI es una CONSTANTE, asi que no necesitamos cargarlo en un registro
(R1, R2) <- getReg(t1 := deep * WI)
```

Y el codigo generado es:

```C
LD R2, deep
MUL R1, R2, WI
```

Y actualizamos los descriptores:

| R1 | R2 | R3 | t1 | t2 | t3 | t4 | t5 | we | have | to | go | deep | er |
|----|----|----|----|----|----|----|----|----|------|----|----|------|----|
| t1 |deep|    |    |    |    |    |    | we | have | to | go | deep | er |

Para la instruccion: `t2 := go[t1]`:

```C
## Aun tenemos reigstros libres para `t2`, asi que usamos R3
## `t1` ya esta calculado en `R1`, asi que lo usamos directamente
(R3,R1) <- getReg(t2 := go =[] t1)
```

Emitimos el codigo:

```
LD R3, go(R1)
```

Y actualizamos los descriptores:

| R1 | R2 | R3 | t1 | t2 | t3 | t4 | t5 | we | have | to | go | deep | er |
|----|----|----|----|----|----|----|----|----|------|----|----|------|----|
| t1 |deep| t2 |    |    |    |    |    | we | have | to | go | deep | er |

Para la instruccion: `t3 := t2 + er`:


```C
# t2 se reutiliza, esta en R3
# No mas descriptores libres para t3 y er!
# Tenemos que liberar 2 registros
# Podemos liberar R2 ya que esta en memoria
# En circunstancias normales NO podemos liberar R1 ya que es usado por t1 y t1 no esta en memoria
# Tenemos que generar codigo que preserve t1
# Sin embargo, t1 no tiene usos futuros, asi que podemos liberar R1 sin preservarlo
(R1,R3,R2) <- getReg(t3 := t2 + er)
```

Emitimos el codigo:

```C
LD R2, er
ADD R1, R3, R2
```

Y actualizamos los descriptores:

| R1 | R2 | R3 | t1 | t2 | t3 | t4 | t5 | we | have | to | go | deep | er |
|----|----|----|----|----|----|----|----|----|------|----|----|------|----|
| t3 | er | t2 | t1 |    |    |    |    | we | have | to | go | deep | er |

Para la instruccion: `t4 := t3 * WWE`:

```C
# t3 esta en R1, asi que lo reutilizamos
# WWE es una constante, asi que no necesitamos cargarla en un registro
# t3 no tiene usos futuros, asi que podemos reutilizar R1
(R1,R1) <- getReg(t4 := t3 * WWE)
```

Emitimos el codigo:

```C
MUL R1, R1, WWE
```

Y actualizamos los descriptores:

| R1 | R2 | R3 | t1 | t2 | t3 | t4 | t5 | we | have | to | go | deep | er |
|----|----|----|----|----|----|----|----|----|------|----|----|------|----|
| t4 | er | t2 | t1 |    |    |    |    | we | have | to | go | deep | er |

Para la instruccion: `t5 := to[t4]`:

```C
# t4 esta en R1, asi que lo reutilizamos
# er esta en memoria, asi que podemos librerar R2
(R2,R1) <- getReg(t5 := to =[] t4)
```

Emitimos el codigo:

```C
LD R2, to(R1)
```

Y actualizamos los descriptores:

| R1 | R2 | R3 | t1 | t2 | t3 | t4 | t5 | we | have | to | go | deep | er |
|----|----|----|----|----|----|----|----|----|------|----|----|------|----|
| t4 | t5 | t2 | t1 |    |    |    |    | we | have | to | go | deep | er |

Para la instruccion: `we[have] := t5`:

```C
# No tenemos informacion de ultimo uso para ningun temporal, asi que arbitrariamente
# escogemos entre los registros R1 y R3  para asignar a `have`
(R1,R2) <- getReg(we := have []= t5)
```

Emitimos el codigo:

```C
LD R1, have
ST R2, we(R1)
```

Y actualizamos los descriptores:

| R1 | R2 | R3 | t1 | t2 | t3 | t4 | t5 | we | have | to | go | deep | er |
|----|----|----|----|----|----|----|----|----|------|----|----|------|----|
|have| t1 | t2 |    |    |    |    |    | we | have | to | go | deep | er |

Siendo asi el codigo final generado:

```C
LD R2, deep
MUL R1, R2, WI
LD R3, go(R1)
LD R2, er
ADD R1, R3, R2
MUL R1, R1, WWE
LD R2, to(R1)
LD R1, have
ST R2, we(R1)
```

## Pregunta 4

### Pregunta 4.a

```C
collatz(n){
  int steps = 0;
  while(n > 1){
      steps++;
      if(n % 2 == 0){
          n = n / 2;
      } else {
          n = 3 * n + 1;
      }
  }
  return steps;
}
```

### Pregunta 4.b

El TAC del programa es:

```C
00 t0 := get_param n
01 steps := 0
02 if t0 > 1 goto 04
03 goto 14
04 steps := steps + 1
05 t1 := t0 % 2
06 if t1 == 0 goto 08
07 goto 10
08 t0 := t0 / 2
09 goto 02
10 t2 := 3 * n
11 t3 := t2 + 1
12 t0 := t3
13 goto 02
14 return steps
````

### Pregunta 4.c

![Flow Graph](./pics/image.svg)
