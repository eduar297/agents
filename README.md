# AGENTES

## Proyecto Final de Simulación y Programación Declarativa

### Eduardo Moreira González, C-411

#### Marco General

El ambiente en el cual intervienen los agentes es discreto y tiene la forma de un rectángulo de $N × M$. El ambiente es de información completa, por tanto todos los agentes conocen toda la información sobre el ambiente. El ambiente puede variar aleatoriamente cada $t$ unidades de tiempo. El valor de $t$ es conocido.

Las acciones que realizan los agentes ocurren por turnos. En un turno, los agentes realizan sus acciones, una sola por cada agente, y modifican el medio sin que este varíe a no ser que cambie por una acción de los agentes. En el siguiente, el ambiente puede variar. Si es el momento de cambio del ambiente, ocurre primero el cambio natural del ambiente y luego la variación aleatoria. En una unidad de tiempo ocurren el turno del agente y el turno de cambio del ambiente.

Los elementos que pueden existir en el ambiente son obstáculos, suciedad, niños, el corral y los agentes, que son llamados Robots de Casa. A continuación se precisan las características de los elementos del ambiente:

* **Obstáculos:** estos ocupan una única casilla en el ambiente. Pueden ser movidos si los niños los empujan, una única casilla. El Robot de Casa sin embargo no puede moverlo. No pueden ser movidos a ninguna de las casillas ocupadas por cualquier otro elemento del ambiente.
* **Suciedad:** la suciedad es por cada casilla del ambiente. Solo puede aparecer en casillas que previamente estuvieron vacías. Esta, o aparece en el estado inicial o es creada por los niños.
* **Corral:** el corral ocupa casillas adyacentes en número igual al del total de niños presentes en el ambiente. El corral no puede moverse. En una casilla del corral solo puede coexistir un niño. En una casilla del corral, que esté vacía, puede entrar un robot. En una misma casilla del corral pueden coexistir un niño y un robot solo si el robot lo carga, o si acaba de dejar al niño.
* **Niño:** los niños ocupan solo una casilla. Ellos en el turno del ambiente se mueven, si es posible (si la casilla no está ocupada: no tiene suciedad, no está el corral, no hay un Robot de Casa), y aleatoriamente (puede que no ocurra movimiento), a una de las casilla adyacentes. Si esa casilla está ocupada por un obstáculo este es empujado por el niño, si en la dirección hay más de un obstáculo, entonces se desplazan todos. Si el obstáculo está en una posición donde no puede ser empujado y el niño lo intenta, entonces el obstáculo no se mueve y el niño ocupa la misma posición. Los niños son los responsables de que aparezca suciedad. Si en una cuadrícula de 3 por 3 hay un solo niño, entonces, luego de que él se mueva aleatoriamente, una de las casillas de la cuadrícula anterior que esté vacía puede haber sido ensuciada. Si hay dos niños se pueden ensuciar hasta 3. Si hay tres niños o más pueden resultar sucias hasta 6. Los niños cuando están en una casilla del corral, ni se mueven ni ensucian. Si un niño es capturado por un Robot de Casa tampoco se mueve ni ensucia.
* **Robot de Casa:** El Robot de Casa se encarga de limpiar y de controlar a los niños. El Robot se mueve a una de las casillas adyacentes, las que decida. Solo se mueve una casilla sino carga un niño. Si carga un niño pude moverse hasta dos casillas consecutivas. También puede realizar las acciones de limpiar y cargar niños. Si se mueve a una casilla con suciedad, en el próximo turno puede decidir limpiar o moverse. Si se mueve a una casilla donde está un niño, inmediatamente lo carga. En ese momento, coexisten en la casilla Robot y niño. Si se mueve a una casilla del corral que está vacía, y carga un niño, puede decidir si lo deja esta casilla o se sigue moviendo. El Robot puede dejar al niño que carga en cualquier casilla. En ese momento cesa el movimiento del Robot en el turno, y coexisten hasta el próximo turno, en la misma casilla, Robot y niño. El objetivo del Robot de Casa es mantener la casa limpia. Se considera la casa limpia si el $60 \%$ de las casillas vacias no están sucias.

#### Principales Ideas

El problema se desarrolla en un ambiente discreto, dinámico, determinista y de información completa. Estas características, permiten definir un conjunto de reglas a seguir por cada agente para alcanzar el objetivo de mantener el 60% del medio limpio. Se presentarán dos enfoques encaminados a cumplir dicho objetivo y se seleccionará el que se considera, como una mejor solución.

La tarea principal del Robot de Casa es mantener el ambiente limpio al menos al $60\%$, para ello implementamos dos enfoques:

* **Directo:** Limpiar las celdas sucias accesibles por el agente, moviéndose siempre a la basura más próxima, sin tener en cuenta a los niños. El inconveniente es que al estar todos los niños fuera del corral, la probabilidad de que la basura aumente es alta dado que los niños seguirán generando basura.
* **Indirecto:** Llevar primero a todos los niños al corral y una vez realizada la tarea, comenzar a seguir el primer enfoque directo. 

La idea del trabajo es entonces realizar una comparación entre ambos enfoques puramente reactivos en cuanto a la limpieza.

Las acciones de los niños no son más que decidir en cada turno si moverse a una casilla de las vecinas, en caso que no se encuentre en el corral, y si se mueve, tiene una probabilidad prefijada de ensuciar o no una de las casillas que componen la submatriz de 3x3 cuyo centro es el propio niño y cuyo valor es Empty. Y si se encuentra dentro del corral, simplemente pasa el turno sin hacer nada.

Cada t unidades de tiempo, el ambiente cambia de la siguiente manera: de entre todas las casillas Empty, se les da una probabilidad a cada casilla de generar una basura.

Las acciones del Robot las explicaremos a continuación paso a paso:

* Definamos como robot de tipo 0 (bot0) al robot que siempre intenta limpiar el mapa, sin tener en cuenta los niños, y robot de tipo 1 (bot1) al que primero intenta llevar todos los niños al corral para luego comenzar a limpiar.

* **Acciones de Bot0**

  * Si está en una casilla con basura, la limpia
  * Si no hay basuras en el mapa, no hace nada
  * Si se puede mover, lo hace
  * En otro caso, espera al próximo turno sin hacer nada

  Dichas acciones se realizan en orden ascendente, y sólo se realiza la acción $i$ si no se pudo realizar algunas de las $i-1$ acciones anteriores y por supuesto, se cumple $i$.

  La forma en que el Robot decide moverse es: luego de realizar un **bfs** y encontrar los caminos mínimos a cada una de las casillas pasadas de tipo perteneciente a un array de filtros, si existe al menos una casilla con una basura, entonces la acción será la de moverse en la dirección de la primera casilla perteneciente al camino mínimo a la basura más próxima, que lógicamente será una casilla adyacente a la posición del Robot. En caso de no existir basuras, este no ejecutará ninguna acción hasta que en alguna iteración resto 0 con t aparezcan basuras nuevas.

* **Acciones de Bot1**

  * Si tiene cargado un niño y lo puede soltar, lo hace (sólo lo decidirá soltar en caso que el robot se encuentre en el corral)
  * Si no quedan niños fuera del corral y puede limpiar, lo hace
  * Si no quedan niños fuera del corral y queda al menos una basura en el mapa, se mueve con la misma idea de bot0 hacia la casilla basura
  * Si no hay basuras en el mapa, no hace nada
  * Si se puede mover, lo hace
  * En otro caso, espera al próximo turno sin hacer nada

Dichas acciones se realizan en orden ascendente, y sólo se realiza la acción $i$ si no se pudo realizar algunas de las $i-1$ acciones anteriores y por supuesto, se cumple $i$.

#### Modelos de agentes considerados

Todos los agentes son sistemas capaces de realizar acciones autónomas con el fin de cumplir un objetivo dado. Dichas acciones autónomas las podemos definir como un conjunto de reglas que ha de seguir el robot y, si se tiene en cuenta la propiedad dinámica del medio en el cual se encuentra y la capacidad del mismo para responder a los cambios del medio, se obtendría entonces que contamos con agentes reactivos proactivos.

#### Agente reactivo:

La principal característica de estos agentes es la capacidad para responder de un modo oportuno a los cambios que ocurren en el medio. Si se tiene en cuenta que el ambiente definido cambia constantemente de forma natural y/o aleatoria en cada turno, el hecho de que el robot sea capaz de responder ante ello, demuestra la propiedad reactiva del mismo.

En cada turno el robot debe ser capaz de determinar cuál es el objetivo inmediato a partir de la percepción que obtiene del medio.

Anteriormente se describieron las condiciones que son necesarias para que el agente determine dicho objetivo. Si dichas condiciones se cumplen, el agente establece su objetivo y la cadena de acciones encaminadas a cumplirlo (de ahí que el robot de la casa sea pro-activo).

Por ejemplo, suponga que el robot se encuentra en una celda cargando un niño. Primeramente se selecciona una ruta hacia una celda del corral libre (asuma que existe la ruta) y se mueve por un camino óptimo (bfs) hasta llegar a ella; en el próximo turno el ambiente podrá cambiar ya sea de forma natural o aleatoria, en cualquier caso, el robot reevaluará nuevamente el camino mínimo para cumplir su objetivo.

Sin embargo, este comportamiento tiene sus desventajas. Una vez el robot establece su objetivo no lo modifica (Si es b0, intentará limpiar su basura más cercana, o si es b1 y aún quedan niños fuera del corral, intentará recogerlos y guardarlos, etc.), por lo que en un medio en el cual intervengan varios agentes sería ineficiente que varios robots se encaminen a cumplir exactamente el mismo objetivo. 

#### Simulación

El código de la simulación se desarrolló en el lenguaje de programación funcional impartido en clases de Programación Declarativa, Haskell.

Como librería adicional usamos random.

Para una mayor facilidad, usamos *stack* para obtener una mayor organización en el proyecto.

Para correr el proyecto abrimos una terminal en la carpeta raíz del proyecto agents y hacemos:

```powershell
stack run
```

esto nos compilará todo y ejecutará nuestro código una vez compilado.

En src/Lib.hs se encuentra toda la lógica, exportándose la función: 

```haskell
simulation :: IO ()
```

que es usada en app/Main.hs

#### Paso a Paso

* La forma en que tenemos el mapa del ambiente creado no es más que una matriz bindimensional de un data Cell, definido de la siguiente manera:

  * ```haskell
    data Cell
      = Obstacle
      | Child
      | Dirt
      | Playpen {cells :: [Cell]}
      | Bot {cells :: [Cell]}
      | Empty
      deriving (Show, Eq)
    ```

    Donde una cell puede ser un obstáculo (Obstacle), niño (Child), basura (Dirt), corral (Playpen), agente (Bot) o vacía (Empty)

    Lo primero que hacemos es crear dicho ambiente:

    ```haskell
    initEnvironment :: Int -> Int -> ([[Cell]], StdGen)
    ```

    Pasamos n, m como la cantidad de filas y columnas respectivamente del mapa y retornamos un array bidimensional de Cell y un generador de random. La dinámica del proyecto es siempre recibir el mapa actual y un generador, realizar modificaciones en dicho mapa, algorítmica o aleatoriamente, y luego devolver el mapa actualizado y el nuevo generador que será usado posteriormente para generar nuevos números random.

    Obstacle, Child, Dirt, Empty, solo tienen información de su tipo, dado que con esto es suficiente, pero Playpen y Bot tienen una mochila donde guardan los elementos que tienen, por ej.:

    ```haskell
    Playpen {cells :: [Child]}
    ```

    es un corral que tiene un niño, pero pudiera tener un niño y un robot. Este último caso pudiese verse de dos maneras:

    ```haskell
    1) Playpen {cells = [Bot {cells = []}, Child]}
    2) Playpen {cells = [Bot {cells = [Child]}]}
    ```

    En ambas tenemos en el corral un robot y un niño, pero en la 1) el corral tiene a un robot que no carga a un niño, y en 2), el robot es el que carga al niño. Por ej: 2) el robot entro al corral a dejar al nilño y aún no lo suelta, 2) el robot dejó al niño o aún no lo carga, pero coexisten en una misma cell.

    #### Continuemos con la simulación

    Luego de iniciar el environment, (por defecto de 20x20), llamamos a la función simulate, definida de la siguiente manera:

    ```haskell
    simulate :: StdGen -> Int -> [[Cell]] -> Int -> Int -> ([[Cell]], StdGen)
    ```

    Esta recibe un generador, el tipo de robot (bot0 o bot1), el board actual, un  $t$ para definir cada cuantas unidades de tiempo cambia el environment y un param $k$ para llevar la cuenta de la $k-ésima$ iteración, en cada paso la decrementamos y al llegar a 0 habremos realizado $k$ iteraciones de la simulación.

    Luego llamamos a getPercent:

    ```haskell
    getPercent :: [[Cell]] -> Float
    ```

    que dado nuestro board final nos da el porcentaje de casillas sucias contra casillas limpias, de esta forma podemos saber si logramos nuestro objetivo o no de mantener limpio el $60\%$ del ambiente.

    De manera más concreta, simulation realiza una simulación del ambiente con robots del tipo bot0 y calcula el percent, lo mismo sucede con bots de tipo bot1.

    Para la representación visual de los datos, se usará una matriz de n x m en la que los elementos del medio tendrán los siguientes símbolos:

    * **"_"** | Empty | Vacío
    * **"C"** | Child | Niño
    * **"O"** | Obstacle | Obstáculo
    * **"D"** | Dirt | Basura
    * **"B"**  | Bot {cells = []} | Robot con mochila vacía
    * **"BC"** | Bot {cells = [Child]} | Robot con un niño
    * **"BD"** | Bot {cells = [Dirt]} | Robot con una basura
    * **"BCD"** | Bot {cells = [_, _]} | Robot con un niño y una basura en la misma casilla
    * **"P"** | Playpen {cells = []} | Corral vacío
    * **"PC"** | Playpen {cells = [Child]} | Corral con niño
    * **"PB"** | Playpen {cells = [Bot {cells = []}]} Corral con robot
    * **"PBC"** Playpen {cells = [Bot {cells = [Child]}]} | Corral con robot que carga niño
    * **"PB&C"** Playpen {cells = [Bot {cells = []}, Child]} | Corral con robot y niño

    Como Haskell es un lenguaje declarativo y los nombres de las funciones son muy instuitivos, y ya hemos dado una panorámica general del proyecto, para no explicar función a función y no entrar tanto en detalle en este informe, para más información sobre cada función, leer el código.

  Aclarar que todas las corridas darán el mismo resultado, para un resultado distinto, cambiar la semilla con la que se comienza a generar números aleatorios.

  Como initEnvironment es la primera función llamada, esta es la primera en usar una semilla:

  ```haskell
  initEnvironment :: Int -> Int -> ([[Cell]], StdGen)
  initEnvironment n m =
    let gen = mkStdGen 12323
        b1 = createBoard n m
        (b2, gen1) = fillBoard gen b1
     in (b2, gen1)
  ```

  Cambiamos   let gen = mkStdGen 12323 por otra semilla cualquiera.

  Otra aclaración es que para no llenar la pantalla con matrices, solo imprimimos por cada simulación de cada tipo de robot, el mapa inicial y el final, si queremos ver paso a paso cada iteración debemos cambiar en el método simulate que retorne un IO() y antes de llamar recursivo para la siguiente iteración, hacemos un print del board actual.

  ```haskell
  simulate :: StdGen -> Int -> [[Cell]] -> Int -> Int -> ([[Cell]], StdGen)
  simulate gen botType board t 0 = (board, gen)
  simulate gen botType board t k =
    let m = mod k t
        (b, g) = if m == 0 then changeEnvironment gen board else (board, gen)
        (bc, gc) = simulateChilds g b (getAllChild board)
        bb = simulateRobots botType bc (getAllBot board)
     in simulate gc botType bb t (k -1)
  ```

  Y tambien printeamos en el caso base que no cambia el board.

#### Ejemplo de corrida del código

![image-20220206130156546](C:\Users\Eduardo\AppData\Roaming\Typora\typora-user-images\image-20220206130156546.png)



Mapa de 20x20 con 300 iteraciones y $t=10$, una cantidad de niños random entre 1 y min(n, m), de igual manera tenemos bots, obstáculos, basuras y el corral.

El primer board mostrado es el board inicial, el segundo es luego de 300 iteraciones usando agentes tipo bot0 dándonos un porcentaje de $72\%$ de limpieza y el tercer board es el resultado de 300 iteraciones usando agentes tipo bot1, con un $86\%$, donde podemos apreciar que al usar este tipo de agente el porciento de limpieza aumenta, tendiendo a $100$ cuando la cantidad de iteraciones tiende a $+\infty$.
