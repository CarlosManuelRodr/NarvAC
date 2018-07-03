# NarvAC
Una computadora mínima implementada en compuertas lógicas

![Captura](img/preview.jpeg?raw=true "Captura")

## Descripción
El objetivo de este proyecto es simple, crear una computadora mínima a partir de operaciones lógicas como And, Or, Xor, Not, de forma que se pueda visualizar la estructura funcional de los circuitos que componen una CPU.

De esta manera, un circuito simple como un semisumador cuya representación esquemática es
![HalfAdder](img/half_adder.jpg?raw=true  "HalfAdder")

Puede ser visualizado como

![HalfAdderTree](img/half_adder2.jpeg?raw=true  "HalfAdderTree")

Esta estructura funcional permite visualizar la complejidad de circuitos más sofisticados, como un sumador completo
![aluadder](img/alu_adder.png?raw=true  "aluadder")

Anidando circuitos se puede construir una CPU completa
![code](img/code.png?raw=true  "code")

En la cual se puede observar el flujo de la corriente en cada compuerta lógica por cada paso de la ejecución de un programa
![cpu_animation](img/cpu_animation.gif?raw=true  "cpu_animation")

Para más información acerca de la estructura de cada módulo de la CPU, [este curso](https://www.youtube.com/watch?v=tpIctyqH29Q&list=PL8dPuuaLjXtNlUrzyH5r6jN9ulIgZBpdo)  es muy recomendable.

## Especificaciones
Actualmente cuenta con una funcionalidad muy limitada, reducida a las instrucciones
```
LoadA, LoadB, StoreA, StoreB, Add, Substract, Increment, Jump, JumpNeg, JumpZero, Print, Halt
```
Cuenta con dos registros A y B de 8 bits, y 16 bytes de memoria total.

## Requisitos
Este proyecto fue creado en Mathematica 11.3.