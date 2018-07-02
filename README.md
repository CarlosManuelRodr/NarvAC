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

## Requisitos
Este proyecto fue creado en Mathematica 11.3.