/**
 * paquete con elementos para implementar el comportamiento
 * de deteccion de condicion de parada en el algoritmo de
 * particionado
 * Contiene:
 * a) ModoConvergencia: enumerado para los tipos de deteccion
 * b) EstrategiaConvergencia: clase abstracta para servir de
 * tipo comun a las formas de particionado consideradas; permite
 * la implementacion del patron estrategia
 * c) ConvergenciaEstabilidad: determina la condicion de parada
 *    analizando las diferencias entre los centroides de
 *    iteraciones sucesivas; considera tambien que se alcance
 *    el limite de iteraciones
 * d) ConvergenciaRuido: determina la convergencia analizando la
 *    relacion señal/ruido; considera tambien que se haya llegado
 *    al limite de iteraciones
 * e) ConvergenciaIteraciones: se limita a comprobar que se ha
 *    alcanzado el limite de iteraciones
 */
package convergencia;