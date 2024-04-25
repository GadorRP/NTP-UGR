/**
 * contiene clases para proporcionar todo el comportamiento
 * admitido de inicializacion
 * a) ModoInicializacion: enumerado con los modos disponibles
 * b) EstrategiaInicializacion: clase base de la que derivan los
 * comportamientos
 * c) ConvergenciaIteraciones: se detiene tras alcanzar un determinado
 * numero de iteraciones
 * d) ConvergenciaEstabilidad: se detiene si los cambios en los centroides
 * no son muy grandes
 * e) ConvergenciaRuido: se detiene si el ruido obtenido al filtrar supera
 * un determinado umbral
 */
package inicializacion;