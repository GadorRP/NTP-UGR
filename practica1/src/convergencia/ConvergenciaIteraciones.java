package convergencia;

import kmedias.KMedias;

/**
 * clase para aportar la deteccion de convergencia
 * mediante el numero de iteraciones realizadas
 */
public class ConvergenciaIteraciones extends EstrategiaConvergencia {
   /**
    * numero maximo de iteraciones a considerar
    */
   private final int maxIteraciones;

   /**
    * constructor de la clase
    *
    * @param maxIteraciones numero maximo de iteraciones a
    *                       ejecutar
    */
   public ConvergenciaIteraciones(int maxIteraciones) {
      this.maxIteraciones = maxIteraciones;
   }

   /**
    * metodo de deteccion de convergencia por numero
    * de iteraciones realizadas
    *
    * @param kmedias objeto analizado
    * @return flag booleano indicando si hay
    * convergencia
    */
   @Override
   public boolean convergencia(KMedias kmedias) {
      medida = kmedias.obtenerContadorIteraciones();
      // se devuelve el resultado de la comparacion
      return medida >= maxIteraciones;
   }
}
