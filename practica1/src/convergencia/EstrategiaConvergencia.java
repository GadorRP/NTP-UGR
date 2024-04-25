package convergencia;

import kmedias.KMedias;

/**
 * interfaz para representar de forma generica
 * la forma de determinar la convergencia del
 * algoritmo de las k-medias
 */
public abstract class EstrategiaConvergencia {
   /**
    * medida obtenida en la determinacion de la convergencia
    */
   protected double medida = 0;

   /**
    * metodo de deteccion de convergencia
    * @param kmedias objeto analizado
    * @return flag booleano para indicar si se detiene
    * el algoritmo
    */
   abstract public boolean convergencia(KMedias kmedias);

   public double obtenerMedida(){
      return medida;
   }
}
