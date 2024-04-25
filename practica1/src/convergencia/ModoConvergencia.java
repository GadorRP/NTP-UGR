package convergencia;

/**
 * enumerado para los modos de determinar convergencia
 * en el algoritmo de las k-medias
 */
public enum ModoConvergencia {
   /**
    * parada por numero de iteraciones
    */
   ITERACIONES,

   /**
    * parada por poca variacion en los centroides
    * entre iteraciones
    */
   ESTABILIDAD,

   /**
    * parada por alcanzar determinado nivel de ruido
    * al realizar el filtrado
    */
   RUIDO
}
