package inicializacion;

/**
 * enumerado para las diferentes forma de
 * inicializacion del algoritmo de las
 * k-medias
 */
public enum ModoInicializacion {
   /**
    * seleccion aleatoria de centroides
    */
   MUESTREO_ALEATORIO,

   /**
    * seleccion de centroides considerando
    * el rango de colores
    */
   SELECCION_UNIFORME,

   /**
    * seleccion de centroides para seleccionar
    * mas centros de los colores mas presentes
    */
   MUESTREO_ESTRATIFICADO
}
