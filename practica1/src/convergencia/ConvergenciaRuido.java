package convergencia;

import kmedias.KMedias;
import imagen.Pixel;

import java.util.List;

/**
 * clase para representar deteccion de parada por
 * alcanzar determinado nivel de ruido
 */
public class ConvergenciaRuido extends EstrategiaConvergencia {
   /**
    * maximo nivel de ruido permitido
    */
   private final double umbral;

   /**
    * maximo numero de iteraciones
    */
   private final int maxIteraciones;

   /**
    * constructor de la clase
    *
    * @param umbral         umbral a considerar para el chequeo de
    *                       convergencia
    * @param maxIteraciones numero maximo de iteraciones a
    *                       ejecutar
    */
   public ConvergenciaRuido(double umbral, int maxIteraciones) {
      this.umbral = umbral;
      this.maxIteraciones = maxIteraciones;
      medida = 0;
   }

   /**
    * metodo de deteccion de convergencia por ruido, comparando
    * la imagen inicial y la final FUNCIONAL
    *
    * @param kmedias objeto analizado
    * @return flag booleano indicando si hay convergencia
    */

   public boolean convergencia(KMedias kmedias) {
      boolean convergencia = false;

      //calcular sennal
      double sennal = kmedias.obtenerPixels().stream().mapToDouble(
              pixel -> pixel.calcularSennal())
              .sum();

      //calcular ruido
      double ruido = kmedias.obtenerPixels().stream().mapToDouble(
              pixel -> pixel.calcularRuido(
                         pixel.obtenerMasCercano(
                              kmedias.obtenerCentrosT2()
                      ))
      ).sum();


      // se obtiene la medida
      medida = sennal / ruido;

      // se determina si hay convergencia
      if(medida > umbral ||
              kmedias.obtenerContadorIteraciones() >= maxIteraciones){
         convergencia = true;
      }

      // se devuelve el resultado
      return convergencia;
   }

   /**
    * metodo de deteccion de convergencia por ruido, comparando
    * la imagen inicial y la final de forma imperativa
    *
    * @param kmedias objeto analizado
    * @return flag booleano indicando si hay convergencia
    */
   public boolean convergenciaImperativo(KMedias kmedias) {
      boolean convergencia = false;
      double sennal = 0;
      double ruido = 0;

      // se obtienen los pixels de la imagen
      List<Pixel> pixels = kmedias.obtenerPixels();

      // bucle de recorrido de los pixels de la imagen
      for(int i=0; i < pixels.size(); i++){
         // obtengo el pixel con indice i
         Pixel pixel = pixels.get(i);

         // obtener el centro final mas cercano
         Pixel centroMasCercano =
                 pixel.obtenerMasCercano(kmedias.obtenerCentrosT2());

         // se obtien el valor de sennal asociado al pixel
         sennal += pixel.calcularSennal();

         // se calcula el valor de ruido
         ruido += pixel.calcularRuido(centroMasCercano);
      }

      // se obtiene la medida
      medida = sennal / ruido;

      // se determina si hay convergencia
      if(medida > umbral ||
              kmedias.obtenerContadorIteraciones() >= maxIteraciones){
         convergencia = true;
      }

      // se devuelve el resultado
      return convergencia;
   }
}
