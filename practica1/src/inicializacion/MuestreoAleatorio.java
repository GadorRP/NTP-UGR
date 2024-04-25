package inicializacion;

import imagen.Pixel;
import kmedias.KMedias;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * clase para realizar la inicializacion de
 * centroides mediante muestreo aleatorio
 */
public class MuestreoAleatorio implements EstrategiaInicializacion {


   /**
    * seleccion aleatoria de centroides utilizando
    * distribucion uniforme sobre el numero de pixels
    * FUNCIONAL
    *
    * @param kmedias objeto a inicializar
    * @return lista de objetos de la clase Pixel
    */
   //@Override
   public List<Pixel> seleccionar(KMedias kmedias) {

      // obtener los pixels de donde se muestrea
      List<Pixel> pixels = kmedias.obtenerPixels();

      List<Integer> indices = IntStream.range(0, pixels.size()).
                      boxed().collect(Collectors.toList());

      // se baraja el array de indices
      Collections.shuffle(indices);

      return indices.stream().map(indice ->
              pixels.get(indice)).limit(kmedias.obtenerK()).
              collect(Collectors.toList());

   }

   /**
    * seleccion aleatoria de centroides utilizando
    * distribucion uniforme sobre el numero de pixels
    * de forma imperativa
    * @param kmedias objeto a inicializar
    * @return lista de objetos de la clase Pixel
    */

   public List<Pixel> seleccionarImperativo(KMedias kmedias) {
      // crear la coleccion a devolver
      ArrayList<Pixel> seleccionados = new ArrayList<>();

      // obtener los pixels de donde se muestrea
      List<Pixel> pixels = kmedias.obtenerPixels();

      // generar un array de indices desde 0 hasta
      // el numero de pixels - 1
      ArrayList<Integer> indices = new ArrayList<>();
      for(int i=0; i < pixels.size(); i++){
         indices.add(i);
      }

      // se baraja el array de indices
      Collections.shuffle(indices);

      // se seleccionan los k primeros elementos de
      // el array barajado de indices
      for(int i=0; i < kmedias.obtenerK(); i++){
         seleccionados.add(pixels.get(indices.get(i)));
      }

      // se devuelve la lista de puntos seleccionados
      return seleccionados;
   }
}
