package inicializacion;

import imagen.Pixel;
import imagen.Utilidades;
import kmedias.KMedias;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;


/**
 * clase para seleccion de colores partiendo la
 * gama de colores disponibles en intervalos y
 * seleccionando las marcas de la division
 */
public class SeleccionUniforme implements EstrategiaInicializacion {
   /**
    * seleccion de centroides considerando la gama
    * de colores FUNCIONAL
    *
    * @param kmedias objeto a inicializar
    * @return lista de pixels seleccionados como
    * centroides
    */
   @Override
   public List<Pixel> seleccionar(KMedias kmedias) {
      // se obtienen los pixels de la imagen
      List<Pixel> pixels = kmedias.obtenerPixels();

      // se obtiene el valor de k
      int k = kmedias.obtenerK();

      // se crea la lista a devolver
      List<Pixel> seleccionados = new ArrayList<>();

      // se determinan los colores maximo y minino
      List<Integer> minMax = Utilidades.obtenerMinimoMaximo(pixels);

      // se agrega a seleccionados un pixel cuyo indice sea
      // el color minimo
      seleccionados.add(new Pixel(minMax.get(0)));

      // calcular el incremento entre marca y marca
      double incremento =
              (minMax.get(1) - minMax.get(0)) / ((k-1) * 1.0);

      //se aniaden los pixels con indices de colores intermedios
      seleccionados.addAll(IntStream.range(1, k-1).boxed().map(numero ->
              minMax.get(0) + (int) Math.round(numero * incremento)).
              map(color -> new Pixel(color)).
              collect(Collectors.toList()));

      // agregar el pixel del mayor indice de color
      seleccionados.add(new Pixel(minMax.get(1)));

      // se devuelve la lista de seleccionados
      return seleccionados;
   }

   public List<Pixel> seleccionarImperativo(KMedias kmedias) {
      // se obtienen los pixels de la imagen
      List<Pixel> pixels = kmedias.obtenerPixels();

      // se obtiene el valor de k
      int k = kmedias.obtenerK();

      // se crea la lista a devolver
      List<Pixel> seleccionados = new ArrayList<>();

      // se determinan los colores maximo y minino
      List<Integer> minMax = Utilidades.obtenerMinimoMaximo(pixels);

      // se agrega a seleccionados un pixel cuyo indice sea
      // el color minimo
      seleccionados.add(new Pixel(minMax.get(0)));

      // calcular el incremento entre marca y marca
      double incremento =
              (minMax.get(1) - minMax.get(0)) / ((k-1) * 1.0);

      // bucle desde i = 1 hasta k-2 para agregar los
      // pixels con indices de colores que corresponden a las
      // marcas intermedias
      int color;
      for(int i=1; i < k-1; i++){
         color = minMax.get(0) + (int) Math.round(incremento*i);
         seleccionados.add(new Pixel(color));
      }

      // agregar el pixel del mayor indice de color
      seleccionados.add(new Pixel(minMax.get(1)));

      // se devuelve la lista de seleccionados
      return seleccionados;
   }
}
