package imagen;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * clase para almacenar y manejar imagenes en formato jpeg
 */
public class Imagen {
   /**
    * dato miembro para el numero de pixels de ancho
    */
   private final int columnas;

   /**
    * dato miembro para numero de pixels de alto
    */
   private final int filas;

   /**
    * lista con valor de color (entre 0 y 255) para los
    * pixels. Esto es lo que se lee directamente del archivo
    * de datos
    */
   private final List<Integer> datos;

   /**
    * constructor de la clase
    *
    * @param columnas numero de filas
    * @param filas    numero de columnas
    * @param datos    lista con indices de colores de todos los
    *                 pixels
    */
   public Imagen(int columnas, int filas, List<Integer> datos) {
      // se asignan los datos miembro
      this.columnas = columnas;
      this.filas = filas;
      this.datos = datos;
   }

   /**
    * devuelve el numero de pixels de ancho
    *
    * @return numero de pixels en horizontal
    */
   public int obtenerColumnas() {
      return columnas;
   }

   /**
    * devuelve el numero de pixels de alto
    *
    * @return numero de pixels en vertical
    */
   public int obtenerFilas() {
      return filas;
   }

   /**
    * obtiene el valor de un pixel concreto en funcion
    * del valor de la fila y columna que ocupa
    *
    * @param columna columna objetivo
    * @param fila    fila objetivo
    * @return devuelve el valor del pixel objetivo
    */
   public int obtenerColorPixel(int columna, int fila) {
      int offset = Utilidades.convertirIndicesDesplazamiento(columna, fila, columnas);
      return datos.get(offset);
   }

   /**
    * obtiene el color de un pixel a partir de un desplazamiento
    *
    * @param desplazamiento desplazamiento para acceder al array
    *                       de datos
    * @return color del pixel de interes
    */
   public int obtenerColorPixel(int desplazamiento) {
      return datos.get(desplazamiento);
   }

   /**
    * convierte los datos de la imagen desde indices de
    * colores a las componentes RGB correspondientes,
    * ya almacenadas en objetos de la clase Pixel
    * FUNCIONAL
    * @return lista de objetos de clase Pixel para
    * representar el contenido de la imagen
    */
   public List<Pixel> convertirIndicesColoresPixels() {
      return datos.stream().map(RGBA::generarPixel).collect(Collectors.toList());
   }


   /**
    * convierte los datos de la imagen desde indices de
    * colores a las componentes RGB correspondientes,
    * ya almacenadas en objetos de la clase Pixel
    * de forma imperativa
    *
    * @return lista de objetos de clase Pixel para
    * representar el contenido de la imagen
    */
   public List<Pixel> convertirIndicesColoresPixelsImperativo() {
      // se crea la lista a devolver
      ArrayList<Pixel> pixels = new ArrayList<>();

      // recorrer todo el array de datos y convertimos
      // cada indice de color en el pixel correspondiente
      for(int i=0; i < datos.size(); i++){
         int indiceColor = datos.get(i);

         // agrego a la lista el nuevo objeto asociado
         // a ese indice de color
         pixels.add(RGBA.generarPixel(indiceColor));
      }

      // devuelve la lista de pixels
      return pixels;
   }

   /**
    * se determina el numero de colores diferentes de
    * la imagen FUNCIONAL
    *
    * @return numero de colores presentes en la imagen
    */
   public long obtenerNumeroColores() {
      return datos.stream().distinct().count();
   }

   /**
    * se determina el numero de colores diferentes de
    * la imagen de forma imperativa
    *
    * @return numero de colores presentes en la imagen
    */
   public long obtenerNumeroColoresImperativo() {
      // diccionario con pares clave - valor
      // clave: el indice del color
      // valor: contador de pixels con el color correpsondiente
      Map<Integer, Integer> mapa = new HashMap<>();

      // se recorren los pixels para considerar su
      // indice de color
      for(int i=0; i < datos.size(); i++){
         // obtenemos el color del pixel considerado
         int color = datos.get(i);
         Integer contadorColor = mapa.get(color);

         // si no estaba ese color, contadorColor sera
         // null y tengo que agregar una nueva entrada.
         // En caso contrario, modifico la entrada para
         // cambiar el contador
         if(contadorColor == null){
            mapa.put(color, 1);
         }
         else{
            mapa.put(color, contadorColor+1);
         }
      }

      // se devuelve el numero de colores
      return mapa.size();
   }
}
