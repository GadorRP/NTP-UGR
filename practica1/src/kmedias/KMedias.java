package kmedias;

import convergencia.*;
import imagen.Utilidades;
import inicializacion.*;
import imagen.Imagen;
import imagen.Pixel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * clase base para implementacion del algoritmo de
 * las k-medias aplicado al filtrado de imagenes
 */
public class KMedias {
   /**
    * imagen a procesar: los datos de entrada para el
    * algoritmo de las k-medias son los pixels de la
    * imagen
    */
   private final Imagen imagen;

   /**
    * coleccion de datos sobre los pixels que componen
    * la imagen
    */
   private final List<Pixel> pixels;

   /**
    * numero de grupos a formar
    */
   private final int k;

   /**
    * atributo para delegar el comportamiento de inicializacion
    */
   private final EstrategiaInicializacion inicializador;

   /**
    * atributo para delegar el comportamiento de determinacion
    * de convergencia
    */
   private final EstrategiaConvergencia parada;

   /**
    * contador de iteraciones ejecutadas por el algoritmo
    */
   private int iteraciones;

   /**
    * clasificacion de pixels en los grupos. Las claves
    * son los centroides y los valores las listas de
    * pixels propias de cada grupo
    */
   private Map<Pixel, List<Pixel>> clasificacion;

   /**
    * lista de centroides de la iteracion al inicio
    */
   private List<Pixel> centrosT1;

   /**
    * lista de centroides generados al final de la
    * iteracion
    */
   private List<Pixel> centrosT2;

   /**
    * valor final determinado por el detector
    * de convergencia
    */
   private double medidaConvergencia;

   /**
    * constructor privado para evitar creacion de
    * objetos que no pase por el metodo factoria
    *
    * @param imagen        imagen a filtrar
    * @param k             numero de colores a considerar
    * @param inicializador estrategia de inicializacion
    * @param parada        estretagia de convergencia
    */
   private KMedias(Imagen imagen, int k, EstrategiaInicializacion inicializador,
                   EstrategiaConvergencia parada) {
      // se asigna valor a los datos miembro
      this.imagen = imagen;
      this.k = k;
      this.inicializador = inicializador;
      this.parada = parada;

      // se obtienen los puntos de la imagen. Esto lo hace
      // el metodo generarPixels, que pasa de la descripcion
      // de los indices de color (de 0 a 255) a niveles de
      // RGB
      pixels = imagen.convertirIndicesColoresPixels();

      // se inicializa el contador de iteraciones
      iteraciones = 1;
   }

   /**
    * metodo factoria: es la unica forma de crear
    * objetos de la clase
    *
    * @param k                  numero de colores a considerar
    * @param modoInicializacion estretegia de inicializacion
    * @param modoConvergencia   estretegia de convergencia
    * @param maxIteraciones     maximo numero de iteraciones a realizar
    * @param umbral             umbral a considerar para la convergencia
    * @param imagen             imagen a analizar
    * @return objeto de la clase KMedias construido de acuerdo
    * a la parametrizacion pasada como argumento
    */
   public static KMedias factoria(int k,
                                  ModoInicializacion modoInicializacion,
                                  ModoConvergencia modoConvergencia,
                                  int maxIteraciones, double umbral, Imagen imagen) {
      EstrategiaInicializacion inicializador = null;
      switch (modoInicializacion) {
         case MUESTREO_ALEATORIO:
            inicializador = new MuestreoAleatorio();
            break;
         case SELECCION_UNIFORME:
            inicializador = new SeleccionUniforme();
            break;
         case MUESTREO_ESTRATIFICADO:
            inicializador = new MuestreoEstratificado();
            break;
      }

      EstrategiaConvergencia parada = null;
      switch (modoConvergencia) {
         case ITERACIONES:
            parada = new ConvergenciaIteraciones(maxIteraciones);
            break;
         case ESTABILIDAD:
            parada = new ConvergenciaEstabilidad(umbral, maxIteraciones);
            break;
         case RUIDO:
            parada = new ConvergenciaRuido(umbral, maxIteraciones);
            break;
      }

      // se crea el objeto y se devuelve
      return new KMedias(imagen, k, inicializador, parada);
   }

   /**
    * obtiene el contador de iteraciones
    *
    * @return devuelve el valor del contador de iteraciones
    */
   public int obtenerContadorIteraciones() {
      return iteraciones;
   }

   /**
    * accede al atributo con la lista de pixels
    *
    * @return devuelve la lista de pixels
    */
   public List<Pixel> obtenerPixels() {
      return pixels;
   }

   /**
    * accede al valor de k
    *
    * @return devuelve el valor de k
    */
   public int obtenerK() {
      return k;
   }

   /**
    * obtiene el primer conjunto de centros
    *
    * @return devuelve el conjunto de centros al inicio
    * de la iteracion en curso
    */
   public List<Pixel> obtenerCentrosT1() {
      return centrosT1;
   }

   /**
    * obtiene el segundo conjunto de centros
    *
    * @return devuelve el conjunto de centro al final
    * de la iteracion en curso
    */
   public List<Pixel> obtenerCentrosT2() {
      return centrosT2;
   }

   /**
    * obtiene acceso a la medida de convergencia
    *
    * @return devuelve el valor a usar para la determinacion
    * de la convergencia
    */
   public double obtenerMedidaConvergencia() {
      return medidaConvergencia;
   }

   /**
    * metodo principal de funcionamiento del algoritmo
    * para generar la agrupacion. El resultado sera una
    * nueva imagen despues de haber realizado el
    * agrupamiento de los pixels
    *
    * @return imagen generada tras el filtrado
    */
   public Imagen agrupar() {
      // se produce la inicializacion de los centroides para
      // empezar el proceso
      centrosT1 = inicializador.seleccionar(this);

      // se llama al metodo que realiza el bucle principal
      // de calculo de distancias - asignacion - determinacion
      // de nuevos centroides, hasta que haya convergencia
      iterar();

      // se crea una nueva imagen a partir de la actual,
      // pero aplicando el filtro dado por el resultado
      // del algoritmo de agrupamiento
      return aplicarFiltro();
   }


   /**
    * metodo que implementa el bucle principal del
    * algoritmo: repite el proceso hasta obtener la
    * convergencia deseada. Por tanto, el criterio de
    * parada tendra que llemarse desde el
    * FUNCIONAL
    */
   private void iterar() {
      // clasificar los pixels por distancia a los
      // centroides iniciales
      clasificacion = clasificar();

      // se actualizan los centroides
      actualizar();

      // comprobar si hay convergencia
      boolean convergencia = parada.convergencia(this);

      // se actualizan los centroides
      centrosT1 = centrosT2;

      // si no hay convergencia, vuelvo a iterar de
      // forma recursiva
      if(!convergencia){
         // incremento el contador de iteraciones
         iteraciones++;

         // se produce la llamada recursiva
         iterar();
      }
      else{
         // se obtiene la medida obtenida para la
         // convergencia
         medidaConvergencia = parada.obtenerMedida();
      }
   }

   /**
    * metodo que implementa el bucle principal del
    * algoritmo: repite el proceso hasta obtener la
    * convergencia deseada. Por tanto, el criterio de
    * parada tendra que llemarse desde el
    * de forma imperativa
    */
   private void iterarImperativo() {
      // clasificar los pixels por distancia a los
      // centroides iniciales
      clasificacion = clasificar();

      // se actualizan los centroides
      actualizar();

      // comprobar si hay convergencia
      boolean convergencia = parada.convergencia(this);

      // se actualizan los centroides
      centrosT1 = centrosT2;

      // si no hay convergencia, vuelvo a iterar de
      // forma recursiva
      if(!convergencia){
         // incremento el contador de iteraciones
         iteraciones++;

         // se produce la llamada recursiva
         iterar();
      }
      else{
         // se obtiene la medida obtenida para la
         // convergencia
         medidaConvergencia = parada.obtenerMedida();
      }
   }

   /**
    * metodo auxiliar que produce los nuevos grupos
    * obtenidos tras el calculo de distancias. Los
    * datos se devuelven en un diccionario en que la
    * clave es un centroide y el valor todos los pixels
    * agrupados al mismo FUNCIONAL
    *
    * @return
    */
   private Map<Pixel, List<Pixel>> clasificar() {
      // para cada pixel tenemos que obtener el centroide
      // mas cercano. El resultado se almacena en un
      // diccionario en que la clave seran los centros
      // y los valores las listas de pixels asociados
      Map<Pixel, List<Pixel>> resultado =
      centrosT1.stream().collect(Collectors.toMap(
              centro -> centro, centro -> new ArrayList<>()));


      pixels.stream().forEach(pixel -> {
                 Pixel centroMasCercano = pixel.obtenerMasCercano(centrosT1);
                 resultado.get(centroMasCercano).add(pixel);
              });

      // se devuelve el mapa resultante
      return resultado;
   }


   /**
    * metodo auxiliar que produce los nuevos grupos
    * obtenidos tras el calculo de distancias. Los
    * datos se devuelven en un diccionario en que la
    * clave es un centroide y el valor todos los pixels
    * agrupados al mismo de forma imperativa
    *
    * @return
    */
   private Map<Pixel, List<Pixel>> clasificarImperativo() {
      // para cada pixel tenemos que obtener el centroide
      // mas cercano. El resultado se almacena en un
      // diccionario en que la clave seran los centros
      // y los valores las listas de pixels asociados
      Map<Pixel, List<Pixel>> resultado = new HashMap<>();

      // se agregan los centroides al resultado
      for(Pixel centro : centrosT1){
         resultado.put(centro, new ArrayList<>());
      }

      // procesado de los pixels uno por uno
      for(Pixel pixel : pixels){
         // se obtiene el centro mas cercano
         Pixel centroMasCercano = pixel.obtenerMasCercano(centrosT1);

         // actualizo la lista correspondiente
         List<Pixel> grupo = resultado.get(centroMasCercano);
         grupo.add(pixel);

      }
      // se devuelve el mapa resultante
      return resultado;
   }


   /**
    * actualizacion de la lista de centros obtenidos de acuerdo
    * a la clasificacion y centroides pasados como argumento
    * FUNCIONAL
    */
   private void actualizar() {
      // se crea almacen para centros finales
      centrosT2 = new ArrayList<>();

      // se calcula la medida para cada grupo de la
      // clasificacion realizada

      centrosT2 = centrosT1.stream().map(centro -> clasificacion.get(centro)).
              filter(grupo -> !grupo.isEmpty())
              .map(grupo -> Utilidades.calcularMedia(grupo))
              .collect(Collectors.toList());

      centrosT2.addAll(centrosT1.stream().filter(grupo ->
              clasificacion.get(grupo).isEmpty())
              .collect(Collectors.toList()));

   }
   /**
    * actualizacion de la lista de centros obtenidos de acuerdo
    * a la clasificacion y centroides pasados como argumento
    * de forma imperativa
    */
   private void actualizarImperativa() {
      // se crea almacen para centros finales
      centrosT2 = new ArrayList<>();

      // se calcula la medida para cada grupo de la
      // clasificacion realizada
      for(int i=0; i < clasificacion.size(); i++){
         List<Pixel> grupo = clasificacion.get(centrosT1.get(i));

         // se comprueba que el grupo no este vacio
         if(!grupo.isEmpty()){
            centrosT2.add(Utilidades.calcularMedia(grupo));
         }
         else{
            centrosT2.add(centrosT1.get(i));
         }
      }
   }

   /**
    * metodo para aplicar el filtro a la imagen y generar
    * asi la imagen resultante
    *
    * @return imagen procesada
    * FUNCIONAL
    */
   private Imagen aplicarFiltro() {

      // considero todos los pixels de la imagen
      int dimension = imagen.obtenerColumnas()*imagen.obtenerFilas();

      //obtengo la lista de pixeles
      List <Integer> pixels = IntStream.range(0, dimension).boxed().
              map(numeroPixel -> new Pixel(imagen.obtenerColorPixel(numeroPixel))).
              map(pixel -> pixel.obtenerMasCercano(centrosT1)).
              map(masCercano -> masCercano.obtenerIndice()).
              collect(Collectors.toList());

      // devuelve la imagen creada
      return new Imagen(imagen.obtenerColumnas(), imagen.obtenerFilas(),
              pixels);
   }

   /**
    * metodo para aplicar el filtro a la imagen y generar
    * asi la imagen resultante de forma imperativa
    *
    * @return imagen procesada
    */
   private Imagen aplicarFiltroImperativo() {
      // se crea una nueva imagen usando unicamente los
      // centros finales como pixels
      List<Integer> pixels = new ArrayList<>();

      // considero todos los pixels de la imagen
      int dimension = imagen.obtenerColumnas()*imagen.obtenerFilas();

      // recorrido de los pixels
      for(int i=0; i < dimension; i++){
         int colorPixel = imagen.obtenerColorPixel(i);
         Pixel pixel = new Pixel(colorPixel);

         // se obtiene el mas cercano
         Pixel masCercano = pixel.obtenerMasCercano(centrosT1);

         // se agrega a la lista de pixels finales
         pixels.add(masCercano.obtenerIndice());
      }

      // devuelve la imagen creada
      return new Imagen(imagen.obtenerColumnas(), imagen.obtenerFilas(),
              pixels);
   }
}
