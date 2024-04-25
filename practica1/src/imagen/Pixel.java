package imagen;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * representa puntos en un espacio tridimensional
 */
public class Pixel implements Comparable<Pixel> {
   /**
    * componente roja del color
    */
   private final double rojo;

   /**
    * componente verde del color
    */
   private final double verde;

   /**
    * componente azul del color
    */
   private final double azul;

   /**
    * indice completo del color, componiendo las componentes,
    * incluido el componente alpha no almacenado
    */
   private final int indice;

   /**
    * Constructor de objeto
    *
    * @param rojo  componente de color rojo
    * @param verde componente de color verde
    * @param azul  componente de color azul
    */
   public Pixel(double rojo, double verde, double azul) {
      this.rojo = rojo;
      this.verde = verde;
      this.azul = azul;
      this.indice = RGBA.convertirRGBIndice(rojo, verde, azul);
   }

   /**
    * constructor que permite crear un objeto a partir de un
    * valor de color
    *
    * @param color valor indice del color, que se descompondra
    *              en sus componentes rojo, verde y azul
    */
   public Pixel(int color) {
      this.rojo = RGBA.obtenerComponente(color, ComponentesRGBA.ROJO);
      this.verde = RGBA.obtenerComponente(color, ComponentesRGBA.VERDE);
      this.azul = RGBA.obtenerComponente(color, ComponentesRGBA.AZUL);
      this.indice = color;
   }

   /**
    * obtiene la componente objetivo
    *
    * @param objetivo componente de interes
    * @return valor del componente seleccionado
    */
   public double obtenerComponente(ComponentesRGBA objetivo) {
      double valor = 0;
      switch (objetivo) {
         case ROJO:
            valor = rojo;
            break;
         case VERDE:
            valor = verde;
            break;
         case AZUL:
            valor = azul;
            break;
         case ALFA:
            valor = 0;
            break;
      }

      // se devuelve el valor seleccionado
      return valor;
   }

   /**
    * devuelve el indice de color asociado
    *
    * @return indice del color
    */
   public int obtenerIndice() {
      return indice;
   }

   /**
    * calcula la distancia cuadratica de este punto y
    * el punto pasado como argumento
    *
    * @param otro pixel con el que medir la distancia
    * @return valor de distancia
    */
   public double distanciaCuadratica(Pixel otro) {
      return Math.pow(rojo - otro.rojo, 2) +
         Math.pow(verde - otro.verde, 2) +
         Math.pow(azul - otro.azul, 2);
   }

   /**
    * encuentra y devuelve el centro mas cercano al punto
    * pasado como argumento FUNCIONAL
    * @param centros lista de centros a considerar
    * @return centro mas cercano
    */
   public Pixel obtenerMasCercano(List<Pixel> centros){
         return centros.stream().sorted(Comparator.comparing(
                 pixel -> distanciaCuadratica(pixel))).limit(1).
                 collect(Collectors.toList()).get(0);
   }

   /**
    * encuentra y devuelve el centro mas cercano al punto
    * pasado como argumento en imperativo
    *
    * @param centros lista de centros a considerar
    * @return centro mas cercano
    */
   public Pixel obtenerMasCercanoImperativo(List<Pixel> centros) {
      Pixel minimo = centros.get(0);
      double minimaDistancia = distanciaCuadratica(minimo);

      // se consideran los demas centros
      for(int i=1; i < centros.size(); i++){
         Pixel centro = centros.get(i);
         double distancia = distanciaCuadratica(centro);

         // si es necesario, se actualiza el minimo
         if(minimaDistancia > distancia){
            minimaDistancia = distancia;
            minimo = centro;
         }
      }
      // devolver el centro mas cercano
      return minimo;
   }



   /**
    * calcula el valor de la señal de un pixel
    *
    * @return valor calculado
    */
   public double calcularSennal() {
      // se devuelve la raiz de la suma de los valores
      // al cuadrado
      return Math.sqrt(Math.pow(rojo, 2) + Math.pow(verde, 2) +
         Math.pow(azul, 2));
   }

   /**
    * calcula el ruido con respecto a otro pixel (centro)
    *
    * @param centro pixel con el que medir
    * @return valor calculado
    */
   public double calcularRuido(Pixel centro) {
      // se calcula la raiz de la diferencia cuadratica
      return Math.sqrt(this.distanciaCuadratica(centro));
   }

   /**
    * determina si el indice del color esta en un intervalo
    * concreto
    *
    * @param minimo valor minimo del intervalo
    * @param maximo valor maximo del intervalo
    * @return resultado del chequeo
    */
   public boolean enIntervalo(double minimo, double maximo) {
      // se considera el valor de indice
      return (indice >= minimo && indice <= maximo);
   }

   /**
    * metodo para mostrar el contenido del objeto por pantalla
    *
    * @return cadena con la informacion del objeto
    */
   public String toString() {
      return " r: " + rojo + " g: " + verde + " b: " + azul;
   }

   /**
    * realiza la comparacion de dos objetos de la clase pixel,
    * para permitir el agrupamiento con las carcteristicas de
    * programacion funciona
    *
    * @param otro the object to be compared.
    * @return resultado de la comparacion
    */
   @Override
   public int compareTo(Pixel otro) {
      return indice - otro.indice;
   }
}
