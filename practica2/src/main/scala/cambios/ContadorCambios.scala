package cambios

/**
 * clase para representar cambios
 *
 * @param cantidad indica la cantidad total a devolver
 * @param cambio   mapa con clave: valor de moneda y
 *                 valor: numero de monedas de este tipo
 */
case class Cambio(val cantidad: Int, val cambio: Map[Int, Int]) {
   /**
    * metodo que calcula la cantidad restante por devolver
    *
    * @return
    */
   def restante = cantidad - cambio.map(entry => entry._1 * entry._2).sum

   /**
    * se agrega una unidad mas a la moneda cuyo valor
    * se pasa como argumento. Se devuelve un nuevo objeto
    *
    * @param moneda
    */
   def agregarMoneda(moneda: Int): Cambio = {
      if (cambio.contains(moneda)) {
         new Cambio(cantidad, cambio + (moneda -> (cambio(moneda) + 1)))
      }
      else {
         new Cambio(cantidad, cambio + (moneda -> 1))
      }
   }

   /**
    * metodo toString
    *
    * @return
    */
   override def toString: String = {
      "Cambio(cantidad: " + cantidad + " restante: " + restante +
         " cambio: " + cambio.mkString(" | ")
   }
}

/**
 * objeto para implementar los metodos de contador de cambios
 */
object ContadorCambios extends  App{
   /**
    * version inicial del contador
    *
    * @param cantidad
    * @param monedas
    * @return
    */
   def listarCambiosPosibles(cantidad: Int, monedas: List[Int]): Int =
      def go(contador: Int, cantidad : Int, monedas: List[Int]) : Int =
         if (cantidad == 0) then contador + 1
         else {
            if (monedas.isEmpty) then contador
            else {
               val nuevaMoneda = monedas.head
               val vecesUsada = cantidad / nuevaMoneda

               
               if (vecesUsada == 0) then go(contador, cantidad, monedas.tail)
               else
                  go(contador, cantidad - nuevaMoneda, monedas) +
                    go(contador, cantidad, monedas.tail)
            }
         }

      go(0,cantidad,monedas)

   def listarCambiosPosiblesV2(cantidad: Int, monedas: List[Int]): List[Cambio] =
      val cambioInicial = Cambio(cantidad, monedas.map(x => (x , 0)).toMap)
      val listaInicial : List[Cambio]= List()

      def go(monedas: List[Int], cambioActual : Cambio, totalCambio : List[Cambio]) : List[Cambio] =
         val cantidadDevolver = cambioActual.restante

         if (cantidadDevolver == 0) then totalCambio :+ cambioActual
         else {
            if (monedas.isEmpty) then totalCambio
            else {
               val nuevaMoneda = monedas.head
               val vecesUsada = cantidadDevolver / nuevaMoneda

               if (vecesUsada == 0) then go(monedas.tail, cambioActual, totalCambio)
               else
                  val nuevoCambio = cambioActual.agregarMoneda(nuevaMoneda)

                  //cambio habiendo utilizado la moneda
                  val cambio1 = go(monedas, nuevoCambio, totalCambio)
                  //cambio si no utilizas esa moneda
                  val cambio2 = go(monedas.tail, cambioActual, totalCambio)

                  //unimos ambas opciones y devolvemos
                  cambio1 ::: cambio2
            }
         }

      go(monedas, cambioInicial, listaInicial)


   var listaMonedas  = List(1,5,2)
   var cantidad = 10

   var numcambios = listarCambiosPosibles(cantidad,listaMonedas)
   var numCambiosV2 = listarCambiosPosiblesV2(cantidad,listaMonedas)
   println("El numero de cambios para la cantidad " + cantidad + " es " + numcambios)
   println("A continuacion se muestran los cambios:")

   var numCambio = 1
   numCambiosV2.foreach(
      cambio => {
         println(numCambio + " " + cambio.toString)
         numCambio += 1
      }
   )
}
