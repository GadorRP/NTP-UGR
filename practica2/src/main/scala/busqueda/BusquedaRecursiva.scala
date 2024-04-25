package busqueda

import scala.annotation.tailrec

object BusquedaRecursiva extends App {
   /**
    * Metodo binario de busqueda
    *
    * @param coleccion
    * @param aBuscar
    * @param esMayor
    * @tparam A
    * @return
    */
   def busquedaBinaria[A](coleccion: List[A], aBuscar: A)
                         (esMayor: (A, A) => Boolean): Int =
      def go(inicio : Int, fin : Int) : Int =
         val mitad =  inicio + (fin - inicio) / 2
         val elementoMitad = coleccion(mitad)

         if (inicio > fin) -1
         else if (elementoMitad == aBuscar) then (mitad)
         else
            if (esMayor(elementoMitad, aBuscar)) then go(inicio,mitad - 1)
            else go(mitad + 1, fin)

      go(0,coleccion.length - 1)

   /**
    * Metodo de busqueda a saltos
    *
    * @param coleccion
    * @param aBuscar
    * @param esMayor
    * @tparam A
    * @return
    */
   def busquedaSaltos[A](coleccion: List[A], aBuscar: A)
                        (esMayor: (A, A) => Boolean): Int =
      val tamBloque = Math.sqrt(coleccion.length).toInt
      val finBloque = coleccion(tamBloque - 1)

      def busquedaLineal(inicio : Int, fin : Int) : Int =
         var posicion = -1
         var encontrado = false
         for (indice <- inicio to fin if !encontrado) {
            if (coleccion(indice) == aBuscar) then
               posicion = indice
               encontrado = true
         }
         posicion

      @tailrec
      def go(iniBloque : Int, finBloque : Int) : Int =
         if (!esMayor(aBuscar, coleccion(finBloque))) {
            busquedaLineal(iniBloque, finBloque)
         }
         else {
            val inferior = Math.min(iniBloque + tamBloque, coleccion.length - 1)
            val superior = Math.min(finBloque + tamBloque, coleccion.length - 1)
            go(inferior, superior)
         }

      go(0,tamBloque - 1)

   /**
    * metodo de busqueda guiada por la serie de Fibonacci
    *
    * @param coleccion
    * @param aBuscar
    * @param esMayor
    * @tparam A
    * @return
    */
   def busquedaFibonacci[A](coleccion: List[A], aBuscar: A)
                           (esMayor: (A, A) => Boolean): Int =

      def buscarF2(n: Int): (Int,Int) = {
         def go(n: Int, prev: Int, act: Int): (Int,Int) = {
            if (act > coleccion.length) (act,prev)
            else if (n == 0) (-1,-1)
            else go(n - 1, act, prev + act)
         }

         //se genere el calculo
         go(n, 0, 1)
      }

      val tam = coleccion.length
      val tuplaF2F1 = buscarF2(tam)
      val f2 = tuplaF2F1._1
      val f1 = tuplaF2F1._2
      val f0 = f2 - f1
      val inicio0 = -1

      def go(inicio : Int, f0 : Int, f1 : Int) : Int =
         var indice =  Math.min(inicio + f0, tam - 1)

         if (f0 < 0 ) -1
         else if (f0 == 0) then
            if (aBuscar == coleccion(tam - 1)) then indice = (tam - 1)
            else indice = -1
            indice
         else
            val valorActual = coleccion(indice)
            if (valorActual == aBuscar) then indice
            else
               if !esMayor(valorActual, aBuscar) then
                  go(indice, f1 - f0, f0)
               else
                  go(inicio, f0 - (f1 - f0), f1 - f0 )

      go(inicio0, f0, f1)

   /**
    * Comparador generico para saber si x > y
    * @param x elemento generico A
    * @param y elemento generico A
    * @param ord
    * @tparam A
    * @return
    */
   def esMayor[A](x: A, y: A)(implicit ord: Ordering[A]) = ord.gt(x, y)


   //Lista y valor a buscar
   var listaBuscar = List(0, 1, 2, 3, 4, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610)
   val valorBuscado = 55

   //Prueba con búsqueda binaris
   val resBinaria = busquedaBinaria(listaBuscar, valorBuscado)(esMayor)
   println("Busqueda Binaria -> La posicion en la lista de " + valorBuscado + " es " + resBinaria)

   //Prueba con la busqueda a saltos
   val resSalto = busquedaSaltos(listaBuscar, valorBuscado)(esMayor)
   println("Busqueda a Saltos -> La posicion en la lista de " + valorBuscado + " es " + resSalto)

   //Prueba con fibonacci
   val resFibonacci = busquedaFibonacci(listaBuscar, valorBuscado)(esMayor)
   println("Busqueda Fibonacci -> La posicion en la lista de " + valorBuscado + " es " + resFibonacci)


}   
