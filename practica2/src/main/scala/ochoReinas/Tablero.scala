package ochoReinas

import scala.annotation.tailrec

/**
 * clase para representar celdas del tablero
 *
 * @param fila
 * @param columna
 */
case class Celda(val fila : Int, val columna : Int)

/**
 * objeto con metodos para determinar conflicto entre celdas
 */
object Conflicto {

   /**
    * determina si se genera conflicto al agregar una nueva reina
    * en la celda indicada
    * @param celda
    * @param tablero
    * @return
    */
   def conflictoCeldaTablero(celda : Celda, tablero : Tablero) : Boolean =
      var conflicto = false
      for (casilla <- tablero.contenido if !conflicto)
         if (casilla.columna == celda.columna) then conflicto = true
         else if (casilla.fila == celda.fila) then conflicto = true
         else

            //las diagonales avanzan las mismas i casillas en filas y columnas
            //desde la casilla donde esta situada la reina que causa la diagonal
            for( i <- 0 until tablero.dimension) {
               val nuevaFila = casilla.fila + i
               val nuevaColumnaIz = casilla.columna + i
               val nuevaColumnaDe = casilla.columna - i

               //si esta en la fila de la nueva posible reina
               if (nuevaFila < tablero.dimension && nuevaFila == celda.fila)
                 if (nuevaColumnaIz < tablero.dimension && nuevaColumnaIz == celda.columna)
                     conflicto = true
                 else if (nuevaColumnaDe < tablero.dimension && nuevaColumnaDe  == celda.columna)
                     conflicto = true
            }

      conflicto

}

/**
 * clase para representar el tablero
 * @param dimension numero de filas y columnas
 * @param contenido contenido del tablero, solo de las
 *                  celdas ocupadas
 */
class Tablero(val dimension : Int, val contenido : List[Celda]) {

   /**
    * se agrega nueva reina al tablero y se genera un tablero
    * nuevo
    * @param fila
    * @param columna
    * @return
    */
   def agregarReina(fila : Int, columna : Int) : Tablero =
      val nuevoContenido = contenido.::(Celda(fila, columna))
      Tablero(dimension, nuevoContenido)

   /**
    * metodo to string
    * @return
    */
   override def toString : String =
      val tableroString = new StringBuilder 

      for (fila <- dimension - 1 to 0 by -1) {
         for (columna <- 0 until dimension) {
            if (contenido.contains(Celda(fila, columna))) {
               tableroString.append("|R| ")
            } else {
               tableroString.append("|_| ")

            }
         }

         tableroString.append("\n") // Append newline after each row to the StringBuilder
      }
      tableroString.toString()
}
