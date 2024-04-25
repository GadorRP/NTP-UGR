package ochoReinas

import scala.annotation.tailrec


class Buscador (val dimension : Int) {
   /**
    * metodo para ubicar obtener el tablero
    * con las reina colocadas
    */
   def resolver : Tablero =
      val tableroIni = Tablero(dimension,List())

      def go(columna : Int, anterior : Tablero) : Tablero =
         val fila = anterior.contenido.length
         val reina = Celda(fila, columna)

         if (anterior.contenido.length == dimension) then
            return anterior

         else if (!Conflicto.conflictoCeldaTablero(reina,anterior))
            val nuevoTablero = anterior.agregarReina(fila, columna)
            val posibleTablero = go(0,nuevoTablero)

            val tamPosible = posibleTablero.contenido.length
            //si el tablero devuelto no tiene todas las reinas -> camino equivocado
            //se continua al if siguiente para avanzar de columna
            if (tamPosible == dimension) then return posibleTablero //tablero final

         if ((columna + 1) < anterior.dimension)
            go(columna + 1, anterior)
         else
            anterior

      go(0, tableroIni)


}

object PruebaReina extends App{
   val tableroReinas = Buscador(4).resolver

   println(tableroReinas.contenido.toString())
   print(tableroReinas.toString)
}
