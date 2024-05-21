import scala.annotation.tailrec

class Conjunto(fCarac : Int => Boolean) :
    private val LIMITE = 10

    def apply(x : Int): Boolean = fCarac(x)

    override def toString: String =
        (-LIMITE to LIMITE).filter(x => apply(x)).toArray.mkString(" ")

    def conjuntoUnElemento(elem : Int) : Conjunto =
        Conjunto(x => elem == x)

    def union(otro : Conjunto) : Conjunto =
        val nuevaFCarac = (x : Int) => fCarac(x) || otro(x)
        Conjunto(nuevaFCarac)

    def interseccion(otro: Conjunto): Conjunto =
        val nuevaFCarac = (x: Int) => fCarac(x) && otro(x)
        Conjunto(nuevaFCarac)

    def diferencia(otro: Conjunto): Conjunto =
        val nuevaFCarac = (x: Int) => fCarac(x) && !otro(x)
        Conjunto(nuevaFCarac)

    def filtrar(filtro : Int => Boolean) : Conjunto =
        val nuevaFCarac = (x : Int) => filtro(x) && fCarac(x)
        Conjunto(nuevaFCarac)

    def paraTodo(condicion : Int => Boolean) : Boolean =
        @tailrec
        def go(elemento : Int) : Boolean =
            if (elemento > LIMITE) true
            else if ( fCarac(elemento) && !condicion(elemento)) false
            else
                go(elemento + 1)

        go(-LIMITE)


    def existe(condicion : Int => Boolean) : Boolean =
        !paraTodo(x => !condicion(x))

    def map(transformacion : Int => Int) : Conjunto =
        Conjunto((y : Int) => existe(x => transformacion(x) == y))

end Conjunto

object Conjuntos extends App{
    var cNegativo = Conjunto(x => x < 0)
    var cPares = Conjunto(x  => x % 2 == 0)
    println("numeros negativos: " + cNegativo.toString)
    println("numeros pares: " + cPares.toString)

    println("Pertenece 5 a los negativos: " + cNegativo(5))

    var cUnElemento = cNegativo.conjuntoUnElemento(2)
    println("conjunto un elemento: " + cUnElemento.toString)

    var cUnionParesNegativos = cNegativo.union(cPares)
    println("union de negativos y pares: " + cUnionParesNegativos)

    var cInterParesNegativos = cNegativo.interseccion(cPares)
    println("interseccion de negativos y pares: " + cInterParesNegativos)

    var cDiferenciaNegativosPares = cNegativo.diferencia(cPares)
    println("diferencia de negativos y pares: " + cDiferenciaNegativosPares)

    var cPositivos = Conjunto(x => x > 0)
    println("Conjunto positivos: " + cPositivos)
    var cPositivoFiltradosImpares = cPositivos.filtrar(x => x % 2 == 1)
    println("Conjunto positivo filtrado para obtener los impares: " + cPositivoFiltradosImpares)

    println("Todos los positivos son mayores que -10: " + cPositivos.paraTodo(x => x > -10))
    println("Todos los positivos son pares: " + cPositivos.paraTodo(x => x % 2 == 0))

    println("Existe un positivo que es 3: " + cPositivos.existe(x => x == 3))
    println("Existe un positivo que es -20: " + cPositivos.existe(x => x == -20))

    println("conjunto de los positivos menos 2 (limite 10): " + cPositivos.map(x => x - 2))
}
