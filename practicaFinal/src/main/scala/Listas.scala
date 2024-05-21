import scala.annotation.tailrec

/**
 * Interfaz generica para la lista
 *
 * @tparam A
 */
sealed trait Lista[+A]

/**
 * Objeto para definir lista vacia
 */
case object Nil extends Lista[Nothing]

/**
 * Clase para definir la lista como compuesta por elemento inicial
 * (cabeza) y resto (cola)
 * @param cabeza elemento inicial
 * @param cola resto
 * @tparam A
 */

case class Cons[+A](cabeza : A, cola : Lista[A]) extends Lista[A]

object Lista extends App:
    /**
     * Metodo para permitir crear listas sin usar new
     * @param elementos secuencia de elementos a incluir en la lista
     * @param A
     * @return
     */
    def apply[A](elementos : A*) : Lista[A] =
        if (elementos.isEmpty) Nil
        else Cons(elementos.head, apply(elementos.tail*))

    /**
     * Obtiene la longitud de una lista
     * @param lista
     * @param A
     * @return
     */
    def longitud[A](lista : Lista[A]) : Int =
        lista match
            case Nil => 0
            case Cons(cabeza, cola) => 1 + longitud(cola)


    /**
     * Metodo para sumar los valores de una lista de enteros
     * @param enteros
     * @return
     */
    def sumaEnteros(enteros : Lista[Int]) : Double =
        enteros match
            case Nil => 0
            case Cons(cabeza, cola) => cabeza + sumaEnteros(cola)


    /**
     * Metodo para multiplicar los valores de una lista de enteros
     * @param enteros
     * @return
     */
    def productoEnteros(enteros : Lista[Int]) : Double =
        enteros match
            case Nil => 1
            case Cons(cabeza,cola) =>
                cabeza * productoEnteros(cola)

    /**
     * Metodo para agregar el contenido de dos listas
    *
     * @param lista1
     * @param lista2
     * @param A
     * @return
     */
    def concatenar[A](lista1: Lista[A], lista2: Lista[A]): Lista[A] =
        lista1 match
            case Nil => lista2 match
                case Nil => Nil
                case Cons(cabeza, cola) => lista2
            case Cons(cabeza,cola) => Cons(cabeza, concatenar(cola,lista2))


    /**
     * Funcion de utilidad para aplicar una funcion de forma sucesiva a los
     * elementos de la lista con asociatividad por la derecha
     *
     * @param lista
     * @param neutro
     * @param funcion
     * @param A
     * @param B
     * @return
     */
    def foldRight[A, B](lista : Lista[A], neutro : B)(funcion : (A, B) => B): B =
        lista match
            case Nil => neutro
            case Cons(cabeza, cola) => funcion(cabeza, foldRight(cola,neutro)(funcion))

    /**
     * Suma mediante foldRight
     * @param listaEnteros
     * @return
     */
    def sumaFoldRight(listaEnteros : Lista[Int]) : Double =
        foldRight(listaEnteros, 0 )((a, b) =>  a + b)

    /**
     * Producto mediante foldRight
     * @param listaEnteros
     * @return
     */
    def productoFoldRight(listaEnteros : Lista[Int]) : Double =
        foldRight(listaEnteros, 1)((a, b) => a * b)

    /**
     * Reemplaza la cabeza por nuevo valor. Se asume que si la lista esta vacia
     * se devuelve una lista con el nuevo elemento
     *
     * @param lista
     * @param cabezaNueva
     * @param A
     * @return
     */
    def asignarCabeza[A](lista : Lista[A], cabezaNueva : A) : Lista[A] =
        lista match
            case Nil => Lista(cabezaNueva)
            case Cons(cabeza, cola) => Cons(cabezaNueva, concatenar(Lista(cabeza), cola) )

    /**
     * Devuelve el primer elemento de la lista
     * (si no esta vacia). Por eso se devuelve Option
 *
     * @param lista
     * @tparam A
     * @return
     */
    def head[A](lista : Lista[A]) : Option[A] =
        lista match
            case Nil => None
            case Cons(cabeza,cola) => Option(cabeza)

    /**
     * Elimina el elemento cabeza de la lista
     *
     * @param lista
     * @param A
     * @return
     */
    def tail[A](lista : Lista[A]): Lista[A] =
        lista match
            case Nil => Nil
            case Cons(cabeza, cola) => cola
    /**
     * Elimina los n primeros elementos de una lista
     *
     * @param lista lista con la que trabajar
     * @param n numero de elementos a eliminar
     * @param A tipo de datos
     * @return
     */
    @tailrec
    def eliminar[A](lista : Lista[A], n: Int) : Lista[A] =
        lista match
            case Nil => Nil
            case Cons(cabeza, cola) =>
                if (n == 0)
                    lista
                else
                    eliminar(tail(lista), n - 1)
    /**
     * Elimina elementos mientra se cumple la condicion pasada como
     * argumento
     *
     * @param lista lista con la que trabajar
     * @param criterio predicado a considerar para continuar con el borrado
     * @return
     */
    @tailrec
    def eliminarMientras[A](lista : Lista[A], criterio: A => Boolean) : Lista[A]
        = lista match
        case Nil => Nil
        case Cons(cabeza, cola) =>
            if (!criterio(cabeza))
                lista
            else
                eliminarMientras(cola,criterio)

    /**
     * Elimina el ultimo elemento de la lista. Aqui no se pueden compartir
     * datos en los objetos y hay que generar una nueva lista copiando
     * datos
     *
     * @param lista lista con la que trabajar
     * @param A tipo de datos de la lista
     * @return
     */
    def eliminarUltimo[A](lista : Lista[A]) : Lista[A] =
        lista match
            case Nil => Nil
            case Cons(cabeza, Nil) => Nil
            case Cons(cabeza,cola) => Cons(cabeza, eliminarUltimo(cola))


    /**
     * foldLeft con recursividad tipo tail
     * @param lista lista con la que trabajar
     * @param neutro elemento neutro
     * @param funcion funcion a aplicar
     * @param A parametros de tipo de elementos de la lista
     * @param B parametro de tipo del elemento neutro
     * @return
     */
    @annotation.tailrec
    def foldLeft[A, B](lista : Lista[A], neutro: B)(funcion : (B, A) => B): B =
        lista match
            case Nil => neutro
            case Cons(cabeza, cola) =>
                val accum = funcion(neutro, cabeza)
                foldLeft(cola,accum)(funcion)


    //comprobaciones
    var listaVacia = Lista()
    println("listaVacia: " + listaVacia)

    var lista1 = Lista(1,2,3,4,5)
    println("lista1: " + lista1)

    println("tamaño lista vacia: " + longitud(listaVacia))
    println("tamaño lista1: " + longitud(lista1))

    //no es lista de ints
    //println("sumaEnteros lista vacia: " + sumaEnteros(listavacia))
    println("sumaEnteros lista1: " + sumaEnteros(lista1))
    println("productoEnteros lista1: " + productoEnteros(lista1))

    var lista2 = Lista(2,2,3)
    println("lista2: " + lista2)
    println("concatenar lista1 lista2:" + concatenar(lista1,lista2))
    println("concatenar lista1 listaVacia:" + concatenar(lista1,listaVacia))
    println("concatenar listaVacia lista2:" + concatenar(listaVacia,lista1))
    println("concatenar listaVacia listaVacia:" + concatenar(listaVacia,listaVacia))
    println()

    println("foldRight sumando dobles lista1:" + foldRight(lista1, 0)((x,y) => x * 2 + y))
    println("foldRight Suma lista1:" + sumaFoldRight(lista1))
    println("foldRight multiplica lista1:" + productoFoldRight(lista1))
    println()

    println("asignar cabeza lista1: " + asignarCabeza(lista1,10))
    println("head lista1: " + head(lista1))
    println("tail lista1: " + tail(lista1))
    println("asignar cabeza listaVacia: " + asignarCabeza(listaVacia, 10))
    println("head listaVacia: " + head(listaVacia))
    println("tail listaVacia: " + tail(listaVacia))
    println()

    println("eliminar 3 lista1: " + eliminar(lista1,3))
    println("eliminar mientras menor 4 lista1: " + eliminarMientras(lista1, x => x < 4))
    println("eliminar ultimo lista1: " + eliminarUltimo(lista1))
    println()

    println("eliminar 3 listaVacia: " + eliminar(listaVacia, 3))
    println("eliminar todos lista1: " + eliminarMientras(lista1, x => x < 20))
    println("eliminar ultimo listaVacia: " + eliminarUltimo(listaVacia))
    println()

    println("foldLeft sumando dobles lista1:" + foldLeft(lista1, 0)((x, y) => x + y * 2 ))

end Lista




