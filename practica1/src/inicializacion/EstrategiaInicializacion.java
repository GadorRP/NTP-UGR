package inicializacion;

import kmedias.KMedias;
import imagen.Pixel;

import java.util.List;

/**
 * interfaz para representar de forma generica
 * el mecanismo de inicializacion del algoritmo
 */
public interface EstrategiaInicializacion {
   /**
    * metodo de seleccion inicial de centroides
    *
    * @param kmedias objeto a inicializar
    * @return lista de pixels seleccionados como
    * centroides
    */
   List<Pixel> seleccionar(KMedias kmedias);
}
