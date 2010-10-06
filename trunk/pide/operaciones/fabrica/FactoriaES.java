package operaciones.fabrica;

import es.texto.Fichero;

/**Clase que crea Objetos del paquete entrada/salida
 * 
 *
 */
public class FactoriaES {
	
	private static FactoriaES instancia;

	//Crea una unica instancia de FactoriaES
	public static FactoriaES getInstance() {
		if (instancia == null)
			instancia = new FactoriaES();
		return instancia;
	}

	public Fichero generaFichero() {
		return new Fichero();
	}
}
