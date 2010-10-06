package operaciones.configuracion;

/**
 * Clase que guarda la ruta del archivo que contiene la gram�tica que usar� la
 * aplicaci�n
 */
public class GramaticaConfig {

	/**
	 * Atributo de la clase que almacena la ruta en la que est� el archivo que
	 * contiene la gram�tica en uso
	 */
	public static String ruta;

	/**
	 * Constructor por defecto de la clase
	 */
	public GramaticaConfig() {
	// Usar la ultima gramatica elegida
	}

	/**
	 * Constructor que inicializa el atributo path de la clase
	 * 
	 * @param path
	 *            Ruta absoluta donde se encuentra el archivo que contiene la
	 *            gram�tica que se quiere usar
	 */
	public GramaticaConfig(String path) {
		ruta = path;
	}

	public static String getRuta() {
		return ruta;
	}

	public static void setRuta(String ruta) {
		GramaticaConfig.ruta = ruta;
	}

}
