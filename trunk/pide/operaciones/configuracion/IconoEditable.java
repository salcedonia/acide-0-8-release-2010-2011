package operaciones.configuracion;

/**
 * Clase que representa a los iconos que contendr� la barra de herramientas y
 * que ser�n configurables por el usuario
 */

public class IconoEditable {
	/**
	 * Nombre identificativo del icono
	 */
	private String nombre;
	/**
	 * Comando que ejecutar� este icono r�pido
	 */
	private String comando;
	/**
	 * Texto explicativo que muestra al dejar el cursor se�alando este icono
	 */
	private String textoAyuda;
	/**
	 * Imagen que tiene asignada este icono, en caso de que tenga alguna
	 */
	private String imagen;
	/**
	 * Indica si el icono tiene asignada una imagen
	 */
	private boolean tieneImagen;

	/**
	 * Constructor por defecto
	 */
	public IconoEditable() {
		nombre = "";
		comando = "";
		textoAyuda = "";
		tieneImagen = false;
	}

	/**
	 * Constructor que inicializa el objeto y sus atributos nombre y textoAyuda
	 * @param name String que indica el nombre que tendr� el icono
	 * @param txt String con el texto que mostrar� el icono como ayuda
	 */
	public IconoEditable(String name, String txt) {
		nombre = name;
		comando = "";
		textoAyuda = txt;
		tieneImagen = false;
	}
	
	/**
	 * Constructor que inicializa el objeto y sus atributos nombre, comando y textoAyuda
	 * @param name String que indica el nombre que tendr� el icono
	 * @param comand String que contiene el comando que ejecutar� el icono al ser pinchado
	 * @param txt String con el texto que mostrar� el icono como ayuda
	 */
	public IconoEditable(String name, String comand, String txt) {
		nombre = name;
		comando = comand;
		textoAyuda = txt;
		tieneImagen = false;
	}
	
	/**
	 * Constructor que inicializa el objeto y sus atributos nombre, comando, textoAyuda, tieneImagen e imagen
	 * @param name String que indica el nombre que tendr� el icono
	 * @param comand String que contiene el comando que ejecutar� el icono al ser pinchado
	 * @param txt String con el texto que mostrar� el icono como ayuda
	 * @param tiene booleano con el que se inicializa el atributo tieneImagen
	 * @param image String que contiene la ruta en que est� la imagen que mostrar� el icono
	 */
	public IconoEditable(String name, String comand, String txt, boolean tiene, String image) {
		nombre = name;
		comando = comand;
		textoAyuda = txt;
		imagen = image;
		tieneImagen = tiene;
	}

	public String getComando() {
		return comando;
	}

	public void setComando(String comando) {
		this.comando = comando;
	}

	public String getTextoAyuda() {
		return textoAyuda;
	}

	public void setTextoAyuda(String textoAyuda) {
		this.textoAyuda = textoAyuda;
	}

	public String getNombre() {
		return nombre;
	}

	public void setNombre(String nombre) {
		this.nombre = nombre;
	}

	public String getImagen() {
		return imagen;
	}

	public void setImagen(String imagen) {
		this.imagen = imagen;
	}

	public boolean getTieneImagen() {
		return tieneImagen;
	}

	public void setTieneImagen(boolean tieneImagen) {
		this.tieneImagen = tieneImagen;
	}

}
