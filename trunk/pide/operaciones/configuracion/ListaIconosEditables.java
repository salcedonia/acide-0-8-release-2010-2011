package operaciones.configuracion;

import idioma.Idioma;
import java.util.ResourceBundle;
import java.util.Vector;
import principal.almacenPropiedades;
import es.texto.Fichero;

/**
 * Clase que implementa la lista en la que estarán almacenados los objetos de
 * tipo IconoEditable
 */

public class ListaIconosEditables {
	
	/**
	 * Atributo en el que se almacena la lista de objetos de tipo IconoEditable
	 */
	private static Vector listaIconos;
	/**
	 * Atributo en el que se almacena la lista de objetos de tipo IconoEditable
	 * que sera usada en la configuración de la barra de herramientas para no
	 * modificar la barra de herramientas en uso hasta que el usuario indique lo
	 * contrario
	 */
	private static Vector listaIconosAux;
	
	/**
	 * Constructor de la clase que inicializa los atributos como vectores nuevos
	 * y vacíos
	 */
	public ListaIconosEditables() {
		listaIconos = new Vector();		
		listaIconosAux = new Vector();
	}
	
	/**
	 * Inicializa de nuevo los atributos de tipo Vector listaIconos y
	 * listaIconosAux
	 */
	public static void limpiaListas() {
		listaIconos = new Vector();		
		listaIconosAux = new Vector();
	}
	
	/**
	 * Método que guarda la lista de iconos editables en un archivo
	 * 
	 * @param ruta
	 *            String que contiene la ruta absoluta del archivo en que se
	 *            quiere guardar
	 * @return booleano que indica si se ha guardado la lista con éxito
	 */	
	public static boolean guardarLista(String ruta) {
		String txt = "";
		for(int i = 0; i < listaIconos.size(); i++) {
			IconoEditable icon = (IconoEditable) listaIconos.get(i);
			String nombre = icon.getNombre();
			String comando = icon.getComando();
			String txtAyuda = icon.getTextoAyuda();
			String image;
			if(icon.getTieneImagen()) {
				image = icon.getImagen(); 
				txt += "]N] " + nombre + " ]C] " + comando + " ]T] " + txtAyuda + " ]B] true ]I] " + image + " ]F]\n"; 			
			}
			else txt += "]N] " + nombre + " ]C] " + comando + " ]T] " + txtAyuda + " ]B] false ]I] ]F]\n";			 
		} 
		Fichero f = new Fichero();
		return f.salvar(ruta,txt);
	}
	
	/**
	 * Método que guarda en un archivo la lista de iconos editables contenida en
	 * listaIconosAux, que se usa para las operaciones de configuración de la
	 * barra de herramientas para no modificar la barra que está en uso hasta
	 * que el usuario indique lo contrario
	 * 
	 * @param ruta
	 *            String que contiene la ruta absoluta del archivo en que se
	 *            quiere guardar
	 * @return booleano que indica si se ha guardado la lista con éxito
	 */
	public static boolean guardarListaAux(String ruta) {
		String txt = "";
		for(int i = 0; i < listaIconosAux.size(); i++) {
			IconoEditable icon = (IconoEditable) listaIconosAux.get(i);
			String nombre = icon.getNombre();
			String comando = icon.getComando();
			String txtAyuda = icon.getTextoAyuda();
			String image;
			if(icon.getTieneImagen()) {
				image = icon.getImagen(); 
				txt += "]N] " + nombre + " ]C] " + comando + " ]T] " + txtAyuda + " ]B] true ]I] " + image + " ]F]\n"; 			
			}
			else txt += "]N] " + nombre + " ]C] " + comando + " ]T] " + txtAyuda + " ]B] false ]I] ]F]\n";			 
		} 
		Fichero f = new Fichero();
		return f.salvar(ruta,txt);
	}
	
	/**
	 * Método que carga la lista de iconos editables desde un archivo
	 * 
	 * @param ruta 
	 * 			String que contiene la ruta absoluta del archivo a cargar
	 * @throws Exception
	 *             En caso de que el formato del archivo no sea el exigido para
	 *             poder cargar la lista. El formato es: ]N] nombre ]C] comando
	 *             ]T] txtAyuda ]B] booleano ]I] imagen ]F]
	 */
	public static void cargaLista(String ruta) throws Exception {
		listaIconos.removeAllElements();
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Fichero f = new Fichero();
		String bhTxt = f.cargar(ruta);
		char c;
		int indexAux = 0;
		String nombre;
		String comando;
		String txtAyuda;
		String imagen;
		for(int index = 0; index < bhTxt.length(); index++) {
			index = bhTxt.indexOf("]N]");
			if(index == -1) {
				/*
				 * Si no encontramos la cadena ]N] e indexAux esta a 0 quiere
				 * decir que estamos en el comienzo del archivo y que no
				 * contiene un formato valido
				 */ 
				if(indexAux == 0) throw new Exception(labels.getString("s129"));
				// No hay mas iconos en el archivo
				else index = bhTxt.length();
			}
			else {
				// Nombre
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]C]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				nombre = bhTxt.substring(0, indexAux - 1);
				index = indexAux;
				// Comando
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]T]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				comando = bhTxt.substring(0,indexAux - 1);
				index = indexAux;
				// Texto Ayuda
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]B]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				txtAyuda = bhTxt.substring(0,indexAux - 1);
				index = indexAux;
				// Tiene Imagen
				index += 4;
				c = bhTxt.charAt(index);
				// Si tiene imagen
				if(c == 't') {
					index = bhTxt.indexOf("]I]");
					if(index == -1) throw new Exception(labels.getString("s129"));
					index += 4;
					bhTxt = bhTxt.substring(index);
					indexAux = bhTxt.indexOf("]F]");
					if(indexAux == -1) throw new Exception(labels.getString("s129"));
					imagen = bhTxt.substring(0,indexAux - 1);
					index = indexAux;
					listaIconos.add(new IconoEditable(nombre,comando,txtAyuda,true,imagen));
					//Comprobamos si hay algun icono mas
					if(bhTxt.indexOf("]N]") == -1) index = bhTxt.length();
				}
				// Si no tiene imagen
				else if (c == 'f') {
					listaIconos.add(new IconoEditable(nombre,comando,txtAyuda));
					// Comprobamos si hay algun icono mas
					if(bhTxt.indexOf("]N]") == -1) index = bhTxt.length();
				}
				else throw new Exception(labels.getString("s129"));
			}
		}
	}
	
	/**
	 * Método que carga la lista de iconos editables desde un archivo en
	 * listaIconosAux, que se usa para las operaciones de configuración de la
	 * barra de herramientas para no modificar la barra que está en uso hasta
	 * que el usuario indique lo contrario
	 * 
	 * @param ruta
	 *            String que contiene la ruta absoluta del archivo a cargar
	 * @throws Exception
	 *             En caso de que el formato del archivo no sea el exigido para
	 *             poder cargar la lista. El formato es: ]N] nombre ]C] comando
	 *             ]T] txtAyuda ]B] booleano ]I] imagen ]F]
	 */
	public static void cargaListaAux(String ruta) throws Exception {
		listaIconosAux.removeAllElements();
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Fichero f = new Fichero();
		String bhTxt = f.cargar(ruta);
		char c;
		int indexAux = 0;
		String nombre;
		String comando;
		String txtAyuda;
		String imagen;
		for(int index = 0; index < bhTxt.length(); index++) {
			index = bhTxt.indexOf("]N]");
			if(index == -1) {
				/*
				 * Si no encontramos la cadena ]N] e indexAux esta a 0 quiere
				 * decir que estamos en el comienzo del archivo y que no
				 * contiene un formato valido
				 */ 
				if(indexAux == 0) throw new Exception(labels.getString("s129"));
				// No hay mas iconos en el archivo
				else index = bhTxt.length();
			}
			else {
				// Nombre
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]C]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				nombre = bhTxt.substring(0, indexAux - 1);
				index = indexAux;
				// Comando
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]T]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				comando = bhTxt.substring(0,indexAux - 1);
				index = indexAux;
				// Texto Ayuda
				index += 4;
				bhTxt = bhTxt.substring(index);
				indexAux = bhTxt.indexOf("]B]");
				if(indexAux == -1) throw new Exception(labels.getString("s129"));
				txtAyuda = bhTxt.substring(0,indexAux - 1);
				index = indexAux;
				// Tiene Imagen
				index += 4;
				c = bhTxt.charAt(index);
				// Si tiene imagen
				if(c == 't') {
					index = bhTxt.indexOf("]I]");
					if(index == -1) throw new Exception(labels.getString("s129"));
					index += 4;
					bhTxt = bhTxt.substring(index);
					indexAux = bhTxt.indexOf("]F]");
					if(indexAux == -1) throw new Exception(labels.getString("s129"));
					imagen = bhTxt.substring(0,indexAux - 1);
					index = indexAux;
					listaIconosAux.add(new IconoEditable(nombre,comando,txtAyuda,true,imagen));
				}
				// Si no tiene imagen
				else if (c == 'f') {
					listaIconosAux.add(new IconoEditable(nombre,comando,txtAyuda));
				}
				else throw new Exception(labels.getString("s129"));
			}
		}
	}
	
	public static boolean añadirIcono(IconoEditable i) {
		return listaIconos.add(i);
	}
	
	public static boolean quitarIcono(IconoEditable i) {
		return listaIconos.remove(i);
	}
	
	public static int getTamaño() {
		return listaIconos.size();
	}
	
	public static IconoEditable getIcono(int i) {
		return (IconoEditable) listaIconos.get(i);
	}

	public static Vector getListaIconos() {
		return listaIconos;
	}

	public static void setListaIconos(Vector lista) {
		listaIconos = lista;
	}
	
	public static void setListaIconosAux(Vector lista) {
		listaIconosAux = lista;
	}
	
	public static Vector getListaIconosAux() {
		return listaIconosAux;
	}

}
