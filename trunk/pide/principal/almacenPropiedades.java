package principal;

import idioma.Idioma;

import java.util.Properties;
import java.util.HashMap;
import java.util.ResourceBundle;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import operaciones.log.Log;
import org.apache.log4j.Logger;

/**
 * Clase encargada de cargar valores de un archivo de recursos
 *
 */
public class almacenPropiedades {

	private static final String CONFIGURATION_FILE = ".\\configuration\\Configuracion.properties";
	private static HashMap propiedades;
	private static Properties propiedadesTemporales;

	

	
	/*
	 * Bloque de inicializacion
	 */
	static {

		try {
			
			FileInputStream f = new FileInputStream(CONFIGURATION_FILE);
			propiedadesTemporales = new Properties();
			propiedadesTemporales.load(f);
			f.close();
			propiedades = new HashMap(propiedadesTemporales);
			
		} catch (Exception e) {
			/*
			 * Manejo de excepciones
			 */
			System.out.print(e);
		}
	}

	private almacenPropiedades() {
	}

	/**
	 * Devuelve un valor asociado a un nombre que le pasamos como parametro
	 * 
	 * @param nombre
	 * @return
	 * @throws FaltaPropiedadException
	 */
	public static String getPropiedad(String nombre)
			throws FaltaPropiedadException {

		String valor = (String) propiedades.get(nombre);

		if (valor == null)
			throw new FaltaPropiedadException(nombre);
		
		if(valor.contains("(Sin especificar)"))
				throw new FaltaPropiedadException(nombre);

		return valor;
	}

	/**
	 * Instancia un valor asociado a un nombre que le pasamos como parametro
	 * 
	 * @param nombre
	 * @param valor
	 */
	public static void setPropiedad(String nombre, String valor) {
		try {
			propiedadesTemporales.setProperty(nombre, valor);
			propiedadesTemporales.store(new FileOutputStream(CONFIGURATION_FILE), null);
			try {
				
				FileInputStream f = new FileInputStream(CONFIGURATION_FILE);
				propiedadesTemporales = new Properties();
				propiedadesTemporales.load(f);
				f.close();
				propiedades = new HashMap(propiedadesTemporales);
				
		} catch (Exception e) {
				/*
				 * Manejo de excepciones
				 */
				System.out.print(e);
			}
		}
	 catch (Exception e) {
		/*
		 * Manejo de excepciones
		 */
		 System.out.print(e);
	}
		}

}

/**
 * Maneja las posibles excepciones asociadas a la clase almacenPropiedades
 *
 */
class FaltaPropiedadException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String nombreParametro;
	private static ResourceBundle labels = Idioma.getInstance().getLabels();
	
	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	/**
	 * Indica que parametro ha fallado
	 * @param nombreParametro
	 */
	public FaltaPropiedadException(String nombreParametro) {
		super(labels.getString("s426") + nombreParametro + "'");
		logger.info(labels.getString("s427") + nombreParametro + "'");
		this.nombreParametro = nombreParametro;
	}

	/**
	 * Devuelve el nombre de un parametro
	 * 
	 * @return
	 */
	public String getNombreParametro() {
		return nombreParametro;
	}
}
