package idioma;

import java.util.Locale;
import java.util.ResourceBundle;
import javax.swing.UIManager;
import operaciones.log.Log;
import org.apache.log4j.Logger;

public class Idioma {

	private static Locale currentLocale;
	private static String bundle;
	private static ResourceBundle labels;
	/**
	 * Atributo: Instancia Idioma
	 * 
	 */
	private static Idioma instancia;

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	
	/**
	 * Crea una unica instancia de Idioma
	 * 
	 * @return
	 */
	public static Idioma getInstance() {
		if (instancia == null)
			instancia = new Idioma();
		return instancia;
	}
	
	/**
	 * Constructor de la clase idioma
	 */
	public Idioma() {
		super();
	}
	
	/**
	 * Seleccionamos el idioma activo en funcion de un paramerto de entrada
	 * @param n
	 */
	public void seleccionIdioma(int n){
		
		try {
			
			
			
			if ( n == 1){
				currentLocale = new Locale("en","EN");
				bundle = "configuration.language.PideBundle_en";
			}
			else{	
				currentLocale = new Locale("es","ES");
				bundle = "configuration.language.PideBundle_es";
			}
			
			labels = ResourceBundle.getBundle(bundle, currentLocale);
			UIManager.put("FileChooser.saveButtonText",labels.getString("s40"));
			UIManager.put("FileChooser.openButtonText",labels.getString("s41"));
			UIManager.put("FileChooser.cancelButtonText",labels.getString("s42"));
			UIManager.put("FileChooser.updateButtonText",labels.getString("s43"));
			UIManager.put("FileChooser.helpButtonText",labels.getString("s44"));
			UIManager.put("FileChooser.saveButtonToolTipText",labels.getString("s45"));			
			UIManager.put ("FileChooser.openButtonToolTipText", labels.getString("s46"));
			UIManager.put ("FileChooser.cancelButtonToolTipText", labels.getString("s47"));
			UIManager.put ("FileChooser.fileNameLabelText", labels.getString("s48"));
			UIManager.put ("FileChooser.lookInLabelText", labels.getString("s49"));
			UIManager.put ("FileChooser.upFolderToolTipText", labels.getString("s50"));
			UIManager.put ("FileChooser.newFolderToolTipText", labels.getString("s51"));
			UIManager.put ("FileChooser.newFolderAccessibleName", labels.getString("s52"));
			UIManager.put ("FileChooser.listViewButtonToolTipText", labels.getString("s53"));
			UIManager.put ("FileChooser.detailsViewButtonToolTipText", labels.getString("s54"));
			UIManager.put ("FileChooser.filesOfTypeLabelText", labels.getString("s55"));
			UIManager.put ("FileChooser.acceptAllFileFilterText", labels.getString("s56"));
			UIManager.put ("FileChooser.fileNameHeaderText", labels.getString("s57"));
			UIManager.put ("FileChooser.fileSizeHeaderText", labels.getString("s58"));
			UIManager.put ("FileChooser.fileTypeHeaderText", labels.getString("s59"));
			UIManager.put ("FileChooser.fileDateHeaderText", labels.getString("s60"));
			UIManager.put ("FileChooser.fileAttrHeaderText", labels.getString("s61"));
			UIManager.put("OptionPane.yesButtonText", labels.getString("s62"));
			UIManager.put("OptionPane.noButtonText", labels.getString("s63"));
			UIManager.put("OptionPane.cancelButtonText", labels.getString("s64"));
			UIManager.put("OptionPane.okButtonText", labels.getString("s548"));

		} catch (RuntimeException e) {
			e.printStackTrace();
		}
		
	}
	
	/**
	 * Prueba del correcto funcionamiento de la clase idioma
	 * @param labels
	 */
	public void prueba(ResourceBundle labels){
		String value = labels.getString("s1");
		System.out.print(value);
	}
	
	/**
	 * Devuelve los textos que usaremos para los labels de toda la aplicacion
	 * @return
	 */
	public ResourceBundle getLabels(){
		return labels; 
	}

	public Locale getCurrentLocale(){
		return currentLocale;
	}
}
