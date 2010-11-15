package language;

import java.util.Locale;
import java.util.ResourceBundle;
import javax.swing.UIManager;

import operations.log.Log;

/************************************************************************																
 * Handles the language to display on ACIDE - A Configurable IDE											
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8																														
 ***********************************************************************/
public class Language {

	/**
	 * Current locale for the language
	 */
	private static Locale _currentLocale;
	/**
	 * Bundle for the language
	 */
	private static String _bundle;
	/**
	 * Labels to display
	 */
	private static ResourceBundle _labels;
	/**
	 * Class instance
	 */
	private static Language _instance;

	/**
	 * Returns the unique class instance
	 * 
	 * @return the unique class instance
	 */
	public static Language getInstance() {
		if (_instance == null)
			_instance = new Language();
		return _instance;
	}

	/**
	 * Class constructor
	 */
	public Language() {
		super();
	}

	/**
	 * Sets the language to display
	 * 
	 * @param string code of the language
	 */
	public void getLanguage(String string) {

		try {

			if(string.matches("english")){
				_currentLocale = new Locale("en", "EN");
				_bundle = "properties.language.english";
			}
			
			if(string.matches("spanish")){ 
				_currentLocale = new Locale("es", "ES");
				_bundle = "properties.language.spanish";
			}

			_labels = ResourceBundle.getBundle(_bundle, _currentLocale);
			
			UIManager.put("FileChooser.saveButtonText",
					_labels.getString("s40"));
			UIManager.put("FileChooser.openButtonText",
					_labels.getString("s41"));
			UIManager.put("FileChooser.cancelButtonText",
					_labels.getString("s42"));
			UIManager.put("FileChooser.updateButtonText",
					_labels.getString("s43"));
			UIManager.put("FileChooser.helpButtonText",
					_labels.getString("s44"));
			UIManager.put("FileChooser.saveButtonToolTipText",
					_labels.getString("s45"));
			UIManager.put("FileChooser.openButtonToolTipText",
					_labels.getString("s46"));
			UIManager.put("FileChooser.cancelButtonToolTipText",
					_labels.getString("s47"));
			UIManager.put("FileChooser.fileNameLabelText",
					_labels.getString("s48"));
			UIManager.put("FileChooser.lookInLabelText",
					_labels.getString("s49"));
			UIManager.put("FileChooser.upFolderToolTipText",
					_labels.getString("s50"));
			UIManager.put("FileChooser.newFolderToolTipText",
					_labels.getString("s51"));
			UIManager.put("FileChooser.newFolderAccessibleName",
					_labels.getString("s52"));
			UIManager.put("FileChooser.listViewButtonToolTipText",
					_labels.getString("s53"));
			UIManager.put("FileChooser.detailsViewButtonToolTipText",
					_labels.getString("s54"));
			UIManager.put("FileChooser.filesOfTypeLabelText",
					_labels.getString("s55"));
			UIManager.put("FileChooser.acceptAllFileFilterText",
					_labels.getString("s56"));
			UIManager.put("FileChooser.fileNameHeaderText",
					_labels.getString("s57"));
			UIManager.put("FileChooser.fileSizeHeaderText",
					_labels.getString("s58"));
			UIManager.put("FileChooser.fileTypeHeaderText",
					_labels.getString("s59"));
			UIManager.put("FileChooser.fileDateHeaderText",
					_labels.getString("s60"));
			UIManager.put("FileChooser.fileAttrHeaderText",
					_labels.getString("s61"));
			UIManager.put("OptionPane.yesButtonText", _labels.getString("s62"));
			UIManager.put("OptionPane.noButtonText", _labels.getString("s63"));
			UIManager.put("OptionPane.cancelButtonText",
					_labels.getString("s64"));
			UIManager.put("OptionPane.okButtonText", _labels.getString("s548"));

		} catch (RuntimeException exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Returns the labels
	 * 
	 * @return the labels
	 * @see ResourceBundle
	 */
	public ResourceBundle getLabels() {
		return _labels;
	}

	/**
	 * Returns the current locale
	 * 
	 * @return the current locale
	 * @see Locale
	 */
	public Locale getCurrentLocale() {
		return _currentLocale;
	}
}
