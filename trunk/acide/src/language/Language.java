package language;

import java.util.Locale;
import java.util.ResourceBundle;
import javax.swing.UIManager;

/**
 * 
 */
public class Language {

	/**
	 * 
	 */
	private static Locale _currentLocale;
	/**
	 * 
	 */
	private static String _bundle;
	/**
	 * 
	 */
	private static ResourceBundle _labels;
	/**
	 * 
	 */
	private static Language _instance;

	/**
	 * 
	 * 
	 * @return
	 */
	public static Language getInstance() {
		if (_instance == null)
			_instance = new Language();
		return _instance;
	}

	/**
	 * Constructor of the class.
	 */
	public Language() {
		super();
	}

	/**
	 * 
	 * @param n
	 */
	public void getLanguage(int n) {

		try {

			if (n == 1) {
				_currentLocale = new Locale("en", "EN");
				_bundle = "properties.language.english";
			} else {
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

		} catch (RuntimeException e) {
			e.printStackTrace();
		}
	}

	/**
	 * 
	 * @return
	 */
	public ResourceBundle getLabels() {
		return _labels;
	}

	/**
	 * 
	 * @return
	 */
	public Locale getCurrentLocale() {
		return _currentLocale;
	}
}
