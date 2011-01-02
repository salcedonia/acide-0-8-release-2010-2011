
/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */package language;

import java.util.Locale;
import java.util.ResourceBundle;
import javax.swing.UIManager;

import operations.log.AcideLog;

/**																
 * ACIDE - A Configurable IDE language manager.
 *					
 * @version 0.8																														
 */
public class AcideLanguageManager {

	/**
	 * ACIDE - A Configurable IDE language manager bundle default path.
	 */
	public static final String BUNDLE_PATH = "resources.language.";
	/**
	 * ACIDE - A Configurable IDE language manager language current locale.
	 */
	private static Locale _currentLocale;
	/**
	 * ACIDE - A Configurable IDE language manager language bundle.
	 */
	private static String _bundle;
	/**
	 * ACIDE - A Configurable IDE language manager labels to display.
	 */
	private static ResourceBundle _labels;
	/**
	 * ACIDE - A Configurable IDE language manager unique class instance
	 */
	private static AcideLanguageManager _instance;

	/**
	 * Returns the ACIDE - A Configurable IDE unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE unique class instance.
	 */
	public static AcideLanguageManager getInstance() {
		if (_instance == null)
			_instance = new AcideLanguageManager();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE language manager.
	 */
	public AcideLanguageManager() {
		super();
	}

	/**
	 * Sets the language to display.
	 * 
	 * @param string code of the language.
	 */
	public void getLanguage(String string) {

		try {

			if(string.matches("english")){
				_currentLocale = new Locale("en", "EN");
				_bundle = BUNDLE_PATH + "english";
			}
			
			if(string.matches("spanish")){ 
				_currentLocale = new Locale("es", "ES");
				_bundle = BUNDLE_PATH + "spanish";
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
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE language manager labels.
	 * 
	 * @return the ACIDE - A Configurable IDE language manager labels.
	 * @see ResourceBundle
	 */
	public ResourceBundle getLabels() {
		return _labels;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE language manager current locale.
	 * 
	 * @return the ACIDE - A Configurable IDE language manager current locale.
	 * @see Locale
	 */
	public Locale getCurrentLocale() {
		return _currentLocale;
	}
}
