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
 */
package acide.language;

import java.util.Locale;
import java.util.ResourceBundle;
import javax.swing.UIManager;

import acide.log.AcideLog;

/**																
 * ACIDE - A Configurable IDE language manager.
 *					
 * @version 0.8																														
 */
public class AcideLanguageManager {

	/**
	 * ACIDE - A Configurable IDE language manager bundle default path.
	 */
	public static final String BUNDLE_PATH = "acide.resources.language.";
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
	 * Sets the language to display in ACIDE - A Configurable IDE.
	 * 
	 * @param language language.
	 */
	public void setLanguage(String language) {

		try {

			// If it is English
			if(language.matches("english")){
				
				// Creates a new English locale
				_currentLocale = new Locale("en", "EN");
				
				// Updates the bundle
				_bundle = BUNDLE_PATH + "english";
			}
			
			// If it is Spanish
			if(language.matches("spanish")){ 
				
				// Creates a new Spanish locale
				_currentLocale = new Locale("es", "ES");
				
				// Updates the bundle
				_bundle = BUNDLE_PATH + "spanish";
			}

			// Creates the labels to display in ACIDE - A configurable IDE
			_labels = ResourceBundle.getBundle(_bundle, _currentLocale);
			
			// Updates the file chooser save button text
			UIManager.put("FileChooser.saveButtonText",
					_labels.getString("s40"));
			
			// Updates the file chooser open button text
			UIManager.put("FileChooser.openButtonText",
					_labels.getString("s41"));
			
			// Updates the file chooser cancel button text
			UIManager.put("FileChooser.cancelButtonText",
					_labels.getString("s42"));
			
			// Updates the file chooser update button text
			UIManager.put("FileChooser.updateButtonText",
					_labels.getString("s43"));
			
			// Updates the file chooser help button text
			UIManager.put("FileChooser.helpButtonText",
					_labels.getString("s44"));
			
			// Updates the file chooser save button tool tip text
			UIManager.put("FileChooser.saveButtonToolTipText",
					_labels.getString("s45"));
			
			// Updates the file chooser open button tool tip text
			UIManager.put("FileChooser.openButtonToolTipText",
					_labels.getString("s46"));
			
			// Updates the file chooser cancel button tool tip text
			UIManager.put("FileChooser.cancelButtonToolTipText",
					_labels.getString("s47"));
			
			// Updates the file chooser file name label text
			UIManager.put("FileChooser.fileNameLabelText",
					_labels.getString("s48"));
			
			// Updates the file chooser look in label text
			UIManager.put("FileChooser.lookInLabelText",
					_labels.getString("s49"));
			
			// Updates the file chooser up folder tool tip text
			UIManager.put("FileChooser.upFolderToolTipText",
					_labels.getString("s50"));
			
			// Updates the file chooser new folder tool tip text
			UIManager.put("FileChooser.newFolderToolTipText",
					_labels.getString("s51"));
			
			// Updates the file chooser up folder accessible name
			UIManager.put("FileChooser.newFolderAccessibleName",
					_labels.getString("s52"));
			
			// Updates the file chooser list view button tool tip text
			UIManager.put("FileChooser.listViewButtonToolTipText",
					_labels.getString("s53"));
			
			// Updates the file chooser details view button tool tip text
			UIManager.put("FileChooser.detailsViewButtonToolTipText",
					_labels.getString("s54"));
			
			// Updates the file chooser files of type label text
			UIManager.put("FileChooser.filesOfTypeLabelText",
					_labels.getString("s55"));
			
			// Updates the file chooser accept all file filter text
			UIManager.put("FileChooser.acceptAllFileFilterText",
					_labels.getString("s56"));
			
			// Updates the file chooser file name header text
			UIManager.put("FileChooser.fileNameHeaderText",
					_labels.getString("s57"));
			
			// Updates the file chooser file size header text
			UIManager.put("FileChooser.fileSizeHeaderText",
					_labels.getString("s58"));
			
			// Updates the file chooser file type header text
			UIManager.put("FileChooser.fileTypeHeaderText",
					_labels.getString("s59"));
			
			// Updates the file chooser file date header text
			UIManager.put("FileChooser.fileDateHeaderText",
					_labels.getString("s60"));
			
			// Updates the file chooser file attribute header text
			UIManager.put("FileChooser.fileAttrHeaderText",
					_labels.getString("s61"));
			
			// Updates the option pane yes button text
			UIManager.put("OptionPane.yesButtonText", _labels.getString("s62"));
			
			// Updates the option pane no button text
			UIManager.put("OptionPane.noButtonText", _labels.getString("s63"));
			
			// Updates the option pane cancel button text
			UIManager.put("OptionPane.cancelButtonText",
					_labels.getString("s64"));
			
			// Updates the option pane ok button text
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
