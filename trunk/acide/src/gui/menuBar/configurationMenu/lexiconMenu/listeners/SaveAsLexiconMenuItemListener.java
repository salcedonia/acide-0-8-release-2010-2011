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
package gui.menuBar.configurationMenu.lexiconMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import language.AcideLanguageManager;
import operations.log.AcideLog;

import resources.AcideResourceManager;
import es.configuration.lexicon.AcideLexiconConfiguration;
import es.text.AcideTextFileExtensionFilterManager;

/**																
 * ACIDE - A Configurable IDE lexicon menu save as lexicon menu item listener.
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class SaveAsLexiconMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels TO DISPLAY
		ResourceBundle labels = language.getLabels();

		JFileChooser fileChooser = new JFileChooser(labels.getString("s126"));
		AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(labels.getString("s287"));
		filter.addExtension("xml");
		fileChooser.setFileFilter(filter);
		fileChooser.setCurrentDirectory(new File("./configuration/lexicon"));

		int chosenOption = fileChooser.showSaveDialog(null);
		String filePath = " ";

		if (chosenOption == JFileChooser.APPROVE_OPTION)
			filePath = fileChooser.getSelectedFile().getAbsolutePath();

		// If the path is ok
		if (!filePath.equals(" ")) {

			// Gets the lexicon name
			int index = filePath.lastIndexOf("\\");
			if (index == -1)
				index = filePath.lastIndexOf("/");
			String fileName = filePath.substring(index + 1, filePath.length());
			
			if (fileName.contains(".")) {
				index = fileName.lastIndexOf(".");
				fileName = fileName.substring(0, index);
			}

			// Save lexicon as
			AcideLexiconConfiguration programmingLanguage = AcideLexiconConfiguration
					.getInstance();
			boolean result = programmingLanguage.saveAs(fileName, false,
					filePath);

			// If it could save it
			if (result) {
				JOptionPane.showMessageDialog(null, labels.getString("s451"),
						labels.getString("s450"), 1);

			} else {
				JOptionPane.showMessageDialog(null, labels.getString("s452"),
						labels.getString("s450"), 0);
			}
		} else
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s92"));
	}
}
