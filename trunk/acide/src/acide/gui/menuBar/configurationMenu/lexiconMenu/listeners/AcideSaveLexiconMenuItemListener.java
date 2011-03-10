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
package acide.gui.menuBar.configurationMenu.lexiconMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import acide.resources.AcideResourceManager;
import acide.configuration.lexicon.AcideLexiconConfiguration;

/**																
 * ACIDE - A Configurable IDE lexicon menu save lexicon menu item listener.
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class AcideSaveLexiconMenuItemListener implements ActionListener {

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

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		AcideLexiconConfiguration programmingLanguage = AcideLexiconConfiguration
				.getInstance();

		// Gets the current lexicon path
		String path = programmingLanguage.getName();

		// If it is ok
		if (!path.equals(" ")) {

			// Gets the lexicon name
			int index = path.lastIndexOf("\\");
			if (index == -1)
				index = path.lastIndexOf("/");
			String languageName = path.substring(index + 1, path.length());

			if (languageName.contains(".")) {
				index = languageName.lastIndexOf(".");
				languageName = languageName.substring(0, index);
			}

			// Saves it
			boolean result = programmingLanguage.save(languageName, false);

			// If it could save it
			if (result)
				JOptionPane.showMessageDialog(null, labels.getString("s451"),
						labels.getString("s450"), 1);
			else
				JOptionPane.showMessageDialog(null, labels.getString("s452"),
						labels.getString("s450"), 0);
		} else {
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s92"));
		}
	}
}
