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

import es.configuration.lexicon.AcideLexiconConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import operations.log.AcideLog;
import resources.AcideResourceManager;

import language.AcideLanguageManager;

/**																
 * ACIDE - A Configurable IDE lexicon menu new lexicon menu item listener.
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class NewLexiconMenuItemListener implements ActionListener {

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

		String languagePath = "";
		String languageName = "";
		languageName = JOptionPane.showInputDialog(null, labels
				.getString("s453"), labels.getString("s454"),
				JOptionPane.INFORMATION_MESSAGE);

		// If it is ok
		if (!languageName.equals("")) {

			if(languageName.contains(".xml"))
				languagePath = "./configuration/lexicon/" + languageName;
			else
				languagePath = "./configuration/lexicon/" + languageName + ".xml";

			// Resets all the editors with the new lexicon configuration
			AcideLexiconConfiguration.getInstance().newLexicon(languagePath);
			
			// Gets the number of opened editors
			int numEditors = MainWindow.getInstance().getFileEditorManager()
					.getNumberOfFileEditorPanels();
			for (int index = 0; index < numEditors; index++)
				MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(index)
						.resetDocument();

			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setLexiconMessage(
							labels.getString("s449")
									+ " "
									+ AcideLexiconConfiguration.getInstance()
											.getName());
			
			// Updates the project configuration
			AcideProjectConfiguration.getInstance()
					.setLexiconConfiguration(
							AcideLexiconConfiguration.getInstance().getName());

			// Not default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {
				
				// The project has been modified
				AcideProjectConfiguration.getInstance()
						.setIsModified(true);
			}
		} else {

			// Missed name
			JOptionPane.showMessageDialog(null, labels.getString("s976"),
					labels.getString("s972"), JOptionPane.WARNING_MESSAGE);
		}
	}
}