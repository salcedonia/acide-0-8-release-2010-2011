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
package gui.menuBar.projectMenu.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE -A Configurable IDE project menu save project menu item listener.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class SaveProjectMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		TextFile textFile = AcideIOFactory.getInstance().buildFile();

		try {

			// Not default project
			if (!AcideProjectConfiguration.getInstance()
					.isDefaultProject()) {

				// IF THIS IS NOT THE FIRST TIME THAT THE PROJECT IS SAVED
				// THEN SAVE IT AS
				if (!AcideProjectConfiguration.getInstance()
						.isFirstSave()) {

					// Enables the save as project menu item
					MainWindow.getInstance().getMenu().getProject()
							.getSaveAsProject().setEnabled(true);
					
					// Does the save as project menu item action
					MainWindow.getInstance().getMenu().getProject()
							.getSaveAsProject().doClick();
				} else {

					// Sets the language configuration
					AcideProjectConfiguration.getInstance()
							.setLanguage(
									AcideResourceManager
											.getInstance().getProperty("language"));

					// Sets the menu configuration
					AcideProjectConfiguration.getInstance()
							.setMenu(
									AcideResourceManager
											.getInstance().getProperty("currentMenuConfiguration"));

					// Sets the tool bar configuration
					AcideProjectConfiguration.getInstance()
							.setToolBar(
									AcideResourceManager
											.getInstance().getProperty("currentToolBarConfiguration"));

					// Sets the grammar configuration
					AcideProjectConfiguration.getInstance()
							.setGrammarConfiguration(
									AcideResourceManager
											.getInstance().getProperty("currentGrammar"));

					// Sets the lexicon configuration
					AcideProjectConfiguration.getInstance()
							.setLexiconConfiguration(
									AcideResourceManager
											.getInstance().getProperty("languagePath"));

					// Sets the console configuration
					AcideProjectConfiguration.getInstance()
					.setOutputConfiguration(
							AcideResourceManager
									.getInstance().getProperty("consoleConfiguration"));

					// Sets the file editor configuration
					AcideProjectConfiguration.getInstance()
					.setFileEditorConfiguration(
							AcideResourceManager
									.getInstance().getProperty("fileEditorConfiguration"));
					
					// Saves the configuration into the file
					String fileContent = AcideProjectConfiguration.getInstance().save();
					textFile.save(AcideProjectConfiguration.getInstance().getProjectPath(),
							fileContent);

					// The project has not been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(false);
				}
			}
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
