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
package acide.gui.menuBar.projectMenu.listeners;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileManager;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE -A Configurable IDE project menu save project menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveProjectMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		try {

			// If it is not the first time the project is saved
			if (!AcideProjectConfiguration.getInstance().isFirstSave()) {

				// Enables the save as project menu item
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getSaveAsProjectMenuItem().setEnabled(true);

				// Does the save as project menu item action
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getSaveAsProjectMenuItem().doClick();
			} else {

				// Sets the the ACIDE - A Configurable IDE language configuration
				AcideProjectConfiguration.getInstance().setLanguageConfiguration(
						AcideResourceManager.getInstance().getProperty(
								"language"));

				// Sets the ACIDE - A Configurable IDE current menu configuration
				AcideProjectConfiguration.getInstance().setMenuConfiguration(
						AcideResourceManager.getInstance().getProperty(
								"currentMenuConfiguration"));

				// Sets the ACIDE - A Configurable IDE current tool bar configuration
				AcideProjectConfiguration.getInstance().setToolBarConfiguration(
						AcideResourceManager.getInstance().getProperty(
								"currentToolBarConfiguration"));

				// Sets the ACIDE - A Configurable IDE current grammar configuration
				AcideProjectConfiguration.getInstance()
						.setGrammarConfiguration(
								AcideResourceManager.getInstance().getProperty(
										"currentGrammarConfiguration"));

				// Sets the ACIDE - A Configurable IDE lexicon configuration
				AcideProjectConfiguration.getInstance()
						.setLexiconConfiguration(
								AcideResourceManager.getInstance().getProperty(
										"lexiconConfiguration"));

				// Sets the ACIDE - A Configurable IDE console configuration
				AcideProjectConfiguration.getInstance().setOutputConfiguration(
						AcideResourceManager.getInstance().getProperty(
								"consoleConfiguration"));

				// Sets the ACIDE - A Configurable IDE file editor configuration
				AcideProjectConfiguration.getInstance()
						.setFileEditorConfiguration(
								AcideResourceManager.getInstance().getProperty(
										"fileEditorConfiguration"));

				// Saves the configuration into the file
				String fileContent = AcideProjectConfiguration.getInstance()
						.save();
				AcideFileManager.getInstance().write(AcideProjectConfiguration.getInstance()
						.getProjectPath(), fileContent);

				// The project has not been modified
				AcideProjectConfiguration.getInstance().setIsModified(false);
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
