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
import es.text.ExtensionFilter;
import es.text.TextFile;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import language.AcideLanguageManager;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE -A Configurable IDE project menu save as menu item listener.
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class SaveAsProjectMenuItemListener implements ActionListener {

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
			
			TextFile f = AcideIOFactory.getInstance().buildFile();

			// Not default project
			if (!AcideProjectConfiguration.getInstance()
					.isDefaultProject()) {

				// Selects the project extension
				String[] ExtPide = new String[] { "acidePrj" };
				f.getFileChooser().addChoosableFileFilter(
						new ExtensionFilter(ExtPide, labels
								.getString("s328")));

				String file = f.write();

				// Sets the language configuration
				AcideProjectConfiguration.getInstance()
						.setLanguage(
								AcideResourceManager.getInstance().getProperty("language"));

				// Sets the menu configuration
				AcideProjectConfiguration.getInstance().setMenu(
						AcideResourceManager
								.getInstance().getProperty("currentMenuConfiguration"));

				// Sets the tool bar configuration
				AcideProjectConfiguration.getInstance()
						.setToolBar(
								AcideResourceManager
										.getInstance().getProperty("currentToolBarConfiguration"));

				// Sets the file editor configuration
				AcideProjectConfiguration.getInstance()
				.setFileEditorConfiguration(
						AcideResourceManager
								.getInstance().getProperty("fileEditorConfiguration"));
				
				// Add the extension if the name does not contain it
				if (!file.contains(".acidePrj"))
					file = file + ".acidePrj";

				// Sets the path
				AcideProjectConfiguration.getInstance().setPath(
						file);

				// Saves the file
				String cad = AcideProjectConfiguration.getInstance().save();
				f.save(AcideProjectConfiguration.getInstance()
						.getProjectPath(), cad);

				// Is the first time that the project has been saved
				AcideProjectConfiguration.getInstance()
						.setFirstSave(true);
				
				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty("defaultAcideProject", file);
				AcideResourceManager.getInstance().setProperty("defaultPath", file);

				// The project has not been modified yet
				AcideProjectConfiguration.getInstance()
						.setIsModified(false);
			}
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
