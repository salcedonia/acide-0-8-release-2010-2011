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
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import language.AcideLanguageManager;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE -A Configurable IDE project menu save project as menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class SaveProjectAsMenuItemListener implements ActionListener {

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

			// Gets the labels
			ResourceBundle labels = AcideLanguageManager.getInstance()
					.getLabels();

			// Creates the text file which is going to be the project file
			TextFile projectFile = AcideIOFactory.getInstance().buildFile();

			// Selects the project extension
			String[] acideProjectExtension = new String[] { "acidePrj" };
			projectFile.getFileChooser().addChoosableFileFilter(
					new ExtensionFilter(acideProjectExtension, labels
							.getString("s328")));

			String filePath = projectFile.write();

			// Sets the language configuration
			AcideProjectConfiguration.getInstance().setLanguage(
					AcideResourceManager.getInstance().getProperty("language"));

			// Sets the menu configuration
			AcideProjectConfiguration.getInstance().setMenu(
					AcideResourceManager.getInstance().getProperty(
							"currentMenuConfiguration"));

			// Sets the tool bar configuration
			AcideProjectConfiguration.getInstance().setToolBar(
					AcideResourceManager.getInstance().getProperty(
							"currentToolBarConfiguration"));

			// Sets the file editor configuration
			AcideProjectConfiguration.getInstance().setFileEditorConfiguration(
					AcideResourceManager.getInstance().getProperty(
							"fileEditorConfiguration"));

			// Add the extension if the name does not contain it
			if (!filePath.contains(".acidePrj"))
				filePath = filePath + ".acidePrj";

			// Gets the new project name
			int lastIndexOfSlash = filePath.lastIndexOf("\\");
			if(lastIndexOfSlash == -1)
				lastIndexOfSlash = filePath.lastIndexOf("/");
			String newProjectName = filePath.substring(lastIndexOfSlash + 1, filePath.lastIndexOf("."));
			
			// Sets the name
			AcideProjectConfiguration.getInstance().setName(newProjectName);
			
			// Sets the path
			AcideProjectConfiguration.getInstance().setPath(filePath);

			// Saves the file
			String fileContent = AcideProjectConfiguration.getInstance().save();
			projectFile.save(AcideProjectConfiguration.getInstance()
					.getProjectPath(), fileContent);

			// Changes the root node in the explorer tree
			// TODO:
			
			// Updates the main window title
			// TODO:
			
			// Validates the changes in the main window
			MainWindow.getInstance().validate();
			
			// Repaint the main window with the new changes
			MainWindow.getInstance().repaint();
			
			// Is the first time that the project has been saved
			AcideProjectConfiguration.getInstance().setFirstSave(true);

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty(
					"defaultAcideProject", filePath);
			AcideResourceManager.getInstance().setProperty("defaultPath",
					filePath);

			// The project has not been modified yet
			AcideProjectConfiguration.getInstance().setIsModified(false);

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
