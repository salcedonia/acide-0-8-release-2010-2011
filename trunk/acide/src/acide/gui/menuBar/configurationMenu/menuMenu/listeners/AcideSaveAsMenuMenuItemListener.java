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
package acide.gui.menuBar.configurationMenu.menuMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;
import acide.files.bytes.AcideByteFile;
import acide.files.text.AcideTextFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.AcideMenuConfigurationWindow;

/**
 * ACIDE - A Configurable IDE menu menu save as menu menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveAsMenuMenuItemListener implements ActionListener {

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
			
			// Gets the ACIDE - A Configurable IDE current menu configuration
			String currentMenuConfiguration = AcideResourceManager.getInstance()
					.getProperty("currentMenuConfiguration");
			
			// Creates and configures the file chooser
			JFileChooser fileChooser = new JFileChooser();
			AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s126"));
			filter.addExtension("menuCfg");
			fileChooser.setFileFilter(filter);
			fileChooser.setCurrentDirectory(new File("./configuration/menu/"));

			String absolutePath = "";
			
			// Asks to the user
			int returnValue = fileChooser.showSaveDialog(fileChooser);
			
			// If ok
			if (returnValue == JFileChooser.APPROVE_OPTION) {
				
				// Gets the absolute path
				absolutePath = fileChooser.getSelectedFile().getAbsolutePath();
				
				// If it does not contain the extension
				if (!absolutePath.endsWith(".menuCfg"))
					
					// Adds it
					absolutePath += ".menuCfg";
				
				// Copies the files
				AcideByteFile.copy(currentMenuConfiguration, absolutePath);

				// Updates the ACIDE - A Configurable IDE current menu configuration
				AcideResourceManager.getInstance().setProperty(
						"currentMenuConfiguration", absolutePath);

				// Disables the save menu item
				AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
						.getMenuMenu().getSaveMenuMenuItem().setEnabled(false);
				
				// The changes are saved
				AcideMenuConfigurationWindow.setChangesAreSaved(true);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s528")
								+ absolutePath
								+ AcideLanguageManager.getInstance()
										.getLabels().getString("s529"));

			} else if (returnValue == JFileChooser.CANCEL_OPTION) {

				fileChooser.cancelSelection();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s527"));
			}
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s291"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}
