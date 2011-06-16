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
import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;
import acide.files.AcideFileExtensionFilterManager;
import acide.files.AcideFileManager;
import acide.files.bytes.AcideByteFileManager;
import acide.files.utils.AcideFileOperation;
import acide.files.utils.AcideFileTarget;
import acide.files.utils.AcideFileType;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.AcideMenuConfigurationWindow;

/**
 * ACIDE - A Configurable IDE menu menu save as menu menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveMenuAsMenuItemListener implements ActionListener {

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

			// Asks the the file to the user
			String absolutePath = AcideFileManager.getInstance().askForFile(
					AcideFileOperation.SAVE,
					AcideFileTarget.FILES,
					AcideFileType.FILE,
					"./configuration/menu/",
					new AcideFileExtensionFilterManager(
							new String[] { "menuConfig" }, AcideLanguageManager
									.getInstance().getLabels().getString("s287")));

			if (absolutePath != null) {

				// If it does not contain the extension
				if (!absolutePath.endsWith(".menuConfig"))

					// Adds it
					absolutePath += ".menuConfig";

				// Gets the ACIDE - A Configurable IDE current menu configuration
				String currentMenuConfiguration = AcideResourceManager
						.getInstance().getProperty("currentMenuConfiguration");

				// Copies the files
				AcideByteFileManager.getInstance().copy(
						currentMenuConfiguration, absolutePath);

				// Updates the ACIDE - A Configurable IDE current menu
				// configuration
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
			} 
		} catch (Exception exception) {

			// Displays an error message
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
