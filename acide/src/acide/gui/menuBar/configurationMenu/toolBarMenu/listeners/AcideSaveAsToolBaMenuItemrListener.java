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
package acide.gui.menuBar.configurationMenu.toolBarMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;
import acide.files.bytes.AcideByteFileManager;
import acide.files.text.AcideTextFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.AcideToolBarConfigurationWindow;

/**
 * ACIDE - A Configurable IDE tool bar menu save as tool bar listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveAsToolBaMenuItemrListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		try {

			// Gets the ACIDE - A Configurable IDE current tool bar
			// configuration
			String currentToolBarConfiguration = AcideResourceManager
					.getInstance().getProperty("currentToolBarConfiguration");

			// Creates and configures the file chooser
			JFileChooser fileChooser = new JFileChooser();

			// Creates the file extension filter
			AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s158"));

			// Adds the extension ".TBcfg"
			filter.addExtension("TBcfg");

			// Sets the file filter
			fileChooser.setFileFilter(filter);

			// Sets the current directory to the tool bar configuration folder
			fileChooser
					.setCurrentDirectory(new File("./configuration/toolbar/"));

			// Asks for saving
			int returnValue = fileChooser.showSaveDialog(fileChooser);

			// If it is OK
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the absolute path
				String absolutePath = fileChooser.getSelectedFile()
						.getAbsolutePath();

				// If it does not contains the valid extension
				if (!absolutePath.endsWith(".TBcfg"))

					// Adds it
					absolutePath += ".TBcfg";

				// Copies the files
				AcideByteFileManager.getInstance().copy(
						currentToolBarConfiguration, absolutePath);

				// Updates the ACIDE - A Configurable IDE current tool bar
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", absolutePath);

				// Disables the save tool bar menu item
				AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
						.getToolBarMenu().getSaveToolBarMenuItem()
						.setEnabled(false);

				// The changes are saved
				AcideToolBarConfigurationWindow.setAreChangesSaved(true);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s900")
								+ absolutePath
								+ AcideLanguageManager.getInstance()
										.getLabels().getString("s901"));
			} else {

				// If it is CANCEL
				if (returnValue == JFileChooser.CANCEL_OPTION) {

					// Cancels selection
					fileChooser.cancelSelection();

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s902"));
				}
			}
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s903"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}
