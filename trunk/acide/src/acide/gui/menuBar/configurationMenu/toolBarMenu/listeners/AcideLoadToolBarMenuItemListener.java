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

import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.toolBar.consoleComandToolBar.AcideConsoleCommandList;
import acide.files.text.AcideTextFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.AcideToolBarConfigurationWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE tool bar menu load tool bar listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideLoadToolBarMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Creates and configures the file chooser
		JFileChooser fileChooser = new JFileChooser();
		AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s904"));
		filter.addExtension("TBcfg");
		fileChooser.setFileFilter(filter);
		fileChooser.setCurrentDirectory(new File("./configuration/toolbar/"));

		// Asks to the user
		int returnValue = fileChooser.showOpenDialog(null);
		
		// If Ok
		if (returnValue == JFileChooser.APPROVE_OPTION) {

			// Gets the absolute path of the file
			String absolutePath = fileChooser.getSelectedFile()
					.getAbsolutePath();

			try {

				// Loads the final list
				AcideConsoleCommandList.loadFinalList(absolutePath);
				
				// Loads the temporal list
				AcideConsoleCommandList.loadTemporalList(absolutePath);
				
				// Updates the tool bar panel
				AcideMainWindow.getInstance().updateToolBarPanel();

				// Updates the ACIDE - A Configurable IDE current tool bar
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", absolutePath);

				// Validates the changes in the main window
				AcideMainWindow.getInstance().validate();
				
				// Repaints the main window
				AcideMainWindow.getInstance().repaint();

				// Disables the tool bar save menu item
				AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
						.getToolBarMenu().getSaveToolBarMenuItem()
						.setEnabled(false);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s905")
								+ absolutePath);

				// The changes are saved
				AcideToolBarConfigurationWindow.setAreChangesSaved(true);

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject())

					// The project has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);

			} catch (Exception exception) {

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s906")
						+ absolutePath, AcideLanguageManager.getInstance()
						.getLabels().getString("s907"),
						JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s906")
								+ absolutePath);
			}
		} else
		// Cancel option
		if (returnValue == JFileChooser.CANCEL_OPTION) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s908"));
		}
	}
}
