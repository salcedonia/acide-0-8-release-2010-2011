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
package acide.gui.menuBar.configurationMenu.grammarMenu.listeners;

import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.files.text.AcideTextFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;

/**																
 * ACIDE - A Configurable IDE load grammar menu item listener.
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class AcideLoadGrammarMenuItemListener implements ActionListener{
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Creates and configures the file chooser
		JFileChooser fileChooser = new JFileChooser();

		// Creates the file extension filter
		AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s270"));

		// Adds the extension ".jar"
		filter.addExtension("jar");

		// Sets the filter
		fileChooser.setFileFilter(filter);

		// Sets the current directory to the grammars
		fileChooser.setCurrentDirectory(new File(
				AcideGrammarConfiguration.DEFAULT_PATH));

		// Asks to the user
		int returnValue = fileChooser.showOpenDialog(null);

		// If OK
		if (returnValue == JFileChooser.APPROVE_OPTION) {

			// Gets the absolute path
			String absolutePath = fileChooser.getSelectedFile()
					.getAbsolutePath();

			// Updates the current grammar configuration path
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getCurrentGrammarConfiguration().setPath(absolutePath);

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s243")
							+ " " + absolutePath);

			// Updates the grammar message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setGrammarMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s248")
									+ " " + AcideMainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getCurrentGrammarConfiguration().getName());

			// Updates the grammar configuration path in the file editor
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getCurrentGrammarConfiguration().setPath(absolutePath);

			// Disables the save grammar menu item
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getGrammarMenu().getSaveGrammarMenuItem()
					.setEnabled(false);

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);
			}

		} else if (returnValue == JFileChooser.CANCEL_OPTION) {
			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s242"));
		}
	}
}