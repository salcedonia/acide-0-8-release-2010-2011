/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
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
import acide.files.bytes.AcideByteFileManager;
import acide.files.text.AcideTextFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

/**
 * ACIDE - A Configurable IDE save as grammar menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveAsGrammarMenuItemListener implements ActionListener {
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		try {

			// Gets the ACIDE - A Configurable IDE current grammar configuration
			String currentGrammarConfiguration = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getCurrentGrammarConfiguration().getPath();

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

			// Sets the current directory to the grammar configuration folder
			fileChooser.setCurrentDirectory(new File(
					AcideGrammarConfiguration.DEFAULT_PATH));

			String absolutePath = "";

			// Asks to the user
			int returnValue = fileChooser.showSaveDialog(fileChooser);

			// If it is OK
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the absolute path
				absolutePath = fileChooser.getSelectedFile().getAbsolutePath();

				// If it does not contains the .jar extension
				if (!absolutePath.endsWith(".jar"))

					// Adds it
					absolutePath += ".jar";

				// Copies the file
				AcideByteFileManager.getInstance().copy(
						currentGrammarConfiguration, absolutePath);

				// Updates the current grammar configuration path
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getCurrentGrammarConfiguration().setPath(absolutePath);

				// Disables the save grammar menu item
				AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
						.getGrammarMenu().getSaveGrammarMenuItem()
						.setEnabled(false);

				// Updates the grammar message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setGrammarMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s248")
										+ " "
										+ AcideMainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getCurrentGrammarConfiguration()
												.getName());

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s941")
								+ ": " + absolutePath);
			} else {

				// If it is CANCEL
				if (returnValue == JFileChooser.CANCEL_OPTION) {

					// Cancels the selection
					fileChooser.cancelSelection();

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s942"));
				}
			}
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s943"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}
