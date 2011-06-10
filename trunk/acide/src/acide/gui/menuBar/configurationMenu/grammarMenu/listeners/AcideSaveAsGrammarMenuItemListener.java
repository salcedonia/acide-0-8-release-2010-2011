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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import acide.files.AcideFileExtensionFilterManager;
import acide.files.bytes.AcideByteFileManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

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

		// Selects the extension for the project
		String[] extensions = new String[] { "jar" };

		File selectedFile = null;
		String absolutePath = null;
		
		// Creates the file chooser
		JFileChooser _fileChooser = new JFileChooser();

		try {

			// Sets the current directory to the default path
			_fileChooser.setCurrentDirectory(new File("./configuration/grammars/"));

			// Adds the filter to the file chooser
			_fileChooser
					.addChoosableFileFilter(
							new AcideFileExtensionFilterManager(extensions,
									AcideLanguageManager.getInstance()
											.getLabels().getString("s270")));
			
			// Disables the multiple selection of files
			_fileChooser.setMultiSelectionEnabled(false);

			// Sets only files
			_fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Ask for the file to the user
			int returnValue = _fileChooser.showSaveDialog(null);

			// If it is ok
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the selected file
				selectedFile = _fileChooser.getSelectedFile();

				// Gets the absolute path
				absolutePath = selectedFile.getAbsolutePath();

				if (absolutePath != null) {

					// If it does not contains the .jar extension
					if (!absolutePath.endsWith(".jar"))

						// Adds it
						absolutePath += ".jar";

					// Gets the ACIDE - A Configurable IDE current grammar
					// configuration
					String currentGrammarConfiguration = AcideMainWindow
							.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getCurrentGrammarConfiguration().getPath();

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

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s300")
									+ absolutePath);

					// Updates the ACIDE - A Configurable IDE last opened
					// file
					// directory
					AcideResourceManager.getInstance().setProperty(
							"lastOpenedFileDirectory", absolutePath);
				}
			} else if (returnValue == JFileChooser.CANCEL_OPTION) {

				// Cancels the selection
				_fileChooser.cancelSelection();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s302"));
			}

		} catch (Exception exception) {

			// Displays an error message
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
