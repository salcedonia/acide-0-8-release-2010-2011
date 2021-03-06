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
package acide.gui.menuBar.fileMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JOptionPane;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.files.AcideFileManager;
import acide.files.utils.AcideFileOperation;
import acide.files.utils.AcideFileTarget;
import acide.files.utils.AcideFileType;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file menu save file as menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveFileAsMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// If there are opened files
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// Asks for the file path to the user
			String absolutePath = AcideFileManager.getInstance().askForFile(
					AcideFileOperation.SAVE, AcideFileTarget.FILES,
					AcideFileType.FILE, "", null);

			// If the user selected something
			if (absolutePath != null) {

				// Builds the file to check if it exists
				File file = new File(absolutePath);

				// If the file exists
				if (file.exists()) {

					// Asks to the user if he wants to overwrite it
					int result = JOptionPane.showConfirmDialog(null,
							AcideLanguageManager.getInstance().getLabels()
									.getString("s954"), AcideLanguageManager
									.getInstance().getLabels()
									.getString("s953"),
							JOptionPane.YES_NO_OPTION);

					// If it overwrites the file
					if (result == JOptionPane.YES_OPTION) {
						// Save the file
						saveFile(absolutePath);
					}
				} else {

					// Save the file
					saveFile(absolutePath);
				}
			}

		} else
			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s89"));
	}

	/**
	 * Saves the file and updates the ACIDE - A Configurable file editor tabbed
	 * pane.
	 * 
	 * @param absoluteFilePath
	 *            file to save.
	 */
	private void saveFile(String absoluteFilePath) {

		// Saves the file
		boolean result = AcideFileManager.getInstance().write(
				absoluteFilePath,
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getTextEditionAreaContent());

		// If it could save it
		if (result) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s93")
							+ absoluteFilePath
							+ AcideLanguageManager.getInstance().getLabels()
									.getString("s94"));

			// Sets the green button
			AcideMainWindow.getInstance().getFileEditorManager()
					.setGreenButton();

			// Gets the file name
			int lastIndexOfSlash = absoluteFilePath.lastIndexOf("\\");
			if (lastIndexOfSlash == -1)
				lastIndexOfSlash = absoluteFilePath.lastIndexOf("/");
			String fileName = absoluteFilePath.substring(lastIndexOfSlash + 1,
					absoluteFilePath.length());

			// Updates the name property
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getStyledDocument()
					.putProperty("name", fileName);

			// Updates the title
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getTabbedPane()
					.setTitleAt(
							AcideMainWindow.getInstance()
									.getFileEditorManager().getTabbedPane()
									.getSelectedIndex(), fileName);

			// Sets the file editor panel absolute path
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel()
					.setAbsolutePath(absoluteFilePath);

			// Sets the file editor panel tool tip text
			AcideMainWindow.getInstance().getFileEditorManager()
					.getTabbedPane().setToolTipText(absoluteFilePath);

			// Builds the file to get the last changes
			File file = new File(AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getAbsolutePath());

			// Sets the last modification change
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel()
					.setLastChange(file.lastModified());

			// Sets the last length size
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().setLastSize(file.length());

			// Adds the new file to the recent files list
			AcideWorkbenchConfiguration.getInstance()
					.getRecentFilesConfiguration()
					.addRecentFileToList(absoluteFilePath);

		} else {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s95")
							+ absoluteFilePath);
		}
	}
}
