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

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.files.AcideFileManager;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file menu save file menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveFileMenuItemListener implements ActionListener {

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
				.getNumberOfFileEditorPanels() != 0) {

			// If it is not the NEW FILE
			if (AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getAbsolutePath()
					.equals(AcideLanguageManager.getInstance().getLabels()
							.getString("s79"))) {

				// Enables the save file as menu item
				AcideMainWindow.getInstance().getMenu().getFileMenu()
						.getSaveFileAsMenuItem().setEnabled(true);

				// Performs the save file as menu item action
				AcideMainWindow.getInstance().getMenu().getFileMenu()
						.getSaveFileAsMenuItem().doClick();

			} else {

				// Try to save the file content
				boolean savingResult = AcideFileManager
						.getInstance()
						.write(AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath(),
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getTextEditionAreaContent());

				// If it could save it
				if (savingResult) {

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s93")
									+ AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath()
									+ AcideLanguageManager.getInstance()
											.getLabels().getString("s94"));

					// Sets the green button
					AcideMainWindow.getInstance().getFileEditorManager()
							.setGreenButton();

					// Gets the file name
					int index = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel().getAbsolutePath()
							.lastIndexOf("\\");
					if (index == -1)
						index = AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath()
								.lastIndexOf("/");
					index++;
					String file = AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getAbsolutePath()
							.substring(
									index,
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath().length());

					// Sets the title
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getTabbedPane()
							.setTitleAt(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getTabbedPane().getSelectedIndex(),
									file);

					// Sets the file path
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setAbsolutePath(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath());

					// Sets the tool tip text
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getTabbedPane()
							.setToolTipText(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath());

					// Saves the original file
					File projectFile = new File(AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel().getAbsolutePath());

					// Sets the last change
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setLastChange(projectFile.lastModified());

					// Sets the last size
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setLastSize(projectFile.length());

					// Updates the file disk copy
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setFileDiskCopy(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getTextEditionAreaContent());

					// Updates the save project in the menu bar tool bar
					AcideMainWindow.getInstance().getToolBarPanel()
							.getMenuBarToolBar().updateStateOfFileButtons();

					// Adds the new file to the recent files list
					AcideWorkbenchConfiguration.getInstance()
							.getRecentFilesConfiguration()
							.addRecentFileToList(file);
				}
			}
		} else
			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s89"));
	}
}
