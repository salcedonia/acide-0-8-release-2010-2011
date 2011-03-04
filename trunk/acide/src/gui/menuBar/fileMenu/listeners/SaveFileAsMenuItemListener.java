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
package gui.menuBar.fileMenu.listeners;

import es.text.AcideFileManager;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import language.AcideLanguageManager;
import operations.log.AcideLog;

/**																
 * ACIDE - A Configurable IDE file menu save file as menu item listener.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class SaveFileAsMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		String filePath = " ";
		
		// If there are opened files
		if (MainWindow.getInstance().getFileEditorManager().getNumberOfFileEditorPanels() != 0) {

			// Asks for the file path to the user
			filePath = AcideFileManager.getInstance().askSavingFileEditorFile();

			// If the file is not empty
			if (!filePath.equals(" ")) {

				// Try to save the file content
				boolean savingResult = AcideFileManager.getInstance().write(filePath, MainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel().getTextEditionAreaContent());

				// If it could save it
				if (savingResult) {
					
					// Updates the log
					AcideLog.getLog().info(AcideLanguageManager.getInstance().getLabels().getString("s93") + filePath
							+ AcideLanguageManager.getInstance().getLabels().getString("s94"));
					
					// Sets the green button
					MainWindow.getInstance().getFileEditorManager()
							.setGreenButton();
					
					// Gets the file name
					int index = filePath.lastIndexOf("\\");
					if(index == -1)
						index = filePath.lastIndexOf("/");
					index++;
					String file = filePath.substring(index, filePath.length());
					
					// Sets the title
					MainWindow.getInstance().getFileEditorManager().getTabbedPane()
							.setTitleAt(
									MainWindow.getInstance()
											.getFileEditorManager().getTabbedPane()
											.getSelectedIndex(), file);
					
					// Sets the file path
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setAbsolutePath(filePath);
					
					// Sets the tool tip text
					MainWindow.getInstance().getFileEditorManager().getTabbedPane()
							.setToolTipText(filePath);

					// Saves the original file
					File projectFile = new File(MainWindow.getInstance()
							.getFileEditorManager().getSelectedFileEditorPanel()
							.getAbsolutePath());
					
					// Sets the last change
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setLastChange(
									projectFile.lastModified());
					
					// Sets the last size
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setLastSize(
									projectFile.length());
					
					// Updates the file disk copy
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setFileDiskCopy(
									MainWindow.getInstance().getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getTextEditionAreaContent());
				} else
					// Updates the log
					AcideLog.getLog().info(AcideLanguageManager.getInstance().getLabels().getString("s95") + filePath);
			} else
				// Updates the log
				AcideLog.getLog().info(AcideLanguageManager.getInstance().getLabels().getString("s92"));
		} else
			// Updates the log
			AcideLog.getLog().info(AcideLanguageManager.getInstance().getLabels().getString("s89"));
	}
}
