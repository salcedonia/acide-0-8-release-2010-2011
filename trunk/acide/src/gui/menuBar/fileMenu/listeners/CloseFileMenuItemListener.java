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

import es.configuration.project.AcideProjectConfiguration;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.AcideLanguageManager;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE - A Configurable IDE file menu close file menu item listener.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class CloseFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		int selectedEditorIndex = MainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanelIndex();

		// If the editor has been modified
		if (MainWindow.getInstance().getFileEditorManager().isRedButton()) {
			
			// Do you want to save the file?
			int chosenOption = JOptionPane.showConfirmDialog(null, labels
					.getString("s643"));

			// If ok
			if (chosenOption == JOptionPane.OK_OPTION) {

				// If it is the NEW FILE
				if (MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getAbsolutePath().equals(
								labels.getString("s79"))) {

					TextFile textFile = AcideIOFactory.getInstance().buildFile();
					String filePath = " ";
					filePath = textFile.write();
					
					if (!filePath.equals(" ")) {
						
						// Saves it
						boolean result = textFile.save(filePath, MainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getTextEditionAreaContent());

						// If it could save it
						if (result) {
							
							// Updates the log
							AcideLog.getLog().info(labels.getString("s93") + filePath
									+ labels.getString("s94"));
							
							// Sets the green button
							MainWindow.getInstance().getFileEditorManager()
									.setGreenButton();
							
							// Sets the path
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(selectedEditorIndex).setAbsolutePath(filePath);
							
							// Sets the tool tip text
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(selectedEditorIndex).setToolTipText(filePath);
							
							// Gets the file name
							int index = filePath.lastIndexOf("\\");
							if(index == -1)
								index = filePath.lastIndexOf("/");
							String file = filePath.substring(index + 1, filePath.length());
							
							// Sets the title
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(selectedEditorIndex).setName(file);
							
							// Saves the original file
							File projectFile = new File(MainWindow
									.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getAbsolutePath());
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().setLastChange(
											projectFile.lastModified());
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().setLastSize(
											projectFile.length());
							
							// Updates the status bar
							MainWindow.getInstance().getStatusBar()
									.setStatusMessage(" ");
						}
						else
							// Updates the log
							AcideLog.getLog().info(labels.getString("s92"));
					} 
					else
						
						// Updates the log
						AcideLog.getLog().info(labels.getString("s95") + filePath);
				}		
				else {
					
					// Saves the file
					MainWindow.getInstance().getMenu().getFile().getSaveFile().setEnabled(true);
					MainWindow.getInstance().getMenu().getFile().getSaveFile().doClick();
					MainWindow.getInstance().getStatusBar().setStatusMessage(" ");
				}

				// Updates the file state in the project configuration
				for (int i = 0; i < AcideProjectConfiguration.getInstance().getFileListSize(); i++) {
					if (AcideProjectConfiguration.getInstance()
							.getFileAt(i).getAbsolutePath().equals(
									MainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(selectedEditorIndex).getAbsolutePath())) {
						AcideProjectConfiguration.getInstance()
								.getFileAt(i).setIsOpened(false);
					}
				}
							
				// Not default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject())
					
					// The project is modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(true);
				
				// Removes the tab
				MainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(selectedEditorIndex);

			} else 
				if (chosenOption == JOptionPane.NO_OPTION) {
					
					// Removes the tab
					MainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(selectedEditorIndex);
					
					// Updates the status bar
					MainWindow.getInstance().getStatusBar().setStatusMessage(" ");
			}
		} else {

			// Updates the file state in the project configuration
			for (int i = 0; i < AcideProjectConfiguration.getInstance().getFileListSize(); i++) {
				
				if (AcideProjectConfiguration.getInstance()
						.getFileAt(i).getAbsolutePath().equals(
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(selectedEditorIndex).getAbsolutePath())) {
					
					// Is not opened
					AcideProjectConfiguration.getInstance()
							.getFileAt(i).setIsOpened(false);
				}
			}
			
			// Not default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject())
				AcideProjectConfiguration.getInstance()
						.setIsModified(true);
			
			// Removes the tab
			MainWindow.getInstance().getFileEditorManager().getTabbedPane().remove(
					selectedEditorIndex);
			
			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setStatusMessage(" ");
		}
		
		// No more opened tabs
		if (MainWindow.getInstance().getFileEditorManager().getTabbedPane()
				.getTabCount() == 0) {
			
			// Disables the FILE menu
			MainWindow.getInstance().getMenu().getFile().disableMenu();
			
			// Disables the EDIT menu
			MainWindow.getInstance().getMenu().disableEditMenu();
		}
	}
}

