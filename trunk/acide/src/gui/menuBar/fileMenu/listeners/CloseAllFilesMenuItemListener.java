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
import es.text.AcideTextFile;
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
 * ACIDE - A Configurable IDE file menu close all files item listener.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class CloseAllFilesMenuItemListener implements ActionListener {

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

		int numEditors = MainWindow.getInstance().getFileEditorManager().getNumberOfFileEditorPanels();
		MainWindow.getInstance().getFileEditorManager().setSelectedFileEditorPanelAt(numEditors - 1);

		// Checks the opened editors
		for (int i = numEditors - 1; i >= 0; i--) {

			MainWindow.getInstance().getFileEditorManager().setSelectedFileEditorPanelAt(i);

			// It is a modified editor
			if (MainWindow.getInstance().getFileEditorManager().isRedButton()) {
				
				// Ask the user for saving the file
				int chosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s643"));

				// If yes
				if (chosenOption == JOptionPane.OK_OPTION) {

					// If it is the new file
					if (MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
							.getAbsolutePath().equals(labels.getString("s79"))) {

						AcideIOFactory ioFactory = AcideIOFactory.getInstance();
						AcideTextFile textFile = ioFactory.buildFile();
						String f = " ";
						f = textFile.askSavingFileEditorFile();
						if (f.equals(" ")) {
							
							// Updates the log
							AcideLog.getLog().info(labels.getString("s92"));
						} else {

							boolean result = textFile.write(f, MainWindow.getInstance()
									.getFileEditorManager().getSelectedFileEditorPanel()
									.getTextEditionAreaContent());

							// If it could save it
							if (result) {
								
								// Updates the log
								AcideLog.getLog().info(labels.getString("s93") + f
										+ labels.getString("s94"));
								
								// Sets green button
								MainWindow.getInstance().getFileEditorManager()
										.setGreenButton();
								
								// Sets the path
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).setAbsolutePath(f);
								
								// Sets the tool tip text
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).setToolTipText(f);
								
								// Gets the name
								int index = f.lastIndexOf("\\");
								if (index == -1)
									index = f.lastIndexOf("/");										
								String file = f
										.substring(index + 1, f.length());
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).setName(file);
								
								// Creates the file
								File projectFile = new File(MainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel().getAbsolutePath());
								MainWindow
										.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.setLastChange(
												projectFile.lastModified());
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel().setLastSize(
												projectFile.length());

							} else {
								
								// Updates the log
								AcideLog.getLog().info(labels.getString("s95") + f);
							}
						}

					} else {
						MainWindow.getInstance().getMenu().getFile().getSaveFile().setEnabled(true);
						MainWindow.getInstance().getMenu().getFile().getSaveFile().doClick();
					}
					
					// Not the default configuration
					if (!AcideProjectConfiguration.getInstance().isDefaultProject())
						
						// The project has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);

					// Sets the editors to closed in the project configuration
					for (int pos = 0; pos < AcideProjectConfiguration.getInstance().getFileListSize(); pos++) {
						
						if (AcideProjectConfiguration.getInstance().getFileAt(
								pos).getAbsolutePath().equals(
										MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).getAbsolutePath())) {
							AcideProjectConfiguration.getInstance().getFileAt(
									pos).setIsOpened(false);
						}
					}
				} else {
					if (chosenOption == JOptionPane.CANCEL_OPTION)
						return;
				}
			}
			
			// Sets the editors to closed in the project configuration
			for (int pos = 0; pos < AcideProjectConfiguration.getInstance()
					.getFileListSize(); pos++) {
				if (AcideProjectConfiguration.getInstance().getFileAt(pos)
						.getAbsolutePath().equals(
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(i).getAbsolutePath())) {
					AcideProjectConfiguration.getInstance().getFileAt(pos)
							.setIsOpened(false);
				}
			}

		}
		
		for (int i = 0; i < numEditors; i++) {
			MainWindow.getInstance().getFileEditorManager().setSelectedFileEditorPanelAt(0);
			MainWindow.getInstance().getFileEditorManager().getTabbedPane().remove(0);
			MainWindow.getInstance().getFileEditorManager().getTabbedPane().validate();
		}

		// Disables the EDIT and FILE menu
		MainWindow.getInstance().getMenu().getFile().disableMenu();
		MainWindow.getInstance().getMenu().disableEditMenu();
	}
}
