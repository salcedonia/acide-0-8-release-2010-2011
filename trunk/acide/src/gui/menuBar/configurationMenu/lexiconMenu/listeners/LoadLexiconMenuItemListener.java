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
package gui.menuBar.configurationMenu.lexiconMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;

import language.AcideLanguageManager;
import es.configuration.lexicon.AcideLexiconConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import es.text.AcideTextFileExtensionFilterManager;
import gui.mainWindow.MainWindow;

/**
 * ACIDE - A Configurable IDE lexicon menu load lexicon menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class LoadLexiconMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Creates a new filter for XML files
		AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s327"));
		filter.addExtension(".xml");

		// Asks the user for the file
		JFileChooser fileChooser = new JFileChooser();
		
		// Only admits files
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		
		// Applies the filter
		fileChooser.addChoosableFileFilter(filter);
		
		// Sets the initial directory
		fileChooser.setCurrentDirectory(new File("./configuration/lexicon/"));

		// Gets the user result from the file chooser
		int resultValue = fileChooser.showOpenDialog(fileChooser);

		String filePath = " ";

		// If ok
		if (resultValue == JFileChooser.APPROVE_OPTION)
			
			// Stores the path
			filePath = fileChooser.getSelectedFile().getAbsolutePath();

		// If the path is ok
		if (!filePath.equals(" ")) {

			// Loads the lexicon configuration
			AcideLexiconConfiguration.getInstance().load(
					fileChooser.getSelectedFile().getAbsolutePath());

			// Resets all the opened file editor panels with the new lexical
			// configuration
			for (int index = 0; index < MainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++){

				// Gets the caret position of the file editor panel
				int caretPosition = MainWindow.getInstance().getFileEditorManager()
				.getFileEditorPanelAt(index).getActiveTextEditionArea().getCaretPosition();
				
				// Resets the file editor text edition area
				MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).resetStyledDocument(caretPosition);
			}
			
			// Updates the lexicon message status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setLexiconMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s449")
									+ " "
									+ AcideLexiconConfiguration.getInstance()
											.getName());

			// Updates the project configuration
			AcideProjectConfiguration.getInstance().setLexiconConfiguration(
					AcideLexiconConfiguration.getInstance().getPath());

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);
			}
		}
	}
}
