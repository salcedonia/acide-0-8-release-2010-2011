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
package acide.gui.menuBar.configurationMenu.lexiconMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JFileChooser;

import acide.language.AcideLanguageManager;
import acide.configuration.project.AcideProjectConfiguration;
import acide.files.text.AcideTextFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE lexicon menu load lexicon menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideLoadLexiconMenuItemListener implements ActionListener {

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
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getLexiconConfiguration()
					.load(fileChooser.getSelectedFile().getAbsolutePath());

			// Gets the selected file editor panel caret position 
			int caretPosition = AcideMainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.getActiveTextEditionArea().getCaretPosition();

			// Resets the selected file editor text edition area
			AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.resetStyledDocument(caretPosition);
			
			// Updates the lexicon message status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setLexiconMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s449")
									+ " "
									+ AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getLexiconConfiguration()
											.getName());

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);
			}
		}
	}
}
