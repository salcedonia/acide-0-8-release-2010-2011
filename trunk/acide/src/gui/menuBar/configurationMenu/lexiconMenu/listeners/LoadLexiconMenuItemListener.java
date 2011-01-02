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
package gui.menuBar.configurationMenu.lexiconMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;

import language.AcideLanguageManager;
import es.configuration.lexicon.AcideLexiconConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import es.text.TextFileFilter;
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

		ResourceBundle labels = AcideLanguageManager.getInstance().getLabels();
		TextFileFilter filter = new TextFileFilter(labels.getString("s327"));
		filter.addExtension(".xml");

		JFileChooser fileChooser = new JFileChooser();
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.addChoosableFileFilter(filter);
		fileChooser.setCurrentDirectory(new File("./configuration/lexicon/"));

		int chosenOption = fileChooser.showOpenDialog(fileChooser);

		String filePath = " ";

		if (chosenOption == JFileChooser.APPROVE_OPTION)
			filePath = fileChooser.getSelectedFile().getAbsolutePath();

		// If the path is ok
		if (!filePath.equals(" ")) {

			// Loads the lexicon configuration
			AcideLexiconConfiguration.getInstance().load(
					fileChooser.getSelectedFile().getAbsolutePath());

			// Resets all the opened files in the editor with the new lexicon configuration
			int numEditors = MainWindow.getInstance().getFileEditorManager()
					.getNumFileEditorPanels();
			for (int index = 0; index < numEditors; index++)
				MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(index)
						.resetDocument();

			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setLexiconMessage(
							labels.getString("s449")
									+ " "
									+ AcideLexiconConfiguration.getInstance()
											.getName());
			
			// Updates the project configuration
			AcideProjectConfiguration.getInstance()
					.setLexiconConfiguration(
							AcideLexiconConfiguration.getInstance().getPath());
			
			// Not default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {
				
				// The project has been modified
				AcideProjectConfiguration.getInstance()
						.setIsModified(true);
			}
		}
	}
}
