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
import javax.swing.JOptionPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import acide.files.text.AcideTextFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE lexicon menu save as lexicon menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveAsLexiconMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Creates and configures the file chooser
		JFileChooser fileChooser = new JFileChooser(AcideLanguageManager
				.getInstance().getLabels().getString("s126"));

		// Creates the file extension filter
		AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s287"));

		// Adds the extension ".xml"
		filter.addExtension("xml");

		// Adds the filter to the file chooser
		fileChooser.setFileFilter(filter);

		// Sets the current directory to the lexicon configuration folder
		fileChooser.setCurrentDirectory(new File("./configuration/lexicon"));

		// Asks to the user
		int returnValue = fileChooser.showSaveDialog(null);
		String absolutePath = " ";

		// If it is OK
		if (returnValue == JFileChooser.APPROVE_OPTION)

			// Gets the absolute path from the selected file
			absolutePath = fileChooser.getSelectedFile().getAbsolutePath();

		// If the path is ok
		if (!absolutePath.equals(" ")) {

			// Gets the lexicon name
			int index = absolutePath.lastIndexOf("\\");
			if (index == -1)
				index = absolutePath.lastIndexOf("/");
			String fileName = absolutePath.substring(index + 1,
					absolutePath.length());

			if (fileName.contains(".")) {
				index = fileName.lastIndexOf(".");
				fileName = fileName.substring(0, index);
			}

			// Save lexicon as
			boolean isSaved = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getLexiconConfiguration()
					.saveAs(fileName, false, absolutePath);

			// If it could save it
			if (isSaved) {

				// Success message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s451"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s450"), 1);

			} else {

				// Error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s452"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s450"), 0);
			}
		} else

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s92"));
	}
}
