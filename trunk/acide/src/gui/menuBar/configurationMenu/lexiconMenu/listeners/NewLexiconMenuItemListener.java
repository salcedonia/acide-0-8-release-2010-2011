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

import javax.swing.JOptionPane;

import language.AcideLanguageManager;
import es.configuration.lexicon.AcideLexiconConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import gui.mainWindow.MainWindow;

/**
 * ACIDE - A Configurable IDE lexicon menu new lexicon menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class NewLexiconMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		String languagePath = "";
		String lexiconConfigurationName = "";
		lexiconConfigurationName = JOptionPane.showInputDialog(null, AcideLanguageManager
				.getInstance().getLabels().getString("s453"),
				AcideLanguageManager.getInstance().getLabels()
						.getString("s454"), JOptionPane.INFORMATION_MESSAGE);

		// If it is ok
		if (!lexiconConfigurationName.equals("")) {

			if (lexiconConfigurationName.contains(".xml"))
				languagePath = "./configuration/lexicon/" + lexiconConfigurationName;
			else
				languagePath = "./configuration/lexicon/" + lexiconConfigurationName
						+ ".xml";

			// Resets all the editors with the new lexicon configuration
			AcideLexiconConfiguration.getInstance().newLexicon(languagePath);

			// Resets all the opened file editor panels with the new lexical
			// configuration
			for (int index = 0; index < MainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

				// Gets the caret position
				int caretPosition = MainWindow.getInstance()
						.getFileEditorManager().getFileEditorPanelAt(index)
						.getActiveTextEditionArea().getCaretPosition();

				// Resets the file editor text edition area
				MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index)
						.resetStyledDocument(caretPosition);
			}

			// Updates the lexicon message in the status bar
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
					AcideLexiconConfiguration.getInstance().getName());

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);
			}
		} else {

			// Missed name
			JOptionPane.showMessageDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s976"),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s972"), JOptionPane.WARNING_MESSAGE);
		}
	}
}