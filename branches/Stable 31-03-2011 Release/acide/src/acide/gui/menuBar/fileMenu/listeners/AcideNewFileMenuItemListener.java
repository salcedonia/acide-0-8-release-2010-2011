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

import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.files.project.AcideProjectFileType;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file menu new file menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideNewFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// TODO: Load the predefined extension

		// Creates the lexicon configuration
		AcideLexiconConfiguration lexiconConfiguration = new AcideLexiconConfiguration();

		// Loads the lexicon configuration
		lexiconConfiguration.load(AcideLexiconConfiguration.DEFAULT_PATH
				+ AcideLexiconConfiguration.DEFAULT_NAME);

		// TODO: Load the predefined extension

		// Creates the current grammar configuration
		AcideGrammarConfiguration currentGrammarConfiguration = new AcideGrammarConfiguration();

		// Sets the current grammar configuration path
		currentGrammarConfiguration
				.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

		// Creates the previous grammar configuration
		AcideGrammarConfiguration previousGrammarConfiguration = new AcideGrammarConfiguration();

		// Sets the previous grammar configuration path
		previousGrammarConfiguration
				.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

		// Updates the tabbed pane in the file editor manager
		AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.updatesTabbedPane(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s79"), "", true,
						AcideProjectFileType.NORMAL, 0, 0, 1,
						lexiconConfiguration, currentGrammarConfiguration,
						previousGrammarConfiguration);

		// Updates the log
		AcideLog.getLog()
				.info(AcideLanguageManager.getInstance().getLabels()
						.getString("s80"));
	}
}