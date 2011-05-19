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

import javax.swing.JOptionPane;
import acide.language.AcideLanguageManager;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE lexicon menu new lexicon menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideNewLexiconMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		String lexiconConfiguration = "";
		String lexiconConfigurationName = "";

		// Asks to the user
		lexiconConfigurationName = JOptionPane.showInputDialog(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s453"), AcideLanguageManager.getInstance()
						.getLabels().getString("s454"),
				JOptionPane.INFORMATION_MESSAGE);

		// If it is ok
		if (!lexiconConfigurationName.equals("")) {

			if (lexiconConfigurationName.contains(".xml"))
				lexiconConfiguration = "./configuration/lexicon/"
						+ lexiconConfigurationName;
			else
				lexiconConfiguration = "./configuration/lexicon/"
						+ lexiconConfigurationName + ".xml";

			// Creates the new lexicon configuration
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getLexiconConfiguration()
					.newLexicon(lexiconConfiguration);
			
		} else {

			// Displays a "missing name" warning message
			JOptionPane.showMessageDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s976"),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s972"), JOptionPane.WARNING_MESSAGE);
		}
	}
}