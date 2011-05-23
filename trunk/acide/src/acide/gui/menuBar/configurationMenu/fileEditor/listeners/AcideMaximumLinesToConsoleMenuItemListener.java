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
package acide.gui.menuBar.configurationMenu.fileEditor.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JOptionPane;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file editor panel popup menu maximum lines to
 * console menu item action listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideMaximumLinesToConsoleMenuItemListener implements
		ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Ask the user for the maximum number of lines
		String maximumNumberOfLines = (String) JOptionPane.showInputDialog(
				null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s2008"), AcideLanguageManager.getInstance()
						.getLabels().getString("s2007"),
				JOptionPane.YES_NO_CANCEL_OPTION, null, null,
				AcideWorkbenchConfiguration.getInstance()
						.getFileEditorConfiguration()
						.getMaximumLinesToConsole());

		// If the user selected something
		if (maximumNumberOfLines != null) {

			try {

				// Parses the number of lines
				int numberOfLines = Integer.parseInt(maximumNumberOfLines);

				// If the number of lines is positive
				if (numberOfLines > 0) {

					// Updates the maximum lines to the console
					AcideWorkbenchConfiguration.getInstance()
							.getFileEditorConfiguration()
							.setMaximumLinesToConsole(numberOfLines);
				} else

					// Displays an error message
					JOptionPane.showMessageDialog(null, AcideLanguageManager
							.getInstance().getLabels().getString("s2009"),
							"Error", JOptionPane.ERROR_MESSAGE);

			} catch (Exception exception) {

				// Displays an error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s2009"), "Error",
						JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}
		}
	}
}
