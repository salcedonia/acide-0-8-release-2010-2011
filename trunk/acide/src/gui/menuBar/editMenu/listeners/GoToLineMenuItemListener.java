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
package gui.menuBar.editMenu.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE edit menu go to line menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class GoToLineMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
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
		ResourceBundle labels = language.getLabels();

		// Ask the user for the line number
		String line = (String) JOptionPane.showInputDialog(null,
				labels.getString("s448"), labels.getString("s447"),
				JOptionPane.YES_NO_CANCEL_OPTION, null, null, null);

		// TODO: COMPROBAR QUE EL NòMERO DE LêNEA SEA UN NòMERO
		if ((line != null)) {
			try {

				int selectedEditor = MainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanelIndex();

				if (selectedEditor >= 0)
					MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedEditor)
							.goToLine(Integer.parseInt(line));
			} catch (Exception exception) {
				
				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}
}
