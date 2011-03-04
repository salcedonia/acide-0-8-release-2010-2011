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
package gui.menuBar.configurationMenu.menuMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.bytes.ByteFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.menuMenu.gui.AcideMenuConfigurationWindow;

/**																
 * ACIDE - A Configurable IDE menu menu save menu menu item listener.											
 *					
 * @version 0.8
 * @see ActionListener																														
 */
public class SaveMenuMenuItemListener implements ActionListener {

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

		try {
			String previous = AcideResourceManager
					.getInstance().getProperty("previousMenuConfiguration");
			String current = AcideResourceManager
					.getInstance().getProperty("currentMenuConfiguration");
			ByteFile.copy(current, previous);
			
			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("currentMenuConfiguration",
					previous);
			
			MainWindow.getInstance().getMenu().getConfiguration().getMenu().getSaveMenu().setEnabled(false);
			AcideMenuConfigurationWindow.setChangesAreSaved(true);
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s293"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}
