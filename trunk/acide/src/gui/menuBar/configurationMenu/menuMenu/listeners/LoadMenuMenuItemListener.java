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
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.configuration.menu.AcideMenuConfiguration;
import es.configuration.menu.AcideMenuItemInformation;
import es.configuration.project.AcideProjectConfiguration;
import es.text.TextFileFilter;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.menuMenu.gui.AcideMenuConfigurationWindow;

/**																
 * ACIDE - A Configurable IDE menu menu load menu menu item listener.
 *					
 * @version 0.8
 * @see ActionListener																														
 */
public class LoadMenuMenuItemListener implements ActionListener {

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

		JFileChooser fileChooser = new JFileChooser();
		TextFileFilter filter = new TextFileFilter(labels.getString("s287"));
		filter.addExtension("menuCfg");
		fileChooser.setFileFilter(filter);
		fileChooser.setCurrentDirectory(new File("./configuration/menu/"));

		int chosenOption = fileChooser.showOpenDialog(null);
		if (chosenOption == JFileChooser.APPROVE_OPTION) {
			
			String menuFile = fileChooser.getSelectedFile().getAbsolutePath();
			ArrayList<AcideMenuItemInformation> menuItemList = null;
			try {
				
				// Loads the menu item list
				menuItemList = AcideMenuConfiguration
						.getInstance().loadMenuConfigurationFile(menuFile);
				
				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty("currentMenuConfiguration",
						menuFile);
				
				// Sets the new menu item list
				AcideMenuConfiguration.getInstance().setMenuElementList(menuItemList);
				
				// Updates the Main Window
				MainWindow.getInstance().getMenu().buildMenu();
				MainWindow.getInstance().validate();
				MainWindow.getInstance().repaint();
				MainWindow.getInstance().getMenu().getConfiguration().getMenu().getSaveMenu().setEnabled(false);

				// Updates the log
				AcideLog.getLog().info(labels.getString("s289"));

				// The changes are saved now
				AcideMenuConfigurationWindow.setChangesAreSaved(true);

				// Not default project
				if (!AcideProjectConfiguration.getInstance()
						.isDefaultProject())
					
					// The project has been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(true);

			} catch (Exception exception) {

				// Error message
				JOptionPane.showMessageDialog(null,
						labels.getString("s288") + " " + menuFile,
						labels.getString("289"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(
						labels.getString("s288") + " " + menuFile);
			}
		} else
		// Cancel option
		if (chosenOption == JFileChooser.CANCEL_OPTION) {
			// Updates the log
			AcideLog.getLog().info(labels.getString("s290"));
		}
	}
}