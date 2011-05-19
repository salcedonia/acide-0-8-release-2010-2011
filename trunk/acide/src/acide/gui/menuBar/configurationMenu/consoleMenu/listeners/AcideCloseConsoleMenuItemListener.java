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
package acide.gui.menuBar.configurationMenu.consoleMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import acide.configuration.project.AcideProjectConfiguration;
import acide.gui.mainWindow.AcideMainWindow;
import acide.resources.AcideResourceManager;

/**																
 * ACIDE - A Configurable IDE close console menu item listener.											
 *					
 * @version 0.8
 * @see ActionListener																														
 */
public class AcideCloseConsoleMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Exits the console
		AcideMainWindow.getInstance().getConsolePanel()
				.executeExitCommand();

		// Sets the shell directory in the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.shellDirectory",
				"");

		// Sets the shell path in the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.shellPath",
				"");

		// Sets the echo command in the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.isEchoCommand",
				"false");

		// Sets the exit command in the resource manager
		AcideResourceManager.getInstance().setProperty(
				"consolePanel.exitCommand",
				"");

		// Resets the console
		AcideMainWindow.getInstance().getConsolePanel()
				.resetConsole();

		// Sets the shell directory in the project
		// configuration
		AcideProjectConfiguration.getInstance().setShellDirectory(
				"");

		// Sets the shell path in the project configuration
		AcideProjectConfiguration.getInstance().setShellPath(
				"");

		// Sets the echo command in the project configuration
		AcideProjectConfiguration.getInstance().setIsEchoCommand(
				false);

		// Sets the exit command in the project configuration
		AcideProjectConfiguration.getInstance().setExitCommand(
				"");

		// Not default project
		if (!AcideProjectConfiguration.getInstance()
				.isDefaultProject())

			// The project has been modified
			AcideProjectConfiguration.getInstance().setIsModified(
					true);
	}
}
