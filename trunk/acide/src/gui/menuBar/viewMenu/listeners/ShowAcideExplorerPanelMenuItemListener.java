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
package gui.menuBar.viewMenu.listeners;

import es.configuration.project.AcideProjectConfiguration;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**																
 * ACIDE - A Configurable IDE view menu show explorer panel check box menu item listener.
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class ShowAcideExplorerPanelMenuItemListener implements ActionListener{
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {
		
		// If the show explorer check box menu item is selected
		if (MainWindow.getInstance().getMenu().getView().getShowExplorerPanel().isSelected())
			
			// Shows the explorer panel
			MainWindow.getInstance().getExplorerPanel().showExplorerPanel();
		else
			
			// Hides the explorer panel.
			MainWindow.getInstance().getExplorerPanel().disposeExplorerPanel();

		// Not default project
		if (!AcideProjectConfiguration.getInstance().isDefaultProject())
			
			// The project has been modified
			AcideProjectConfiguration.getInstance().setIsModified(true);
	}
}
