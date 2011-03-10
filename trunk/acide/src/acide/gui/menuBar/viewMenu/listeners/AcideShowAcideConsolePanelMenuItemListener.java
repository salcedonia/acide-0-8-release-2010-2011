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
package acide.gui.menuBar.viewMenu.listeners;

import acide.configuration.project.AcideProjectConfiguration;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**																
 * ACIDE - A Configurable IDE view menu show console panel menu item listener.
 *					
 * @version 0.8		
 * @see ActionListener																												
 */
public class AcideShowAcideConsolePanelMenuItemListener implements ActionListener{
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {
		
		// If the show console panel check box menu item is selected
		if (AcideMainWindow.getInstance().getMenu().getViewMenu().getShowConsolePanelCheckBoxMenuItem().isSelected()) {
			
			// Sets the horizontal split panel divider location to the end
			AcideMainWindow.getInstance().getHorizontalSplitPane().setDividerLocation(AcideMainWindow.getInstance().getMenu().getViewMenu().getConsoleSize());
			
			// Hides the console panel
			AcideMainWindow.getInstance().getHorizontalSplitPane().getBottomComponent()
					.setVisible(true);
		} else {
			
			// Sets the console size
			AcideMainWindow.getInstance().getMenu().getViewMenu().setConsoleSize(AcideMainWindow.getInstance().getHorizontalSplitPane().getDividerLocation());
			
			// Sets the horizontal split panel divider location to 0
			AcideMainWindow.getInstance().getHorizontalSplitPane().setDividerLocation(0);
			
			// Shows the console panel
			AcideMainWindow.getInstance().getHorizontalSplitPane().getBottomComponent()
					.setVisible(false);
		}
		
		// If it is not the default project
		if (!AcideProjectConfiguration.getInstance().isDefaultProject())
			
			// The project has been modified
			AcideProjectConfiguration.getInstance().setIsModified(true);
	}
}
