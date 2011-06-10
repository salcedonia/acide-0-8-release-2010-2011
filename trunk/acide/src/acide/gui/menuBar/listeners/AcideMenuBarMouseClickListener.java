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
package acide.gui.menuBar.listeners;

import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * ACIDE - A Configurable IDE menu bar mouse click listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideMenuBarMouseClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {

		// If file menu is selected
		if (AcideMainWindow.getInstance().getMenu().getFileMenu().isSelected())

			// Configures the file menu
			AcideMainWindow.getInstance().getMenu().getFileMenu().configure();

		// If edit menu is selected
		if (AcideMainWindow.getInstance().getMenu().getEditMenu().isSelected())

			// Configures the edit menu
			AcideMainWindow.getInstance().getMenu().getEditMenu().configure();

		// If project menu is selected
		if (AcideMainWindow.getInstance().getMenu().getProjectMenu()
				.isSelected())

			// Configures the project menu
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.configure();
	
		// If lexicon menu is selected
		if (AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
				.getLexiconMenu().isSelected())

			// Configures the lexicon menu
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getLexiconMenu().configure();
		
		// If grammar menu is selected
		if (AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
				.getGrammarMenu().isSelected())

			// Configures the grammar menu
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getGrammarMenu().configure();

		// If lexicon menu is selected
		if (AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
				.getFileEditorMenu().isSelected())
			
			// Configures the file editor menu
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getFileEditorMenu().configure();
		
		// Updates the is console panel focused variable for the copy, cut and
		// paste menu items
		AcideMainWindow
				.getInstance()
				.getMenu()
				.setIsConsoleFocused(
						AcideMainWindow.getInstance().getConsolePanel()
								.getTextPane().isFocusOwner());

		// Validates the changes in the menu
		AcideMainWindow.getInstance().getMenu().validate();

		// Repaints the menu
		AcideMainWindow.getInstance().getMenu().repaint();
	}
}
