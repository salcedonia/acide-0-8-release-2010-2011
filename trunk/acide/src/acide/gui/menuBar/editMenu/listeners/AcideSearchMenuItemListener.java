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
package acide.gui.menuBar.editMenu.listeners;

import acide.gui.mainWindow.AcideMainWindow;

import acide.gui.menuBar.editMenu.gui.AcideSearchWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * ACIDE - A Configurable IDE edit menu search menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSearchMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// If it is being showing
		if (AcideSearchWindow.getInstance().isShowing())

			// Hides the window
			AcideSearchWindow.getInstance().setVisible(false);
		else {

			String selectedText = null;

			// Gets the selected file editor panel index
			int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// Gets the selected text
			selectedText = AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(selectedFileEditorPanelIndex)
					.getActiveTextEditionArea().getSelectedText();
			
			// If there is selected text
			if (selectedText != null) {
				
				// Selects its selected text radio button
				AcideSearchWindow.getInstance().getSelectedTextRadioButton()
						.setSelected(true);
				
				// Deselects its both directions radio button
				AcideSearchWindow.getInstance().getBothDirectionsRadioButton()
						.setEnabled(false);
				
				// Clears its search text field
				AcideSearchWindow.getInstance().getSearchTextField().setText("");

			} else {
				
				// Deselects its current document radio button
				AcideSearchWindow.getInstance().getCurrentDocumentRadioButton()
						.setSelected(false);
				
				// Selects its selected text radio button
				AcideSearchWindow.getInstance().getSelectedTextRadioButton()
						.setEnabled(true);
				
				// Clears its search text field
				AcideSearchWindow.getInstance().getSearchTextField().setText("");
			}
			// Shows the replace window
			AcideSearchWindow.getInstance().setVisible(true);
		}
	}
}
