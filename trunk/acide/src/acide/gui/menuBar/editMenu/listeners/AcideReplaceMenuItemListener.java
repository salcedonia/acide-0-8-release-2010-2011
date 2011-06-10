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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.gui.AcideReplaceWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE edit menu replace menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideReplaceMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Updates the log
		AcideLog.getLog()
				.info(AcideLanguageManager.getInstance().getLabels()
						.getString("s96"));

		// If it is being showing
		if (AcideReplaceWindow.getInstance().isShowing())

			// Hides the window
			AcideReplaceWindow.getInstance().setVisible(false);
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
				AcideReplaceWindow.getInstance().getSelectedTextRadioButton()
						.setSelected(true);
				
				// Deselects its both directions radio button
				AcideReplaceWindow.getInstance().getBothDirectionsRadioButton()
						.setEnabled(false);
				
				// Clears its search text field
				AcideReplaceWindow.getInstance().getSearchTextField().setText("");
			} else {
				
				// Selects its current document radio button
				AcideReplaceWindow.getInstance()
						.getCurrentDocumentRadioButton().setSelected(true);
				
				// Selects its both directions radio button
				AcideReplaceWindow.getInstance().getBothDirectionsRadioButton()
						.setEnabled(true);
				
				// Clears its search text field
				AcideReplaceWindow.getInstance().getSearchTextField().setText("");
			}
			
			// Shows the replace window
			AcideReplaceWindow.getInstance().setVisible(true);
		}
	}
}
