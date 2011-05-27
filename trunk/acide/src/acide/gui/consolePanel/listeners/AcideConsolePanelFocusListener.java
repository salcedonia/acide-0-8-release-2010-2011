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
package acide.gui.consolePanel.listeners;

import acide.gui.mainWindow.AcideMainWindow;

import java.awt.Component;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

/**																
 * ACIDE - A Configurable IDE console panel focus listener.
 *					
 * @version 0.8	
 * @see FocusListener																													
 */
public class AcideConsolePanelFocusListener implements FocusListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.FocusListener#focusGained(java.awt.event. FocusEvent)
	 */
	@Override
	public void focusGained(FocusEvent focusEvent) {
		
		//System.out.println("Console: " + focusEvent.toString() +"\n");
		
		// Updates the last element on focus in the main window
		AcideMainWindow.getInstance().setLastElementOnFocus(
				(Component) focusEvent.getSource());
		
		dispatchEvent(focusEvent, false);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.FocusListener#focusLost(java.awt.event.FocusEvent )
	 */
	@Override
	public void focusLost(FocusEvent focusEvent) {
		
		//System.out.println("Console: " + focusEvent.toString() +"\n");
		
		// Updates the last element on focus in the main window
		AcideMainWindow.getInstance().setLastElementOnFocus(
				(Component) focusEvent.getSource());
		
		dispatchEvent(focusEvent, true);
	}
	
	/**
	 * Dispatches the focus event.
	 * 
	 * @param focusEvent focus event.
	 * @param isVisible indicates if the caret is visible or not.
	 */
	private void dispatchEvent(FocusEvent focusEvent, boolean isVisible){
		
		// Gets the selected file editor panel index 
		int selectedEditorPanelIndex = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// If there is anything selected
		if (selectedEditorPanelIndex != -1)
			
			// Hides the caret in the opened editor
			AcideMainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
					.getActiveTextEditionArea().getCaret().setVisible(isVisible);
	}
}
