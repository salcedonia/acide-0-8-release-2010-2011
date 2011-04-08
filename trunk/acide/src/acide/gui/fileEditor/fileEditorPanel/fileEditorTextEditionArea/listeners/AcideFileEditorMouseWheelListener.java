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
package acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

import javax.swing.JPanel;

import acide.configuration.fileEditor.AcideFileEditorConfiguration;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * <p>
 * ACIDE - A Configurable IDE file editor mouse wheel listener.
 * </p>
 * <p>
 * When the user uses the combination control key + mouse wheel in a file editor
 * text edition area the font size of all the opened file editors changes.
 * </p>
 * 
 * @version 0.8
 * @see JPanel
 */
public class AcideFileEditorMouseWheelListener implements MouseWheelListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseWheelListener#mouseWheelMoved(java.awt.event.
	 * MouseWheelEvent)
	 */
	@Override
	public void mouseWheelMoved(MouseWheelEvent mouseWheelEvent) {

		if (mouseWheelEvent.isControlDown()) {

			// Apply the changes to the opened file editor panels
			for (int index = 0; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

				// Sets the new font style
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(index)
						.zoomFont(mouseWheelEvent.getScrollAmount(),
								mouseWheelEvent.getWheelRotation() > 0);

				
						
				// Resets the selected file editor text edition area
				AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.resetStyledDocument();
			}

			// Saves the file editor configuration
			AcideFileEditorConfiguration.getInstance().save();
		}
	}
}
