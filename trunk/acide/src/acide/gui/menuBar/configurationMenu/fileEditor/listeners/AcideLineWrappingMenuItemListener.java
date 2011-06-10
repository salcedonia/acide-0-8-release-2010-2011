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
package acide.gui.menuBar.configurationMenu.fileEditor.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.text.BadLocationException;
import javax.swing.text.SimpleAttributeSet;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.gui.mainWindow.AcideMainWindow;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file editor panel popup menu line wrapping check
 * box menu item action listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideLineWrappingMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Updates the ACIDE - A Configurable IDE workbench configuration
		AcideWorkbenchConfiguration
				.getInstance()
				.getFileEditorConfiguration()
				.setLineWrapping(
						AcideMainWindow.getInstance().getMenu()
								.getConfigurationMenu().getFileEditorMenu()
								.getLineWrappingCheckBoxMenuItem().isSelected());
		
		try {

			for (int index = 0; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {
				
				// Provokes the changes in the document to apply the changes
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getActiveTextEditionArea().getDocument()
						.insertString(0, "", new SimpleAttributeSet());
			}
		} catch (BadLocationException exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
