/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando S�enz P�rez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan Jos� Ortiz S�nchez.
 *          - Delf�n Rup�rez Ca�as.
 *      - Version 0.7:
 *          - Miguel Mart�n L�zaro.
 *      - Version 0.8:
 *      	- Javier Salcedo G�mez.
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
package acide.gui.fileEditor.fileEditorPanel.popup.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JOptionPane;
import javax.swing.text.Element;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE file editor panel popup menu send file content to
 * console menu item action listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSendFileContentToConsoleMenuItemAction implements
		ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the root element from the styled document
		Element rootElement = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel()
				.getStyledDocument().getDefaultRootElement();

		// Gets its number of lines
		int numberOfLines = rootElement.getElementCount();

		// If the number of lines to send is bigger than the maximum
		if (numberOfLines > AcideWorkbenchConfiguration.getInstance()
				.getFileEditorConfiguration().getMaximumLinesToConsole()) {

			// Ask if are you sure about the operation
			int returnValueAreYouSure = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s2006"), AcideLanguageManager
							.getInstance().getLabels().getString("s953"),
					JOptionPane.YES_NO_OPTION);

			// If it is OK
			if (returnValueAreYouSure == JOptionPane.OK_OPTION) {

				// Sends the command to the ACIDE - A Configurable IDE console panel
				AcideMainWindow
						.getInstance()
						.getConsolePanel()
						.sendCommandToConsole(
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getTextEditionAreaContent(), "");
			}
		}else
			
			// Sends the command to the ACIDE - A Configurable IDE console panel
			AcideMainWindow
					.getInstance()
					.getConsolePanel()
					.sendCommandToConsole(
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getTextEditionAreaContent(), "");
	}
}
