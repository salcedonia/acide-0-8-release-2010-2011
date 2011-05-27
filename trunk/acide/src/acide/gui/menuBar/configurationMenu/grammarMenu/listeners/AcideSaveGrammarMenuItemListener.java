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
package acide.gui.menuBar.configurationMenu.grammarMenu.listeners;

import acide.files.bytes.AcideByteFileManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JOptionPane;

/**
 * ACIDE - A Configurable IDE save grammar menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveGrammarMenuItemListener implements ActionListener {
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		try {

			// Gets the ACIDE - A Configurable IDE previous grammar
			// configuration
			String previousGrammarConfiguration = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getPreviousGrammarConfiguration().getPath();

			// Gets the ACIDE - A Configurable IDE previous grammar
			// configuration
			String currentGrammarConfiguration = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getCurrentGrammarConfiguration().getPath();

			// Copies the files
			AcideByteFileManager.getInstance().copy(
					currentGrammarConfiguration, previousGrammarConfiguration);

			// Updates the current grammar configuration path
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getCurrentGrammarConfiguration()
					.setPath(previousGrammarConfiguration);
			
			// Disables the save grammar menu item
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getGrammarMenu().getSaveGrammarMenuItem()
					.setEnabled(false);

			// Updates the grammar message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setGrammarMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s248")
									+ " "
									+ AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getCurrentGrammarConfiguration()
											.getName());

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s940")
							+ ": " + previousGrammarConfiguration);

		} catch (Exception exception) {

			// Displays an error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s939"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}
