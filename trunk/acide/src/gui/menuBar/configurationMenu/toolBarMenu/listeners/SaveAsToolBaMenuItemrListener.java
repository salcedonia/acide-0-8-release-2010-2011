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
package gui.menuBar.configurationMenu.toolBarMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;
import es.bytes.ByteFile;
import es.text.AcideTextFileExtensionFilterManager;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.toolBarMenu.gui.AcideToolBarConfigurationWindow;

/**																
 * ACIDE - A Configurable IDE tool bar menu save as tool bar listener.
 *					
 * @version 0.8
 * @see ActionListener																														
 */
public class SaveAsToolBaMenuItemrListener implements ActionListener{
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		try {
			
			String current = AcideResourceManager
					.getInstance().getProperty("currentToolBarConfiguration");
			JFileChooser fileChooser = new JFileChooser();
			AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(labels
					.getString("s158"));
			filter.addExtension("TBcfg");
			fileChooser.setFileFilter(filter);
			fileChooser.setCurrentDirectory(new File(
					"./configuration/toolbar/"));
			String fileName = "";
			
			// Asks for saving
			int chosenOption = fileChooser.showSaveDialog(fileChooser);
			if (chosenOption == JFileChooser.APPROVE_OPTION) {
				
				fileName = fileChooser.getSelectedFile()
						.getAbsolutePath();
				if (!fileName.endsWith(".TBcfg"))
					fileName += ".TBcfg";
				ByteFile.copy(current, fileName);
				
				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", fileName);
				
				MainWindow.getInstance().getMenu().getConfiguration().getToolBar().getSaveToolBar().setEnabled(false);
				
				AcideToolBarConfigurationWindow.setAreChangesSaved(true);
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s900") + fileName
						+ labels.getString("s901"));
			} else 
				// Cancel option
				if (chosenOption == JFileChooser.CANCEL_OPTION) {
				
				fileChooser.cancelSelection();
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s902"));
			}
		} catch (Exception exception) {
			
			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s903"), JOptionPane.ERROR_MESSAGE);
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}
