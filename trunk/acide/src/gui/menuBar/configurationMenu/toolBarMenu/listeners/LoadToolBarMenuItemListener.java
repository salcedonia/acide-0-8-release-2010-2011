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

import es.configuration.project.AcideProjectConfiguration;
import es.configuration.toolBar.consoleComandToolBar.ConsoleCommandList;
import es.text.AcideTextFileExtensionFilterManager;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.toolBarMenu.gui.AcideToolBarConfigurationWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE - A Configurable IDE tool bar menu load tool bar listener.											
 *					
 * @version 0.8
 * @see ActionListener																														
 */
public class LoadToolBarMenuItemListener implements ActionListener{

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
		
		JFileChooser fileChooser = new JFileChooser();
		AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(labels
				.getString("s904"));
		filter.addExtension("TBcfg");
		fileChooser.setFileFilter(filter);
		fileChooser.setCurrentDirectory(new File("./configuration/toolbar/"));
		
		int chosenOption = fileChooser.showOpenDialog(null);
		if (chosenOption == JFileChooser.APPROVE_OPTION) {
			
			String toolBarFile = fileChooser.getSelectedFile().getAbsolutePath();
			
			try {
				
				// Loads the lists
				ConsoleCommandList.loadFinalList(toolBarFile);
				ConsoleCommandList.loadTemporalList(toolBarFile);
				MainWindow.getInstance().updateToolBarPanel();
				
				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration", toolBarFile);
				
				// Updates the MAIN WINDOW
				MainWindow.getInstance().validate();
				MainWindow.getInstance().repaint();
				
				// Disables the tool bar save menu item
				MainWindow.getInstance().getMenu().getConfiguration().getToolBar().getSaveToolBar().setEnabled(false);
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s905") + toolBarFile);
				
				AcideToolBarConfigurationWindow.setAreChangesSaved(true);
								
				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject())
					
					// The project has been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(true);

			} catch (Exception exception) {
				
				// Error message
				JOptionPane.showMessageDialog(null,
						labels.getString("s906") + toolBarFile,
						labels.getString("s907"),
						JOptionPane.ERROR_MESSAGE);
				
				// Updates the log
				AcideLog.getLog().error(labels.getString("s906") + toolBarFile);
			}
		} else 
			// Cancel option
			if (chosenOption == JFileChooser.CANCEL_OPTION) {
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s908"));
		}
	}
}
