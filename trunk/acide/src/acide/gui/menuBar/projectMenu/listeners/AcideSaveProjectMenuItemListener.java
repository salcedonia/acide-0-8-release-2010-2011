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
package acide.gui.menuBar.projectMenu.listeners;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.AcideFileManager;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import acide.log.AcideLog;
import acide.resources.AcideResourceManager;
import acide.resources.exception.MissedPropertyException;

/**
 * ACIDE -A Configurable IDE project menu save project menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSaveProjectMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		try {

			// If it is not the first time the project is saved
			if (!AcideProjectConfiguration.getInstance().isFirstSave()) {

				// Enables the save as project menu item
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getSaveAsProjectMenuItem().setEnabled(true);

				// Does the save as project menu item action
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getSaveAsProjectMenuItem().doClick();
			} else {

				// Saves the ACIDE - A Configurable IDE language configuration
				// into the project configuration
				saveLanguageConfiguration();

				// Saves the ACIDE - A Configurable IDE menu configuration
				// into the project configuration
				saveMenuConfiguration();

				// Saves the ACIDE - A Configurable IDE tool bar configuration
				// into the project configuration
				saveToolBarConfiguration();

				// Saves the ACIDE - A Configurable IDE console panel
				// configuration
				// into the project configuration
				saveConsolePanelConfiguration();

				// Saves the ACIDE - A Configurable IDE main window
				// configuration
				// into the project configuration
				saveMainWindowConfiguration();

				// Saves the configuration into the file
				String fileContent = AcideProjectConfiguration.getInstance()
						.save();

				// Writes the file content into it
				AcideFileManager.getInstance().write(
						AcideProjectConfiguration.getInstance()
								.getProjectPath(), fileContent);

				// The project has not been modified
				AcideProjectConfiguration.getInstance().setIsModified(false);
			}

		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Saves the ACIDE - A Configurable IDE tool bar configuration into the
	 * project configuration.
	 * 
	 * @throws MissedPropertyException
	 */
	public void saveToolBarConfiguration() throws MissedPropertyException {

		// Sets the ACIDE - A Configurable IDE current tool bar
		// configuration
		AcideProjectConfiguration.getInstance().setToolBarConfiguration(
				AcideResourceManager.getInstance().getProperty(
						"currentToolBarConfiguration"));
	}

	/**
	 * Saves the ACIDE - A Configurable IDE main window configuration into the
	 * project configuration.
	 */
	public void saveMainWindowConfiguration() {

		// Sets the is explorer panel showed flag as  as the value of the show
		// explorer panel check box menu item
		AcideProjectConfiguration.getInstance().setIsExplorerPanelShowed(
				AcideMainWindow.getInstance().getMenu().getViewMenu()
						.getShowExplorerPanelCheckBoxMenuItem().isSelected());

		// Sets the is console panel showed flag as the value of the show
		// console panel check box menu item
		AcideProjectConfiguration.getInstance().setIsConsolePanelShowed(
				AcideMainWindow.getInstance().getMenu().getViewMenu()
						.getShowConsolePanelCheckBoxMenuItem().isSelected());

		// Sets the ACIDE - A Configurable IDE main window width
		AcideProjectConfiguration.getInstance().setWindowWidth(
				AcideMainWindow.getInstance().getWidth());

		// Sets the ACIDE - A Configurable IDE main window height
		AcideProjectConfiguration.getInstance().setWindowHeight(
				AcideMainWindow.getInstance().getHeight());

		// Sets the ACIDE - A Configurable IDE main window x coordinate
		AcideProjectConfiguration.getInstance().setXCoordinate(
				AcideMainWindow.getInstance().getX());

		// Sets the ACIDE - A Configurable IDE main window y coordinate
		AcideProjectConfiguration.getInstance().setYCoordinate(
				AcideMainWindow.getInstance().getY());

		// Sets the ACIDE - A Configurable IDE main window vertical
		// split pane divider location
		AcideProjectConfiguration.getInstance()
				.setVerticalSplitPaneDividerLocation(
						AcideMainWindow.getInstance().getVerticalSplitPane()
								.getDividerLocation());

		// Sets the ACIDE - A Configurable IDE main window horizontal
		// split pane divider location
		AcideProjectConfiguration.getInstance()
				.setHorizontalSplitPaneDividerLocation(
						AcideMainWindow.getInstance().getHorizontalSplitPane()
								.getDividerLocation());
	}

	/**
	 * Saves the ACIDE - A Configurable IDE console panel configuration into the
	 * project configuration.
	 * 
	 * @throws MissedPropertyException
	 */
	public void saveConsolePanelConfiguration() throws MissedPropertyException {

		// Sets the ACIDE - A Configurable IDE console panel shell path
		AcideProjectConfiguration.getInstance().setShellPath(
				AcideResourceManager.getInstance().getProperty(
						"consolePanel.shellPath"));

		// Sets the ACIDE - A Configurable IDE console panel shell
		// directory
		AcideProjectConfiguration.getInstance().setShellDirectory(
				AcideResourceManager.getInstance().getProperty(
						"consolePanel.shellDirectory"));

		// Sets the ACIDE - A Configurable IDE console panel exit
		// command
		AcideProjectConfiguration.getInstance().setExitCommand(
				AcideResourceManager.getInstance().getProperty(
						"consolePanel.exitCommand"));

		// Sets the ACIDE - A Configurable IDE console panel is echo
		// command
		AcideProjectConfiguration.getInstance().setIsEchoCommand(
				Boolean.parseBoolean(AcideResourceManager.getInstance()
						.getProperty("consolePanel.isEchoCommand")));

		// Sets the ACIDE - A Configurable IDE console panel foreground
		// color
		AcideProjectConfiguration.getInstance().setForegroundColor(
				new Color(Integer.parseInt(AcideResourceManager.getInstance()
						.getProperty("consolePanel.foregroundColor"))));

		// Sets the ACIDE - A Configurable IDE console panel background
		// color
		AcideProjectConfiguration.getInstance().setBackgroundColor(
				new Color(Integer.parseInt(AcideResourceManager.getInstance()
						.getProperty("consolePanel.backgroundColor"))));

		// Sets the ACIDE - A Configurable IDE console panel buffer
		// size
		AcideProjectConfiguration.getInstance().setBufferSize(
				Integer.parseInt(AcideResourceManager
						.getInstance().getProperty(
								"consolePanel.bufferSize")));
		
		// Sets the ACIDE - A Configurable IDE console panel font name
		AcideProjectConfiguration.getInstance().setFontName(
				AcideResourceManager.getInstance().getProperty(
						"consolePanel.fontName"));

		// Sets the ACIDE - A Configurable IDE console panel font style
		AcideProjectConfiguration.getInstance().setFontStyle(
				Integer.parseInt(AcideResourceManager.getInstance()
						.getProperty("consolePanel.fontStyle")));

		// Sets the ACIDE - A Configurable IDE console panel font size
		AcideProjectConfiguration.getInstance().setFontSize(
				Integer.parseInt(AcideResourceManager.getInstance()
						.getProperty("consolePanel.fontSize")));
	}

	/**
	 * Saves the ACIDE - A Configurable IDE menu configuration into the project
	 * configuration.
	 * 
	 * @throws MissedPropertyException
	 */
	public void saveMenuConfiguration() throws MissedPropertyException {

		// Sets the ACIDE - A Configurable IDE current menu
		// configuration
		AcideProjectConfiguration.getInstance().setMenuConfiguration(
				AcideResourceManager.getInstance().getProperty(
						"currentMenuConfiguration"));
	}

	/**
	 * Saves the ACIDE - A Configurable IDE language configuration into the
	 * project configuration.
	 * 
	 * @throws MissedPropertyException
	 */
	public void saveLanguageConfiguration() throws MissedPropertyException {
		// Sets the the ACIDE - A Configurable IDE language
		// configuration
		AcideProjectConfiguration.getInstance().setLanguageConfiguration(
				AcideResourceManager.getInstance().getProperty("language"));
	}
}
