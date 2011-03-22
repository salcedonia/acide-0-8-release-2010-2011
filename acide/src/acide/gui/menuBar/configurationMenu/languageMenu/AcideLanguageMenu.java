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
package acide.gui.menuBar.configurationMenu.languageMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.languageMenu.listeners.AcideEnglishMenuItemListener;
import acide.gui.menuBar.configurationMenu.languageMenu.listeners.AcideSpanishMenuItemListener;
import acide.gui.menuBar.editMenu.gui.replace.AcideReplaceWindow;
import acide.gui.menuBar.editMenu.gui.search.AcideSearchWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE language menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideLanguageMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE language menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE language menu Spanish menu item name.
	 */
	public static final String SPANISH_NAME = "Spanish";
	/**
	 * ACIDE - A Configurable IDE language menu English menu item name.
	 */
	public static final String ENGLISH_NAME = "English";
	/**
	 * ACIDE - A Configurable IDE language menu Spanish menu item image icon.
	 */
	private static final ImageIcon SPANISH_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/language/spanish.png");
	/**
	 * ACIDE - A Configurable IDE language menu English menu item image icon.
	 */
	private static final ImageIcon ENGLISH_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/language/english.png");
	/**
	 * ACIDE - A Configurable IDE language menu Spanish menu item.
	 */
	private JMenuItem _spanishMenuItem;
	/**
	 * ACIDE - A Configurable IDE language menu English menu item.
	 */
	private JMenuItem _englishMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE language menu.
	 */
	public AcideLanguageMenu() {

		// SPANISH MENU ITEM
		_spanishMenuItem = new JMenuItem(SPANISH_IMAGE);

		// ENGLISH MENU ITEM
		_englishMenuItem = new JMenuItem(ENGLISH_IMAGE);

		// Sets the text of the language menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE language menu components
	 * with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		// SPANISH MENU ITEM
		_spanishMenuItem.setText(labels.getString("s11"));
		_spanishMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
				ActionEvent.ALT_MASK));

		// ENGLISH MENU ITEM
		_englishMenuItem.setText(labels.getString("s12"));
		_englishMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,
				ActionEvent.ALT_MASK));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE language menu.
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// SPANISH MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SPANISH_NAME))
			add(_spanishMenuItem);

		// ENGLISH MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(ENGLISH_NAME))
			add(_englishMenuItem);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE language menu menu item listeners.
	 */
	public void setListeners() {

		// SPANISH MENU ITEM
		_spanishMenuItem.addActionListener(new AcideSpanishMenuItemListener());

		// ENGLISH MENU ITEM
		_englishMenuItem.addActionListener(new AcideEnglishMenuItemListener());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE language menu English menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE language menu English menu item.
	 */
	public JMenuItem getEnglishMenuItem() {
		return _englishMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE language menu Spanish menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE language menu Spanish menu item.
	 */
	public JMenuItem getSpanishMenuItem() {
		return _spanishMenuItem;
	}

	/**
	 * Changes the language to display in the application and reset all the
	 * components with the new language.
	 * 
	 * @param selectedLanguage
	 *            new language to set.
	 */
	public void changeLanguage(String selectedLanguage) {

		try {

			// Gets the ACIDE - A Configurable IDE language
			AcideLanguageManager.getInstance().getLanguage(
					AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Updates the ACIDE - A Configurable IDE language
		AcideResourceManager.getInstance().setProperty("language",
				selectedLanguage);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s100"));

		// Resets the text of the menu bar
		AcideMainWindow.getInstance().getMenu().setTextOfMenuComponents();

		// Resets the tool bar panel
		AcideMainWindow.getInstance().buildToolBarPanel();

		// Resets the explorer panel popup menu
		AcideMainWindow.getInstance().getExplorerPanel().buildPopupMenu();

		// Resets the console panel popup menu
		AcideMainWindow.getInstance().getConsolePanel().buildPopupMenu();

		// Resets the status bar popup menu
		AcideMainWindow.getInstance().getStatusBar().buildPopupMenu();

		// Resets the file editor panels
		for (int index = 0; index < AcideMainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels(); index++)

			// Updates the file editor panel popup menus
			AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(index).buildPopupMenu();

		// Validates the changes in the main window
		AcideMainWindow.getInstance().validate();

		// Repaints the main window
		AcideMainWindow.getInstance().repaint();

		// Resets the SEARCH GUI
		AcideSearchWindow searchGUI = AcideSearchWindow.getInstance();
		searchGUI.initialize();
		searchGUI.validate();
		searchGUI.repaint();

		// Resets the REPLACE GUI
		AcideReplaceWindow replaceGUI = AcideReplaceWindow.getInstance();
		replaceGUI.initialize();
		replaceGUI.validate();
		replaceGUI.repaint();

		// Gets the current lines number message
		String currentNumLinesMessage = AcideMainWindow.getInstance()
				.getStatusBar().getNumberOfLinesMessage();

		// Gets the number of lines
		int lastIndexOfDouble = currentNumLinesMessage.lastIndexOf(":");
		if (lastIndexOfDouble != -1) {
			String numLines = currentNumLinesMessage.substring(
					lastIndexOfDouble, currentNumLinesMessage.length());

			// Updates the number of lines in the status bar
			String numLinesMessage = AcideLanguageManager.getInstance()
					.getLabels().getString("s1001")
					+ numLines;

			// Updates the number of lines message in the status bar
			AcideMainWindow.getInstance().getStatusBar()
					.setNumberOfLinesMessage(numLinesMessage);
		}

		// If it is not the default project
		if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

			// Enables the project menu
			AcideMainWindow.getInstance().getMenu().enableProjectMenu();

			// Enables the open all files menu item
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getOpenAllFilesMenuItem().setEnabled(true);

			// The project configuration has been modified
			AcideProjectConfiguration.getInstance().setIsModified(true);
		}
	}
}