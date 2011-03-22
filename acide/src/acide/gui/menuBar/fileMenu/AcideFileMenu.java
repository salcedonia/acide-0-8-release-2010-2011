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
package acide.gui.menuBar.fileMenu;

import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.project.workbench.AcideWorkbenchManager;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFileType;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.fileMenu.listeners.AcideCloseAllFilesMenuItemListener;
import acide.gui.menuBar.fileMenu.listeners.AcideCloseFileMenuItemListener;
import acide.gui.menuBar.fileMenu.listeners.AcideExitMenuItemListener;
import acide.gui.menuBar.fileMenu.listeners.AcideNewFileMenuItemListener;
import acide.gui.menuBar.fileMenu.listeners.AcideOpenAllFilesMenuItemListener;
import acide.gui.menuBar.fileMenu.listeners.AcideOpenFileMenuItemListener;
import acide.gui.menuBar.fileMenu.listeners.AcidePrintFileMenuItemListener;
import acide.gui.menuBar.fileMenu.listeners.AcideSaveAllFilesMenuItemListener;
import acide.gui.menuBar.fileMenu.listeners.AcideSaveFileAsMenuItemListener;
import acide.gui.menuBar.fileMenu.listeners.AcideSaveFileMenuItemListener;
import acide.gui.menuBar.fileMenu.recentFilesMenu.AcideRecentFilesMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE file menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideFileMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE file menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE file menu new file menu item name.
	 */
	public final static String NEW_FILE_NAME = "New File";
	/**
	 * ACIDE - A Configurable IDE file menu open file menu item name.
	 */
	public final static String OPEN_FILE_NAME = "Open File";
	/**
	 * ACIDE - A Configurable IDE file menu open recent files menu item name.
	 */
	public final static String OPEN_RECENT_FILES_NAME = "Open Recent Files";
	/**
	 * ACIDE - A Configurable IDE file menu open all files menu item name.
	 */
	public final static String OPEN_ALL_FILES_NAME = "Open All Files";
	/**
	 * ACIDE - A Configurable IDE file menu close file menu item name.
	 */
	public final static String CLOSE_FILE_NAME = "Close File";
	/**
	 * ACIDE - A Configurable IDE file menu close all files menu item name.
	 */
	public final static String CLOSE_ALL_FILES_NAME = "Close All Files";
	/**
	 * ACIDE - A Configurable IDE file menu save file menu item name.
	 */
	public final static String SAVE_FILE_NAME = "Save File";
	/**
	 * ACIDE - A Configurable IDE file menu save all files menu item name.
	 */
	public final static String SAVE_ALL_FILES_NAME = "Save All Files";
	/**
	 * ACIDE - A Configurable IDE file menu save file menu item name.
	 */
	public final static String SAVE_FILE_AS_NAME = "Save File As";
	/**
	 * ACIDE - A Configurable IDE file menu print file menu item name.
	 */
	public final static String PRINT_FILE_NAME = "Print File";
	/**
	 * ACIDE - A Configurable IDE file menu exit menu item name.
	 */
	public final static String EXIT_NAME = "Exit File";
	/**
	 * ACIDE - A Configurable IDE file menu new file menu item image icon.
	 */
	private final static ImageIcon NEW_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/newFile.png");
	/**
	 * ACIDE - A Configurable IDE file menu open file menu item image icon.
	 */
	private final static ImageIcon OPEN_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/openFile.png");
	/**
	 * ACIDE - A Configurable IDE file menu close file menu item image icon.
	 */
	private final static ImageIcon CLOSE_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/closeFile.png");
	/**
	 * ACIDE - A Configurable IDE file menu close all files menu item image
	 * icon.
	 */
	private final static ImageIcon CLOSE_ALL_FILES_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/closeAllFiles.png");
	/**
	 * ACIDE - A Configurable IDE file menu save file menu item image icon.
	 */
	private final static ImageIcon SAVE_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/saveFile.png");
	/**
	 * ACIDE - A Configurable IDE file menu save all files menu item image icon.
	 */
	private final static ImageIcon SAVE_ALL_FILES_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/saveAllFiles.png");
	/**
	 * ACIDE - A Configurable IDE file menu save file as menu item image icon.
	 */
	private final static ImageIcon SAVE_FILE_AS_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/saveFileAs.png");
	/**
	 * ACIDE - A Configurable IDE file menu print file menu item image icon.
	 */
	private final static ImageIcon PRINT_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/printFile.png");
	/**
	 * ACIDE - A Configurable IDE file menu exit menu item image icon.
	 */
	private final static ImageIcon EXIT_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/exit.png");
	/**
	 * ACIDE - A Configurable IDE file menu open all files menu item image icon.
	 */
	private final static ImageIcon OPEN_ALL_FILES_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/openAllFiles.png");
	/**
	 * ACIDE - A Configurable IDE file menu new file menu item.
	 */
	private JMenuItem _newFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file menu open file menu item.
	 */
	private JMenuItem _openFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file menu open recent files menu.
	 */
	private AcideRecentFilesMenu _openRecentFilesMenu;
	/**
	 * ACIDE - A Configurable IDE file menu open all files menu item.
	 */
	private JMenuItem _openAllFilesMenuItem;
	/**
	 * ACIDE - A Configurable IDE file menu close file menu item.
	 */
	private JMenuItem _closeFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file menu close all files menu item.
	 */
	private JMenuItem _closeAllFilesMenuItem;
	/**
	 * ACIDE - A Configurable IDE file menu save file as menu item.
	 */
	private JMenuItem _saveFileAsMenuItem;
	/**
	 * ACIDE - A Configurable IDE file menu save file menu item.
	 */
	private JMenuItem _saveFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file menu save all file menu item.
	 */
	private JMenuItem _saveAllFilesMenuItem;
	/**
	 * ACIDE - A Configurable IDE file menu print file menu item.
	 */
	private JMenuItem _printFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file menu exit menu item.
	 */
	private JMenuItem _exitMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE file menu.
	 */
	public AcideFileMenu() {

		// NEW FILE MENU ITEM
		_newFileMenuItem = new JMenuItem(NEW_FILE_IMAGE);

		// OPEN FILE MENU ITEM
		_openFileMenuItem = new JMenuItem(OPEN_FILE_IMAGE);

		// OPEN RECENT FILES MENU
		_openRecentFilesMenu = new AcideRecentFilesMenu();

		// OPEN ALL FILE MENU ITEM
		_openAllFilesMenuItem = new JMenuItem(OPEN_ALL_FILES_IMAGE);

		// CLOSE FILE MENU ITEM
		_closeFileMenuItem = new JMenuItem(CLOSE_FILE_IMAGE);

		// CLOSE ALL FILE MENU ITEM
		_closeAllFilesMenuItem = new JMenuItem(CLOSE_ALL_FILES_IMAGE);

		// SAVE FILE AS MENU ITEM
		_saveFileAsMenuItem = new JMenuItem(SAVE_FILE_AS_IMAGE);

		// SAVE ALL FILE MENU ITEM
		_saveAllFilesMenuItem = new JMenuItem(SAVE_ALL_FILES_IMAGE);

		// SAVE FILE MENU ITEM
		_saveFileMenuItem = new JMenuItem(SAVE_FILE_IMAGE);

		// PRINT FILE MENU ITEM
		_printFileMenuItem = new JMenuItem(PRINT_FILE_IMAGE);

		// EXIT MENU ITEM
		_exitMenuItem = new JMenuItem(EXIT_IMAGE);

		// Sets the text of the file menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE file menu components with
	 * the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
		} catch (Exception exception) {
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// NEW FILE MENU ITEM
		_newFileMenuItem.setText(labels.getString("s8"));
		_newFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.CTRL_MASK));

		// OPEN FILE MENU ITEM
		_openFileMenuItem.setText(labels.getString("s9"));
		if (language.getCurrentLocale().equals(new Locale("en", "EN")))
			_openFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_O, ActionEvent.CTRL_MASK));
		else
			_openFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_A, ActionEvent.CTRL_MASK));

		// OPEN RECENT FILES MENU
		_openRecentFilesMenu.setText(labels.getString("s1038"));

		// OPEN ALL FILES MENU ITEM
		_openAllFilesMenuItem.setText(labels.getString("s1004"));

		// CLOSE FILE MENU ITEM
		_closeFileMenuItem.setText(labels.getString("s238"));

		// CLOSE ALL FILES MENU ITEM
		_closeAllFilesMenuItem.setText(labels.getString("s239"));

		// SAVE FILE MENU ITEM
		_saveFileAsMenuItem.setText(labels.getString("s10"));

		// SAVE FILE AS MENU ITEM
		_saveFileMenuItem.setText(labels.getString("s617"));
		if (language.getCurrentLocale().equals(new Locale("en", "EN")))
			_saveFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_S, ActionEvent.CTRL_MASK));
		else
			_saveFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_G, ActionEvent.CTRL_MASK));

		// SAVE ALL FILES MENU ITEM
		_saveAllFilesMenuItem.setText(labels.getString("s217"));
		if (language.getCurrentLocale().equals(new Locale("en", "EN")))
			_saveAllFilesMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_S, ActionEvent.CTRL_MASK
							+ ActionEvent.SHIFT_MASK));
		else
			_saveAllFilesMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_G, ActionEvent.CTRL_MASK
							+ ActionEvent.SHIFT_MASK));

		// PRINT FILE MENU ITEM
		_printFileMenuItem.setText(labels.getString("s624"));
		_printFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
				ActionEvent.CTRL_MASK));

		// EXIT MENU ITEM
		_exitMenuItem.setText(labels.getString("s13"));
		_exitMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
				ActionEvent.ALT_MASK));

		// Disables the open all files menu item
		_openAllFilesMenuItem.setEnabled(false);

		// Disables the close file menu item
		_closeFileMenuItem.setEnabled(false);

		// Disables the close all files menu item
		_closeAllFilesMenuItem.setEnabled(false);

		// Disables the save file as menu item
		_saveFileAsMenuItem.setEnabled(false);

		// Disables the save file menu item
		_saveFileMenuItem.setEnabled(false);

		// Disables the save all files menu item
		_saveAllFilesMenuItem.setEnabled(false);

		// Disables the print file menu item
		_printFileMenuItem.setEnabled(false);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE file menu.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 * @param language
	 *            selected language.
	 */
	public void buildMenu(ResourceBundle labels, AcideLanguageManager language) {

		// Removes all the menu components
		removeAll();

		// NEW FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_FILE_NAME))
			add(_newFileMenuItem);

		// OPEN FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(OPEN_FILE_NAME))
			add(_openFileMenuItem);

		// OPEN RECENT FILES MENU
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				OPEN_RECENT_FILES_NAME))
			add(_openRecentFilesMenu);

		// OPEN ALL FILES MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				OPEN_ALL_FILES_NAME))
			add(_openAllFilesMenuItem);

		// CLOSE FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance()
				.getIsDisplayed(CLOSE_FILE_NAME))
			add(_closeFileMenuItem);

		// CLOSE ALL FILES MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				CLOSE_ALL_FILES_NAME))
			add(_closeAllFilesMenuItem);

		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						OPEN_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						CLOSE_FILE_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(CLOSE_ALL_FILES_NAME))
				&& (AcideMenuConfiguration.getInstance().getIsDisplayed(
						SAVE_FILE_NAME)
						|| AcideMenuConfiguration.getInstance().getIsDisplayed(
								SAVE_FILE_AS_NAME) || AcideMenuConfiguration
						.getInstance().getIsDisplayed(SAVE_ALL_FILES_NAME)))
			addSeparator();

		// SAVE FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_FILE_NAME))
			add(_saveFileMenuItem);

		// SAVE FILE AS MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_FILE_AS_NAME))
			add(_saveFileAsMenuItem);

		// SAVE ALL FILES MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_ALL_FILES_NAME))
			add(_saveAllFilesMenuItem);

		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						OPEN_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						CLOSE_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						CLOSE_ALL_FILES_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SAVE_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SAVE_FILE_AS_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(SAVE_ALL_FILES_NAME))
				&& (AcideMenuConfiguration.getInstance()
						.getIsDisplayed(PRINT_FILE_NAME)))
			addSeparator();

		// PRINT FILE MENU ITEM
		if (AcideMenuConfiguration.getInstance()
				.getIsDisplayed(PRINT_FILE_NAME))
			add(_printFileMenuItem);

		// SEPARATOR
		if ((AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SAVE_FILE_AS_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SAVE_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						SAVE_ALL_FILES_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						PRINT_FILE_NAME)
				|| AcideMenuConfiguration.getInstance().getIsDisplayed(
						CLOSE_FILE_NAME) || AcideMenuConfiguration
				.getInstance().getIsDisplayed(CLOSE_ALL_FILES_NAME))
				&& AcideMenuConfiguration.getInstance().getIsDisplayed(
						EXIT_NAME))
			addSeparator();

		// EXIT MENU ITEM
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(EXIT_NAME))
			add(_exitMenuItem);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE file menu item listeners.
	 */
	public void setListeners() {

		// NEW FILE MENU ITEM
		_newFileMenuItem.addActionListener(new AcideNewFileMenuItemListener());

		// OPEN FILE MENU ITEM
		_openFileMenuItem
				.addActionListener(new AcideOpenFileMenuItemListener());

		// OPEN ALL FILES MENU ITEM
		_openAllFilesMenuItem
				.addActionListener(new AcideOpenAllFilesMenuItemListener());

		// SAVE FILE AS MENU ITEM
		_saveFileAsMenuItem
				.addActionListener(new AcideSaveFileAsMenuItemListener());

		// SAVE FILE MENU ITEM
		_saveFileMenuItem
				.addActionListener(new AcideSaveFileMenuItemListener());

		// PRINT FILE MENU ITEM
		_printFileMenuItem
				.addActionListener(new AcidePrintFileMenuItemListener());

		// EXIT MENU ITEM
		_exitMenuItem.addActionListener(new AcideExitMenuItemListener());

		// CLOSE FILE MENU ITEM
		_closeFileMenuItem
				.addActionListener(new AcideCloseFileMenuItemListener());

		// CLOSE ALL FILES MENU ITEM
		_closeAllFilesMenuItem
				.addActionListener(new AcideCloseAllFilesMenuItemListener());

		// SAVE ALL FILES MENU ITEM
		_saveAllFilesMenuItem
				.addActionListener(new AcideSaveAllFilesMenuItemListener());
	}

	/**
	 * Saves the file the opened in the editor depending on the status of it. If
	 * it is red it will save it as. If it is green it will just save it.
	 * 
	 * @param index
	 *            tab index.
	 */
	public void saveFile(int index) {

		// If it is the NEW FILE
		if (AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanel()
				.getAbsolutePath()
				.equals(AcideLanguageManager.getInstance().getLabels()
						.getString("s79"))) {

			// Enables the save file as menu item
			_saveFileAsMenuItem.setEnabled(true);

			// Performs the save file as menu item action
			_saveFileAsMenuItem.doClick();
		} else {

			// Enables the save file menu item
			_saveFileMenuItem.setEnabled(true);

			// Performs the save file menu item action
			_saveFileMenuItem.doClick();
		}

		// Updates the file disk copy
		AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.getFileEditorPanelAt(index)
				.setFileDiskCopy(
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getTextEditionAreaContent());
	}

	/**
	 * Opens file in the file editor it is not already opened yet.
	 * 
	 * If so, puts the focus on its text edition area.
	 * 
	 * @param filePath
	 *            path of the file to be opened.
	 */
	public void openFile(String filePath) {

		// Checks if it is already opened in the file editor
		int fileEditorPanelIndex = -1;
		for (int index = 0; index < AcideMainWindow.getInstance()
				.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(index).getAbsolutePath()
					.equals(filePath)) {

				// Stores the tab index
				fileEditorPanelIndex = index;
			}
		}

		// If it is not opened
		if (fileEditorPanelIndex == -1) {

			// Loads the file content
			String fileContent = null;
			fileContent = AcideFileManager.getInstance().load(filePath);

			// If the file content is not empty
			if (fileContent != null) {

				// Gets the file project index
				int fileProjectIndex = AcideProjectConfiguration.getInstance()
						.getIndexOfFile(filePath);
				
				// TODO: Load the predefined extension

				// Creates the lexicon configuration
				AcideLexiconConfiguration lexiconConfiguration = new AcideLexiconConfiguration();

				// Loads the lexicon configuration
				lexiconConfiguration
						.load(AcideLexiconConfiguration.DEFAULT_PATH
								+ AcideLexiconConfiguration.DEFAULT_NAME);

				// TODO: Load the predefined extension

				// Creates the grammar configuration
				AcideGrammarConfiguration grammarConfiguration = new AcideGrammarConfiguration();

				// Sets the grammar configuration path
				grammarConfiguration
						.setPath(AcideGrammarConfiguration.DEFAULT_PATH);

				// It is a normal file
				AcideProjectFileType fileType = AcideProjectFileType.NORMAL;

				// If belongs to the project
				if (fileProjectIndex != -1)

					// Gets its type
					fileType = AcideProjectConfiguration.getInstance()
							.getFileAt(fileProjectIndex).getType();

				// Updates the tabbed pane in the file editor manager
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.updatesTabbedPane(filePath, fileContent, true,
								fileType, 0, 0, 1, lexiconConfiguration,
								grammarConfiguration);

				// If belongs to the project
				if (fileProjectIndex != -1)
					// Sets the new file state to opened in the project
					// configuration
					AcideProjectConfiguration.getInstance()
							.getFileAt(fileProjectIndex).setIsOpened(true);

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject())

					// The project has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s84")
								+ filePath);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s85")
								+ filePath
								+ AcideLanguageManager.getInstance()
										.getLabels().getString("s86"));

			} else {

				// EMPTY FILE

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s88"));
			}

			// Adds the file to the recent files list
			AcideWorkbenchManager.getInstance().addRecentFileToList(filePath);

		} else {

			// IT IS ALREADY OPENED

			// Sets the selected file editor panel at it
			AcideMainWindow.getInstance().getFileEditorManager()
					.updatesFileEditorAt(fileEditorPanelIndex);
		}
	}

	/**
	 * Enables the ACIDE - A Configurable IDE file menu.
	 */
	public void enableMenu() {

		_closeFileMenuItem.setEnabled(true);
		_closeAllFilesMenuItem.setEnabled(true);
		_saveFileAsMenuItem.setEnabled(true);
		_saveFileMenuItem.setEnabled(true);
		_saveAllFilesMenuItem.setEnabled(true);
		_printFileMenuItem.setEnabled(true);
	}

	/**
	 * Disables the ACIDE - A Configurable IDE file menu.
	 */
	public void disableMenu() {

		_closeFileMenuItem.setEnabled(false);
		_closeAllFilesMenuItem.setEnabled(false);
		_saveFileAsMenuItem.setEnabled(false);
		_saveFileMenuItem.setEnabled(false);
		_saveAllFilesMenuItem.setEnabled(false);
		_printFileMenuItem.setEnabled(false);
	}

	/**
	 * Enables the ACIDE - A Configurable IDE file menu save file as menu item.
	 */
	public void enableSaveFileAs() {

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

		_saveFileAsMenuItem.setEnabled(true);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s75"));
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu new file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu new file menu item.
	 */
	public JMenuItem getNewFileMenuItem() {
		return _newFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu exit menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu exit menu item.
	 */
	public JMenuItem getExitMenuItem() {
		return _exitMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu save file as menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu save file as menu item.
	 */
	public JMenuItem getSaveFileAsMenuItem() {
		return _saveFileAsMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu save file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu save file menu item.
	 */
	public JMenuItem getSaveFileMenuItem() {
		return _saveFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu save all files menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu save all files menu
	 *         item.
	 */
	public JMenuItem getSaveAllFilesMenuItem() {
		return _saveAllFilesMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu print file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu print file menu item.
	 */
	public JMenuItem getPrintFileMenuItem() {
		return _printFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu close all files menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu close all files menu
	 *         item.
	 */
	public JMenuItem getCloseAllFilesMenuItem() {
		return _closeAllFilesMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu close file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu close file menu item.
	 */
	public JMenuItem getCloseFileMenuItem() {
		return _closeFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu open file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu open file menu item.
	 */
	public JMenuItem getOpenFileMenuItem() {
		return _openFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu open recent files menu.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu open recent files menu.
	 */
	public AcideRecentFilesMenu getOpenRecentFilesMenu() {
		return _openRecentFilesMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu open all files menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu open all files menu
	 *         item.
	 */
	public JMenuItem getOpenAllFilesMenuItem() {
		return _openAllFilesMenuItem;
	}
}
