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
package acide.gui.menuBar.fileMenu;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.Locale;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.workbench.AcideWorkbenchManager;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFileType;
import acide.gui.fileEditor.fileEditorManager.utils.logic.closeButton.AcideFileEditorCloseButton;
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
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

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

		// Creates the new file menu item
		_newFileMenuItem = new JMenuItem(NEW_FILE_IMAGE);

		// Creates the open file menu item
		_openFileMenuItem = new JMenuItem(OPEN_FILE_IMAGE);

		// Creates the open recent files menu item
		_openRecentFilesMenu = new AcideRecentFilesMenu();

		// Creates the open all files menu item
		_openAllFilesMenuItem = new JMenuItem(OPEN_ALL_FILES_IMAGE);

		// Creates the close file menu item
		_closeFileMenuItem = new JMenuItem(CLOSE_FILE_IMAGE);

		// Creates the all files menu item
		_closeAllFilesMenuItem = new JMenuItem(CLOSE_ALL_FILES_IMAGE);

		// Creates the save file as menu item
		_saveFileAsMenuItem = new JMenuItem(SAVE_FILE_AS_IMAGE);

		// Creates the save all files menu item
		_saveAllFilesMenuItem = new JMenuItem(SAVE_ALL_FILES_IMAGE);

		// Creates the save file menu item
		_saveFileMenuItem = new JMenuItem(SAVE_FILE_IMAGE);

		// Creates the print file menu item
		_printFileMenuItem = new JMenuItem(PRINT_FILE_IMAGE);

		// Creates the exit menu item
		_exitMenuItem = new JMenuItem(EXIT_IMAGE);

		// Sets the text of the file menu components
		setTextOfMenuComponents();
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE file menu components with
	 * the labels in the selected language to display.
	 */
	public void setTextOfMenuComponents() {

		// Sets the new file menu item text
		_newFileMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s8"));

		// Sets the new file menu item accelerator
		_newFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.CTRL_MASK));

		// Sets the open file menu item text
		_openFileMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s9"));

		// Sets the open file menu item accelerator
		if (AcideLanguageManager.getInstance().getCurrentLocale()
				.equals(new Locale("en", "EN")))
			_openFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_O, ActionEvent.CTRL_MASK));
		else
			_openFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_A, ActionEvent.CTRL_MASK));

		// Sets the open recent files menu item text
		_openRecentFilesMenu.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s1038"));

		// Sets the open all files menu item text
		_openAllFilesMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s1004"));

		// Sets the close file menu item text
		_closeFileMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s238"));

		// Sets the close all files menu item text
		_closeAllFilesMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s239"));

		// Sets the save file as menu item text
		_saveFileAsMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s10"));

		// Sets the save file menu item text
		_saveFileMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s617"));

		// Sets the save file menu item accelerator
		if (AcideLanguageManager.getInstance().getCurrentLocale()
				.equals(new Locale("en", "EN")))
			_saveFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_S, ActionEvent.CTRL_MASK));
		else
			_saveFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_G, ActionEvent.CTRL_MASK));

		// Sets the save all files menu item text
		_saveAllFilesMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s217"));

		// Sets the save all files menu item accelerator
		if (AcideLanguageManager.getInstance().getCurrentLocale()
				.equals(new Locale("en", "EN")))
			_saveAllFilesMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_S, ActionEvent.CTRL_MASK
							+ ActionEvent.SHIFT_MASK));
		else
			_saveAllFilesMenuItem.setAccelerator(KeyStroke.getKeyStroke(
					KeyEvent.VK_G, ActionEvent.CTRL_MASK
							+ ActionEvent.SHIFT_MASK));

		// Sets the print file menu item text
		_printFileMenuItem.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s624"));

		// Sets the print file menu item accelerator
		_printFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
				ActionEvent.CTRL_MASK));

		// Sets the exit menu item text
		_exitMenuItem.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s13"));

		// Sets the exit file menu item accelerator
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
	 */
	public void build() {

		// Removes all the menu components
		removeAll();

		// Adds the new file menu item to the file menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_FILE_NAME))
			add(_newFileMenuItem);

		// Adds the open file menu item to the file menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(OPEN_FILE_NAME))
			add(_openFileMenuItem);

		// Adds the open recent files menu item to the file menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				OPEN_RECENT_FILES_NAME))
			add(_openRecentFilesMenu);

		// Adds the open all files menu item to the file menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				OPEN_ALL_FILES_NAME))
			add(_openAllFilesMenuItem);

		// Adds the close file menu item to the file menu
		if (AcideMenuConfiguration.getInstance()
				.getIsDisplayed(CLOSE_FILE_NAME))
			add(_closeFileMenuItem);

		// Adds the close all files menu item to the file menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				CLOSE_ALL_FILES_NAME))
			add(_closeAllFilesMenuItem);

		// Adds a separator to the file menu
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

		// Adds the save file menu item to the file menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_FILE_NAME))
			add(_saveFileMenuItem);

		// Adds the save file as menu item to the file menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_FILE_AS_NAME))
			add(_saveFileAsMenuItem);

		// Adds the save all files menu item to the file menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_ALL_FILES_NAME))
			add(_saveAllFilesMenuItem);

		// Adds a separator to the file menu
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

		// Adds the print file menu item to the file menu
		if (AcideMenuConfiguration.getInstance()
				.getIsDisplayed(PRINT_FILE_NAME))
			add(_printFileMenuItem);

		// Adds a separator to the file menu
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

		// Adds the exit menu item to the file menu
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(EXIT_NAME))
			add(_exitMenuItem);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE file menu item listeners.
	 */
	public void setListeners() {

		// Sets the new file menu item action listener
		_newFileMenuItem.addActionListener(new AcideNewFileMenuItemListener());

		// Sets the open file menu item action listener
		_openFileMenuItem
				.addActionListener(new AcideOpenFileMenuItemListener());

		// Sets the open all files menu item action listener
		_openAllFilesMenuItem
				.addActionListener(new AcideOpenAllFilesMenuItemListener());

		// Sets the save file as menu item action listener
		_saveFileAsMenuItem
				.addActionListener(new AcideSaveFileAsMenuItemListener());

		// Sets the save file menu item action listener
		_saveFileMenuItem
				.addActionListener(new AcideSaveFileMenuItemListener());

		// Sets the print file menu item action listener
		_printFileMenuItem
				.addActionListener(new AcidePrintFileMenuItemListener());

		// Sets the exit menu item action listener
		_exitMenuItem.addActionListener(new AcideExitMenuItemListener());

		// Sets the close file menu item action listener
		_closeFileMenuItem
				.addActionListener(new AcideCloseFileMenuItemListener());

		// Sets the close all files menu item action listener
		_closeAllFilesMenuItem
				.addActionListener(new AcideCloseAllFilesMenuItemListener());

		// Sets the save all files menu item action listener
		_saveAllFilesMenuItem
				.addActionListener(new AcideSaveAllFilesMenuItemListener());
	}

	/**
	 * Saves the file the opened in the editor depending on the status of it. If
	 * it is red it will save it as. If it is green it will just save it.
	 * 
	 * @param index
	 *            tab index
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
	 * If so, puts the focus on its text edition area
	 * 
	 * @param filePath
	 *            path of the file to be opened
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

				// Creates the current grammar configuration
				AcideGrammarConfiguration currentGrammarConfiguration = new AcideGrammarConfiguration();

				// Sets the current grammar configuration path
				currentGrammarConfiguration
						.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

				// Creates the previous grammar configuration
				AcideGrammarConfiguration previousGrammarConfiguration = new AcideGrammarConfiguration();

				// Sets the previous grammar configuration path
				previousGrammarConfiguration
						.setPath(AcideGrammarConfiguration.DEFAULT_FILE);

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
								currentGrammarConfiguration,
								previousGrammarConfiguration);

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

			// The file is already opened

			// Updates the selected file editor index
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.updateRelatedComponentsAt(fileEditorPanelIndex);
		}
	}

	/**
	 * Closes a tab in the file editor manager specified by a parameter.
	 * 
	 * @param fileEditorIndex
	 *            file editor index to close.
	 */
	public void closeFile(int fileEditorIndex) {

		boolean isCancelOption = false;

		// Is the file modified?
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.isRedButton(fileEditorIndex)) {

			// Asks the user if he wants to save it
			int returnValue = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s643"), AcideLanguageManager
							.getInstance().getLabels().getString("s994"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If it is ok
			if (returnValue == JOptionPane.OK_OPTION) {

				// Saves the file
				AcideMainWindow.getInstance().getMenu().getFileMenu()
						.saveFile(fileEditorIndex);

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// Sets opened to false to the project file
					for (int index = 0; index < AcideProjectConfiguration
							.getInstance().getFileListSize(); index++) {

						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index)
								.getAbsolutePath()
								.equals(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(fileEditorIndex)
										.getAbsolutePath())) {

							// Sets the file as not opened in the project
							// configuration
							AcideProjectConfiguration.getInstance()
									.getFileAt(index).setIsOpened(false);
						}
					}

					// Sets the project to modified
					AcideProjectConfiguration.getInstance().setIsModified(true);
				}

			} else if (returnValue == JOptionPane.NO_OPTION) {

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

					for (int index = 0; index < AcideProjectConfiguration
							.getInstance().getFileListSize(); index++) {

						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index)
								.getAbsolutePath()
								.equals(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(fileEditorIndex)
										.getAbsolutePath())) {

							// Sets the file as not opened at the project
							// configuration
							AcideProjectConfiguration.getInstance()
									.getFileAt(index).setIsOpened(false);
						}
					}

					// Sets the project to modified
					AcideProjectConfiguration.getInstance().setIsModified(true);
				}
			} else
				isCancelOption = true;
		} else {

			// Is not modified

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getFileListSize(); index++) {

					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index)
							.getAbsolutePath()
							.equals(AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(fileEditorIndex)
									.getAbsolutePath())) {

						// Sets the file as not opened in the project
						// configuration
						AcideProjectConfiguration.getInstance()
								.getFileAt(index).setIsOpened(false);
					}
				}

				// Sets the project to modified
				AcideProjectConfiguration.getInstance().setIsModified(true);
			}
		}

		// If it is not cancel option
		if (!isCancelOption) {

			// Removes the tab from the file editor
			AcideMainWindow.getInstance().getFileEditorManager()
					.getTabbedPane().remove(fileEditorIndex);

			// Exchanges the closing buttons
			for (int index = fileEditorIndex; index < AcideMainWindow
					.getInstance().getFileEditorManager().getTabbedPaneUI()
					.getCloseButtons().size() - 1; index++) {

				// Gets the current close button
				AcideFileEditorCloseButton currentCloseButton = (AcideFileEditorCloseButton) AcideMainWindow
						.getInstance().getFileEditorManager().getTabbedPaneUI()
						.getCloseButtons().get(index);

				// Gets the next close button
				AcideFileEditorCloseButton nextCloseButton = (AcideFileEditorCloseButton) AcideMainWindow
						.getInstance().getFileEditorManager().getTabbedPaneUI()
						.getCloseButtons().get(index + 1);

				// If it is red button
				if (nextCloseButton.isRedButton())

					// Sets the red button
					currentCloseButton.setRedCloseButton();
				else
					// Sets the green button
					currentCloseButton.setGreenCloseButton();

				// Sets the position
				AcideMainWindow.getInstance().getFileEditorManager()
						.getTabbedPaneUI().getCloseButtons()
						.set(index, currentCloseButton);
			}

			// Validates the changes in the tabbed pane
			AcideMainWindow.getInstance().getFileEditorManager()
					.getTabbedPane().validate();
		}

		// If there are opened file editor panels
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// Updates the selected file editor index
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.updateRelatedComponentsAt(
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanelIndex());		
		}
	}

	/**
	 * Enables the ACIDE - A Configurable IDE file menu.
	 */
	public void enableMenu() {

		_closeFileMenuItem.setEnabled(true);
		_closeAllFilesMenuItem.setEnabled(true);
		_saveFileAsMenuItem.setEnabled(true);
		_saveFileMenuItem.setEnabled(false);
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

		// Enables the save file as menu item
		_saveFileAsMenuItem.setEnabled(true);

		// Updates the log
		AcideLog.getLog()
				.info(AcideLanguageManager.getInstance().getLabels()
						.getString("s75"));
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu new file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu new file menu item
	 */
	public JMenuItem getNewFileMenuItem() {
		return _newFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu exit menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu exit menu item
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
	 * @return the ACIDE - A Configurable IDE file menu save file menu item
	 */
	public JMenuItem getSaveFileMenuItem() {
		return _saveFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu save all files menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu save all files menu item
	 */
	public JMenuItem getSaveAllFilesMenuItem() {
		return _saveAllFilesMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu print file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu print file menu item
	 */
	public JMenuItem getPrintFileMenuItem() {
		return _printFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu close all files menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu close all files menu
	 *         item
	 */
	public JMenuItem getCloseAllFilesMenuItem() {
		return _closeAllFilesMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu close file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu close file menu item
	 */
	public JMenuItem getCloseFileMenuItem() {
		return _closeFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu open file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu open file menu item
	 */
	public JMenuItem getOpenFileMenuItem() {
		return _openFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu open recent files menu.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu open recent files menu
	 */
	public AcideRecentFilesMenu getOpenRecentFilesMenu() {
		return _openRecentFilesMenu;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu open all files menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu open all files menu item
	 */
	public JMenuItem getOpenAllFilesMenuItem() {
		return _openAllFilesMenuItem;
	}
}