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
package gui.menuBar.fileMenu;

import es.configuration.menu.AcideMenuConfiguration;
import es.text.TextFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.fileMenu.listeners.CloseAllFilesMenuItemListener;
import gui.menuBar.fileMenu.listeners.CloseFileMenuItemListener;
import gui.menuBar.fileMenu.listeners.ExitMenuItemListener;
import gui.menuBar.fileMenu.listeners.NewFileMenuItemListener;
import gui.menuBar.fileMenu.listeners.OpenAllFilesMenuItemListener;
import gui.menuBar.fileMenu.listeners.OpenFileMenuItemListener;
import gui.menuBar.fileMenu.listeners.PrintFileMenuItemListener;
import gui.menuBar.fileMenu.listeners.SaveAllFilesMenuItemListener;
import gui.menuBar.fileMenu.listeners.SaveFileAsMenuItemListener;
import gui.menuBar.fileMenu.listeners.SaveFileMenuItemListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import language.AcideLanguageManager;

import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE file menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class FileMenu extends JMenu {

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
	 * ACIDE - A Configurable IDE file menu new file menu item.
	 */
	private JMenuItem _newFile;
	/**
	 * ACIDE - A Configurable IDE file menu open file menu item.
	 */
	private JMenuItem _openFile;
	/**
	 * ACIDE - A Configurable IDE file menu open all files menu item.
	 */
	private JMenuItem _openAllFiles;
	/**
	 * ACIDE - A Configurable IDE file menu close file menu item.
	 */
	private JMenuItem _closeFile;
	/**
	 * ACIDE - A Configurable IDE file menu close all files menu item.
	 */
	private JMenuItem _closeAllFiles;
	/**
	 * ACIDE - A Configurable IDE file menu save file as menu item.
	 */
	private JMenuItem _saveFileAs;
	/**
	 * ACIDE - A Configurable IDE file menu save file menu item.
	 */
	private JMenuItem _saveFile;
	/**
	 * ACIDE - A Configurable IDE file menu save all file menu item.
	 */
	private JMenuItem _saveAllFiles;
	/**
	 * ACIDE - A Configurable IDE file menu print file menu item.
	 */
	private JMenuItem _printFile;
	/**
	 * ACIDE - A Configurable IDE file menu exit menu item.
	 */
	private JMenuItem _exit;

	/**
	 * Creates a new ACIDE - A Configurable IDE file menu.
	 */
	public FileMenu() {

		// MENU ITEM
		_newFile = new JMenuItem(NEW_FILE_IMAGE);
		_openFile = new JMenuItem(OPEN_FILE_IMAGE);
		_openAllFiles = new JMenuItem();
		_closeFile = new JMenuItem(CLOSE_FILE_IMAGE);
		_closeAllFiles = new JMenuItem(CLOSE_ALL_FILES_IMAGE);
		_saveFileAs = new JMenuItem(SAVE_FILE_AS_IMAGE);
		_saveAllFiles = new JMenuItem(SAVE_ALL_FILES_IMAGE);
		_saveFile = new JMenuItem(SAVE_FILE_IMAGE);
		_printFile = new JMenuItem(PRINT_FILE_IMAGE);
		_exit = new JMenuItem(EXIT_IMAGE);

		setLanguageLabels();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE file menu language labels.
	 */
	public void setLanguageLabels() {

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

		// NEW FILE
		_newFile.setText(labels.getString("s8"));
		_newFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.CTRL_MASK));

		// OPEN FILE
		_openFile.setText(labels.getString("s9"));
		if (language.getCurrentLocale().equals(new Locale("en", "EN")))
			_openFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
					ActionEvent.CTRL_MASK));
		else
			_openFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,
					ActionEvent.CTRL_MASK));

		// OPEN ALL FILES
		_openAllFiles.setText(labels.getString("s1004"));

		// CLOSE FILE
		_closeFile.setText(labels.getString("s238"));

		// CLOSE ALL FILES
		_closeAllFiles.setText(labels.getString("s239"));

		// SAVE FILE
		_saveFileAs.setText(labels.getString("s10"));

		// SAVE AS
		_saveFile.setText(labels.getString("s617"));
		if (language.getCurrentLocale().equals(new Locale("en", "EN")))
			_saveFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
					ActionEvent.CTRL_MASK));
		else
			_saveFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
					ActionEvent.CTRL_MASK));

		// SAVE ALL FILES
		_saveAllFiles.setText(labels.getString("s217"));
		if (language.getCurrentLocale().equals(new Locale("en", "EN")))
			_saveAllFiles.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
					ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		else
			_saveAllFiles.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
					ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// PRINT FILE
		_printFile.setText(labels.getString("s624"));
		_printFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
				ActionEvent.CTRL_MASK));

		// EXIT
		_exit.setText(labels.getString("s13"));
		_exit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
				ActionEvent.ALT_MASK));

		_openAllFiles.setEnabled(false);
		_closeFile.setEnabled(false);
		_closeAllFiles.setEnabled(false);
		_saveFileAs.setEnabled(false);
		_saveFile.setEnabled(false);
		_saveAllFiles.setEnabled(false);
		_printFile.setEnabled(false);
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

		removeAll();

		if (AcideMenuConfiguration.getInstance().getIsDisplayed(NEW_FILE_NAME))
			add(_newFile);
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(OPEN_FILE_NAME))
			add(_openFile);
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				OPEN_ALL_FILES_NAME))
			add(_openAllFiles);
		if (AcideMenuConfiguration.getInstance()
				.getIsDisplayed(CLOSE_FILE_NAME))
			add(_closeFile);
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				CLOSE_ALL_FILES_NAME))
			add(_closeAllFiles);
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
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(SAVE_FILE_NAME))
			add(_saveFile);
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_FILE_AS_NAME))
			add(_saveFileAs);
		if (AcideMenuConfiguration.getInstance().getIsDisplayed(
				SAVE_ALL_FILES_NAME))
			add(_saveAllFiles);
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

		if (AcideMenuConfiguration.getInstance()
				.getIsDisplayed(PRINT_FILE_NAME))
			add(_printFile);

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

		if (AcideMenuConfiguration.getInstance().getIsDisplayed(EXIT_NAME))
			add(_exit);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE file menu item listeners.
	 */
	public void setListeners() {

		// NEW FILE
		_newFile.addActionListener(new NewFileMenuItemListener());

		// OPEN FILE
		_openFile.addActionListener(new OpenFileMenuItemListener());

		// OPEN ALL FILES
		_openAllFiles.addActionListener(new OpenAllFilesMenuItemListener());

		// SAVE FILE AS
		_saveFileAs.addActionListener(new SaveFileAsMenuItemListener());

		// SAVE FILE
		_saveFile.addActionListener(new SaveFileMenuItemListener());

		// PRINT FILE
		_printFile.addActionListener(new PrintFileMenuItemListener());

		// EXIT
		_exit.addActionListener(new ExitMenuItemListener());

		// CLOSE FILE
		_closeFile.addActionListener(new CloseFileMenuItemListener());

		// CLOSE ALL FILES
		_closeAllFiles.addActionListener(new CloseAllFilesMenuItemListener());

		// SAVE ALL FILES
		_saveAllFiles.addActionListener(new SaveAllFilesMenuItemListener());
	}

	/**
	 * Saves the file the opened in the editor depending on the status of it. If
	 * it is red it will save it as. If it is green it will just save it.
	 * 
	 * @return the selected editor index of the operation.
	 */
	public int saveOrSaveAS() {

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

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// If the file has been modified
		if (MainWindow.getInstance().getFileEditorManager().isRedButton()) {

			// If it is the NEW FILE
			if (MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getAbsolutePath()
					.equals(labels.getString("s79"))) {

				// Creates the file in the disk
				TextFile textFile = AcideIOFactory.getInstance().buildFile();
				String filePath = " ";
				filePath = textFile.write();

				// If it is not empty
				if (!filePath.equals(" ")) {

					// Saves the file
					boolean saveResult = textFile.save(filePath, MainWindow
							.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getTextEditionAreaContent());

					// If it could save it
					if (saveResult) {

						// Updates the log
						AcideLog.getLog().info(
								labels.getString("s93") + filePath
										+ labels.getString("s94"));

						// Sets the green button
						MainWindow.getInstance().getFileEditorManager()
								.setGreenButton();

						// Sets the new file path
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(
										selectedFileEditorPanelIndex)
								.setAbsolutePath(filePath);

						// Sets the tool tip text
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(
										selectedFileEditorPanelIndex)
								.setToolTipText(filePath);

						// Gets the file name
						int index = filePath.lastIndexOf("\\");
						if (index == -1)
							index = filePath.lastIndexOf("/");
						String name = filePath.substring(index + 1,
								filePath.length());

						// Sets the name
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(
										selectedFileEditorPanelIndex)
								.setName(name);

						File file = new File(MainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath());

						// Sets the last change
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastChange(file.lastModified());

						// Sets the size
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.setLastSize(file.length());

					} else

						// Updates the log
						AcideLog.getLog().info(
								labels.getString("s95") + filePath);

				} else

					// Updates the log
					AcideLog.getLog().info(labels.getString("s92"));
			} else {

				// Enables the SAVE FILE menu
				_saveFile.setEnabled(true);

				// Saves the file
				_saveFile.doClick();
			}

			// Updates the file disk copy
			MainWindow
					.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(selectedFileEditorPanelIndex)
					.setFileDiskCopy(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getTextEditionAreaContent());
		}

		// Returns the selected file editor panel index
		return selectedFileEditorPanelIndex;
	}

	/**
	 * Enables the ACIDE - A Configurable IDE file menu.
	 */
	public void enableMenu() {

		_closeFile.setEnabled(true);
		_closeAllFiles.setEnabled(true);
		_saveFileAs.setEnabled(true);
		_saveFile.setEnabled(false);
		_saveAllFiles.setEnabled(false);
		_printFile.setEnabled(true);
	}

	/**
	 * Disables the ACIDE - A Configurable IDE file menu.
	 */
	public void disableMenu() {

		_closeFile.setEnabled(false);
		_closeAllFiles.setEnabled(false);
		_saveFileAs.setEnabled(false);
		_saveFile.setEnabled(false);
		_saveAllFiles.setEnabled(false);
		_printFile.setEnabled(false);
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

		_saveFileAs.setEnabled(true);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s75"));
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu new file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu new file menu item.
	 */
	public JMenuItem getNewFile() {
		return _newFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file menu new file
	 * menu item.
	 * 
	 * @param newFile
	 *            new value to set.
	 */
	public void setNewFile(JMenuItem newFile) {
		_newFile = newFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu exit menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu exit menu item.
	 */
	public JMenuItem getExit() {
		return _exit;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file menu exit menu
	 * item.
	 * 
	 * @param exit
	 *            new value to set.
	 */
	public void setExit(JMenuItem exit) {
		_exit = exit;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu save file as menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu save file as menu item.
	 */
	public JMenuItem getSaveFileAs() {
		return _saveFileAs;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file menu save file as
	 * menu item.
	 * 
	 * @param saveFileAs
	 *            new value to set.
	 */
	public void setSaveFileAs(JMenuItem saveFileAs) {
		_saveFileAs = saveFileAs;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu save file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu save file menu item.
	 */
	public JMenuItem getSaveFile() {
		return _saveFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file menu save file
	 * menu item.
	 * 
	 * @param saveFile
	 *            new value to set.
	 */
	public void setSaveFile(JMenuItem saveFile) {
		_saveFile = saveFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu save all files menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu save all files menu
	 *         item.
	 */
	public JMenuItem getSaveAllFiles() {
		return _saveAllFiles;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file menu save all
	 * files menu item.
	 * 
	 * @param saveAllFiles
	 *            new value to set.
	 */
	public void setSaveAllFiles(JMenuItem saveAllFiles) {
		_saveAllFiles = saveAllFiles;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu print file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu print file menu item.
	 */
	public JMenuItem getPrintFile() {
		return _printFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file menu print file
	 * menu item.
	 * 
	 * @param printFile
	 *            new value to set.
	 */
	public void setPrintFile(JMenuItem printFile) {
		_printFile = printFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu close all files menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu close all files menu
	 *         item.
	 */
	public JMenuItem getCloseAllFiles() {
		return _closeAllFiles;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file menu close all
	 * files menu item.
	 * 
	 * @param closeAllFiles
	 *            new value to set.
	 */
	public void setCloseAllFiles(JMenuItem closeAllFiles) {
		_closeAllFiles = closeAllFiles;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu close file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu close file menu item.
	 */
	public JMenuItem getCloseFile() {
		return _closeFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file menu close file
	 * menu item.
	 * 
	 * @param closeFile
	 *            new value to set.
	 */
	public void setCloseFile(JMenuItem closeFile) {
		_closeFile = closeFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu open file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu open file menu item.
	 */
	public JMenuItem getOpenFile() {
		return _openFile;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file menu open file
	 * menu item.
	 * 
	 * @param openFile
	 *            new value to set.
	 */
	public void setOpenFile(JMenuItem openFile) {
		_openFile = openFile;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file menu open all files menu
	 * item.
	 * 
	 * @return the ACIDE - A Configurable IDE file menu open all files menu
	 *         item.
	 */
	public JMenuItem getOpenAllFiles() {
		return _openAllFiles;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file menu open all
	 * files menu item.
	 * 
	 * @param openAllFiles
	 *            new value to set.
	 */
	public void setOpenAllFiles(JMenuItem openAllFiles) {
		_openAllFiles = openAllFiles;
	}
}
