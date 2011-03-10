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
package acide.gui.menuBar.configurationMenu.menuMenu.gui;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.border.TitledBorder;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;
import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.menu.AcideMenuItemInformation;
import acide.configuration.project.AcideProjectConfiguration;
import acide.files.text.AcideTextFileExtensionFilterManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.AcideConfigurationMenu;
import acide.gui.menuBar.configurationMenu.consoleMenu.AcideConsoleMenu;
import acide.gui.menuBar.configurationMenu.fileEditor.AcideFileEditorMenu;
import acide.gui.menuBar.configurationMenu.grammarMenu.AcideGrammarMenu;
import acide.gui.menuBar.configurationMenu.languageMenu.AcideLanguageMenu;
import acide.gui.menuBar.configurationMenu.lexiconMenu.AcideLexiconMenu;
import acide.gui.menuBar.configurationMenu.menuMenu.AcideMenuMenu;
import acide.gui.menuBar.configurationMenu.toolBarMenu.AcideToolBarMenu;
import acide.gui.menuBar.editMenu.AcideEditMenu;
import acide.gui.menuBar.fileMenu.AcideFileMenu;
import acide.gui.menuBar.helpMenu.AcideHelpMenu;
import acide.gui.menuBar.projectMenu.AcideProjectMenu;
import acide.gui.menuBar.viewMenu.AcideViewMenu;

/**
 * ACIDE - A Configurable IDE menu configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideMenuConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE menu configuration window class serial version
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE menu configuration window image icon.
	 */
	private static final String WINDOW_ICON = "./resources/images/icon.png";
	/**
	 * ACIDE - A Configurable IDE menu configuration window main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window file panel.
	 */
	private JPanel _filePanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window edit panel.
	 */
	private JPanel _editPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window project panel.
	 */
	private JPanel _projectPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window view panel.
	 */
	private JPanel _viewPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window configuration panel.
	 */
	private JPanel _configurationPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window help panel.
	 */
	private JPanel _helpPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save button.
	 */
	private JButton _saveButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration window select all button.
	 */
	private JButton _selectAllButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration window select none button.
	 */
	private JButton _selectNoneButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration window new file check box.
	 */
	private final JCheckBox _newFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window open file check box.
	 */
	private final JCheckBox _openFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window open recent files
	 * check box.
	 */
	private final JCheckBox _openRecentFilesCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window open all files check
	 * box.
	 */
	private final JCheckBox _openAllFilesCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save file as check
	 * box.
	 */
	private final JCheckBox _saveFileAsCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save file check box.
	 */
	private final JCheckBox _saveFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save all files check
	 * box.
	 */
	private final JCheckBox _saveAllFilesCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window close file check
	 * box.
	 */
	private final JCheckBox _closeFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window close all files
	 * check box.
	 */
	private final JCheckBox _closeAllFilesCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window print file check
	 * box.
	 */
	private final JCheckBox _printFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window exit check box.
	 */
	private final JCheckBox _exitCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window undo check box.
	 */
	private final JCheckBox _undoCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window redo check box.
	 */
	private final JCheckBox _redoCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window copy check box.
	 */
	private final JCheckBox _copyCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window paste check box.
	 */
	private final JCheckBox _pasteCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window cut check box.
	 */
	private final JCheckBox _cutCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window select all files
	 * check box.
	 */
	private final JCheckBox _selectAllFilesCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window go to line check
	 * box.
	 */
	private final JCheckBox _goToLineCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window search check box.
	 */
	private final JCheckBox _searchCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window replace check box.
	 */
	private final JCheckBox _replaceCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window new project check
	 * box.
	 */
	private final JCheckBox _newProjectCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window open project check
	 * box.
	 */
	private final JCheckBox _openProjectCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save project check
	 * box.
	 */
	private final JCheckBox _saveProjectCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window close project check
	 * box.
	 */
	private final JCheckBox _closeProjectCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window new project file
	 * check box.
	 */
	private final JCheckBox _newProjectFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window add file check box.
	 */
	private final JCheckBox _addFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window remove file check
	 * box.
	 */
	private final JCheckBox _removeFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window delete file check
	 * box.
	 */
	private final JCheckBox _deleteFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window add folder check
	 * box.
	 */
	private final JCheckBox _addFolderCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window remove folder check
	 * box.
	 */
	private final JCheckBox _removeFolderCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window compile check box.
	 */
	private final JCheckBox _compileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window execute check box.
	 */
	private final JCheckBox _executeCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window set compilable file
	 * check box.
	 */
	private final JCheckBox _setCompilableFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window unset compilable
	 * check box.
	 */
	private final JCheckBox _unsetCompilableFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window set main file check
	 * box.
	 */
	private final JCheckBox _setMainFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window unset main file
	 * check box.
	 */
	private final JCheckBox _unsetMainFileCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save project as
	 * check box.
	 */
	private final JCheckBox _saveProjectAsCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window show log tab check
	 * box.
	 */
	private final JCheckBox _showLogTabCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window show explorer panel
	 * check box.
	 */
	private final JCheckBox _showExplorerPanelCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window show output panel
	 * check box.
	 */
	private final JCheckBox _showOutputPanelCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window new lexicon check
	 * box.
	 */
	private final JCheckBox _newLexiconCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window load lexicon check
	 * box.
	 */
	private final JCheckBox _loadLexiconCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window modify lexicon check
	 * box.
	 */
	private final JCheckBox _modifyLexiconCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save lexicon check
	 * box.
	 */
	private final JCheckBox _saveLexiconCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save lexicon as
	 * check box.
	 */
	private final JCheckBox _saveLexiconAsCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window new grammar check
	 * box.
	 */
	private final JCheckBox _newGrammarCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window load grammar check
	 * box.
	 */
	private final JCheckBox _loadGrammarCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window modify grammar check
	 * box.
	 */
	private final JCheckBox _modifyGrammarCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save grammar check
	 * box.
	 */
	private final JCheckBox _saveGrammarCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save grammar as
	 * check box.
	 */
	private final JCheckBox _saveGrammarAsCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window set paths check box.
	 */
	private final JCheckBox _setPathsCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window auto analysis check
	 * box.
	 */
	private final JCheckBox _autoAnalysisCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window configure check box.
	 */
	private final JCheckBox _configureCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window external command
	 * check box.
	 */
	private final JCheckBox _externalCommandCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window Spanish check box.
	 */
	private final JCheckBox _spanishCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window English check box.
	 */
	private final JCheckBox _englishCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window new menu check box.
	 */
	private final JCheckBox _newMenuCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window load menu check box.
	 */
	private final JCheckBox _loadMenuCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window modify menu check
	 * box.
	 */
	private final JCheckBox _modifyMenu;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save menu check box.
	 */
	private final JCheckBox _saveMenuCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save menu as check
	 * box.
	 */
	private final JCheckBox _saveMenuAsCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window new tool bar check
	 * box.
	 */
	private final JCheckBox _newToolBarCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window load tool bar check
	 * box.
	 */
	private final JCheckBox _loadToolBarCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window modify tool bar
	 * check box.
	 */
	private final JCheckBox _modifyToolBarCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save tool bar check
	 * box.
	 */
	private final JCheckBox _saveToolBarCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window save tool bar as
	 * check box.
	 */
	private final JCheckBox _saveToolBarAsCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window compiler check box.
	 */
	private final JCheckBox _compilerCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window show help check box.
	 */
	private final JCheckBox _showHelpCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window show about us check
	 * box.
	 */
	private final JCheckBox _showAboutUsCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window menu check box.
	 */
	private final JCheckBox _menuCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window tool bar check box.
	 */
	private final JCheckBox _toolBarCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window shell display
	 * options check box.
	 */
	private final JCheckBox _shellDisplayOptionsCheckBox;
	/**
	 * ACIDE - A Configurable IDE menu configuration window file editor display
	 * options check box.
	 */
	private final JCheckBox _fileEditorDisplayOptionsCheckBox;
	/**
	 * Flag that indicates if the changes are saved or not.
	 */
	private static boolean _changesAreSaved;
	/**
	 * Flag that indicates if the window is used for modifying an existent menu
	 * configuration or to create a new one.
	 */
	private boolean _forModifying;

	/**
	 * Creates a new menu configuration window.
	 * 
	 * @param forModifying
	 *            indicates if the window is used for modify the menu
	 *            configuration or for create it.
	 */
	public AcideMenuConfigurationWindow(boolean forModifying) {

		// Stores the flag
		_forModifying = forModifying;

		// The changes are saved
		_changesAreSaved = true;

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s531"));

		// Sets the layout
		setLayout(new BorderLayout());

		// MAIN PANEL
		_mainPanel = new JPanel(new GridBagLayout());

		// Sets the window icon
		setIconImage(new ImageIcon(WINDOW_ICON).getImage());

		// Sets the title
		if (forModifying) {

			String currentMenuConfiguration = null;

			try {

				// Gets the the ACIDE - A Configurable IDE current menu
				// configuration
				currentMenuConfiguration = AcideResourceManager.getInstance()
						.getProperty("currentMenuConfiguration");

				// Gets the name
				int index = currentMenuConfiguration.lastIndexOf("\\");
				if (index == -1)
					index = currentMenuConfiguration.lastIndexOf("/");
				currentMenuConfiguration = currentMenuConfiguration.substring(
						index + 1, currentMenuConfiguration.length() - 8);
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());

				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s295"), JOptionPane.ERROR_MESSAGE);
			}

			// Sets the window title
			setTitle(AcideLanguageManager.getInstance().getLabels()
					.getString("s532")
					+ " - " + currentMenuConfiguration);
		} else
			// Sets the window title
			setTitle(AcideLanguageManager.getInstance().getLabels()
					.getString("s298"));

		// FILE PANEL
		_filePanel = new JPanel(new GridLayout(0, 3));
		_filePanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s500"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_newFileCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s8"));
		_openFileCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s9"));
		_openRecentFilesCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s1038"));
		_openAllFilesCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s1004"));
		_saveFileAsCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s10"));
		_saveFileCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s617"));
		_saveAllFilesCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s217"));
		_closeFileCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s238"));
		_closeAllFilesCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s239"));
		_printFileCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s624"));
		_exitCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s13"));

		// EDIT PANEL
		_editPanel = new JPanel();
		_editPanel.setLayout(new GridLayout(0, 3));
		_editPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s501"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_undoCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s21"));
		_redoCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s22"));
		_copyCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s23"));
		_pasteCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s25"));
		_cutCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s24"));
		_selectAllFilesCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s190"));
		_goToLineCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s222"));
		_searchCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s26"));
		_replaceCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s27"));

		// PROJECT PANEL
		_projectPanel = new JPanel(new GridLayout(0, 3));
		_projectPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s502"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_newProjectCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s14"));
		_openProjectCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s15"));
		_saveProjectCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s16"));
		_closeProjectCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s228"));
		_newProjectFileCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s947"));
		_addFileCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s17"));
		_removeFileCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s218"));
		_deleteFileCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s950"));
		_addFolderCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s219"));
		_removeFolderCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s220"));
		_compileCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s18"));
		_executeCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s19"));
		_setCompilableFileCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s254"));
		_unsetCompilableFileCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s255"));
		_setMainFileCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s256"));
		_unsetMainFileCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s952"));
		_saveProjectAsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s926"));

		// VIEW PANEL
		_viewPanel = new JPanel(new GridLayout(0, 3));
		_viewPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s503"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_showLogTabCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s28"));
		_showExplorerPanelCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s221"));
		_showOutputPanelCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s223"));

		// CONFIGURATION PANEL
		_configurationPanel = new JPanel(new GridLayout(0, 3));
		_configurationPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s504"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// LEXICON SUBMENU
		_newLexiconCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s224")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s249"));
		_loadLexiconCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s224")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s35"));
		_modifyLexiconCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s224")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s29"));
		_saveLexiconCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s224")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s250"));
		_saveLexiconAsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s224")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s285"));

		// GRAMMAR SUBMENU
		_newGrammarCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s225")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s30"));
		_loadGrammarCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s225")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s226"));
		_modifyGrammarCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s225")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s227"));
		_saveGrammarCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s225")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s251"));
		_saveGrammarAsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s225")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s286"));
		_setPathsCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s225")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s912"));
		_autoAnalysisCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s225")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s911"));

		// FILE EDITOR
		_fileEditorDisplayOptionsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s1045")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s1041"));

		// CONSOLE SUBMENU
		_configureCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s332")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s333"));
		_externalCommandCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s332")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s341"));
		_shellDisplayOptionsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s332")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s986"));

		// LANGUAGE SUBMENU
		_spanishCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s6")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s11"));
		_englishCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s6")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s12"));

		// MENU SUBMENU
		_menuCheckBox = new JCheckBox();
		_newMenuCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s34")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s275"));
		_loadMenuCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s34")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s276"));
		_modifyMenu = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s34")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s277"));
		_saveMenuCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s34")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s278"));
		_saveMenuAsCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s34")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s279"));

		// TOOL BAR SUBMENU
		_toolBarCheckBox = new JCheckBox();
		_newToolBarCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s169")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s280"));
		_loadToolBarCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s169")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s281"));
		_modifyToolBarCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s169")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s282"));
		_saveToolBarCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s169")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s283"));
		_saveToolBarAsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s169")
				+ " - "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s284"));

		// COMPILER
		_compilerCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s240"));

		// HELP PANEL
		_helpPanel = new JPanel(new GridLayout(0, 3));
		_helpPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s506"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_showHelpCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s38"));
		_showAboutUsCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s39"));

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// ACCEPT BUTTON
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s507"));
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s508"));

		// CANCEL BUTTON
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s509"));
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s510"));

		// SAVE BUTTON
		_saveButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s513"));
		_saveButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s514"));

		// SELECT ALL BUTTON
		_selectAllButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s515"));
		_selectAllButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s516"));

		// SELECT NONE BUTTON
		_selectNoneButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s517"));
		_selectNoneButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s518"));

		// Modify menu always enabled
		_modifyMenu.setSelected(true);
		_modifyMenu.setEnabled(false);

		// If the window is for modifying the menu items
		if (forModifying)
			// Sets the check boxes from the menu item list
			setCheckBoxesFromMenuItemList();

		// Sets the listeners of the window components
		setListeners();

		// Adds the components with the layout to the window
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(0, 0, 0, 0);
		// constraints.gridwidth = 6;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;

		// FILE PANEL
		_filePanel.add(_newFileCheckBox);
		_filePanel.add(_openFileCheckBox);
		_filePanel.add(_openRecentFilesCheckBox);
		_filePanel.add(_openAllFilesCheckBox);
		_filePanel.add(_saveFileAsCheckBox);
		_filePanel.add(_saveFileCheckBox);
		_filePanel.add(_saveAllFilesCheckBox);
		_filePanel.add(_closeFileCheckBox);
		_filePanel.add(_closeAllFilesCheckBox);
		_filePanel.add(_printFileCheckBox);
		_filePanel.add(_exitCheckBox);
		_mainPanel.add(_filePanel, constraints);

		// EDIT PANEL
		_editPanel.add(_undoCheckBox);
		_editPanel.add(_redoCheckBox);
		_editPanel.add(_copyCheckBox);
		_editPanel.add(_pasteCheckBox);
		_editPanel.add(_cutCheckBox);
		_editPanel.add(_selectAllFilesCheckBox);
		_editPanel.add(_goToLineCheckBox);
		_editPanel.add(_searchCheckBox);
		_editPanel.add(_replaceCheckBox);
		constraints.gridx = 1;
		constraints.gridy = 0;
		_mainPanel.add(_editPanel, constraints);

		// PROJECT PANEL
		_projectPanel.add(_newProjectCheckBox);
		_projectPanel.add(_openProjectCheckBox);
		_projectPanel.add(_closeProjectCheckBox);
		_projectPanel.add(_saveProjectCheckBox);
		_projectPanel.add(_saveProjectAsCheckBox);
		_projectPanel.add(_newProjectFileCheckBox);
		_projectPanel.add(_addFileCheckBox);
		_projectPanel.add(_removeFileCheckBox);
		_projectPanel.add(_deleteFileCheckBox);
		_projectPanel.add(_addFolderCheckBox);
		_projectPanel.add(_removeFolderCheckBox);
		_projectPanel.add(_compileCheckBox);
		_projectPanel.add(_executeCheckBox);
		_projectPanel.add(_setCompilableFileCheckBox);
		_projectPanel.add(_unsetCompilableFileCheckBox);
		_projectPanel.add(_setMainFileCheckBox);
		_projectPanel.add(_unsetMainFileCheckBox);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_projectPanel, constraints);

		// VIEW PANEL
		_viewPanel.add(_showLogTabCheckBox);
		_viewPanel.add(_showExplorerPanelCheckBox);
		_viewPanel.add(_showOutputPanelCheckBox);
		constraints.gridx = 1;
		constraints.gridy = 1;
		_mainPanel.add(_viewPanel, constraints);

		// CONFIGURATION PANEL
		_configurationPanel.add(_compilerCheckBox);

		// LEXICON SUBMENU
		_configurationPanel.add(_newLexiconCheckBox);
		_configurationPanel.add(_loadLexiconCheckBox);
		_configurationPanel.add(_modifyLexiconCheckBox);
		_configurationPanel.add(_saveLexiconCheckBox);
		_configurationPanel.add(_saveLexiconAsCheckBox);

		// GRAMMAR SUBMENU
		_configurationPanel.add(_newGrammarCheckBox);
		_configurationPanel.add(_loadGrammarCheckBox);
		_configurationPanel.add(_modifyGrammarCheckBox);
		_configurationPanel.add(_saveGrammarCheckBox);
		_configurationPanel.add(_saveGrammarAsCheckBox);
		_configurationPanel.add(_setPathsCheckBox);
		_configurationPanel.add(_autoAnalysisCheckBox);

		// FILE EDITOR SUBMENU
		_configurationPanel.add(_fileEditorDisplayOptionsCheckBox);

		// CONSOLE SUBMENU
		_configurationPanel.add(_configureCheckBox);
		_configurationPanel.add(_externalCommandCheckBox);
		_configurationPanel.add(_shellDisplayOptionsCheckBox);

		// LANGUAGE SUBMENU
		_configurationPanel.add(_spanishCheckBox);
		_configurationPanel.add(_englishCheckBox);

		// MENU SUBMENU
		_configurationPanel.add(_newMenuCheckBox);
		_configurationPanel.add(_loadMenuCheckBox);
		_configurationPanel.add(_modifyMenu);
		_configurationPanel.add(_saveMenuCheckBox);
		_configurationPanel.add(_saveMenuAsCheckBox);

		// TOOL BAR MENU
		_configurationPanel.add(_newToolBarCheckBox);
		_configurationPanel.add(_loadToolBarCheckBox);
		_configurationPanel.add(_modifyToolBarCheckBox);
		_configurationPanel.add(_saveToolBarCheckBox);
		_configurationPanel.add(_saveToolBarAsCheckBox);
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 2;
		_mainPanel.add(_configurationPanel, constraints);

		// HELP PANEL
		_helpPanel.add(_showHelpCheckBox);
		_helpPanel.add(_showAboutUsCheckBox);
		constraints.gridy = 3;
		_mainPanel.add(_helpPanel, constraints);

		// MAIN PANEL
		add(_mainPanel, BorderLayout.CENTER);

		// BUTTON PANEL
		_buttonPanel.add(_selectAllButton);
		_buttonPanel.add(_selectNoneButton);
		_buttonPanel.add(_acceptButton);
		_buttonPanel.add(_cancelButton);
		add(_buttonPanel, BorderLayout.SOUTH);

		// FRAME
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s530"));
	}

	/**
	 * Sets the listeners to the window components.
	 */
	private void setListeners() {

		// ACCEPT BUTTON
		_acceptButton.addActionListener(new AcceptButtonAction());

		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonAction());
		_cancelButton.registerKeyboardAction(new EscapeKeyAction(),
				"EscapeKey",
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// SAVE BUTTON
		_saveButton.addActionListener(new SaveButtonAction());

		// SELECT ALL BUTTON
		_selectAllButton.addActionListener(new SelectAllButtonAction());

		// SELECT NONE BUTTON
		_selectNoneButton.addActionListener(new SelectNoneButtonAction());
	}

	/**
	 * Returns the changes are saved flag.
	 * 
	 * @return the changes saved flag.
	 */
	public static boolean getChangesAreSaved() {
		return _changesAreSaved;
	}

	/**
	 * Sets a new value to the changes are saved flag.
	 * 
	 * @param changesAreSaved
	 *            new value to set.
	 */
	public static void setChangesAreSaved(boolean changesAreSaved) {
		_changesAreSaved = changesAreSaved;
	}

	/**
	 * Sets the check box values from the menu item list of the menu
	 * configuration.
	 */
	public void setCheckBoxesFromMenuItemList() {

		// FILE MENU
		_newFileCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideFileMenu.NEW_FILE_NAME));
		_openFileCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideFileMenu.OPEN_FILE_NAME));
		_openRecentFilesCheckBox.setSelected(AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideFileMenu.OPEN_RECENT_FILES_NAME));
		_openAllFilesCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideFileMenu.OPEN_ALL_FILES_NAME));
		_saveFileAsCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideFileMenu.SAVE_FILE_AS_NAME));
		_saveFileCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideFileMenu.SAVE_FILE_NAME));
		_saveAllFilesCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideFileMenu.SAVE_ALL_FILES_NAME));
		_closeFileCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideFileMenu.CLOSE_FILE_NAME));
		_closeAllFilesCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideFileMenu.CLOSE_ALL_FILES_NAME));
		_printFileCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideFileMenu.PRINT_FILE_NAME));
		_exitCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideFileMenu.EXIT_NAME));

		// EDIT FILE
		_undoCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.UNDO_NAME));
		_redoCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.REDO_NAME));
		_copyCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.COPY_NAME));
		_pasteCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.PASTE_NAME));
		_cutCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.CUT_NAME));
		_selectAllFilesCheckBox.setSelected(AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideEditMenu.SELECT_ALL_FILES_NAME));
		_goToLineCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.GO_TO_LINE_NAME));
		_searchCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.SEARCH_NAME));
		_replaceCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.REPLACE_NAME));

		// PROJECT MENU
		_newProjectCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.NEW_PROJECT_NAME));
		_openProjectCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.OPEN_PROJECT_NAME));
		_saveProjectCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.SAVE_PROJECT_NAME));
		_addFileCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.ADD_FILE_NAME));
		_removeFileCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.REMOVE_FILE_NAME));
		_addFolderCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.ADD_FOLDER_NAME));
		_removeFolderCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.REMOVE_FOLDER_NAME));
		_compileCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.COMPILE_NAME));
		_executeCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.EXECUTE_NAME));
		_saveProjectAsCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.SAVE_PROJECT_AS_NAME));
		_newProjectFileCheckBox.setSelected(AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideProjectMenu.NEW_PROJECT_FILE_NAME));
		_closeProjectCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.CLOSE_PROJECT_NAME));
		_deleteFileCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.DELETE_FILE_NAME));
		_setCompilableFileCheckBox.setSelected(AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideProjectMenu.SET_COMPILABLE_FILE_NAME));
		_unsetCompilableFileCheckBox.setSelected(AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideProjectMenu.UNSET_COMPILABLE_FILE_NAME));
		_setMainFileCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.SET_MAIN_FILE_NAME));
		_unsetMainFileCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideProjectMenu.UNSET_MAIN_FILE_NAME));

		// VIEW MENU
		_showLogTabCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideViewMenu.SHOW_LOG_TAB_NAME));
		_showExplorerPanelCheckBox.setSelected(AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideViewMenu.SHOW_EXPLORER_PANEL_NAME));
		_showOutputPanelCheckBox.setSelected(AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideViewMenu.SHOW_CONSOLE_PANEL_NAME));

		// LEXICON MENU
		_loadLexiconCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideLexiconMenu.LOAD_LEXICON_NAME));
		_modifyLexiconCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideLexiconMenu.MODIFY_LEXICON_NAME));
		_newLexiconCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideLexiconMenu.NEW_LEXICON_NAME));
		_saveLexiconCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideLexiconMenu.SAVE_LEXICON_NAME));
		_saveLexiconAsCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideLexiconMenu.SAVE_LEXICON_AS_NAME));

		// GRAMMAR MENU
		_newGrammarCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideGrammarMenu.NEW_GRAMMAR_NAME));
		_loadGrammarCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideGrammarMenu.LOAD_GRAMMAR_NAME));
		_modifyGrammarCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideGrammarMenu.MODIFY_GRAMMAR_NAME));
		_saveGrammarCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideGrammarMenu.SAVE_GRAMMAR_NAME));
		_saveGrammarAsCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideGrammarMenu.SAVE_GRAMMAR_AS_NAME));
		_setPathsCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideGrammarMenu.SET_PATHS_NAME));
		_autoAnalysisCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideGrammarMenu.AUTO_ANALYSIS_NAME));

		// FILE EDITOR MENU
		_fileEditorDisplayOptionsCheckBox.setSelected(AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideFileEditorMenu.FILE_EDITOR_DISPLAY_OPTIONS_NAME));

		// OUTPUT MENU
		_configureCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideConsoleMenu.CONFIGURE_NAME));
		_externalCommandCheckBox.setSelected(AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideConsoleMenu.EXTERNAL_COMMAND_NAME));
		_shellDisplayOptionsCheckBox.setSelected(AcideMenuConfiguration
				.getInstance().getIsDisplayed(
						AcideConsoleMenu.CONSOLE_DISPLAY_OPTIONS_NAME));

		// LANGUAGE MENU
		_spanishCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideLanguageMenu.SPANISH_NAME));
		_englishCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideLanguageMenu.ENGLISH_NAME));

		// CONFIGURATION MENU
		_menuCheckBox.setSelected(true);
		_toolBarCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideConfigurationMenu.TOOLBAR_NAME));
		_compilerCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideConfigurationMenu.COMPILER_NAME));

		// HELP MENU
		_showHelpCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideHelpMenu.SHOW_HELP_NAME));
		_showAboutUsCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideHelpMenu.SHOW_ABOUT_US_NAME));

		// MENU MENU
		_newMenuCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideMenuMenu.NEW_MENU_NAME));
		_loadMenuCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideMenuMenu.LOAD_MENU_NAME));
		_modifyMenu.setSelected(true);
		_saveMenuCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideMenuMenu.SAVE_MENU_NAME));
		_saveMenuAsCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideMenuMenu.SAVE_MENU_AS_NAME));

		// TOOL BAR MENU
		_newToolBarCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideToolBarMenu.NEW_TOOLBAR_NAME));
		_loadToolBarCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideToolBarMenu.LOAD_TOOLBAR_NAME));
		_modifyToolBarCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideToolBarMenu.MODIFY_TOOLBAR_NAME));
		_saveToolBarCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideToolBarMenu.SAVE_TOOLBAR_NAME));
		_saveToolBarAsCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideToolBarMenu.SAVE_TOOLBAR_AS_NAME));
	}

	/**
	 * Builds the menu item information list with the check box values.
	 * 
	 * @return the menu item information list with the check box values.
	 */
	public ArrayList<AcideMenuItemInformation> buildMenuItemInformationList() {

		ArrayList<AcideMenuItemInformation> menuItemList = new ArrayList<AcideMenuItemInformation>();

		// FILE MENU
		addFileMenuInformation(menuItemList);

		// EDIT MENU
		addEditMenuInformation(menuItemList);

		// PROJECT MENU
		addProjectMenuInformation(menuItemList);

		// CONFIGURATION MENU
		addConfigurationMenuInformation(menuItemList);

		// LEXICON MENU
		addLexiconMenuInformation(menuItemList);

		// GRAMMAR MENU
		AddGrammarMenuInformation(menuItemList);

		// LANGUAGE
		addLanguageMenuInformation(menuItemList);

		// MENU
		addMenuMenuInformation(menuItemList);

		// FILE EDITOR
		addFileEditorMenuInformation(menuItemList);

		// OUTPUT
		addOutputMenuInformation(menuItemList);

		// TOOLBAR
		addToolBarMenuInformation(menuItemList);

		// VIEW MENU
		addViewMenuInformation(menuItemList);

		// HELP MENU
		addHelpMenuInformation(menuItemList);

		return menuItemList;
	}

	/**
	 * Adds the project menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addProjectMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {

		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.NEW_PROJECT_NAME, _newProjectCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.OPEN_PROJECT_NAME, _openProjectCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.SAVE_PROJECT_NAME, _saveProjectCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.ADD_FILE_NAME, _addFileCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.CLOSE_PROJECT_NAME, _closeProjectCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.REMOVE_FILE_NAME, _removeFileCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.ADD_FOLDER_NAME, _addFolderCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.REMOVE_FOLDER_NAME, _removeFolderCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.COMPILE_NAME, _compileCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.EXECUTE_NAME, _executeCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.SAVE_PROJECT_AS_NAME, _saveProjectAsCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.NEW_PROJECT_FILE_NAME, _newProjectFileCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.DELETE_FILE_NAME, _deleteFileCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.SET_COMPILABLE_FILE_NAME,
				_setCompilableFileCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.UNSET_COMPILABLE_FILE_NAME,
				_unsetCompilableFileCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.SET_MAIN_FILE_NAME, _setMainFileCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideProjectMenu.UNSET_MAIN_FILE_NAME, _unsetMainFileCheckBox
						.isSelected()));
	}

	/**
	 * Adds the configuration menu information to the menu item list, based on
	 * the window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addConfigurationMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {

		menuItemList.add(new AcideMenuItemInformation(
				AcideConfigurationMenu.MENU_NAME, true));
		menuItemList.add(new AcideMenuItemInformation(
				AcideConfigurationMenu.TOOLBAR_NAME, _toolBarCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideConfigurationMenu.COMPILER_NAME, _compilerCheckBox
						.isSelected()));
	}

	/**
	 * Adds the lexicon menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addLexiconMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {

		menuItemList.add(new AcideMenuItemInformation(
				AcideLexiconMenu.LOAD_LEXICON_NAME, _loadLexiconCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideLexiconMenu.MODIFY_LEXICON_NAME, _modifyLexiconCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideLexiconMenu.NEW_LEXICON_NAME, _newLexiconCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideLexiconMenu.SAVE_LEXICON_NAME, _saveLexiconCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideLexiconMenu.SAVE_LEXICON_AS_NAME, _saveLexiconAsCheckBox
						.isSelected()));
	}

	/**
	 * Adds the grammar menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void AddGrammarMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {

		menuItemList.add(new AcideMenuItemInformation(
				AcideGrammarMenu.NEW_GRAMMAR_NAME, _newGrammarCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideGrammarMenu.LOAD_GRAMMAR_NAME, _loadGrammarCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideGrammarMenu.MODIFY_GRAMMAR_NAME, _modifyGrammarCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideGrammarMenu.SAVE_GRAMMAR_NAME, _saveGrammarCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideGrammarMenu.SAVE_GRAMMAR_AS_NAME, _saveGrammarAsCheckBox
						.isSelected()));
		menuItemList
				.add(new AcideMenuItemInformation(
						AcideGrammarMenu.SET_PATHS_NAME, _setPathsCheckBox
								.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideGrammarMenu.AUTO_ANALYSIS_NAME, _autoAnalysisCheckBox
						.isSelected()));
	}

	/**
	 * Adds the menu menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addMenuMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {
		menuItemList.add(new AcideMenuItemInformation(
				AcideMenuMenu.NEW_MENU_NAME, _newMenuCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideMenuMenu.LOAD_MENU_NAME, _loadMenuCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideMenuMenu.MODIFY_MENU_NAME, true));
		menuItemList.add(new AcideMenuItemInformation(
				AcideMenuMenu.SAVE_MENU_NAME, _saveMenuCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideMenuMenu.SAVE_MENU_AS_NAME, _saveMenuAsCheckBox
						.isSelected()));
	}

	/**
	 * Adds the language menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addLanguageMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {
		menuItemList.add(new AcideMenuItemInformation(
				AcideLanguageMenu.ENGLISH_NAME, _englishCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideLanguageMenu.SPANISH_NAME, _spanishCheckBox.isSelected()));
	}

	/**
	 * Adds the file editor menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addFileEditorMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {

		menuItemList.add(new AcideMenuItemInformation(
				AcideFileEditorMenu.FILE_EDITOR_DISPLAY_OPTIONS_NAME,
				_fileEditorDisplayOptionsCheckBox.isSelected()));
	}

	/**
	 * Adds the output menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addOutputMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {
		menuItemList.add(new AcideMenuItemInformation(
				AcideConsoleMenu.CONFIGURE_NAME, _configureCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideConsoleMenu.EXTERNAL_COMMAND_NAME,
				_externalCommandCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideConsoleMenu.CONSOLE_DISPLAY_OPTIONS_NAME,
				_shellDisplayOptionsCheckBox.isSelected()));
	}

	/**
	 * Adds the tool bar menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addToolBarMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {
		menuItemList.add(new AcideMenuItemInformation(
				AcideToolBarMenu.NEW_TOOLBAR_NAME, _newToolBarCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideToolBarMenu.LOAD_TOOLBAR_NAME, _loadToolBarCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideToolBarMenu.MODIFY_TOOLBAR_NAME, _modifyToolBarCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideToolBarMenu.SAVE_TOOLBAR_NAME, _saveToolBarCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideToolBarMenu.SAVE_TOOLBAR_AS_NAME, _saveToolBarAsCheckBox
						.isSelected()));
	}

	/**
	 * Adds the view menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addViewMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {
		menuItemList.add(new AcideMenuItemInformation(
				AcideViewMenu.SHOW_LOG_TAB_NAME, _showLogTabCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideViewMenu.SHOW_EXPLORER_PANEL_NAME,
				_showExplorerPanelCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideViewMenu.SHOW_CONSOLE_PANEL_NAME, _showOutputPanelCheckBox
						.isSelected()));
	}

	/**
	 * Adds the help menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addHelpMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {
		menuItemList.add(new AcideMenuItemInformation(
				AcideHelpMenu.SHOW_HELP_NAME, _showHelpCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideHelpMenu.SHOW_ABOUT_US_NAME, _showAboutUsCheckBox
						.isSelected()));
	}

	/**
	 * Adds the edit menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addEditMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {

		menuItemList.add(new AcideMenuItemInformation(AcideEditMenu.UNDO_NAME,
				_undoCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(AcideEditMenu.REDO_NAME,
				_redoCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(AcideEditMenu.COPY_NAME,
				_copyCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(AcideEditMenu.PASTE_NAME,
				_pasteCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(AcideEditMenu.CUT_NAME,
				_cutCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideEditMenu.SELECT_ALL_FILES_NAME, _selectAllFilesCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideEditMenu.GO_TO_LINE_NAME, _goToLineCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideEditMenu.SEARCH_NAME, _searchCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideEditMenu.REPLACE_NAME, _replaceCheckBox.isSelected()));
	}

	/**
	 * Adds the file menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addFileMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {

		menuItemList.add(new AcideMenuItemInformation(
				AcideFileMenu.NEW_FILE_NAME, _newFileCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideFileMenu.OPEN_FILE_NAME, _openFileCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideFileMenu.OPEN_RECENT_FILES_NAME, _openRecentFilesCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideFileMenu.OPEN_ALL_FILES_NAME, _openFileCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideFileMenu.SAVE_FILE_AS_NAME, _saveFileAsCheckBox
						.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideFileMenu.SAVE_FILE_NAME, _saveFileCheckBox.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideFileMenu.SAVE_ALL_FILES_NAME, _saveAllFilesCheckBox
						.isSelected()));
		menuItemList
				.add(new AcideMenuItemInformation(
						AcideFileMenu.CLOSE_FILE_NAME, _closeFileCheckBox
								.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(
				AcideFileMenu.CLOSE_ALL_FILES_NAME, _closeAllFilesCheckBox
						.isSelected()));
		menuItemList
				.add(new AcideMenuItemInformation(
						AcideFileMenu.PRINT_FILE_NAME, _printFileCheckBox
								.isSelected()));
		menuItemList.add(new AcideMenuItemInformation(AcideFileMenu.EXIT_NAME,
				_exitCheckBox.isSelected()));
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration window select none button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SelectNoneButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// FILE MENU
			_newFileCheckBox.setSelected(false);
			_openFileCheckBox.setSelected(false);
			_openRecentFilesCheckBox.setSelected(false);
			_openAllFilesCheckBox.setSelected(false);
			_saveFileAsCheckBox.setSelected(false);
			_saveFileCheckBox.setSelected(false);
			_saveAllFilesCheckBox.setSelected(false);
			_closeFileCheckBox.setSelected(false);
			_closeAllFilesCheckBox.setSelected(false);
			_printFileCheckBox.setSelected(false);
			_exitCheckBox.setSelected(false);

			// EDIT MENU
			_undoCheckBox.setSelected(false);
			_redoCheckBox.setSelected(false);
			_copyCheckBox.setSelected(false);
			_pasteCheckBox.setSelected(false);
			_cutCheckBox.setSelected(false);
			_selectAllFilesCheckBox.setSelected(false);
			_goToLineCheckBox.setSelected(false);
			_searchCheckBox.setSelected(false);
			_replaceCheckBox.setSelected(false);

			// PROJECT MENU
			_newProjectCheckBox.setSelected(false);
			_openProjectCheckBox.setSelected(false);
			_saveProjectCheckBox.setSelected(false);
			_addFileCheckBox.setSelected(false);
			_removeFileCheckBox.setSelected(false);
			_addFolderCheckBox.setSelected(false);
			_removeFolderCheckBox.setSelected(false);
			_closeProjectCheckBox.setSelected(false);
			_compileCheckBox.setSelected(false);
			_executeCheckBox.setSelected(false);
			_setCompilableFileCheckBox.setSelected(false);
			_unsetCompilableFileCheckBox.setSelected(true);
			_setMainFileCheckBox.setSelected(false);
			_saveProjectAsCheckBox.setSelected(false);
			_newProjectFileCheckBox.setSelected(false);
			_deleteFileCheckBox.setSelected(false);
			_unsetMainFileCheckBox.setSelected(false);

			// VIEW MENU
			_showLogTabCheckBox.setSelected(false);
			_showExplorerPanelCheckBox.setSelected(false);
			_showOutputPanelCheckBox.setSelected(false);

			// LEXICON MENU
			_newLexiconCheckBox.setSelected(false);
			_loadLexiconCheckBox.setSelected(false);
			_modifyLexiconCheckBox.setSelected(false);
			_saveLexiconCheckBox.setSelected(false);
			_saveLexiconAsCheckBox.setSelected(false);

			// GRAMMAR MENU
			_newGrammarCheckBox.setSelected(false);
			_loadGrammarCheckBox.setSelected(false);
			_modifyGrammarCheckBox.setSelected(false);
			_saveGrammarCheckBox.setSelected(false);
			_saveGrammarAsCheckBox.setSelected(false);
			_setPathsCheckBox.setSelected(false);
			_autoAnalysisCheckBox.setSelected(false);

			// FILE EDITOR MENU
			_fileEditorDisplayOptionsCheckBox.setSelected(false);

			// OUTPUT MENU
			_configureCheckBox.setSelected(false);
			_externalCommandCheckBox.setSelected(false);
			_shellDisplayOptionsCheckBox.setSelected(false);

			// LANGUAGE MENU
			_spanishCheckBox.setSelected(false);
			_englishCheckBox.setSelected(false);

			// CONFIGURATION MENU
			_menuCheckBox.setSelected(true);
			_toolBarCheckBox.setSelected(false);
			_compilerCheckBox.setSelected(false);

			// HELP MENU
			_showHelpCheckBox.setSelected(false);
			_showAboutUsCheckBox.setSelected(false);

			// MENU MENU
			_newMenuCheckBox.setSelected(false);
			_loadMenuCheckBox.setSelected(false);
			_modifyMenu.setSelected(true);
			_saveMenuCheckBox.setSelected(false);
			_saveMenuAsCheckBox.setSelected(false);

			// TOOL BAR MENU
			_newToolBarCheckBox.setSelected(false);
			_loadToolBarCheckBox.setSelected(false);
			_modifyToolBarCheckBox.setSelected(false);
			_saveToolBarCheckBox.setSelected(false);
			_saveToolBarAsCheckBox.setSelected(false);
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration window select all button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SelectAllButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// FILE MENU
			_newFileCheckBox.setSelected(true);
			_openFileCheckBox.setSelected(true);
			_openRecentFilesCheckBox.setSelected(true);
			_openAllFilesCheckBox.setSelected(true);
			_saveFileAsCheckBox.setSelected(true);
			_saveFileCheckBox.setSelected(true);
			_saveAllFilesCheckBox.setSelected(true);
			_closeFileCheckBox.setSelected(true);
			_closeAllFilesCheckBox.setSelected(true);
			_printFileCheckBox.setSelected(true);
			_exitCheckBox.setSelected(true);

			// EDIT MENU
			_undoCheckBox.setSelected(true);
			_redoCheckBox.setSelected(true);
			_copyCheckBox.setSelected(true);
			_pasteCheckBox.setSelected(true);
			_cutCheckBox.setSelected(true);
			_selectAllFilesCheckBox.setSelected(true);
			_goToLineCheckBox.setSelected(true);
			_searchCheckBox.setSelected(true);
			_replaceCheckBox.setSelected(true);

			// PROJECT MENU
			_newProjectCheckBox.setSelected(true);
			_openProjectCheckBox.setSelected(true);
			_saveProjectCheckBox.setSelected(true);
			_addFileCheckBox.setSelected(true);
			_removeFileCheckBox.setSelected(true);
			_addFolderCheckBox.setSelected(true);
			_removeFolderCheckBox.setSelected(true);
			_closeProjectCheckBox.setSelected(true);
			_compileCheckBox.setSelected(true);
			_executeCheckBox.setSelected(true);
			_setCompilableFileCheckBox.setSelected(true);
			_unsetCompilableFileCheckBox.setSelected(true);
			_setMainFileCheckBox.setSelected(true);
			_saveProjectAsCheckBox.setSelected(true);
			_newProjectFileCheckBox.setSelected(true);
			_deleteFileCheckBox.setSelected(true);
			_unsetMainFileCheckBox.setSelected(true);

			// VIEW MENU
			_showLogTabCheckBox.setSelected(true);
			_showExplorerPanelCheckBox.setSelected(true);
			_showOutputPanelCheckBox.setSelected(true);

			// LEXICON MENU
			_newLexiconCheckBox.setSelected(true);
			_loadLexiconCheckBox.setSelected(true);
			_modifyLexiconCheckBox.setSelected(true);
			_saveLexiconCheckBox.setSelected(true);
			_saveLexiconAsCheckBox.setSelected(true);

			// GRAMMAR MENU
			_newGrammarCheckBox.setSelected(true);
			_loadGrammarCheckBox.setSelected(true);
			_modifyGrammarCheckBox.setSelected(true);
			_saveGrammarCheckBox.setSelected(true);
			_saveGrammarAsCheckBox.setSelected(true);
			_setPathsCheckBox.setSelected(true);
			_autoAnalysisCheckBox.setSelected(true);

			// FILE EDITOR MENU
			_fileEditorDisplayOptionsCheckBox.setSelected(true);

			// OUTPUT MENU
			_configureCheckBox.setSelected(true);
			_externalCommandCheckBox.setSelected(true);
			_shellDisplayOptionsCheckBox.setSelected(true);

			// LANGUAGE MENU
			_spanishCheckBox.setSelected(true);
			_englishCheckBox.setSelected(true);

			// CONFIGURATION MENU
			_menuCheckBox.setSelected(true);
			_toolBarCheckBox.setSelected(true);
			_compilerCheckBox.setSelected(true);

			// HELP MENU
			_showHelpCheckBox.setSelected(true);
			_showAboutUsCheckBox.setSelected(true);

			// MENU MENU
			_newMenuCheckBox.setSelected(true);
			_loadMenuCheckBox.setSelected(true);
			_modifyMenu.setSelected(true);
			_saveMenuCheckBox.setSelected(true);
			_saveMenuAsCheckBox.setSelected(true);

			// TOOL BAR MENU
			_newToolBarCheckBox.setSelected(true);
			_loadToolBarCheckBox.setSelected(true);
			_modifyToolBarCheckBox.setSelected(true);
			_saveToolBarCheckBox.setSelected(true);
			_saveToolBarAsCheckBox.setSelected(true);
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration window save button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SaveButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Creates the menu item information list
			ArrayList<AcideMenuItemInformation> menuItemList = buildMenuItemInformationList();

			// Creates the file chooser which only accepts
			// menuCfg extensions
			JFileChooser fileChooser = new JFileChooser();
			AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s126"));
			filter.addExtension("menuCfg");
			fileChooser.setFileFilter(filter);

			String fileName = "";

			// Ask for saving
			int returnValue = fileChooser.showSaveDialog(fileChooser);

			// If yes
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// If the name of the file does not content menuCfg
				// adds to it
				fileName = fileChooser.getSelectedFile().getAbsolutePath();
				if (!fileName.endsWith(".menuCfg"))
					fileName += ".menuCfg";

				// Save the menu configuration
				AcideMenuConfiguration.getInstance().saveMenuConfigurationFile(
						fileName, menuItemList);

				// Updates the the ACIDE - A Configurable IDE current menu
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentMenuConfiguration", fileName);

				// The changes are saved
				_changesAreSaved = true;

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s528")
								+ fileName
								+ AcideLanguageManager.getInstance()
										.getLabels().getString("s529"));
			} else if (returnValue == JFileChooser.CANCEL_OPTION) {

				// Cancel selection
				fileChooser.cancelSelection();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s527"));
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration window cancel button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s520"));

			// Closes the window
			dispose();
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration window accept button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Creates the menu item information list
			ArrayList<AcideMenuItemInformation> menuItemList = buildMenuItemInformationList();

			// Stores the new menu item information list
			AcideMenuConfiguration.getInstance().setMenuElementList(
					menuItemList);

			// Picks the new name
			String newName = "";
			if (_forModifying)
				newName = "./configuration/menu/lastModified.menuCfg";
			else
				newName = "./configuration/menu/newMenu.menuCfg";

			// Saves the new configuration
			AcideMenuConfiguration.getInstance().saveMenuConfigurationFile(
					newName, menuItemList);

			try {

				// Gets the the ACIDE - A Configurable IDE current menu
				// configuration
				String currentMenuConfiguration = AcideResourceManager
						.getInstance().getProperty("currentMenuConfiguration");

				if (_changesAreSaved) {

					if (!currentMenuConfiguration
							.endsWith("lastModified.menuCfg")
							|| !currentMenuConfiguration
									.endsWith("newMenu.menuCfg")) {

						// Updates the the ACIDE - A Configurable IDE previous
						// menu
						// configuration
						AcideResourceManager.getInstance().setProperty(
								"previousMenuConfiguration",
								currentMenuConfiguration);
					}
				}

				// Updates the the ACIDE - A Configurable IDE current menu
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentMenuConfiguration", newName);

				// Builds the menu
				AcideMainWindow.getInstance().getMenu().build();

				// Enables the save menu item in the configuration menu
				AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
						.getMenuMenu().getSaveMenuMenuItem().setEnabled(true);
				
				// Validates the changes in the main window
				AcideMainWindow.getInstance().validate();

				// Repaints the main window
				AcideMainWindow.getInstance().repaint();

				// The changes are not saved
				_changesAreSaved = false;

				// Closes the window
				dispose();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s519"));

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject())
					// The project has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());

				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s292"), JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration window escape key action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class EscapeKeyAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Closes the window
			dispose();
		}
	}
}
