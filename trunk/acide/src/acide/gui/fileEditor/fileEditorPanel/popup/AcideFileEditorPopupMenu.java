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
package acide.gui.fileEditor.fileEditorPanel.popup;

import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideAddFileMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideAutomaticIndentMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideCopyMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideCutMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideDeleteFileMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcidePasteMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcidePrintFileMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideRemoveFileMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideSelectAllMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideSendFileContentToConsoleMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideSetCompilableFileMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideSetMainFileMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideShowDisplayOptionsWindowAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideUnsetCompilableFileMenuItemAction;
import acide.gui.fileEditor.fileEditorPanel.popup.listeners.AcideUnsetMainFileMenuItemAction;
import acide.gui.menuBar.configurationMenu.lexiconMenu.listeners.AcideDocumentLexiconMenuItemListener;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE file editor panel popup menu.
 * 
 * @version 0.8
 * @see JPopupMenu
 */
public class AcideFileEditorPopupMenu extends JPopupMenu {

	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu display options
	 * window menu item image icon.
	 */
	private final static ImageIcon SHOW_DISPLAY_OPTIONS_WINDOW_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/fileEditor/fileEditorDisplayOptions.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu automatic indent
	 * menu item image icon.
	 */
	private final static ImageIcon AUTOMATIC_INDENT_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/fileEditor/automaticIndent.png");
	/**
	 * ACIDE - A Configurable IDE file editor menu document lexicon menu item
	 * image icon.
	 */
	private final static ImageIcon DOCUMENT_LEXICON_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/fileEditor/documentLexicon.png");
	/**
	 * ACIDE - A Configurable IDE file editor menu send file content to console
	 * menu item image icon.
	 */
	private final static ImageIcon SEND_FILE_CONTENT_TO_CONSOLE_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/fileEditor/sendFileContentToConsole.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu copy menu item
	 * image icon.
	 */
	private final static ImageIcon COPY_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/copy.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu paste menu item
	 * image icon.
	 */
	private final static ImageIcon PASTE_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/paste.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu cut menu item
	 * image icon.
	 */
	private final static ImageIcon CUT_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/cut.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu select all menu
	 * item image icon.
	 */
	private final static ImageIcon SELECT_ALL_IMAGE = new ImageIcon(
			"./resources/icons/menu/edit/selectAll.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu add file menu
	 * item image icon.
	 */
	private static final ImageIcon ADD_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/addFile.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu remove file menu
	 * item image icon.
	 */
	private static final ImageIcon REMOVE_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/removeFile.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu delete file menu
	 * item image icon.
	 */
	private static final ImageIcon DELETE_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/deleteFile.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set main menu
	 * item image icon.
	 */
	private static final ImageIcon SET_MAIN_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/setMain.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset main menu
	 * item image icon.
	 */
	private static final ImageIcon UNSET_MAIN_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/unsetMain.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set compilable
	 * menu item image icon.
	 */
	private static final ImageIcon SET_COMPILABLE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/setCompilable.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset compilable
	 * menu item image icon.
	 */
	private static final ImageIcon UNSET_COMPILABLE_IMAGE = new ImageIcon(
			"./resources/icons/menu/project/unsetCompilable.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu print file menu
	 * item image icon.
	 */
	private final static ImageIcon PRINT_FILE_IMAGE = new ImageIcon(
			"./resources/icons/menu/file/printFile.png");
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu show display
	 * options window menu item.
	 */
	private JMenuItem _showDisplayOptionsWindowMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu document lexicon
	 * menu item.
	 */
	private JMenuItem _documentLexiconMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu add file menu
	 * item.
	 */
	private JMenuItem _addFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu delete file menu
	 * item.
	 */
	private JMenuItem _deleteFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu remove file menu
	 * item.
	 */
	private JMenuItem _removeFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu select all menu
	 * item.
	 */
	private JMenuItem _selectAllMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu copy menu item.
	 */
	private JMenuItem _copyMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu paste menu item.
	 */
	private JMenuItem _pasteMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu cut menu item.
	 */
	private JMenuItem _cutMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set compilable
	 * file menu item.
	 */
	private JMenuItem _setCompilableFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset compilable
	 * file menu item.
	 */
	private JMenuItem _unsetCompilableFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu set main file
	 * menu item.
	 */
	private JMenuItem _setMainFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu unset main file
	 * menu item.
	 */
	private JMenuItem _unsetMainFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu print file menu
	 * item.
	 */
	private JMenuItem _printFileMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu automatic indent
	 * menu item.
	 */
	private JCheckBoxMenuItem _automaticIndentCheckBoxMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor panel popup menu send file content
	 * to console menu item.
	 */
	private JMenuItem _sendFileContentToConsoleMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor panel popup menu.
	 */
	public AcideFileEditorPopupMenu() {

		// Builds the popup menu components
		buildComponents();

		// Adds the components to the popup menu
		addComponents();

		// Sets the menu item listeners
		setListeners();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE file editor popup
	 * menu.
	 */
	private void addComponents() {

		// Adds the show display options window menu item to the popup menu
		add(_showDisplayOptionsWindowMenuItem);

		// Adds the automatic indent check box menu item to the popup menu
		add(_automaticIndentCheckBoxMenuItem);

		// Adds a separator to the popup menu
		addSeparator();

		// Adds the document lexicon menu item to the popup menu
		add(_documentLexiconMenuItem);

		// Adds a separator to the popup menu
		addSeparator();

		// Adds the send file content to console menu item to the popup menu
		add(_sendFileContentToConsoleMenuItem);

		// Adds a separator to the popup menu
		addSeparator();

		// Adds the copy menu item to the popup menu
		add(_copyMenuItem);

		// Adds the cut menu item to the popup menu
		add(_cutMenuItem);

		// Adds the paste menu item to the popup menu
		add(_pasteMenuItem);

		// Adds the select all menu item to the popup menu
		add(_selectAllMenuItem);

		// Adds a separator to the popup menu
		addSeparator();

		// Adds the add file menu item to the popup menu
		add(_addFileMenuItem);

		// Adds the remove file menu item to the popup menu
		add(_removeFileMenuItem);

		// Adds the delete file menu item to the popup menu
		add(_deleteFileMenuItem);

		// Adds a separator to the popup menu
		addSeparator();

		// Adds the set compilable file menu item to the popup menu
		add(_setCompilableFileMenuItem);

		// Adds the unset compilable file menu item to the popup menu
		add(_unsetCompilableFileMenuItem);

		// Adds the set main file menu item to the popup menu
		add(_setMainFileMenuItem);

		// Adds the unset main file menu item to the popup menu
		add(_unsetMainFileMenuItem);

		// Adds a separator to the popup menu
		addSeparator();

		// Adds the print file menu item to the popup menu
		add(_printFileMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE file editor popup menu components.
	 */
	private void buildComponents() {

		// Creates the show display options window menu item
		_showDisplayOptionsWindowMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s1041"),
				SHOW_DISPLAY_OPTIONS_WINDOW_IMAGE);

		// Creates the automatic indent check box menu item
		_automaticIndentCheckBoxMenuItem = new JCheckBoxMenuItem(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1097"), AUTOMATIC_INDENT_IMAGE);

		// Updates its state with the file editor configuration
		_automaticIndentCheckBoxMenuItem
				.setSelected(AcideWorkbenchConfiguration.getInstance()
						.getFileEditorConfiguration().getAutomaticIndent());

		// Creates the document lexicon menu item
		_documentLexiconMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s1093"),
				DOCUMENT_LEXICON_IMAGE);

		// Creates the file send file content menu item
		_sendFileContentToConsoleMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s2005"),
				SEND_FILE_CONTENT_TO_CONSOLE_IMAGE);

		// Creates the copy menu item
		_copyMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s187"), COPY_IMAGE);

		// Creates the cut menu item
		_cutMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s188"), CUT_IMAGE);

		// Creates the paste menu item
		_pasteMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s189"), PASTE_IMAGE);

		// Creates the select all menu item
		_selectAllMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s191"), SELECT_ALL_IMAGE);

		// Creates the add file menu item
		_addFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s1037"), ADD_FILE_IMAGE);

		// Creates the remove file menu item
		_removeFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s618"), REMOVE_FILE_IMAGE);

		// Creates the delete file menu item
		_deleteFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s950"), DELETE_FILE_IMAGE);

		// Creates the set compilable file menu item
		_setCompilableFileMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s254"),
				SET_COMPILABLE_IMAGE);

		// Creates the unset compilable file menu item
		_unsetCompilableFileMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s255"),
				UNSET_COMPILABLE_IMAGE);

		// Creates the set main file menu item
		_setMainFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s256"), SET_MAIN_IMAGE);

		// Creates the unset main file menu item
		_unsetMainFileMenuItem = new JMenuItem(AcideLanguageManager
				.getInstance().getLabels().getString("s952"), UNSET_MAIN_IMAGE);

		// Creates the print file menu item
		_printFileMenuItem = new JMenuItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s624"), PRINT_FILE_IMAGE);
	}

	/**
	 * Sets the menu item listeners of the ACIDE - A Configurable IDE file
	 * editor popup menu.
	 */
	private void setListeners() {

		// Sets the show display options window menu item action listener
		_showDisplayOptionsWindowMenuItem
				.addActionListener(new AcideShowDisplayOptionsWindowAction());

		// Sets the automatic indent menu item action listener
		_automaticIndentCheckBoxMenuItem
				.addActionListener(new AcideAutomaticIndentMenuItemAction());

		// Sets the document lexicon menu item action listener
		_documentLexiconMenuItem
				.addActionListener(new AcideDocumentLexiconMenuItemListener());

		// Sets the send file content to console menu item action listener
		_sendFileContentToConsoleMenuItem
				.addActionListener(new AcideSendFileContentToConsoleMenuItemAction());

		// Sets the copy menu item action listener
		_copyMenuItem.addActionListener(new AcideCopyMenuItemAction());

		// Sets the cut menu item action listener
		_cutMenuItem.addActionListener(new AcideCutMenuItemAction());

		// Sets the paste menu item action listener
		_pasteMenuItem.addActionListener(new AcidePasteMenuItemAction());

		// Sets the select all menu item action listener
		_selectAllMenuItem
				.addActionListener(new AcideSelectAllMenuItemAction());

		// Sets the add file item action listener
		_addFileMenuItem.addActionListener(new AcideAddFileMenuItemAction());

		// Sets the remove file menu item action listener
		_removeFileMenuItem
				.addActionListener(new AcideRemoveFileMenuItemAction());

		// Sets the delete file menu item action listener
		_deleteFileMenuItem
				.addActionListener(new AcideDeleteFileMenuItemAction());

		// Sets the set compilable file menu item action listener
		_setCompilableFileMenuItem
				.addActionListener(new AcideSetCompilableFileMenuItemAction());

		// Sets the unset compilable file menu item action listener
		_unsetCompilableFileMenuItem
				.addActionListener(new AcideUnsetCompilableFileMenuItemAction());

		// Sets the set main file menu item action listener
		_setMainFileMenuItem
				.addActionListener(new AcideSetMainFileMenuItemAction());

		// Sets the unset main file menu item action listener
		_unsetMainFileMenuItem
				.addActionListener(new AcideUnsetMainFileMenuItemAction());

		// Sets the print file menu item action listener
		_printFileMenuItem
				.addActionListener(new AcidePrintFileMenuItemAction());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu cut
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu cut
	 *         menu item.
	 */
	public JMenuItem getCutMenuItem() {
		return _cutMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu copy
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu copy
	 *         menu item.
	 */
	public JMenuItem getCopyMenuItem() {
		return _copyMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu paste
	 * menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu paste
	 *         menu item.
	 */
	public JMenuItem getPasteMenuItem() {
		return _pasteMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu add
	 * file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu add
	 *         file menu item.
	 */
	public JMenuItem getAddFileMenuItem() {
		return _addFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu
	 * remove file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu
	 *         remove file menu item.
	 */
	public JMenuItem getRemoveFileMenuItem() {
		return _removeFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu set
	 * compilable file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu set
	 *         compilable file menu item.
	 */
	public JMenuItem getSetCompilableFileMenuItem() {
		return _setCompilableFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu unset
	 * compilable file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu unset
	 *         compilable file menu item.
	 */
	public JMenuItem getUnsetCompilableFileMenuItem() {
		return _unsetCompilableFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu set
	 * main file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu set
	 *         main file menu item.
	 */
	public JMenuItem getSetMainFileMenuItem() {
		return _setMainFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu unset
	 * main file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu unset
	 *         main file menu item.
	 */
	public JMenuItem getUnsetMainFileMenuItem() {
		return _unsetMainFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu
	 * delete file menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu
	 *         delete file menu item.
	 */
	public JMenuItem getDeleteFileMenuItem() {
		return _deleteFileMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu
	 * document lexicon menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu
	 *         document lexicon menu item.
	 */
	public JMenuItem getDocumentLexiconMenuItem() {
		return _documentLexiconMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu send
	 * file content to console menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu send
	 *         file content to console menu item.
	 */
	public JMenuItem getSendFileContentToConsoleMenuItem() {
		return _sendFileContentToConsoleMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor panel popup menu
	 * automatic indent check box menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor panel popup menu
	 *         automatic indent check box menu item.
	 */
	public JMenuItem getAutomaticIndentCheckBoxMenuItem() {
		return _automaticIndentCheckBoxMenuItem;
	}
}
