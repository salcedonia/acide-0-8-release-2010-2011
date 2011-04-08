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
package acide.gui.fileEditor.fileEditorManager;

import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.configuration.lexicon.AcideLexiconConfiguration;
import acide.files.project.AcideProjectFileType;
import acide.gui.fileEditor.fileEditorManager.listeners.AcideFileEditorManagerChangeListener;
import acide.gui.fileEditor.fileEditorManager.listeners.AcideFileEditorManagerMouseClickListener;
import acide.gui.fileEditor.fileEditorManager.utils.gui.AcideDragAndDropTabbedPane;
import acide.gui.fileEditor.fileEditorManager.utils.logic.UI.AcideFileEditorTabbedPaneUI;
import acide.gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import acide.gui.fileEditor.fileEditorPanel.listeners.AcideFileEditorPanelDocumentListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.utils.AcideUndoManager;
import acide.gui.toolBarPanel.menuBarToolBar.AcideMenuBarToolBar;

import java.io.File;

import javax.swing.ImageIcon;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * <p>
 * ACIDE - A Configurable IDE file editor manager.
 * </p>
 * <p>
 * Handles the creation and destruction of the different editor tabs and gives
 * all the required methods to interact with the tabbed pane which contains the
 * files.
 * </p>
 * 
 * @version 0.8
 * @see AcideDragAndDropTabbedPane
 * @see AcideFileEditorTabbedPaneUI
 */
public class AcideFileEditorManager {

	/**
	 * ACIDE - A Configurable IDE file editor manager path where all the
	 * resources of the application are.
	 */
	private static final String RESOURCE_PATH = "./resources/icons/editor/";
	/**
	 * ACIDE - A Configurable IDE file editor manager tabbed pane for the
	 * editor.
	 */
	private AcideDragAndDropTabbedPane _tabbedPane;
	/**
	 * ACIDE - A Configurable IDE file editor manager tabbed pane UI.
	 * 
	 * Handles the painting for the tabbed pane and the closing buttons.
	 */
	private AcideFileEditorTabbedPaneUI _tabbedPanelUI;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor manager.
	 */
	public AcideFileEditorManager() {

		try {

			// Creates the drag and drop tabbed pane
			_tabbedPane = new AcideDragAndDropTabbedPane();

			// Creates the tabbed pane UI
			_tabbedPanelUI = new AcideFileEditorTabbedPaneUI();

			// Sets the tabbed pane UI
			_tabbedPane.setUI(_tabbedPanelUI);

			// Sets the tabbed pane listeners
			setListeners();

		} catch (RuntimeException exception) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s315"));
			exception.printStackTrace();
		}
	}

	/**
	 * Sets the ACIDE - A Configurable IDE file editor manager listener.
	 */
	public void setListeners() {

		// Sets the tabbed pane mouse listener
		_tabbedPane
				.addMouseListener(new AcideFileEditorManagerMouseClickListener());

		// Sets the tabbed pane change listener
		_tabbedPane
				.addChangeListener(new AcideFileEditorManagerChangeListener());
	}

	/**
	 * Checks if the tab is already opened in the ACIDE - A Configurable IDE
	 * file editor manager tabbed pane. If so, it puts it as the selected tab.
	 * If not adds the new tab to the tabbed pane. In both cases also sets the
	 * caret position in the file content.
	 * 
	 * @param filePath
	 *            file absolute path.
	 * @param fileContent
	 *            file content to display.
	 * @param isEditable
	 *            indicates if the editor is modifiable or not. It is not
	 *            editable when the Log tab is opened.
	 * @param fileType
	 *            explorer file type.
	 * @param caretPosition
	 *            caret position to set.
	 * @param splitPaneDividerLocation
	 *            split pane divider location to set.
	 * @param activeTextEditionArea
	 *            active text edition area.
	 * @param lexiconConfiguration
	 *            file editor lexicon configuration.
	 * @param currentGrammarConfiguration
	 *            file editor current grammar configuration.
	 * @param previousGrammarConfiguration
	 *            file editor previous grammar configuration.
	 */
	public void updatesTabbedPane(String filePath, final String fileContent,
			boolean isEditable, AcideProjectFileType fileType,
			final int caretPosition, int splitPaneDividerLocation,
			int activeTextEditionArea,
			AcideLexiconConfiguration lexiconConfiguration,
			AcideGrammarConfiguration currentGrammarConfiguration,
			AcideGrammarConfiguration previousGrammarConfiguration) {

		// Checks if the file is already opened
		int tabPosition = -1;
		for (int index = 0; index < getNumberOfFileEditorPanels(); index++)
			if (getFileEditorPanelAt(index).getAbsolutePath() == filePath)
				tabPosition = index;

		// if it is not opened yet
		if (tabPosition == -1) {

			String fileName = null;

			// Gets the file name from the file absolute path
			if (filePath != null) {

				int lastIndexOfSlash = filePath.lastIndexOf("\\");
				if (lastIndexOfSlash == -1)
					lastIndexOfSlash = filePath.lastIndexOf("/");
				fileName = filePath.substring(lastIndexOfSlash + 1,
						filePath.length());
			}

			// Adds the tab to the tabbed pane
			addTab(fileName, filePath, isEditable, fileType,
					activeTextEditionArea, splitPaneDividerLocation,
					lexiconConfiguration, currentGrammarConfiguration,
					previousGrammarConfiguration);

			/*
			 * Once the tabbed pane and tabbed pane UI are updated, the file
			 * content is loaded and the lexicon configuration is applied to
			 * provoke the document events without any risk.
			 */

			// Sets the file content
			getSelectedFileEditorPanel().setFileContent(fileContent);

			// Puts the focus in the active area
			getSelectedFileEditorPanel().getActiveTextEditionArea()
					.requestFocusInWindow();

			// Applies the lexicon configuration
			getSelectedFileEditorPanel().resetStyledDocument();

			// Updates the undo manager
			AcideUndoManager.getInstance().update();

			// Sets the caret position in the position stored in the file editor
			// configuration
			getSelectedFileEditorPanel().getActiveTextEditionArea()
					.setCaretPosition(caretPosition);

		} else {

			// Updates the selected file editor index
			updateRelatedComponentsAt(tabPosition);
		}
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor manager tab and adds
	 * it to the tabbed pane with the selected configuration.
	 * 
	 * @param fileName
	 *            file name.
	 * @param filePath
	 *            Absolute path of the file displayed on the tab used as its
	 *            tool tip text.
	 * @param isEditable
	 *            indicates if the editor is modifiable or not. It is not
	 *            editable when the Log tab is opened.
	 * @param fileType
	 *            file type.
	 * @param activeTextEditionArea
	 *            file editor lexicon configuration.
	 * @param splitPaneDividerLocation
	 *            split pane divider location.
	 * @param currentGrammarConfiguration
	 *            file editor current grammar configuration.
	 * @param previousGrammarConfiguration
	 *            file editor previous grammar configuration.
	 * 
	 */
	public void addTab(String fileName, String filePath, boolean isEditable,
			AcideProjectFileType fileType, int activeTextEditionArea,
			int splitPaneDividerLocation,
			AcideLexiconConfiguration lexiconConfiguration,
			AcideGrammarConfiguration currentGrammarConfiguration,
			AcideGrammarConfiguration previousGrammarConfiguration) {

		// Creates the file editor panel
		AcideFileEditorPanel fileEditorPanel = new AcideFileEditorPanel(
				filePath, isEditable, new File(filePath).lastModified(),
				new File(filePath).length(), activeTextEditionArea,
				splitPaneDividerLocation, lexiconConfiguration,
				currentGrammarConfiguration, previousGrammarConfiguration);

		// Configures and adds the tab to the tabbed pane based on the file type
		switch (fileType) {
		case NORMAL:

			// Adds the tab
			_tabbedPane.addTab(fileName, null, fileEditorPanel, filePath);

			// It has no icon
			fileEditorPanel.setIcon(null);

			// It is not a compilable file
			fileEditorPanel.setCompilableFile(false);

			// It is not a main file
			fileEditorPanel.setMainFile(false);

			break;

		case MAIN:

			// Adds the tab
			_tabbedPane.addTab(fileName, new ImageIcon(RESOURCE_PATH
					+ "main.png"), fileEditorPanel, filePath);

			// Sets the icon for the main files
			fileEditorPanel.setIcon(new ImageIcon(RESOURCE_PATH + "main.png"));

			// It is compilable file
			fileEditorPanel.setCompilableFile(true);

			// It is main file
			fileEditorPanel.setMainFile(true);
			break;

		case COMPILABLE:

			// Adds the tab
			_tabbedPane.addTab(fileName, new ImageIcon(RESOURCE_PATH
					+ "compilable.png"), fileEditorPanel, filePath);

			// Sets the icon for the compilable files
			fileEditorPanel.setIcon(new ImageIcon(RESOURCE_PATH
					+ "compilable.png"));

			// It is compilable file
			fileEditorPanel.setCompilableFile(true);

			// It is main file
			fileEditorPanel.setMainFile(false);

			break;
		}

		// The new tab is the selected one
		_tabbedPane.setSelectedIndex(_tabbedPane.getTabCount() - 1);

		// Validates the changes in the tabbed pane
		_tabbedPane.validate();
	}

	/**
	 * Puts the selected index to update the status bar, the menu bar and so on.
	 * After that puts the focus in window on the active text edition area,
	 * displays the caret and selects the node in the explorer panel which
	 * corresponds with the new index tab given as a parameter.
	 * 
	 * @param index
	 *            tab index to update.
	 */
	public void updateRelatedComponentsAt(int index) {

		// Sets the selected file editor at the index
		setSelectedFileEditorPanelAt(index);

		// Puts the caret position in its place so it can scroll to it
		getSelectedFileEditorPanel().getActiveTextEditionArea()
				.setCaretPosition(
						getSelectedFileEditorPanel().getActiveTextEditionArea()
								.getCaretPosition());

		// Sets the caret visible
		getSelectedFileEditorPanel().getActiveTextEditionArea().getCaret()
				.setVisible(true);

		// Puts the focus in the active text edition area
		getSelectedFileEditorPanel().getActiveTextEditionArea()
				.requestFocusInWindow();

		// Selects the tree node in the explorer panel
		AcideMainWindow.getInstance().getExplorerPanel()
				.selectTreeNodeFromFileEditor();

		// Updates the static tool bar
		AcideMenuBarToolBar.getInstance().updateStateOfFileButtons();
	}

	/**
	 * Closes a ACIDE - A Configurable IDE file editor manager tab in the
	 * position at the list given as a parameter.
	 * 
	 * @param index
	 *            tab index to be closed.
	 */
	public void removeTab(final int index) {
		_tabbedPanelUI.getCloseButtonAt(index).doClick();
	}

	/**
	 * Returns the file editor panel at the position of the list given as a
	 * parameter.
	 * 
	 * @param index
	 *            position of the file editor to return.
	 * 
	 * @return the editor at the position of the list given as a parameter.
	 */
	public AcideFileEditorPanel getFileEditorPanelAt(int index) {
		if ((index < _tabbedPane.getComponentCount()) && (index >= 0)) {
			return (AcideFileEditorPanel) _tabbedPane.getComponentAt(index);
		} else {
			return null;
		}
	}

	/**
	 * <p>
	 * Returns the file editor panel index from the name given as a parameter.
	 * </p>
	 * <p>
	 * <b>Note:</b> The name contains also the file extension. Otherwise when
	 * the user adds two file editors with the same name but different file
	 * extension, the focus will change in an unappropriated way.
	 * </p>
	 * <p>
	 * This method is invoked only in the
	 * {@link AcideFileEditorPanelDocumentListener}.
	 * </p>
	 * 
	 * @param name
	 *            full name to look for.
	 * @return the file editor panel index from the name given as a parameter.
	 *         Returns -1 if the file editor is not opened in the file editor.
	 */
	public int getFileEditorPanelAt(String name) {

		for (int index = 0; index < _tabbedPane.getTabCount(); index++) {

			// Gets the file editor panel name with extension
			String fileEditorPanelName = ((AcideFileEditorPanel) _tabbedPane
					.getComponentAt(index)).getFileNameWithExtension();

			// If it is the name
			if (fileEditorPanelName != null && name != null
					&& fileEditorPanelName.matches(name))

				// Returns the index
				return index;
		}

		// Returns -1
		return -1;
	}

	/**
	 * Returns the number of the tabbed pane file editor panels.
	 * 
	 * @return the number of the tabbed pane file editor panels.
	 */
	public int getNumberOfFileEditorPanels() {
		return _tabbedPane.getTabCount();
	}

	/**
	 * Sets the selected file editor panel at the position given as a parameter.
	 * 
	 * @param index
	 *            index to select.
	 */
	public void setSelectedFileEditorPanelAt(int index) {

		// Sets the selected index in the tabbed pane
		_tabbedPane.setSelectedIndex(index);

		// Validates the changes in the tabbed pane
		_tabbedPane.validate();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor manager selected file
	 * editor panel index.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager selected file
	 *         editor panel index.
	 */
	public int getSelectedFileEditorPanelIndex() {
		return _tabbedPane.getSelectedIndex();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor manager selected file
	 * editor panel.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager selected file
	 *         editor panel.
	 */
	public AcideFileEditorPanel getSelectedFileEditorPanel() {
		return getFileEditorPanelAt(_tabbedPane.getSelectedIndex());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor manager tabbedPane.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager tabbedPane.
	 */
	public AcideDragAndDropTabbedPane getTabbedPane() {
		return _tabbedPane;
	}

	/**
	 * Returns true if any of the opened file editors is modified and false in
	 * other case.
	 * 
	 * @return true if any of the opened file editors is modified and false in
	 *         other case.
	 */
	public boolean isModified() {

		for (int index = 0; index < _tabbedPane.getTabCount(); index++) {

			if (isRedButton(index))
				return true;
		}

		return false;
	}

	/**
	 * Returns the editor marked as Main File.
	 * 
	 * @return the main editor marked as Main File.
	 */
	public AcideFileEditorPanel getMainFileEditorPanel() {

		for (int index = 0; index < _tabbedPane.getTabCount(); index++) {

			if (getFileEditorPanelAt(index).isMainFile())
				return getFileEditorPanelAt(index);
		}

		return null;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor manager tabbed pane
	 * UI.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager tabbed pane
	 *         UI.
	 * @see AcideFileEditorTabbedPaneUI
	 */
	public AcideFileEditorTabbedPaneUI getTabbedPaneUI() {
		return _tabbedPanelUI;
	}

	/**
	 * Sets the close button to green.
	 */
	public void setGreenButton() {
		_tabbedPanelUI.getCloseButtonAt(_tabbedPane.getSelectedIndex())
				.setGreenCloseButton();
	}

	/**
	 * Returns true if the button is red and false in the other case.
	 * 
	 * @return true if the button is red and false in the other case.
	 */
	public boolean isRedButton() {
		return _tabbedPanelUI.getCloseButtonAt(_tabbedPane.getSelectedIndex())
				.isRedButton();
	}

	/**
	 * Sets the close button to red in the tabbed pane UI at the index given as
	 * a parameter.
	 * 
	 * @param index
	 *            button list index to set.
	 */
	public void setRedButtonAt(int index) {
		_tabbedPanelUI.getCloseButtonAt(index).setRedCloseButton();
	}

	/**
	 * Sets the close button to green in the tabbed pane UI at the index given
	 * as a parameter.
	 * 
	 * @param index
	 *            button list index to set.
	 */
	public void setGreenButtonAt(int index) {
		_tabbedPanelUI.getCloseButtonAt(index).setGreenCloseButton();
	}

	/**
	 * Returns true if the button is red and false in the other case.
	 * 
	 * @param index
	 *            list position of the button.
	 * 
	 * @return True if the button is red and false in the other case.
	 */
	public boolean isRedButton(int index) {
		return _tabbedPanelUI.getCloseButtonAt(index).isRedButton();
	}

	/**
	 * Updates the button icons of the selected file editor in the tabbed pane.
	 */
	public void updatesButtonIcons() {

		if (_tabbedPane.getTabCount() > 0) {

			// Gets the current image icon
			ImageIcon icon = (ImageIcon) _tabbedPane.getIconAt(_tabbedPane
					.getSelectedIndex());

			// Sets the current image icon
			getSelectedFileEditorPanel().setIcon(icon);
		}
	}

	/**
	 * Returns the new file index in the tabbed pane.
	 * 
	 * @return the new file index in the tabbed pane.
	 */
	public int getNewFileIndex() {

		for (int index = _tabbedPane.getTabCount() - 1; index >= 0; index--)
			// If it is new file
			if (getFileEditorPanelAt(index).getAbsolutePath().equals(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s79")))
				// Found it
				return index;
		return -1;
	}

	/**
	 * Returns the log file index in the tabbed pane.
	 * 
	 * @return the log file index in the tabbed pane.
	 */
	public int getLogFileIndex() {

		for (int index = _tabbedPane.getTabCount() - 1; index >= 0; index--)
			// If it is the log file
			if (getFileEditorPanelAt(index).getAbsolutePath().equals("Log"))
				// Found it
				return index;
		return -1;
	}
}
