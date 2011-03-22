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
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.utils.AcideUndoManager;

import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.SwingUtilities;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE file editor manager.
 * 
 * Handles the creation and destruction of the different editor tabs.
 * 
 * @version 0.8
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
	 * Flag that indicates if the file editor has been modified or not.
	 */
	private boolean _isModified;

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

		// MOUSE CLICK LISTENER
		_tabbedPane
				.addMouseListener(new AcideFileEditorManagerMouseClickListener());

		// CHANGE LISTENER
		_tabbedPane
				.addChangeListener(new AcideFileEditorManagerChangeListener());
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor manager tab with the
	 * name, tool tip and type specified.
	 * 
	 * @param fileName
	 *            file name.
	 * @param filePath
	 *            Absolute path of the file displayed on the tab used as its
	 *            tool tip text.
	 * @param isEditable
	 *            indicates if the editor is modifiable or not. It is not
	 *            editable when the Log tab is opened.
	 * @param fileContent
	 *            file content to display in the file editor.
	 * @param fileType
	 *            file type.
	 * @param lexiconConfiguration
	 *            file editor lexicon configuration.
	 * @param grammarConfiguration
	 *            file editor grammar configuration.
	 * 
	 */
	public void addTab(String fileName, String filePath, String fileContent,
			boolean isEditable, AcideProjectFileType fileType,
			AcideLexiconConfiguration lexiconConfiguration,
			AcideGrammarConfiguration grammarConfiguration) {

		// Creates the file editor panel
		AcideFileEditorPanel fileEditorPanel = new AcideFileEditorPanel(
				lexiconConfiguration, grammarConfiguration);

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

		// Sets the absolute path
		fileEditorPanel.setAbsolutePath(filePath);

		// Sets the file content into the file editor panel
		fileEditorPanel.setFileContent(fileContent);

		// Sets if it is editable or not
		fileEditorPanel.setEditable(isEditable);

		// Sets the last change
		fileEditorPanel.setLastChange(new File(filePath).lastModified());

		// Sets the last size
		fileEditorPanel.setLastSize(new File(filePath).length());

		// The new tab is the selected one
		_tabbedPane.setSelectedIndex(_tabbedPane.getTabCount() - 1);

		// Validates the changes in the tabbed pane
		_tabbedPane.validate();

		// Updates the undo manager
		AcideUndoManager.getInstance().update();
	}

	/**
	 * Closes a ACIDE - A Configurable IDE file editor manager tab in the
	 * position at the list given as a parameter.
	 * 
	 * @param index
	 *            tab index to be closed.
	 */
	public void removeTab(int index) {
		_tabbedPanelUI.getCloseButtonAt(index).doClick();
	}

	/**
	 * Returns the file editor panel at the position of the list given as a
	 * parameter.
	 * 
	 * @param index
	 *            position of the editor to return.
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
	 * Returns the number of the tabbed pane file editor panels.
	 * 
	 * @return the number of the tabbed pane file editor panels.
	 */
	public int getNumberOfFileEditorPanels() {
		return _tabbedPane.getTabCount();
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
	 * @param grammarConfiguration
	 *            file editor grammar configuration.
	 */
	public void updatesTabbedPane(String filePath, String fileContent,
			boolean isEditable, AcideProjectFileType fileType,
			int caretPosition, int splitPaneDividerLocation,
			int activeTextEditionArea,
			AcideLexiconConfiguration lexiconConfiguration,
			AcideGrammarConfiguration grammarConfiguration) {

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
			addTab(fileName, filePath, fileContent, isEditable, fileType,
					lexiconConfiguration, grammarConfiguration);

		} else {

			// If it is already opened, sets the focus on it
			setSelectedFileEditorPanelAt(tabPosition);
		}

		// Puts the focus in the active text area, shows the caret and selects
		// the file in the explorer panel
		updatesNewFileEditor(caretPosition, splitPaneDividerLocation,
				activeTextEditionArea);
	}

	/**
	 * Does the following actions: - Sets the active text edition area. - Sets
	 * the split pane divider location. - Puts the caret position and shows it.
	 * - Updates the status message in the status bar. - Selects the file in the
	 * explorer panel which corresponds with the tab.
	 * 
	 * This method is invoked i.e. when a new tab is added to the tab.
	 * 
	 * @param caretPosition
	 *            caret position to set.
	 * @param splitPaneDividerLocation
	 *            split pane divider location to set.
	 * @param activeTextEditionArea
	 *            active text edition area to set.
	 */
	public void updatesNewFileEditor(int caretPosition,
			int splitPaneDividerLocation, int activeTextEditionArea) {

		try {

			// Sets the active text edition area in the position stored in the
			// file editor
			// configuration
			getSelectedFileEditorPanel().setActiveTextEditionAreaIndex(
					activeTextEditionArea);

			// Sets the caret position in the position stored in the file editor
			// configuration
			getSelectedFileEditorPanel().setCaretPosition(caretPosition);

			// Sets the caret visible
			getSelectedFileEditorPanel().setCaretVisible(true);

			// Sets the split pane divider location stored in the file editor
			// configuration
			getSelectedFileEditorPanel().getHorizontalSplitPane()
					.setDividerLocation(splitPaneDividerLocation);

			// Selects the tree node
			AcideMainWindow.getInstance().getExplorerPanel()
					.selectTreeNodeFromFileEditor();

			// Updates the status bar with the selected editor
			AcideMainWindow.getInstance().getStatusBar()
					.updateStatusMessageFromFileEditor();

		} catch (Exception exception) {

			// This is exception is raise when the user didn't save the
			// file when the window was closing.

			// By default it is set to the first position
			setSelectedFileEditorPanelAt(0);
		}
	}

	/**
	 * Does the following actions: - Sets the active text edition area. - Puts
	 * the caret position and shows it. - Updates the status message in the
	 * status bar. - Selects the file in the explorer panel which corresponds
	 * with the tab.
	 * 
	 * This method is invoked i.e. when a file is opened or when a new tab is
	 * added to the tab.
	 * 
	 * @param index
	 *            file editor panel index
	 */
	public void updatesFileEditorAt(final int index) {

		// Sets the selected file editor panel at it
		setSelectedFileEditorPanelAt(index);

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				if (getSelectedFileEditorPanel() != null) {

					// Sets the active text edition area
					getSelectedFileEditorPanel().setActiveTextEditionAreaIndex(
							getSelectedFileEditorPanel()
									.getActiveTextEditionAreaIndex());

					// Sets the caret position in the position stored in the
					// file
					// editor
					// configuration
					getSelectedFileEditorPanel().setCaretPosition(
							getSelectedFileEditorPanel()
									.getActiveTextEditionArea()
									.getCaretPosition());

					// Sets the caret visible
					getSelectedFileEditorPanel().setCaretVisible(true);

					// Selects the tree node
					AcideMainWindow.getInstance().getExplorerPanel()
							.selectTreeNodeFromFileEditor();

					// Updates the status bar with the selected editor
					AcideMainWindow.getInstance().getStatusBar()
							.updateStatusMessageFromFileEditor();
				}
			}
		});
	}

	/**
	 * Sets the selected file editor panel at the position given as a parameter.
	 * 
	 * @param index
	 *            index to select.
	 */
	public void setSelectedFileEditorPanelAt(int index) {
		_tabbedPane.setSelectedIndex(index);
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
	 * Returns the ACIDE - A Configurable IDE file editor manager is modified
	 * flag.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager is modified
	 *         flag.
	 */
	public boolean isModified() {
		return _isModified;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE file editor manager is
	 * modified flag.
	 * 
	 * @param isModified
	 *            new value to set.
	 */
	public void setIsModified(boolean isModified) {
		_isModified = isModified;
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
	 * Sets the green button of the editor at the list position given as a
	 * parameter.
	 * 
	 * @param index
	 *            list position of the button.
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
	 * Returns the file editor panel index from the name given as a parameter.
	 * 
	 * @param name
	 *            name to look for.
	 * @return the file editor panel index from the name given as a parameter.
	 *         Returns -1 if the file editor is not opened in the file editor.
	 */
	public int getIndexOfFileEditorPanelByName(String name) {

		for (int index = 0; index < _tabbedPane.getTabCount(); index++) {

			// Gets the file editor panel name
			String fileEditorPanelName = ((AcideFileEditorPanel) _tabbedPane
					.getComponentAt(index)).getFileName();

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
