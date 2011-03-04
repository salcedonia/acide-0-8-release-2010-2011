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
package gui.fileEditor.fileEditorManager;

import es.project.AcideProjectFileType;
import gui.fileEditor.fileEditorManager.listeners.AcideFileEditorManagerChangeListener;
import gui.fileEditor.fileEditorManager.listeners.AcideFileEditorManagerMouseClickListener;
import gui.fileEditor.fileEditorManager.utils.gui.DragAndDropTabbedPane;
import gui.fileEditor.fileEditorManager.utils.logic.UI.AcideFileEditorTabbedPaneUI;
import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.io.File;

import javax.swing.ImageIcon;

import language.AcideLanguageManager;
import operations.log.AcideLog;

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
	private DragAndDropTabbedPane _tabbedPane;
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
			_tabbedPane = new DragAndDropTabbedPane();

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
	 */
	public void addTab(String fileName, String filePath, String fileContent,
			boolean isEditable, AcideProjectFileType fileType) {

		// Creates the file editor panel
		AcideFileEditorPanel fileEditorPanel = new AcideFileEditorPanel();

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
	}

	/**
	 * Closes a ACIDE - A Configurable IDE file editor manager tab in the
	 * position at the list given as a parameter.
	 * 
	 * @param pos
	 *            tab position to be closed.
	 */
	public void removeTab(int pos) {
		_tabbedPanelUI.getCloseButtonAt(pos).doClick();
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
	 */
	public void updatesTabbedPane(String filePath, String fileContent,
			boolean isEditable, AcideProjectFileType fileType,
			int caretPosition, int splitPaneDividerLocation) {

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
			addTab(fileName, filePath, fileContent, isEditable, fileType);

		} else {

			// If it is already opened, sets the focus on it
			setSelectedFileEditorPanelAt(tabPosition);
		}

		try {

			// Sets the caret position in the position stored in the file editor
			// configuration
			getSelectedFileEditorPanel().getActiveTextEditionArea()
					.setCaretPosition(caretPosition);

			// Sets the split pane divider location stored in the file editor
			// configuration
			getSelectedFileEditorPanel().getHorizontalSplitPane()
					.setDividerLocation(splitPaneDividerLocation);

		} catch (Exception exception) {

			// This is exception is raise when the user didn't save the
			// file when the window was closing.

			// By default it is set to the first position
			setSelectedFileEditorPanelAt(0);
		}
	}

	/**
	 * Sets the selected file editor panel at the position given as a parameter.
	 * 
	 * @param position
	 *            position to select.
	 */
	public void setSelectedFileEditorPanelAt(int position) {
		_tabbedPane.setSelectedIndex(position);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor manager selected file
	 * editor panel index.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager selected file
	 *         editor panel index.
	 */
	public int getSelectedFileEditorPanelIndex() {
		return getTabbedPane().getSelectedIndex();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor manager selected file
	 * editor panel.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager selected file
	 *         editor panel.
	 */
	public AcideFileEditorPanel getSelectedFileEditorPanel() {
		return getFileEditorPanelAt(getTabbedPane().getSelectedIndex());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor manager tabbedPane.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor manager tabbedPane.
	 */
	public DragAndDropTabbedPane getTabbedPane() {
		return _tabbedPane;
	}

	/**
	 * Returns the editor marked as Main File.
	 * 
	 * @return the main editor marked as Main File.
	 */
	public AcideFileEditorPanel getMainEditor() {

		for (int index = 0; index < getNumberOfFileEditorPanels(); index++) {

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
		MainWindow
				.getInstance()
				.getFileEditorManager()
				.getTabbedPaneUI()
				.getCloseButtonAt(
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex())
				.setGreenCloseButton();
	}

	/**
	 * Returns true if the button is red and false in the other case.
	 * 
	 * @return true if the button is red and false in the other case.
	 */
	public boolean isRedButton() {
		return MainWindow
				.getInstance()
				.getFileEditorManager()
				.getTabbedPaneUI()
				.getCloseButtonAt(
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex())
				.isRedButton();
	}

	/**
	 * Sets the green button of the editor at the list position given as a
	 * parameter.
	 * 
	 * @param position
	 *            list position of the button.
	 */
	public void setGreenButtonAt(int position) {
		MainWindow.getInstance().getFileEditorManager().getTabbedPaneUI()
				.getCloseButtonAt(position).setGreenCloseButton();
	}

	/**
	 * Returns true if the button is red and false in the other case.
	 * 
	 * @param position
	 *            list position of the button.
	 * 
	 * @return True if the button is red and false in the other case.
	 */
	public boolean isRedButton(int position) {
		return MainWindow.getInstance().getFileEditorManager()
				.getTabbedPaneUI().getCloseButtonAt(position).isRedButton();
	}

	/**
	 * Updates the button icons of the selected file editor in the tabbed pane.
	 */
	public void updatesButtonIcons() {

		if (MainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// Gets the selected editor index
			int selectedEditorIndex = MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// Gets the current image icon
			ImageIcon icon = (ImageIcon) MainWindow.getInstance()
					.getFileEditorManager().getTabbedPane()
					.getIconAt(selectedEditorIndex);

			// Sets the current image icon
			MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().setIcon(icon);
		}
	}

	/**
	 * Returns the file editor panel index from the name given as a parameter.
	 * 
	 * @param name
	 *            name to look for.
	 * @return the file editor panel index from the name given as a parameter.
	 */
	public int getIndexOfFileEditorPanel(String name) {

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
}
