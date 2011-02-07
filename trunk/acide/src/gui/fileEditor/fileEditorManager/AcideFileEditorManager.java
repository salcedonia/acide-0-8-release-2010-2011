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

import es.configuration.project.AcideProjectConfiguration;
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
			
			// Sets the tabbed pane UI
			_tabbedPanelUI = new AcideFileEditorTabbedPaneUI();
			_tabbedPane.setUI(_tabbedPanelUI);
			
			// Listeners
			_tabbedPane
					.addMouseListener(new AcideFileEditorManagerMouseClickListener());
			_tabbedPane
					.addChangeListener(new AcideFileEditorManagerChangeListener());

		} catch (RuntimeException exception) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s315"));
			exception.printStackTrace();
		}
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor manager tab with the
	 * name, tool tip and type specified.
	 * 
	 * @param name
	 *            name of the tab.
	 * @param toolTip
	 *            tool tip for the tab.
	 * @param fileType
	 *            type of the file.
	 */
	public void newAcideFileEditorPanel(String name, String toolTip,
			AcideProjectFileType fileType) {

		AcideFileEditorPanel editorPanel = new AcideFileEditorPanel();

		switch (fileType) {
		case NORMAL:
			
			// Adds the tab
			_tabbedPane.addTab(name, null, editorPanel, toolTip);
			
			// It has no icon
			editorPanel.setIcon(null);
			
			// It is not a compilable file
			editorPanel.setCompilableFile(false);
			
			// It is not a main file
			editorPanel.setMainFile(false);
			
			break;

		case MAIN:
			
			// Adds the tab
			_tabbedPane.addTab(name, new ImageIcon(RESOURCE_PATH + "main.png"),
					editorPanel, toolTip);
			
			// Sets the icon for the main files
			editorPanel.setIcon(new ImageIcon(RESOURCE_PATH + "main.png"));
			
			// It is compilable file
			editorPanel.setCompilableFile(true);
			
			// It is main file
			editorPanel.setMainFile(true);
			break;

		case COMPILABLE:
			
			// Adds the tab
			_tabbedPane.addTab(name, new ImageIcon(RESOURCE_PATH
					+ "compilable.png"), editorPanel, toolTip);
			
			// Sets the icon for the compilable files
			editorPanel
					.setIcon(new ImageIcon(RESOURCE_PATH + "compilable.png"));
			
			// It is compilable file
			editorPanel.setCompilableFile(true);
			
			// It is main file
			editorPanel.setMainFile(false);
			
			break;
		}

		// Adds the popup menu
		_tabbedPane
				.addMouseListener(new AcideFileEditorManagerMouseClickListener());

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
	 * Creates a new ACIDE - A Configurable IDE file editor manager tab in the
	 * tabbedPane.
	 * 
	 * @param path
	 *            path of the file to open.
	 * @param textContent
	 *            text type.
	 * @param text
	 *            content of the text to display.
	 * @param modifiable
	 *            indicates if the editor is modifiable or not.
	 * @param fileType
	 *            explorer file type.
	 * @param caretPosition
	 *            caret position.
	 */
	public void newTab(String path, String textContent, String text,
			boolean modifiable, AcideProjectFileType fileType, int caretPosition) {

		// Checks if the file is already opened
		int position = -1;

		for (int index = 0; index < getNumberOfFileEditorPanels(); index++)
			if (getFileEditorPanelAt(index).getAbsolutePath() == textContent)
				position = index;

		// if it is not opened yet
		if (position == -1) {

			File file = new File(textContent);

			// Gets the name
			int lastIndexOfSlash = path.lastIndexOf("\\");
			if (lastIndexOfSlash == -1)
				lastIndexOfSlash = path.lastIndexOf("/");
			String fileName = path.substring(lastIndexOfSlash + 1);

			// Creates the new text editor internal panel
			newAcideFileEditorPanel(fileName, textContent, fileType);

			// Starts from the last opened editor
			int posEditor = getNumberOfFileEditorPanels() - 1;
			getFileEditorPanelAt(posEditor).loadText(text);
			getFileEditorPanelAt(posEditor).setEditable(modifiable);
			getTabbedPane().setSelectedIndex(posEditor);
			getFileEditorPanelAt(posEditor).setAbsolutePath(textContent);
			getFileEditorPanelAt(posEditor).setLastChange(file.lastModified());
			getFileEditorPanelAt(posEditor).setLastSize(file.length());

		} else {

			// If it is already opened, sets the focus on it
			setSelectedFileEditorPanelAt(position);
		}
		try {

			// Sets the caret position in the first position of the text pane
			getSelectedFileEditorPanel().getActiveTextEditionArea()
					.setCaretPosition(caretPosition);
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
	 * Sets the file in the editor as compilable.
	 */
	public void setCompilableFile() {

		if (!MainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().isCompilableFile()
				|| (MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isCompilableFile() && MainWindow
						.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile())) {

			// Default project
			if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Sets the file as compiled
				MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().setCompilableFile(true);

				// If it is already a MAIN FILE
				if (MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile())
					// Removes the main file property
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setMainFile(false);

				// Searches for the file into the project configuration file
				// list
				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index++) {

					// If Exists
					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index)
							.getAbsolutePath()
							.equals(MainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath()))
						// Marks it as COMPILABLE FILE
						AcideProjectConfiguration.getInstance()
								.getFileAt(index).setIsCompilableFile(true);

					// Is it already a MAIN FILE?
					if (AcideProjectConfiguration.getInstance()
							.getFileAt(index).isMainFile())
						// Removes the main property
						AcideProjectConfiguration.getInstance()
								.getFileAt(index).setIsMainFile(false);
				}

				// Puts the COMPILABLE icon in the tab
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getTabbedPane()
						.setIconAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex(),
								new ImageIcon(
										"./resources/icons/editor/compilable.PNG"));

				// Updates the status message in the status bar
				MainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath()
										+ " <COMPILABLE>");
			} else {

				// Not default project

				// Sets it as COMPILABLE FILE
				MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().setCompilableFile(true);

				// If it is MAIN FILE
				if (MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile())
					// Unsets it as MAIN FILE
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setMainFile(false);

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);

				// Updates the status message in the status bar
				MainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getAbsolutePath()
										+ " <COMPILABLE>");

				// Search for the file into the project configuration file list
				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index++) {

					// If exists
					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index)
							.getAbsolutePath()
							.equals(MainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath())) {

						// Marks it as COMPILABLE FILE
						AcideProjectConfiguration.getInstance()
								.getFileAt(index).setIsCompilableFile(true);

						// It is MAIN FILE
						if (AcideProjectConfiguration.getInstance()
								.getFileAt(index).isMainFile())

							// Removes the main file property
							AcideProjectConfiguration.getInstance()
									.getFileAt(index).setIsMainFile(false);

						// Put the COMPILABLE icon in the tab
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.getTabbedPane()
								.setIconAt(
										MainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex(),
										new ImageIcon(
												"./resources/icons/editor/compilable.PNG"));
					}
				}
			}
		}
	}

	/**
	 * Unsets the file in the editor as compilable.
	 */
	public void unsetCompilableFile() {

		// If it is COMPILABLE FILE and not MAIN FILE
		if (MainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().isCompilableFile()
				&& !MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile()) {

			// Sets COMPILER FILE to false
			MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().setCompilableFile(false);

			// Updates the status message in the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setStatusMessage(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath());

			// Quits the icon tab
			MainWindow
					.getInstance()
					.getFileEditorManager()
					.getTabbedPane()
					.setIconAt(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanelIndex(), null);

			// Not default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);

				// Searches for the file into the project configuration file
				// list
				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index++) {

					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index)
							.getAbsolutePath()
							.equals(MainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath()))
						// Sets the COMPILABLE FILE as false
						AcideProjectConfiguration.getInstance()
								.getFileAt(index).setIsCompilableFile(false);
				}
			}
		}
	}

	/**
	 * Sets a file of the editor as a main file.
	 */
	public void setMainFile() {

		// IF it is not MAIN FILE
		if (!MainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().isMainFile()) {

			// Removes the previous MAIN FILE
			for (int index = 0; index < MainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

				// Finds the previous MAIN FILE
				if (MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(index).isMainFile()) {

					// Sets MAIN FILE as false
					MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index).setMainFile(false);

					// Sets COMPILER FILE as false
					MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(index)
							.setCompilableFile(false);

					// Updates the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									MainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(index)
											.getAbsolutePath());

					// Removes the tab icon
					MainWindow.getInstance().getFileEditorManager()
							.getTabbedPane().setIconAt(index, null);
				}
			}

			// Sets MAIN FILE as true
			MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().setMainFile(true);

			// Sets COMPILER FILE as true
			MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().setCompilableFile(true);

			// Updates the status message in the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setStatusMessage(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath()
									+ " <MAIN>");

			// Not default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Updates the file into the project configuration
				for (int index1 = 0; index1 < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index1++) {

					// If exists
					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index1)
							.getAbsolutePath()
							.equals(MainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath())) {

						for (int index2 = 0; index2 < AcideProjectConfiguration
								.getInstance().getFileListSize(); index2++) {

							// MAIN FILE?
							if (AcideProjectConfiguration.getInstance()
									.getFileAt(index2).isMainFile()) {

								// Sets MAIN FILE to false
								AcideProjectConfiguration.getInstance()
										.getFileAt(index2).setIsMainFile(false);

								// Sets COMPILABLE FILE to false
								AcideProjectConfiguration.getInstance()
										.getFileAt(index2)
										.setIsCompilableFile(false);

								for (int position = 0; position < MainWindow
										.getInstance().getFileEditorManager()
										.getNumberOfFileEditorPanels(); position++) {

									if (MainWindow
											.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(position)
											.getAbsolutePath()
											.equals(AcideProjectConfiguration
													.getInstance()
													.getFileAt(index2)
													.getAbsolutePath()))

										// Removes the icon from the tab
										MainWindow.getInstance()
												.getFileEditorManager()
												.getTabbedPane()
												.setIconAt(position, null);
								}
							}
						}

						// Sets it as MAIN FILE
						AcideProjectConfiguration.getInstance()
								.getFileAt(index1).setIsMainFile(true);

						// Sets it as COMPILABLE FILE
						AcideProjectConfiguration.getInstance()
								.getFileAt(index1).setIsCompilableFile(true);

						// The project has been modified
						AcideProjectConfiguration.getInstance().setIsModified(
								true);

						// Puts the tab icon
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.getTabbedPane()
								.setIconAt(
										MainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex(),
										new ImageIcon(
												"./resources/icons/editor/main.PNG"));
					}
				}
			}
		}
	}

	/**
	 * Unsets the file in the editor as a main file.
	 */
	public void unsetMainFile() {

		// If it is MAIN FILE
		if (MainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().isMainFile()) {

			// Sets the MAIN FILE as false
			MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().setMainFile(false);

			// Updates the status message in the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setStatusMessage(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath());

			// Quits the tab icon
			MainWindow
					.getInstance()
					.getFileEditorManager()
					.getTabbedPane()
					.setIconAt(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanelIndex(), null);

			// Not default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);

				// Searches for the file into the project configuration file
				// list
				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index++) {

					// If exists
					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index)
							.getAbsolutePath()
							.equals(MainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath()))
						// Sets MAIN FILE as false
						AcideProjectConfiguration.getInstance()
								.getFileAt(index).setIsMainFile(false);
				}
			}
		}
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
