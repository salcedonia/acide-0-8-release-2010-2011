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
package acide.gui.menuBar.listeners;

import acide.configuration.project.AcideProjectConfiguration;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.utils.AcideUndoManager;

import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * ACIDE - A Configurable IDE menu bar mouse click listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideMenuBarMouseClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {

		// PROJECT MENU
		if (AcideMainWindow.getInstance().getMenu().getProjectMenu().isSelected())
			configureProjectMenu();

		// FILE MENU
		if (AcideMainWindow.getInstance().getMenu().getFileMenu().isSelected())
			configureFileMenu();

		// EDIT MENU
		if (AcideMainWindow.getInstance().getMenu().getEditMenu().isSelected())
			configureEditMenu();

		// Updates the is console panel focused variable
		AcideMainWindow
				.getInstance()
				.getMenu()
				.setIsConsoleFocused(
						AcideMainWindow.getInstance().getConsolePanel().getTextPane()
								.isFocusOwner());
	}

	/**
	 * Configures the edit menu menu item options.
	 */
	public void configureEditMenu() {
		
		// Disables the undo menu item
		AcideMainWindow.getInstance().getMenu().getEditMenu().getUndoMenuItem()
				.setEnabled(false);

		// Disables the redo menu item
		AcideMainWindow.getInstance().getMenu().getEditMenu().getRedoMenuItem()
				.setEnabled(false);

		// Disables the copy menu item
		AcideMainWindow.getInstance().getMenu().getEditMenu().getCopyMenuItem()
				.setEnabled(false);

		// Disables the paste menu item
		AcideMainWindow.getInstance().getMenu().getEditMenu().getPasteMenuItem()
				.setEnabled(false);

		// Disables the cut menu item
		AcideMainWindow.getInstance().getMenu().getEditMenu().getCutMenuItem()
				.setEnabled(false);

		// If can undo
		if (AcideUndoManager.getInstance().canUndo()) {

			// Enables the undo menu item
			AcideMainWindow.getInstance().getMenu().getEditMenu().getUndoMenuItem()
					.setEnabled(true);
		}

		// If can redo
		if (AcideUndoManager.getInstance().canRedo()) {

			// Disables the redo menu item
			AcideMainWindow.getInstance().getMenu().getEditMenu().getRedoMenuItem()
					.setEnabled(true);
		}

		// If the system clipboard is not empty
		if (Toolkit.getDefaultToolkit().getSystemClipboard()
				.getContents(null) != null) {

			// If the console panel does not have the focus in the window
			if (!AcideMainWindow.getInstance().getConsolePanel().getTextPane()
					.hasFocus())

				// Enables the paste menu item
				AcideMainWindow.getInstance().getMenu().getEditMenu().getPasteMenuItem()
						.setEnabled(true);
			else
				// If the caret is after the prompt position
				if (AcideMainWindow.getInstance().getConsolePanel()
					.getTextPane().getSelectionStart() >= AcideMainWindow
					.getInstance().getConsolePanel().getPromptCaretPosition())

				// Enables the paste menu item
				AcideMainWindow.getInstance().getMenu().getEditMenu().getPasteMenuItem()
						.setEnabled(true);
		}

		// Gets the number of file editor panels
		int numberOfFileEditorPanels = AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels();

		// If there are opened editors
		if (numberOfFileEditorPanels > 0) {

			// If the console panel has the focus and there is selected text
			if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
					.hasFocus()
					&& AcideMainWindow.getInstance().getConsolePanel()
							.getTextPane().getSelectedText() != null) {

				// Enables the copy menu item
				AcideMainWindow.getInstance().getMenu().getEditMenu().getCopyMenuItem()
						.setEnabled(true);

				// If the caret position is after the prompt position
				if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.getSelectionStart() >= AcideMainWindow.getInstance()
						.getConsolePanel().getPromptCaretPosition())

					// Enables the cut menu item
					AcideMainWindow.getInstance().getMenu().getEditMenu().getCutMenuItem()
							.setEnabled(true);
			} else
				
				// If the file editor text edition area has the focus and
				// there is something selected
				if (AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel()
					.getActiveTextEditionArea().hasFocus()
					&& AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getActiveTextEditionArea().getSelectedText() != null) {

				// Enables the copy menu item
				AcideMainWindow.getInstance().getMenu().getEditMenu().getCopyMenuItem()
						.setEnabled(true);

				// Enables the cut menu item
				AcideMainWindow.getInstance().getMenu().getEditMenu().getCutMenuItem()
						.setEnabled(true);
			}
		} else {

			// We can copy from the output
			if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
					.getSelectedText() != null) {

				// Enables the copy menu item
				AcideMainWindow.getInstance().getMenu().getEditMenu().getCopyMenuItem()
						.setEnabled(true);

				// If the caret position is after the prompt position
				if (AcideMainWindow.getInstance().getConsolePanel().getTextPane()
						.getSelectionStart() >= AcideMainWindow.getInstance()
						.getConsolePanel().getPromptCaretPosition())

					// Enables the cut menu item
					AcideMainWindow.getInstance().getMenu().getEditMenu().getCutMenuItem()
							.setEnabled(true);
			}
		}
	}

	/**
	 * Configures the file menu menu item options.
	 */
	public void configureFileMenu() {
		
		// Disables the save file menu item
		AcideMainWindow.getInstance().getMenu().getFileMenu().getSaveFileMenuItem()
				.setEnabled(false);

		// Disables the save all files menu item
		AcideMainWindow.getInstance().getMenu().getFileMenu().getSaveAllFilesMenuItem()
				.setEnabled(false);

		// Gets the number of file editor panels
		int numberOfFileEditorPanels = AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels();

		// If there are opened editors
		if (numberOfFileEditorPanels > 0) {

			// If there is a modified opened editor
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.isRedButton())

				// Enables the save file menu item
				AcideMainWindow.getInstance().getMenu().getFileMenu().getSaveFileMenuItem()
						.setEnabled(true);
		}

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Start checking from the last opened editor
		AcideMainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(numberOfFileEditorPanels - 1);

		for (int index = numberOfFileEditorPanels - 1; index >= 0; index--) {

			AcideMainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(index);

			// If the file is modified
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.isRedButton()) {

				// Enables the save all files menu item
				AcideMainWindow.getInstance().getMenu().getFileMenu()
						.getSaveAllFilesMenuItem().setEnabled(true);
			}
		}

		// Restores the original selected editor
		AcideMainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(selectedFileEditorPanelIndex);
	}

	/**
	 * Configures the project menu menu item options.
	 */
	public void configureProjectMenu() {
		
		// Disables the save project menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getSaveProjectMenuItem()
				.setEnabled(false);

		// Disables the remove file menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getRemoveFileMenuItem()
				.setEnabled(false);

		// Disables the delete file menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getDeleteFileMenuItem()
				.setEnabled(false);

		// Disables the set main file menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getSetMainMenuItem()
				.setEnabled(false);

		// Disables the unset main file menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getUnsetMainMenuItem()
				.setEnabled(false);

		// Disables the set compilable file menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getSetCompilableMenuItem()
				.setEnabled(false);

		// Disables the unset compilable menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu()
				.getUnsetCompilableMenuItem().setEnabled(false);

		// Disables the remove folder menu item
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getRemoveFolderMenuItem()
				.setEnabled(false);

		// If the project configuration has been modified
		if (AcideProjectConfiguration.getInstance().isModified())

			// Enables the save project menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getSaveProjectMenuItem().setEnabled(true);

		// Gets the selection path from the explorer tree
		TreePath treeSelection = AcideMainWindow.getInstance().getExplorerPanel().getTree()
				.getSelectionPath();
		
		DefaultMutableTreeNode fileNode;
		AcideProjectFile projectFile;

		// If there is anything selected in the explorer tree
		if (treeSelection != null) {

			// Gets the node from the explorer tree from the tree selection
			fileNode = (DefaultMutableTreeNode) treeSelection.getLastPathComponent();

			// Gets the project file from the file node info
			projectFile = (AcideProjectFile) fileNode.getUserObject();

			// It is not a directory
			if (!projectFile.isDirectory()) {

				// Enables the remove file menu item
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getRemoveFileMenuItem().setEnabled(true);

				// Enables the delete file menu item
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getDeleteFileMenuItem().setEnabled(true);

				if (!projectFile.isMainFile())
					// Enables the set main menu item
					AcideMainWindow.getInstance().getMenu().getProjectMenu()
							.getSetMainMenuItem().setEnabled(true);

				if (projectFile.isMainFile())
					// Enables the unset main menu item
					AcideMainWindow.getInstance().getMenu().getProjectMenu()
							.getUnsetMainMenuItem().setEnabled(true);

				if (!projectFile.isCompilableFile()
						|| (projectFile.isCompilableFile() && projectFile
								.isMainFile()))
					// Enables the set compilable menu item
					AcideMainWindow.getInstance().getMenu().getProjectMenu()
							.getSetCompilableMenuItem().setEnabled(true);

				if (projectFile.isCompilableFile()
						&& !projectFile.isMainFile())
					// Enables the unset compilable menu item
					AcideMainWindow.getInstance().getMenu().getProjectMenu()
							.getUnsetCompilableMenuItem().setEnabled(true);
			} else {

				// Enables the remove folder menu item
				AcideMainWindow.getInstance().getMenu().getProjectMenu()
						.getRemoveFolderMenuItem().setEnabled(true);
			}
		}

		// If it is not the default project
		if (!AcideProjectConfiguration.getInstance()
				.isDefaultProject()) {

			// Gets the number of the file editor panels
			int numberOfFileEditorPanels = AcideMainWindow.getInstance()
					.getFileEditorManager().getNumberOfFileEditorPanels();

			// If there are opened editor
			if (numberOfFileEditorPanels > 0) {

				// SET MAIN FILE
				if (!AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile())
					
					// Enables the set main menu item
					AcideMainWindow.getInstance().getMenu().getProjectMenu()
							.getSetMainMenuItem().setEnabled(true);

				// UNSET MAIN FILE
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile())
					
					// Enables the unset main menu item
					AcideMainWindow.getInstance().getMenu().getProjectMenu()
							.getUnsetMainMenuItem().setEnabled(true);

				// SET COMPILABLE FILE
				if (!AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isCompilableFile()
						|| (AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.isCompilableFile() && AcideMainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().isMainFile()))
					
					// Enables the set compilable menu item
					AcideMainWindow.getInstance().getMenu().getProjectMenu()
							.getSetCompilableMenuItem().setEnabled(true);

				// UNSET COMPILABLE FILE
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isCompilableFile()
						&& !AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().isMainFile())
					
					// Enables the unset compilable menu item
					AcideMainWindow.getInstance().getMenu().getProjectMenu()
							.getUnsetCompilableMenuItem().setEnabled(true);
			}
		}
	}
}
