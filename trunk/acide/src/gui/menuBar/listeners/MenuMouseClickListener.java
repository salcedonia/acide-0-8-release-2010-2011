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
package gui.menuBar.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * Menu mouse click listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class MenuMouseClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {

		// PROJECT MENU
		if (MainWindow.getInstance().getMenu().getProject().isSelected()) {

			// Disables the save project menu item
			MainWindow.getInstance().getMenu().getProject().getSaveProject()
					.setEnabled(false);

			// Disables the remove file menu item
			MainWindow.getInstance().getMenu().getProject().getRemoveFile()
					.setEnabled(false);

			// Disables the delete file menu item
			MainWindow.getInstance().getMenu().getProject().getDeleteFile()
					.setEnabled(false);

			// Disables the set main file menu item
			MainWindow.getInstance().getMenu().getProject().getSetMain()
					.setEnabled(false);

			// Disables the unset main file menu item
			MainWindow.getInstance().getMenu().getProject().getUnsetMain()
					.setEnabled(false);

			// Disables the set compilable file menu item
			MainWindow.getInstance().getMenu().getProject().getSetCompilable()
					.setEnabled(false);

			// Disables the unset compilable menu item
			MainWindow.getInstance().getMenu().getProject()
					.getUnsetCompilable().setEnabled(false);

			// Disables the remove folder menu item
			MainWindow.getInstance().getMenu().getProject().getRemoveFolder()
					.setEnabled(false);

			// If the project configuration has been modified
			if (AcideProjectConfiguration.getInstance().isModified())

				// Enables the save project menu item
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().setEnabled(true);

			TreePath path = MainWindow.getInstance().getExplorerPanel().getTree()
					.getSelectionPath();
			DefaultMutableTreeNode filePath;
			AcideProjectFile projectFile;

			if (path != null) {

				filePath = (DefaultMutableTreeNode) path.getLastPathComponent();

				// Gets the node info
				projectFile = (AcideProjectFile) filePath.getUserObject();

				// It is not a directory
				if (!projectFile.isDirectory()) {

					// Enables the remove file menu item
					MainWindow.getInstance().getMenu().getProject()
							.getRemoveFile().setEnabled(true);

					// Enables the delete file menu item
					MainWindow.getInstance().getMenu().getProject()
							.getDeleteFile().setEnabled(true);

					// Enables the set main menu item
					if (!projectFile.isMainFile())
						// Enables the set main menu item
						MainWindow.getInstance().getMenu().getProject()
								.getSetMain().setEnabled(true);

					if (projectFile.isMainFile())
						// Enables the unset main menu item
						MainWindow.getInstance().getMenu().getProject()
								.getUnsetMain().setEnabled(true);

					if (!projectFile.isCompilableFile()
							|| (projectFile.isCompilableFile() && projectFile
									.isMainFile()))
						// Enables the set compilable menu item
						MainWindow.getInstance().getMenu().getProject()
								.getSetCompilable().setEnabled(true);

					if (projectFile.isCompilableFile()
							&& !projectFile.isMainFile())
						// Enables the unset compilable menu item
						MainWindow.getInstance().getMenu().getProject()
								.getUnsetCompilable().setEnabled(true);
				} else {

					// Enables the remove folder menu item
					MainWindow.getInstance().getMenu().getProject()
							.getRemoveFolder().setEnabled(true);
				}
			}

			// Default project
			if (AcideProjectConfiguration.getInstance()
					.isDefaultProject()) {

				// Gets the number of the editors
				int numberEditors = MainWindow.getInstance()
						.getFileEditorManager().getNumberOfFileEditorPanels();

				// If there are opened editor
				if (numberEditors > 0) {

					// SET MAIN FILE
					if (!MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().isMainFile())
						MainWindow.getInstance().getMenu().getProject()
								.getSetMain().setEnabled(true);

					// UNSET MAIN FILE
					if (MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().isMainFile())
						MainWindow.getInstance().getMenu().getProject()
								.getUnsetMain().setEnabled(true);

					// SET COMPILABLE FILE
					if (!MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().isCompilableFile()
							|| (MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel()
									.isCompilableFile() && MainWindow
									.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().isMainFile()))
						MainWindow.getInstance().getMenu().getProject()
								.getSetCompilable().setEnabled(true);

					// UNSET COMPILABLE FILE
					if (MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().isCompilableFile()
							&& !MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().isMainFile())
						MainWindow.getInstance().getMenu().getProject()
								.getUnsetCompilable().setEnabled(true);
				}
			}
		}

		// FILE MENU
		if (MainWindow.getInstance().getMenu().getFile().isSelected()) {

			// Disables the save file menu item
			MainWindow.getInstance().getMenu().getFile().getSaveFile()
					.setEnabled(false);

			// Disables the save all files menu item
			MainWindow.getInstance().getMenu().getFile().getSaveAllFiles()
					.setEnabled(false);

			// Gets the number of editors
			int numberEditors = MainWindow.getInstance().getFileEditorManager()
					.getNumberOfFileEditorPanels();

			// If there are opened editors
			if (numberEditors > 0) {

				// If there is a modified opened editor
				if (MainWindow.getInstance().getFileEditorManager()
						.isRedButton())

					// Enables the save file menu item
					MainWindow.getInstance().getMenu().getFile().getSaveFile()
							.setEnabled(true);
			}

			// Gets the selected editor index
			int selectedEditorIndex = MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// Start checking from the last opened editor
			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(numberEditors - 1);

			for (int index = numberEditors - 1; index >= 0; index--) {

				MainWindow.getInstance().getFileEditorManager()
						.setSelectedFileEditorPanelAt(index);

				// If the file is modified
				if (MainWindow.getInstance().getFileEditorManager()
						.isRedButton()) {

					// Enables the save all files menu item
					MainWindow.getInstance().getMenu().getFile()
							.getSaveAllFiles().setEnabled(true);
				}
			}

			// Restores the original selected editor
			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(selectedEditorIndex);
		}

		// EDIT MENU
		if (MainWindow.getInstance().getMenu().getEdit().isSelected()) {

			// Disables the undo menu item
			MainWindow.getInstance().getMenu().getEdit().getUndo()
					.setEnabled(false);

			// Disables the redo menu item
			MainWindow.getInstance().getMenu().getEdit().getRedo()
					.setEnabled(false);

			// Disables the copy menu item
			MainWindow.getInstance().getMenu().getEdit().getCopy()
					.setEnabled(false);

			// Disables the paste menu item
			MainWindow.getInstance().getMenu().getEdit().getPaste()
					.setEnabled(false);

			// Disables the cut menu item
			MainWindow.getInstance().getMenu().getEdit().getCut()
					.setEnabled(false);

			// UNDO MENU ITEM
			if (AcideUndoRedoManager.getInstance().canUndo()) {

				// Enables the undo menu item
				MainWindow.getInstance().getMenu().getEdit().getUndo()
						.setEnabled(true);
			}

			// REDO MENU ITEM
			if (AcideUndoRedoManager.getInstance().canRedo()) {

				// Disables the redo menu item
				MainWindow.getInstance().getMenu().getEdit().getRedo()
						.setEnabled(true);
			}

			// If the system clipboard is not empty
			if (Toolkit.getDefaultToolkit().getSystemClipboard()
					.getContents(null) != null) {

				if (!MainWindow.getInstance().getConsolePanel().getTextPane()
						.hasFocus())

					// Enables the paste menu item
					MainWindow.getInstance().getMenu().getEdit().getPaste()
							.setEnabled(true);
				else if (MainWindow.getInstance().getConsolePanel()
						.getTextPane().getSelectionStart() >= MainWindow
						.getInstance().getConsolePanel().getPromptCaretPosition())

					// Enables the paste menu item
					MainWindow.getInstance().getMenu().getEdit().getPaste()
							.setEnabled(true);
			}

			// Gets the number of opened editors
			int editor = MainWindow.getInstance().getFileEditorManager()
					.getNumberOfFileEditorPanels();

			// If there are opened editors
			if (editor > 0) {

				if (MainWindow.getInstance().getConsolePanel().getTextPane()
						.hasFocus()
						&& MainWindow.getInstance().getConsolePanel()
								.getTextPane().getSelectedText() != null) {

					// Enables the copy menu item
					MainWindow.getInstance().getMenu().getEdit().getCopy()
							.setEnabled(true);

					if (MainWindow.getInstance().getConsolePanel().getTextPane()
							.getSelectionStart() >= MainWindow.getInstance()
							.getConsolePanel().getPromptCaretPosition())

						// Enables the cut menu item
						MainWindow.getInstance().getMenu().getEdit().getCut()
								.setEnabled(true);
				} else if (MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getActiveTextEditionArea().hasFocus()
						&& MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getActiveTextEditionArea().getSelectedText() != null) {

					// Enables the copy menu item
					MainWindow.getInstance().getMenu().getEdit().getCopy()
							.setEnabled(true);

					// Enables the cut menu item
					MainWindow.getInstance().getMenu().getEdit().getCut()
							.setEnabled(true);
				}
			} else {

				// We can copy from the output
				if (MainWindow.getInstance().getConsolePanel().getTextPane()
						.getSelectedText() != null) {

					// Enables the copy menu item
					MainWindow.getInstance().getMenu().getEdit().getCopy()
							.setEnabled(true);

					if (MainWindow.getInstance().getConsolePanel().getTextPane()
							.getSelectionStart() >= MainWindow.getInstance()
							.getConsolePanel().getPromptCaretPosition())

						// Enables the cut menu item
						MainWindow.getInstance().getMenu().getEdit().getCut()
								.setEnabled(true);
				}
			}
		}

		// Updates the is shell focus
		MainWindow
				.getInstance()
				.getMenu()
				.setIsConsoleFocused(
						MainWindow.getInstance().getConsolePanel().getTextPane()
								.isFocusOwner());
	}
}
