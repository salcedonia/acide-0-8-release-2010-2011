package gui.menuBar.listeners;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/************************************************************************
 * Menu mouse click listener.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
 * 
 ************************************************************************ 
 * @author <ul>
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan José Ortiz Sánchez
 *         </ul>
 *         <ul>
 *         Delfín Rupérez Cañas
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Martín Lázaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo Gómez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 * @see MouseAdapter
 ***********************************************************************/
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
			if (MainWindow.getInstance().getProjectConfiguration().isModified())

				// Enables the save project menu item
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().setEnabled(true);

			TreePath path = MainWindow.getInstance().getExplorerPanel().getTree()
					.getSelectionPath();
			DefaultMutableTreeNode filePath;
			ExplorerFile explorerFile;

			if (path != null) {

				filePath = (DefaultMutableTreeNode) path.getLastPathComponent();

				// Gets the node info
				explorerFile = (ExplorerFile) filePath.getUserObject();

				// It is not a directory
				if (!explorerFile.isDirectory()) {

					// Enables the remove file menu item
					MainWindow.getInstance().getMenu().getProject()
							.getRemoveFile().setEnabled(true);

					// Enables the delete file menu item
					MainWindow.getInstance().getMenu().getProject()
							.getDeleteFile().setEnabled(true);

					// Enables the set main menu item
					if (!explorerFile.isMainFile())
						// Enables the set main menu item
						MainWindow.getInstance().getMenu().getProject()
								.getSetMain().setEnabled(true);

					if (explorerFile.isMainFile())
						// Enables the unset main menu item
						MainWindow.getInstance().getMenu().getProject()
								.getUnsetMain().setEnabled(true);

					if (!explorerFile.isCompilableFile()
							|| (explorerFile.isCompilableFile() && explorerFile
									.isMainFile()))
						// Enables the set compilable menu item
						MainWindow.getInstance().getMenu().getProject()
								.getSetCompilable().setEnabled(true);

					if (explorerFile.isCompilableFile()
							&& !explorerFile.isMainFile())
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
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// Gets the number of the editors
				int numberEditors = MainWindow.getInstance()
						.getFileEditorManager().getNumFileEditorPanels();

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
							.getSelectedFileEditorPanel().isCompilerFile()
							|| (MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel()
									.isCompilerFile() && MainWindow
									.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().isMainFile()))
						MainWindow.getInstance().getMenu().getProject()
								.getSetCompilable().setEnabled(true);

					// UNSET COMPILABLE FILE
					if (MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().isCompilerFile()
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
					.getNumFileEditorPanels();

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

				if (!MainWindow.getInstance().getOutputPanel().getTextComponent()
						.hasFocus())

					// Enables the paste menu item
					MainWindow.getInstance().getMenu().getEdit().getPaste()
							.setEnabled(true);
				else if (MainWindow.getInstance().getOutputPanel()
						.getTextComponent().getSelectionStart() >= MainWindow
						.getInstance().getOutputPanel().getPromptCaretPosition())

					// Enables the paste menu item
					MainWindow.getInstance().getMenu().getEdit().getPaste()
							.setEnabled(true);
			}

			// Gets the number of opened editors
			int editor = MainWindow.getInstance().getFileEditorManager()
					.getNumFileEditorPanels();

			// If there are opened editors
			if (editor > 0) {

				if (MainWindow.getInstance().getOutputPanel().getTextComponent()
						.hasFocus()
						&& MainWindow.getInstance().getOutputPanel()
								.getTextComponent().getSelectedText() != null) {

					// Enables the copy menu item
					MainWindow.getInstance().getMenu().getEdit().getCopy()
							.setEnabled(true);

					if (MainWindow.getInstance().getOutputPanel().getTextComponent()
							.getSelectionStart() >= MainWindow.getInstance()
							.getOutputPanel().getPromptCaretPosition())

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
				if (MainWindow.getInstance().getOutputPanel().getTextComponent()
						.getSelectedText() != null) {

					// Enables the copy menu item
					MainWindow.getInstance().getMenu().getEdit().getCopy()
							.setEnabled(true);

					if (MainWindow.getInstance().getOutputPanel().getTextComponent()
							.getSelectionStart() >= MainWindow.getInstance()
							.getOutputPanel().getPromptCaretPosition())

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
				.setIsShellFocus(
						MainWindow.getInstance().getOutputPanel().getTextComponent()
								.isFocusOwner());
	}
}
