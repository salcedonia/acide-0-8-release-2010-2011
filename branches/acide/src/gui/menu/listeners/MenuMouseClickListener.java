package gui.menu.listeners;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;

import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/************************************************************************																
 * Menu mouse click listener											
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
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
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {

		// PROJECT MENU
		if (MainWindow.getInstance().getMenu().getProject().isSelected()) {
			
			MainWindow.getInstance().getMenu().getProject().getSaveProject().setEnabled(false);
			MainWindow.getInstance().getMenu().getProject().getRemoveFile().setEnabled(false);
			MainWindow.getInstance().getMenu().getProject().getDeleteFile().setEnabled(false);
			MainWindow.getInstance().getMenu().getProject().getSetMain().setEnabled(false);
			MainWindow.getInstance().getMenu().getProject().getUnsetMain().setEnabled(false);
			MainWindow.getInstance().getMenu().getProject().getSetCompilable().setEnabled(false);
			MainWindow.getInstance().getMenu().getProject().getUnsetCompilable().setEnabled(false);
			MainWindow.getInstance().getMenu().getProject().getRemoveFolder().setEnabled(false);

			if (MainWindow.getInstance().getProjectConfiguration().isModified())
				MainWindow.getInstance().getMenu().getProject().getSaveProject().setEnabled(true);

			TreePath path = MainWindow.getInstance().getExplorer().getTree()
					.getSelectionPath();
			DefaultMutableTreeNode filePath;
			ExplorerFile fc;
			
			if (path != null) {

				filePath = (DefaultMutableTreeNode) path.getLastPathComponent();
				fc = (ExplorerFile) filePath.getUserObject();

				if (!fc.isDirectory()) {
					MainWindow.getInstance().getMenu().getProject().getRemoveFile().setEnabled(true);
					MainWindow.getInstance().getMenu().getProject().getDeleteFile().setEnabled(true);
					if (!fc.isMainFile())
						MainWindow.getInstance().getMenu().getProject().getSetMain().setEnabled(true);
					if (fc.isMainFile())
						MainWindow.getInstance().getMenu().getProject().getUnsetMain().setEnabled(true);
					if (!fc.isCompilableFile() || (fc.isCompilableFile() && fc.isMainFile()))
						MainWindow.getInstance().getMenu().getProject().getSetCompilable()
								.setEnabled(true);
					if (fc.isCompilableFile() && !fc.isMainFile())
						MainWindow.getInstance().getMenu().getProject().getUnsetCompilable()
								.setEnabled(true);
				} else {
					MainWindow.getInstance().getMenu().getProject().getRemoveFolder().setEnabled(true);
				}
			}

			// DEFAULT PROJECT
			if (MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
				
				int editor = MainWindow.getInstance().getEditorManager().getNumEditors();
				if (editor > 0) {
					if (!MainWindow.getInstance().getEditorManager().getSelectedEditor()
							.isMainFile())
						MainWindow.getInstance().getMenu().getProject().getSetMain().setEnabled(true);
					if (MainWindow.getInstance().getEditorManager().getSelectedEditor()
							.isMainFile())
						MainWindow.getInstance().getMenu().getProject().getUnsetMain().setEnabled(true);
					if (!MainWindow.getInstance().getEditorManager().getSelectedEditor()
							.isCompilerFile()
							|| (MainWindow.getInstance().getEditorManager()
									.getSelectedEditor().isCompilerFile() && MainWindow.getInstance()
									.getEditorManager().getSelectedEditor()
									.isMainFile()))
						MainWindow.getInstance().getMenu().getProject().getSetCompilable()
								.setEnabled(true);
					if (MainWindow.getInstance().getEditorManager().getSelectedEditor()
							.isCompilerFile()
							&& !MainWindow.getInstance().getEditorManager()
									.getSelectedEditor().isMainFile())
						MainWindow.getInstance().getMenu().getProject().getUnsetCompilable()
								.setEnabled(true);
				}
			}
		}
		
		// FILE MENU
		if (MainWindow.getInstance().getMenu().getFile().isSelected()) {
			MainWindow.getInstance().getMenu().getFile().getSaveFile().setEnabled(false);
			MainWindow.getInstance().getMenu().getFile().getSaveAllFiles().setEnabled(false);

			int editor = MainWindow.getInstance().getEditorManager().getNumEditors();
			if (editor > 0) {

				if (MainWindow.getInstance().getEditorManager().isRedButton() == true) {
					MainWindow.getInstance().getMenu().getFile().getSaveFile().setEnabled(true);
				}
			}
			
			int eS = MainWindow.getInstance().getEditorManager().getSelectedEditorIndex();
			MainWindow.getInstance().getEditorManager().setSelectedEditorAt(editor - 1);
			for (int i = editor - 1; i >= 0; i--) {
				MainWindow.getInstance().getEditorManager().setSelectedEditorAt(i);
				if (MainWindow.getInstance().getEditorManager().isRedButton() == true) {
					MainWindow.getInstance().getMenu().getFile().getSaveAllFiles().setEnabled(true);
				}
			}
			MainWindow.getInstance().getEditorManager().setSelectedEditorAt(eS);
		}

		// EDIT MENU
		if (MainWindow.getInstance().getMenu().getEdit().isSelected()) {
			MainWindow.getInstance().getMenu().getEdit().getUndo().setEnabled(false);
			MainWindow.getInstance().getMenu().getEdit().getRedo().setEnabled(false);
			MainWindow.getInstance().getMenu().getEdit().getCopy().setEnabled(false);
			MainWindow.getInstance().getMenu().getEdit().getPaste().setEnabled(false);
			MainWindow.getInstance().getMenu().getEdit().getCut().setEnabled(false);

			if (MainWindow.getInstance().getMenu().getEdit().getUndoManager().canUndo()) {
				MainWindow.getInstance().getMenu().getEdit().getUndo().setEnabled(true);
			}
			if (MainWindow.getInstance().getMenu().getEdit().getUndoManager().canRedo()) {
				MainWindow.getInstance().getMenu().getEdit().getRedo().setEnabled(true);
			}
			if (Toolkit.getDefaultToolkit().getSystemClipboard()
					.getContents(null) != null) {
				if (!MainWindow.getInstance().getOutput().getTextComponent().hasFocus())
					MainWindow.getInstance().getMenu().getEdit().getPaste().setEnabled(true);
				else if (MainWindow.getInstance().getOutput().getTextComponent().getSelectionStart() >= MainWindow.getInstance()
						.getOutput().getPromptCaretPosition())
					MainWindow.getInstance().getMenu().getEdit().getPaste().setEnabled(true);
			}
			int editor = MainWindow.getInstance().getEditorManager().getNumEditors();
			if (editor > 0) {
				if (MainWindow.getInstance().getOutput().getTextComponent().hasFocus()
						&& MainWindow.getInstance().getOutput().getTextComponent().getSelectedText() != null) {
					MainWindow.getInstance().getMenu().getEdit().getCopy().setEnabled(true);
					if (MainWindow.getInstance().getOutput().getTextComponent().getSelectionStart() >= MainWindow.getInstance()
							.getOutput().getPromptCaretPosition())
						MainWindow.getInstance().getMenu().getEdit().getCut().setEnabled(true);
				} else if (MainWindow.getInstance().getEditorManager().getSelectedEditor()
						.getEditor().hasFocus()
						&& MainWindow.getInstance().getEditorManager().getSelectedEditor()
								.getEditor().getSelectedText() != null) {
					MainWindow.getInstance().getMenu().getEdit().getCopy().setEnabled(true);
					MainWindow.getInstance().getMenu().getEdit().getCut().setEnabled(true);
				}
			} else {
				// We can copy from the output
				if (MainWindow.getInstance().getOutput().getTextComponent().getSelectedText() != null) {
					MainWindow.getInstance().getMenu().getEdit().getCopy().setEnabled(true);
					if (MainWindow.getInstance().getOutput().getTextComponent().getSelectionStart() >= MainWindow.getInstance()
							.getOutput().getPromptCaretPosition())
						MainWindow.getInstance().getMenu().getEdit().getCut().setEnabled(true);
				}
			}
		}
		if (MainWindow.getInstance().getOutput().getTextComponent().isFocusOwner()) {
			MainWindow.getInstance().getMenu().setIsShellFocus(true);
		} else
			MainWindow.getInstance().getMenu().setIsShellFocus(false);
	}
}
