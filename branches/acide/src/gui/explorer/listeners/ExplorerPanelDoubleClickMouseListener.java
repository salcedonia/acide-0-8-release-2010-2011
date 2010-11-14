package gui.explorer.listeners;

import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.editor.editorManager.EditorManager;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.Document;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.undo.UndoableEdit;

/************************************************************************																
 * Explorer panel double click listener										
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
public class ExplorerPanelDoubleClickMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// DOUBLE CLICK
		if (mouseEvent.getClickCount() > 1) {

			// GET THE SELECTED NODE
			TreePath selectedNode = MainWindow.getInstance().getExplorer().getTree().getPathForLocation(mouseEvent.getX(),
					mouseEvent.getY());

			// IF SOMETHING IS SELECTED
			if (selectedNode != null) {

				// Updates the status bar
				String filePath = selectedNode.getLastPathComponent()
						.toString();
				MainWindow.getInstance().getStatusBar()
						.setMessage(filePath);

				DefaultMutableTreeNode defaultMutableTreeNode = (DefaultMutableTreeNode) selectedNode
						.getLastPathComponent();
				Object node = defaultMutableTreeNode.getUserObject();
				ExplorerFile explorerFile = (ExplorerFile) node;

				// SEARCH THE FILE INTO THE EDITOR
				int posEditor = -1;
				for (int pos = 0; pos < MainWindow.getInstance()
						.getEditorManager().getNumEditors(); pos++) {
					if (MainWindow.getInstance().getEditorManager()
							.getEditorAt(pos).getAbsolutePath()
							.equals(explorerFile.getPath())) {
						posEditor = pos;
					}
				}

				// IF IT IS NOT OPENED
				if (posEditor == -1) {

					// NOT A DIRECTORY
					if (!explorerFile.isDirectory()) {

						TextFile textFile = new TextFile();
						EditorManager editorBuilder = MainWindow
								.getInstance().getEditorManager();

						String fileContent = "";
						fileContent = textFile.load(explorerFile.getPath());

						// SEARCH FOR THE FILE INTO THE PROJECT OPENED FILES
						// LIST
						posEditor = -1;
						for (int pos = 0; pos < MainWindow.getInstance()
								.getProjectConfiguration()
								.getNumFilesFromList(); pos++) {
							if (MainWindow.getInstance()
									.getProjectConfiguration()
									.getFileAt(pos).getPath()
									.equals(explorerFile.getPath()))
								posEditor = pos;
						}

						
						// Checks the type
						int t = 0;

						// IS COMPILABLE FILE?
						if (MainWindow.getInstance()
								.getProjectConfiguration()
								.getFileAt(posEditor).isCompilableFile())

							// IS MAIN FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration()
									.getFileAt(posEditor).isMainFile()){

								t = 1;
								
								// Updates the status bar
								MainWindow
										.getInstance()
										.getStatusBar()
										.setMessage(
												MainWindow
														.getInstance()
														.getProjectConfiguration()
														.getFileAt(posEditor)
														.getPath()
														+ " <MAIN>");
							}
							else{
								
								t = 2;
								// Updates the status bar
								MainWindow
									.getInstance()
									.getStatusBar()
									.setMessage(
											MainWindow
													.getInstance()
													.getProjectConfiguration()
													.getFileAt(posEditor)
													.getPath()
													+ " <COMPILABLE>");
							}
						else{
							
							t = 0;
							// Updates the status bar
							MainWindow
								.getInstance()
								.getStatusBar()
								.setMessage(
										MainWindow
												.getInstance()
												.getProjectConfiguration()
												.getFileAt(posEditor)
												.getPath());	
						}
						
						// OPEN THE TAB IN THE EDITOR
						editorBuilder.newTab(explorerFile.getPath(),
								explorerFile.getPath(), fileContent, true,
								t);

						// UNDO REDO
						MainWindow.getInstance().getMenu().enableFileMenu();
						MainWindow.getInstance().getMenu().enableEditMenu();
						int selectedEditorIndex = MainWindow.getInstance()
								.getEditorManager()
								.getSelectedEditorIndex();
						Document document = MainWindow.getInstance()
								.getEditorManager()
								.getEditorAt(selectedEditorIndex)
								.getEditor().getDocument();
						document.addUndoableEditListener(new UndoableEditListener() {
							/*
							 * (non-Javadoc)
							 * 
							 * @see javax.swing.event.UndoableEditListener#
							 * undoableEditHappened
							 * (javax.swing.event.UndoableEditEvent)
							 */
							@Override
							public void undoableEditHappened(
									UndoableEditEvent undoableEditEvent) {

								UndoableEdit edit = undoableEditEvent
										.getEdit();
								if (edit instanceof DefaultDocumentEvent
										&& ((DefaultDocumentEvent) edit)
												.getType() == DefaultDocumentEvent.EventType.CHANGE) {
									return;
								} else {
									MainWindow
											.getInstance()
											.getMenu()
											.getEdit()
											.getUndoManager()
											.addEdit(
													undoableEditEvent
															.getEdit());
								}
							}
						});

						// Sets the caret in the first position of the editor OF THE ACTIVE
						// EDITOR
						selectedEditorIndex = MainWindow.getInstance()
								.getEditorManager()
								.getSelectedEditorIndex();
						MainWindow.getInstance().getEditorManager()
								.getEditorAt(selectedEditorIndex)
								.getEditor().setCaretPosition(0);

						// SET THE FILE STATE IN THE PROJECT CONFIGURATION
						for (int pos = 0; pos < MainWindow.getInstance()
								.getProjectConfiguration()
								.getFileListSize(); pos++) {
							if (MainWindow.getInstance()
									.getProjectConfiguration()
									.getFileAt(pos).getPath()
									.equals(explorerFile.getPath())) {
								MainWindow.getInstance()
										.getProjectConfiguration()
										.getFileAt(pos).setIsOpened(true);
							}
						}

						// THE PROJECT HAS BEEN MODIFIED
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);
					}
				} else {

					// IF IT IS ALREADY OPEN

					// SET THE FOCUS ON IT
					MainWindow.getInstance().getEditorManager()
							.setSelectedEditorAt(posEditor);
				}
			}
		}
	}
}
