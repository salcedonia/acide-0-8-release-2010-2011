package gui.menuBar.projectMenu.listeners;

import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.Document;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.undo.UndoableEdit;

import operations.factory.AcideIOFactory;
import operations.log.AcideLog;

/************************************************************************																
 * Add file menu item listener.
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
 * @see ActionListener																													
 ***********************************************************************/
public class AddFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Builds the text file
		TextFile textFile = AcideIOFactory.getInstance().buildFile();

		try {

			// Builds the file content
			String fileContent = "";
			fileContent = textFile.read();

			if (fileContent != null) {

				TreePath path = MainWindow.getInstance().getExplorer().getTree()
						.getSelectionPath();
				DefaultMutableTreeNode filePath;
				ExplorerFile fc;

				if (path != null) {
					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					fc = (ExplorerFile) filePath.getUserObject();

					if (!fc.isDirectory()) {
						filePath = MainWindow.getInstance().getExplorer().getRoot()
								.getNextNode();
						fc = (ExplorerFile) filePath.getUserObject();
					}

				} else {
					filePath = MainWindow.getInstance().getExplorer().getRoot()
							.getNextNode();
					fc = (ExplorerFile) filePath.getUserObject();
				}

				// Gets the file name
				String name = "";
				int index = fileContent.lastIndexOf("\\");
				if (index == -1)
					index = fileContent.lastIndexOf("/");
				name = fileContent.substring(index + 1, fileContent.length());

				ExplorerFile explorerFile = new ExplorerFile();
				explorerFile.setPath(fileContent);
				explorerFile.setName(name);
				explorerFile.setParent(fc.getName());

				boolean isAdded = false;
				for (int i = 0; i < MainWindow.getInstance().getProjectConfiguration()
						.getNumFilesFromList(); i++) {
					if (MainWindow.getInstance().getProjectConfiguration().getFileAt(i)
							.getPath().equals(explorerFile.getPath())) {
						isAdded = true;
					}
				}

				if (!isAdded) {

					MainWindow.getInstance().getProjectConfiguration().addFile(
							explorerFile);
					MainWindow.getInstance().getProjectConfiguration().getFileAt(
							MainWindow.getInstance().getProjectConfiguration()
									.getNumFilesFromList() - 1)
							.setIsOpened(true);

					DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
							explorerFile);
					defaultMutableTreeNode.setAllowsChildren(false);
					filePath.add(defaultMutableTreeNode);
					MainWindow.getInstance().validate();
					MainWindow.getInstance().repaint();
					MainWindow.getInstance().getExplorer().getTreeModel().reload();
					MainWindow.getInstance().getExplorer().expandTree();
					MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile()
							.setEnabled(true);
					MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile()
							.setEnabled(true);
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);
				}

				// Checks if the file is opened in the editor
				boolean isOpened = false;
				for (int i = 0; i < MainWindow.getInstance().getFileEditorManager()
						.getNumFileEditorPanels(); i++) {
					if (MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(i)
							.getAbsolutePath().equals(
									explorerFile.getPath())) {
						isOpened = true;
					}
				}

				// If it is not opened in the editor
				if (!isOpened) {

					TextFile fd = new TextFile();
					int type = 0;

					// Updates the status bar
					MainWindow.getInstance().getStatusBar().setMessage(
							explorerFile.getPath());

					// Opens a new tab in the editor
					MainWindow.getInstance().getFileEditorManager().newTab(
							explorerFile.getPath(), explorerFile.getPath(),
							fd.load(explorerFile.getPath()), true, type);

					// UNDO REDO
					MainWindow.getInstance().getMenu().enableFileMenu();
					MainWindow.getInstance().getMenu().enableEditMenu();
					int selectedEditorIndex = MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanelIndex();
					Document document = MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex).getActiveTextEditionArea()
							.getDocument();
					document.addUndoableEditListener(new UndoableEditListener() {
						/*
						 * (non-Javadoc)
						 * 
						 * @seejavax.swing.event.UndoableEditListener#
						 * undoableEditHappened
						 * (javax.swing.event.UndoableEditEvent)
						 */
						@Override
						public void undoableEditHappened(
								UndoableEditEvent undoableEditEvent) {

							UndoableEdit edit = undoableEditEvent.getEdit();

							if (edit instanceof DefaultDocumentEvent
									&& ((DefaultDocumentEvent) edit)
											.getType() == DefaultDocumentEvent.EventType.CHANGE) {
								return;
							} else {

								// Gets the selected editor index
								int selectedEditorIndex = MainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanelIndex();
								
								// Set the edit property over the selected editor undo manager
								MainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditorIndex)
								.getUndoManager().addEdit(undoableEditEvent
										.getEdit());
							}
						}
					});

					// Sets the caret in the first position of the editor
					selectedEditorIndex = MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanelIndex();
					MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(
							selectedEditorIndex).getActiveTextEditionArea()
							.setCaretPosition(0);
				}
			}
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}