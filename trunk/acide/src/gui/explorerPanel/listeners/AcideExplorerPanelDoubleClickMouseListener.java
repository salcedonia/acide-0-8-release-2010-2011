package gui.explorerPanel.listeners;

import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.fileEditor.fileEditorManager.AcideFileEditorManager;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.AcideUndoRedoManager;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/************************************************************************
 * ACIDE - A Configurable IDE explorer panel double click listener.
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
public class AcideExplorerPanelDoubleClickMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// Double click
		if (mouseEvent.getClickCount() == 2) {

			// Gets the selected node
			TreePath selectedNode = MainWindow.getInstance().getExplorerPanel()
					.getTree()
					.getPathForLocation(mouseEvent.getX(), mouseEvent.getY());

			// If something is selected
			if (selectedNode != null) {

				// Updates the status bar
				String filePath = selectedNode.getLastPathComponent()
						.toString();
				MainWindow.getInstance().getStatusBar().setMessage(filePath);

				DefaultMutableTreeNode defaultMutableTreeNode = (DefaultMutableTreeNode) selectedNode
						.getLastPathComponent();
				Object node = defaultMutableTreeNode.getUserObject();
				ExplorerFile explorerFile = (ExplorerFile) node;

				// Searches for the file into the editor files
				int posEditor = -1;
				for (int pos = 0; pos < MainWindow.getInstance()
						.getFileEditorManager().getNumFileEditorPanels(); pos++) {
					if (MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(pos).getAbsolutePath()
							.equals(explorerFile.getPath())) {
						posEditor = pos;
					}
				}

				// If it is not opened
				if (posEditor == -1) {

					// Not a directory
					if (!explorerFile.isDirectory()) {

						TextFile textFile = new TextFile();
						AcideFileEditorManager editorBuilder = MainWindow
								.getInstance().getFileEditorManager();

						String fileContent = "";
						fileContent = textFile.load(explorerFile.getPath());

						// Searches for the file into the project opened files
						// list
						posEditor = -1;
						for (int pos = 0; pos < MainWindow.getInstance()
								.getProjectConfiguration()
								.getNumFilesFromList(); pos++) {
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(pos)
									.getPath().equals(explorerFile.getPath()))
								posEditor = pos;
						}

						// Checks the type
						int t = 0;

						// COMPILABLE FILE?
						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(posEditor).isCompilableFile())

							// MAIN FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration()
									.getFileAt(posEditor).isMainFile()) {

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
							} else {

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
						else {

							t = 0;
							// Updates the status bar
							MainWindow
									.getInstance()
									.getStatusBar()
									.setMessage(
											MainWindow.getInstance()
													.getProjectConfiguration()
													.getFileAt(posEditor)
													.getPath());
						}

						// Opens a new tab in the editor
						editorBuilder.newTab(explorerFile.getPath(),
								explorerFile.getPath(), fileContent, true, t);

						// Enables the file menu
						MainWindow.getInstance().getMenu().enableFileMenu();
						
						// Enables the edit menu
						MainWindow.getInstance().getMenu().enableEditMenu();
						
						// Updates the undo manager
						AcideUndoRedoManager.getInstance().update();

						// Sets the focus on the selected file at the editor
						for (int i = 0; i < MainWindow.getInstance()
								.getFileEditorManager()
								.getNumFileEditorPanels(); i++) {

							final int index = i;

							if (MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(i).getAbsolutePath()
									.equals(explorerFile.getPath())) {

								SwingUtilities.invokeLater(new Runnable() {
									/*
									 * (non-Javadoc)
									 * 
									 * @see java.lang.Runnable#run()
									 */
									@Override
									public void run() {

										// Sets the focus on the text area
										MainWindow.getInstance()
												.getFileEditorManager()
												.getFileEditorPanelAt(index)
												.getActiveTextEditionArea()
												.requestFocusInWindow();
									}
								});
							}
						}

						// Updates the main window
						MainWindow.getInstance().validate();
						MainWindow.getInstance().repaint();

						// Sets the file status in the project configuration
						for (int pos = 0; pos < MainWindow.getInstance()
								.getProjectConfiguration().getFileListSize(); pos++) {
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(pos)
									.getPath().equals(explorerFile.getPath())) {
								MainWindow.getInstance()
										.getProjectConfiguration()
										.getFileAt(pos).setIsOpened(true);
							}
						}

						// The project has been modified
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);
					}
				} else {

					// If it is already opened

					final int editorIndex = posEditor;

					SwingUtilities.invokeLater(new Runnable() {
						/*
						 * (non-Javadoc)
						 * 
						 * @see java.lang.Runnable#run()
						 */
						@Override
						public void run() {

							// Sets the selected editor
							MainWindow.getInstance().getFileEditorManager()
									.setSelectedFileEditorPanelAt(editorIndex);

							// Sets the focus on the text component
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(editorIndex)
									.getActiveTextEditionArea()
									.requestFocusInWindow();

						}
					});

				}
			}
		}
	}
}
