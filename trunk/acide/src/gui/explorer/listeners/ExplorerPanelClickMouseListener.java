package gui.explorer.listeners;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/************************************************************************																
 * Explorer panel mouse click listener										
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
public class ExplorerPanelClickMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// GET THE SELECTED NODE
		TreePath selectedNode = MainWindow.getInstance().getExplorer().getTree().getPathForLocation(mouseEvent.getX(),
				mouseEvent.getY());

		if (selectedNode != null) {

			// Updates the status bar
			String filePath = selectedNode.getLastPathComponent()
					.toString();
			MainWindow.getInstance().getStatusBar().setMessage(filePath);

			// GET THE FILE FROM THE TREE NODE
			DefaultMutableTreeNode defaultMutableTreeNode = (DefaultMutableTreeNode) selectedNode
					.getLastPathComponent();
			Object node = defaultMutableTreeNode.getUserObject();
			ExplorerFile explorerFile = (ExplorerFile) node;
			explorerFile.getPath();

			// SEARCH THE EXPLORER FILE INTO THE EDITOR FILES
			for (int fileIndex = 0; fileIndex < MainWindow.getInstance()
					.getProjectConfiguration().getNumFilesFromList(); fileIndex++) {

				// IF IS THE SEARCHED FILE
				if (MainWindow.getInstance().getProjectConfiguration()
						.getFileAt(fileIndex).getPath()
						.equals(explorerFile.getPath()))

					// NOT DIRECTORY
					if (!MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(fileIndex).isDirectory()) {

						// IS COMPILABLE FILE?
						if (MainWindow.getInstance()
								.getProjectConfiguration()
								.getFileAt(fileIndex).isCompilableFile())

							// IS MAIN FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration()
									.getFileAt(fileIndex).isMainFile())

								// Updates the status bar
								MainWindow
										.getInstance()
										.getStatusBar()
										.setMessage(
												MainWindow
														.getInstance()
														.getProjectConfiguration()
														.getFileAt(fileIndex)
														.getPath()
														+ " <MAIN>");
							else
								// Updates the status bar
								MainWindow
									.getInstance()
									.getStatusBar()
									.setMessage(
											MainWindow
													.getInstance()
													.getProjectConfiguration()
													.getFileAt(fileIndex)
													.getPath()
													+ " <COMPILABLE>");
						else
							// Updates the status bar
							MainWindow
								.getInstance()
								.getStatusBar()
								.setMessage(
										MainWindow
												.getInstance()
												.getProjectConfiguration()
												.getFileAt(fileIndex)
												.getPath());	
					}
			}

			// SET THE FOCUS ON THE SELECTED FILE AT THE EDITOR
			for (int i = 0; i < MainWindow.getInstance().getEditorManager()
					.getNumEditors(); i++) {
				if (MainWindow.getInstance().getEditorManager()
						.getEditorAt(i).getAbsolutePath()
						.equals(explorerFile.getPath())) {
					MainWindow.getInstance().getEditorManager()
							.setSelectedEditorAt(i);
				}
			}

			MainWindow.getInstance().validate();
			MainWindow.getInstance().repaint();
		}
	}
}
