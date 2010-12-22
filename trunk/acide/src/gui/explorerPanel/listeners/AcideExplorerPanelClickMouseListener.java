package gui.explorerPanel.listeners;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/************************************************************************																
 * ACIDE - A Configurable IDE explorer panel mouse click listener.
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
public class AcideExplorerPanelClickMouseListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {

		// Gets the selected node from the explorer tree
		TreePath selectedNode = MainWindow.getInstance().getExplorerPanel().getTree().getPathForLocation(mouseEvent.getX(),
				mouseEvent.getY());

		if (selectedNode != null) {

			// Updates the status bar
			String filePath = selectedNode.getLastPathComponent()
					.toString();
			MainWindow.getInstance().getStatusBar().setMessage(filePath);

			// Gets the file form the tree node
			DefaultMutableTreeNode defaultMutableTreeNode = (DefaultMutableTreeNode) selectedNode
					.getLastPathComponent();
			Object node = defaultMutableTreeNode.getUserObject();
			ExplorerFile explorerFile = (ExplorerFile) node;
			explorerFile.getPath();

			// Searches the explorer file into the editor files
			for (int fileIndex = 0; fileIndex < MainWindow.getInstance()
					.getProjectConfiguration().getNumFilesFromList(); fileIndex++) {

				// If it is the searched file
				if (MainWindow.getInstance().getProjectConfiguration()
						.getFileAt(fileIndex).getPath()
						.equals(explorerFile.getPath()))

					// Is not a directory
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

			// Sets the focus on the selected file at the editor
			for (int i = 0; i < MainWindow.getInstance().getFileEditorManager()
					.getNumFileEditorPanels(); i++) {
				if (MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(i).getAbsolutePath()
						.equals(explorerFile.getPath())) {
					MainWindow.getInstance().getFileEditorManager()
							.setSelectedFileEditorPanelAt(i);
				}
			}

			MainWindow.getInstance().validate();
			MainWindow.getInstance().repaint();
		}
	}
}
