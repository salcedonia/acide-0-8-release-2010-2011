package gui.menuBar.projectMenu.listeners;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/************************************************************************																
 * Unset main file menu item listener.											
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
 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan Jos� Ortiz S�nchez										
 *         </ul>														
 *         <ul>															
 *         Delf�n Rup�rez Ca�as											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Mart�n L�zaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo G�mez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see ActionListener																													
 ***********************************************************************/
public class UnsetMainFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the selection in the explorer tree
		TreePath explorerSelection = MainWindow.getInstance().getExplorer().getTree()
				.getSelectionPath();
		DefaultMutableTreeNode selectedNode;
		ExplorerFile explorerFile;

		// If something is selected
		if (explorerSelection != null) {

			// Gets the selected node in the explorer tree
			selectedNode = (DefaultMutableTreeNode) explorerSelection.getLastPathComponent();
			
			// Transforms it into a explorer file
			explorerFile = (ExplorerFile) selectedNode.getUserObject();

			// Is MAIN FILE
			if (explorerFile.isMainFile()) {

				// Is a file and not a directory
				if (!explorerFile.isDirectory()) {

					explorerFile.setIsMainFile(false);
					explorerFile.setIsCompilableFile(false);

					// The project has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);

					// Updates the status bar
					MainWindow.getInstance().getStatusBar().setMessage(
							explorerFile.getPath());

					// Quits the icon tab
					for (int j = 0; j < MainWindow.getInstance()
							.getFileEditorManager().getNumFileEditorPanels(); j++) {

						if (MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(j).getAbsolutePath().equals(
										explorerFile.getPath())) {
							MainWindow.getInstance().getFileEditorManager()
									.getTabbedPane().setIconAt(j, null);
						}
					}
				}
			}
		} else {

			// Default configuration
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject())

				// If there are opened editors
				if (MainWindow.getInstance().getFileEditorManager()
						.getNumFileEditorPanels() > 0)

					// Unset the MAIN FILE
					MainWindow.getInstance().getFileEditorManager()
							.unsetMainFile();
		}
	}
}
