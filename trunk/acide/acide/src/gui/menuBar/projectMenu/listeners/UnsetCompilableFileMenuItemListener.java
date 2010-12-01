package gui.menuBar.projectMenu.listeners;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/************************************************************************																
 * Unset compilable menu item listener.											
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
public class UnsetCompilableFileMenuItemListener implements ActionListener {

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

			// Gets the selected node in the explorer
			selectedNode = (DefaultMutableTreeNode) explorerSelection.getLastPathComponent();
			
			// Transforms it into a explorer file
			explorerFile = (ExplorerFile) selectedNode.getUserObject();

			// If is COMPILABLE and not MAIN FILE
			if (explorerFile.isCompilableFile()
					&& !explorerFile.isMainFile()) {

				// If it is a file and not a directory
				if (!explorerFile.isDirectory()) {

					// The file is not COMPILABLE FILE
					explorerFile.setIsCompilableFile(false);

					// The project has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);

					// Updates the status bar
					MainWindow.getInstance().getStatusBar().setMessage(
							explorerFile.getPath());

					// Quits the icon in the editor
					for (int position = 0; position < MainWindow.getInstance()
							.getFileEditorManager().getNumFileEditorPanels(); position++) {
						if (MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(position).getAbsolutePath().equals(
										explorerFile.getPath())) {
							MainWindow.getInstance().getFileEditorManager()
									.getTabbedPane().setIconAt(position, null);
						}
					}
				}
			}
		} else {

			// Default project
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject())

				// If there are opened editors
				if (MainWindow.getInstance().getFileEditorManager()
						.getNumFileEditorPanels() > 0)

					// Unsets the COMPILABLE FILE
					MainWindow.getInstance().getFileEditorManager()
							.unsetCompilableFile();
		}
	}
}
