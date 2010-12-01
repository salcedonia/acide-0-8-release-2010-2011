package gui.menuBar.projectMenu.listeners;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/************************************************************************																
 * Set compilable menu item listener.											
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
public class SetCompilableFileMenuItemListener implements ActionListener {

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

			// If is not COMPILABLE FILE or COMPILABLE and MAIN FILE
			if (!explorerFile.isCompilableFile()
					|| (explorerFile.isCompilableFile() && explorerFile
							.isMainFile())) {

				// Is a file and not a directory
				if (!explorerFile.isDirectory()) {

					if (explorerFile.isMainFile())
						explorerFile.setIsMainFile(false);

					explorerFile.setIsCompilableFile(true);

					// The project has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);

					// Puts the icon in the editor
					for (int j = 0; j < MainWindow.getInstance()
							.getFileEditorManager().getNumFileEditorPanels(); j++) {

						if (MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(j).getAbsolutePath().equals(
										explorerFile.getPath())) {

							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.setIconAt(
											j,
											new ImageIcon(
													"./resources/icons/editor/compilable.PNG"));

							// Updates the status bar
							MainWindow.getInstance().getStatusBar()
									.setMessage(
											MainWindow.getInstance()
													.getFileEditorManager()
													.getFileEditorPanelAt(j)
													.getAbsolutePath()
													+ " <COMPILABLE>");
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

					// Sets the COMPILABLE FILE
					MainWindow.getInstance().getFileEditorManager()
							.setCompilableFile();
		}
	}
}
