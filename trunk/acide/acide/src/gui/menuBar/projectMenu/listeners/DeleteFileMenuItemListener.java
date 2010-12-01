package gui.menuBar.projectMenu.listeners;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Delete file menu item listener.											
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
public class DeleteFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		// Are you sure?
		int chosenOption = JOptionPane.showConfirmDialog(null, labels
				.getString("s951"));
		
		// If yes
		if (chosenOption == JOptionPane.OK_OPTION) {

			// Gets the selection in the explorer tree
			TreePath currentSelection = MainWindow.getInstance().getExplorer().getTree()
					.getSelectionPath();

			// If there is something selected
			if (currentSelection != null) {
				
				// Gets the selected node in the explorer tree
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
						.getLastPathComponent());

				// Transforms it into a explorer file
				ExplorerFile explorerFile = (ExplorerFile) currentNode.getUserObject();
				
				// If it is not a directory but a file
				if (!explorerFile.isDirectory()) {
					
					// Gets the parent
					MutableTreeNode parent = (MutableTreeNode) (currentNode
							.getParent());
					
					// If it has parent
					if (parent != null) {
						
						// Removes its parent
						MainWindow.getInstance().getExplorer().getTreeModel()
								.removeNodeFromParent(currentNode);
						
						// Searches for the file into the project configuration
						int posExplorer = -1;
						for (int position = 0; position < MainWindow.getInstance()
								.getProjectConfiguration()
								.getNumFilesFromList(); position++) {
							
							if (MainWindow.getInstance().getProjectConfiguration()
									.getFileAt(position).getPath().equals(
											explorerFile.getPath())) {
								posExplorer = position;
							}
						}

						// Gets the file from the project configuration file list
						ExplorerFile configurationFile = MainWindow.getInstance()
								.getProjectConfiguration().getFileAt(posExplorer);
						
						String fileRemove = configurationFile.getPath();
						
						// Removes the file from the project configuration
						MainWindow.getInstance().getProjectConfiguration().removeFileAt(
								posExplorer);

						// Deletes this file
						File physicalFile = new File(fileRemove);
						physicalFile.delete();

						// Updates the status bar
						MainWindow.getInstance().getStatusBar().setMessage("");
						
						// The project has been modified
						MainWindow.getInstance().getProjectConfiguration().setIsModified(
								true);

						return;
					}
				}
			}

			// IF THERE ARE MORE FILES IN THE PROJECT
			if (MainWindow.getInstance().getProjectConfiguration().getNumFilesFromList() > 0) {
				
				// UPDATES THE EXPLORER POPUP MENU
				MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile()
						.setEnabled(true);
				MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile()
						.setEnabled(true);
			} else {
				
				// UPDATES THE EXPLORER POPUP MENU
				MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile()
						.setEnabled(false);
				MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile()
						.setEnabled(false);
			}
		}
	}
}
