package gui.menu.project.listeners;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Add folder menu item listener											
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
public class AddFolderListener implements ActionListener {

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
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Ask for the name of the folder
		String newFolder = JOptionPane.showInputDialog(null, labels
				.getString("s656"));

		// If it is a valid folder
		if (newFolder != null && !newFolder.matches("")) {
			
			// Gets the selected node in the explorer tree
			TreePath path = MainWindow.getInstance().getExplorer().getTree()
					.getSelectionPath();

			// Creates the explorer folder
			DefaultMutableTreeNode folderPath;
			ExplorerFile folder;

			// If folder selected
			if (path != null) {
				
				// Gets the selected node in the explorer tree
				folderPath = (DefaultMutableTreeNode) path
						.getLastPathComponent();
				
				// Transforms the node into a explorer file
				folder = (ExplorerFile) folderPath.getUserObject();

				// If it is a file and not a directory
				if (!folder.isDirectory()) {
					folderPath = MainWindow.getInstance().getExplorer().getRoot()
							.getNextNode();
					folder = (ExplorerFile) folderPath.getUserObject();
				}

			} else {
				
				// File selected 
				
				folderPath = MainWindow.getInstance().getExplorer().getRoot().getNextNode();
				folder = (ExplorerFile) folderPath.getUserObject();
			}

			ExplorerFile explorerFile = new ExplorerFile();
			explorerFile.setPath(newFolder);
			explorerFile.setName(newFolder);
			explorerFile.setParent(folder.getName());
			explorerFile.setIsDirectory(true);
			
			// Adds the folder to the configuration
			MainWindow.getInstance().getProjectConfiguration().addFile(explorerFile);
			
			// Updates the explorer tree with the new folder
			DefaultMutableTreeNode node = new DefaultMutableTreeNode(
					explorerFile);
			node.setAllowsChildren(true);
			folderPath.add(node);
			folderPath.setAllowsChildren(true);
			MainWindow.getInstance().getExplorer().getTreeModel().reload();
			MainWindow.getInstance().getExplorer().expandTree();
			MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
		}
	}
}

