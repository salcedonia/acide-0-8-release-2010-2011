package gui.menu.project.listeners;

import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import language.Language;
import operations.factory.IOFactory;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Remove file menu item listener											
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
public class RemoveFileListener implements ActionListener {

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
		ResourceBundle labels = language.getLabels();

		// Are you sure?
		int chosenOption = JOptionPane.showConfirmDialog(null, labels
				.getString("s623"));

		// If yes
		if (chosenOption == JOptionPane.OK_OPTION) {

			// Gets the selection over the explorer tree
			TreePath currentSelection = MainWindow.getInstance().getExplorer().getTree()
					.getSelectionPath();

			// If something has been selected
			if (currentSelection != null) {
				
				// Gets the select node in the explorer tree
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
						.getLastPathComponent());

				// Transform the node into a explorer file
				ExplorerFile explorerFile = (ExplorerFile) currentNode.getUserObject();
				
				// If it is a file and not a directory
				if (!explorerFile.isDirectory()) {
					
					// Gets the node parent
					MutableTreeNode parent = (MutableTreeNode) (currentNode
							.getParent());
					
					// If it has parent
					if (parent != null) {
						
						// Removes the node
						MainWindow.getInstance().getExplorer().getTreeModel()
								.removeNodeFromParent(currentNode);
						
						// Searches for the file in the explorer
						int posFile = -1;
						for (int position = 0; position < MainWindow.getInstance()
								.getProjectConfiguration()
								.getNumFilesFromList(); position++) {
							
							if (MainWindow.getInstance().getProjectConfiguration()
									.getFileAt(position).getPath().equals(
											explorerFile.getPath())) {
								
								posFile = position;
							}
						}

						// Removes the file from the project configuration
						MainWindow.getInstance().getProjectConfiguration().removeFileAt(
								posFile);
						
						// Updates the status bar
						MainWindow.getInstance().getStatusBar().setMessage("");
						
						// Searches for the file in the editor
						int posEditor = -1;
						for (int position = 0; position < MainWindow.getInstance().getEditorManager()
								.getNumEditors(); position++) {
							if (MainWindow.getInstance().getEditorManager()
									.getEditorAt(position).getAbsolutePath()
									.equals(explorerFile.getPath()))
								posEditor = position;
						}
						
						// If it exists
						if (posEditor != -1) {

							// Is the file modified?
							if (MainWindow.getInstance().getEditorManager().isRedButton(
									posEditor)) {

								// Do you want to save it?
								chosenOption = JOptionPane.showConfirmDialog(
										null, labels.getString("s643"),
										labels.getString("s953"),
										JOptionPane.YES_NO_OPTION);

								// If yes
								if (chosenOption == JOptionPane.OK_OPTION) {
	
									// Creates the external file
									TextFile textFile = IOFactory
									.getInstance().buildFile();

									// Saves the file
									boolean result = textFile.save(MainWindow.getInstance()
											.getEditorManager()
											.getEditorAt(posEditor)
											.getAbsolutePath(), MainWindow.getInstance()
											.getEditorManager()
											.getEditorAt(posEditor).getText());
									
									// If it could save it
									if (result) {
										
										// Sets the green button
										MainWindow.getInstance()
												.getEditorManager()
												.setGreenButtonAt(posEditor);
									}
								}
							}
						}

						// Closes the editor tab
						MainWindow.getInstance().getEditorManager().getPane().remove(
								posEditor);

						// If there are no more opened tabs
						if (MainWindow.getInstance().getEditorManager().getPane()
								.getTabCount() == 0) {
							
							// Disables the FILE and EDIT menu
							MainWindow.getInstance().getMenu().disableFileMenu();
							MainWindow.getInstance().getMenu().disableEditMenu();
						}
					}

					// The project has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);
					return;
				}
			}
		}

		// If there are more opened files 
		if (MainWindow.getInstance().getProjectConfiguration().getNumFilesFromList() > 0) {
			
			// Updates the explorer popup menu
			MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile()
					.setEnabled(true);
			MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile()
					.setEnabled(true);
		} else {
			
			// Updates the explorer popup menu
			MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile()
					.setEnabled(false);
			MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile()
					.setEnabled(false);
		}
	}
}
