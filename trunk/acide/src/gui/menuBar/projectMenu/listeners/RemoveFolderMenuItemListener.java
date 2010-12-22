package gui.menuBar.projectMenu.listeners;

import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Remove folder menu item listener.										
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
public class RemoveFolderMenuItemListener implements ActionListener {

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
				.getString("s654"));
		
		// If yes
		if (chosenOption == JOptionPane.OK_OPTION) {

			// The project has been modified
			MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
			
			// Gets the selection in the explorer
			TreePath currentSelection = MainWindow.getInstance().getExplorerPanel().getTree()
					.getSelectionPath();
			
			// Something selected
			if (currentSelection != null) {
				
				// Gets the selected node in the explorer tree 
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
						.getLastPathComponent());
				
				// Transforms the node into a explorer file
				ExplorerFile folder = (ExplorerFile) currentNode.getUserObject();
				
				// If it is a directory
				if (folder.isDirectory()) {
					
					// Gets the node parent
					MutableTreeNode parent = (MutableTreeNode) (currentNode
							.getParent());
					
					// If it has parent
					if (parent != null) {
						
						// Removes the node
						MainWindow.getInstance().getExplorerPanel().getTreeModel()
								.removeNodeFromParent(currentNode);
						
						ArrayList<String> contRemove = new ArrayList<String>();
						
						if ((currentNode.getDepth() <= 2)
								&& (folder.getName().equals(MainWindow.getInstance()
										.getProjectConfiguration()
										.getName()))) {
							
							// Updates the explorer popup menu 
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getAddFile().setEnabled(false);
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getSaveProject().setEnabled(false);
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getRemoveFile().setEnabled(false);
							MainWindow.getInstance().getExplorerPanel().getPopupMenu()
									.getDeleteFile().setEnabled(false);
							
							MainWindow.getInstance().setTitle(labels.getString("s425")
									+ " - <empty>");
							TextFile f = new TextFile();

							f.save("./configuration/file_acidePrj",
									"<EMPTY>");
							MainWindow.getInstance().validate();
							MainWindow.getInstance().repaint();
							MainWindow.getInstance().getProjectConfiguration()
									.setName("");
							
							// Updates the RESOURCE MANAGER
							ResourceManager.getInstance().setProperty(
									"defaultAcideProject",
									"./configuration/project/default.acidePrj");
						}

						// Searches for the file in the explorer
						int posExplorer = -1;
						for (int position = 0; position < MainWindow.getInstance()
								.getProjectConfiguration()
								.getNumFilesFromList(); position++) {
							
							if (!folder.getName().equals(
									MainWindow.getInstance().getProjectConfiguration()
											.getName())) {
								if (MainWindow.getInstance().getProjectConfiguration()
										.getFileAt(position).getName().equals(
												folder.getName())) {
									posExplorer = position;

								} else if (MainWindow.getInstance()
										.getProjectConfiguration()
										.getFileAt(position).getParent().equals(
												folder.getName())) {
									if (!MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(position).isDirectory()) {
										
										contRemove.add(MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(position).getPath());
										
										if (MainWindow.getInstance()
												.getProjectConfiguration()
												.getNumFilesFromList() != 1)
											MainWindow.getInstance()
													.getProjectConfiguration()
													.removeFileAt(position);
										else
											MainWindow.getInstance()
													.getProjectConfiguration()
													.removeFileAt(0);
									} else {
										String dir = MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(position).getName();
										
										for (int k = position + 1; k < MainWindow.getInstance()
												.getProjectConfiguration()
												.getNumFilesFromList(); k++) {
											if (MainWindow.getInstance()
													.getProjectConfiguration()
													.getFileAt(position)
													.getParent()
													.equals(dir)) {
												contRemove
														.add(MainWindow.getInstance()
																.getProjectConfiguration()
																.getFileAt(
																		k)
																.getPath());

												if (MainWindow.getInstance()
														.getProjectConfiguration()
														.getNumFilesFromList() != 1)
													MainWindow.getInstance()
															.getProjectConfiguration()
															.removeFileAt(k);
												else
													MainWindow.getInstance()
															.getProjectConfiguration()
															.removeFileAt(0);
											}
										}
									}
								}
							}
						}
						
						// If it exists
						if (posExplorer != -1)
							
							// If it is not the last file in the project 
							if (MainWindow.getInstance().getProjectConfiguration()
									.getNumFilesFromList() != 1)
								MainWindow.getInstance().getProjectConfiguration()
										.removeFileAt(posExplorer);
							else
								
								// Last file in the project
								MainWindow.getInstance().getProjectConfiguration()
										.removeFileAt(0);
						
						// Are you sure?
						chosenOption = JOptionPane.showConfirmDialog(null, labels
								.getString("s655"));
						
						// If so
						if (chosenOption == JOptionPane.OK_OPTION) {

							// Deletes the file
							for (int j = 0; j < contRemove.size(); j++) {
								File fi = new File(contRemove.get(j));
								if (fi.isFile())
									fi.delete();
							}

						} else
							// Updates the status bar
							MainWindow.getInstance().getStatusBar().setMessage(
									"Option cancel");
						return;

					}
				}
			}
		}

		// UPDATES THE EXPLORER POPUP MENU
		if (MainWindow.getInstance().getProjectConfiguration().getNumFilesFromList() > 0) {
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getRemoveFile()
					.setEnabled(true);
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getDeleteFile()
					.setEnabled(true);
		} else {
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getRemoveFile()
					.setEnabled(false);
			MainWindow.getInstance().getExplorerPanel().getPopupMenu().getDeleteFile()
					.setEnabled(false);
		}
	}
}