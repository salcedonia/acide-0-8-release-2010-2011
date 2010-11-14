package gui.menu.project.listeners;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/************************************************************************																
 * Set main file menu item listener											
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
public class SetMainListener implements ActionListener {

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
		DefaultMutableTreeNode filePath;
		ExplorerFile explorerFile;

		// If something is selected
		if (explorerSelection != null) {

			filePath = (DefaultMutableTreeNode) explorerSelection.getLastPathComponent();
			explorerFile = (ExplorerFile) filePath.getUserObject();

			// Is not MAIN FILE
			if (!explorerFile.isMainFile()) {

				// Is file and not a directory
				if (!explorerFile.isDirectory()) {

					for (int i = 0; i < MainWindow.getInstance()
							.getProjectConfiguration().getFileListSize(); i++) {

						// Quits previous MAIN
						if (MainWindow.getInstance()
								.getProjectConfiguration().getFileAt(i)
								.isMainFile()) {
							MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(i)
									.setIsMainFile(false);
							MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(i)
									.setIsCompilableFile(false);

							// Quits the icon tab
							for (int j = 0; j < MainWindow.getInstance()
									.getEditorManager().getNumEditors(); j++) {

								if (MainWindow
										.getInstance()
										.getEditorManager()
										.getEditorAt(j)
										.getAbsolutePath()
										.equals(
												MainWindow
														.getInstance()
														.getProjectConfiguration()
														.getFileAt(i)
														.getPath()))
									MainWindow.getInstance()
											.getEditorManager().getPane()
											.setIconAt(j, null);
							}
						}
					}

					explorerFile.setIsMainFile(true);
					explorerFile.setIsCompilableFile(true);

					// The project has been modified
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);

					// Updates the status bar
					MainWindow.getInstance().getStatusBar().setMessage(
							explorerFile.getPath() + " <MAIN>");

					// Puts the icon 
					for (int j = 0; j < MainWindow.getInstance()
							.getEditorManager().getNumEditors(); j++) {
						if (MainWindow.getInstance().getEditorManager()
								.getEditorAt(j).getAbsolutePath().equals(
										explorerFile.getPath())) {
							MainWindow
									.getInstance()
									.getEditorManager()
									.getPane()
									.setIconAt(
											j,
											new ImageIcon(
													"./resources/icons/editor/main.PNG"));
						}
					}
				}
			}
		} else {

			// Default configuration
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// If there are more opened editor
				if (MainWindow.getInstance().getEditorManager()
						.getNumEditors() > 0)

					// Sets the MAIN FILE
					MainWindow.getInstance().getEditorManager()
							.setMainFile();
			}
		}
	}
}
