package gui.menu.project.listeners;

import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import language.Language;
import operations.factory.IOFactory;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * New project file menu item listener											
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
public class NewProjectFileListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		int selectedEditorIndex = MainWindow.getInstance()
				.getEditorManager().getSelectedEditorIndex();

		// Creates the file
		MainWindow.getInstance().getMenu().getFile().getNewFile().doClick();

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

		TextFile textFile = IOFactory.getInstance().buildFile();
		String textContent = " ";

		if (MainWindow.getInstance().getEditorManager().getNumEditors() == 0) {
			
			// Updates the log
			Log.getLog().info(labels.getString("s89"));

		} else {

			MainWindow.getInstance().getMenu().setIsNPF(true);
			textContent = textFile.write();
			MainWindow.getInstance().getMenu().setIsNPF(false);

			if (textContent.equals(" ")) {
				MainWindow.getInstance().getEditorManager()
						.setSelectedEditorAt(selectedEditorIndex);
				
				// Updates the log
				Log.getLog().info(labels.getString("s92"));
			} else {

				boolean savingResult = textFile.save(textContent,
						MainWindow.getInstance().getEditorManager()
								.getSelectedEditor().getText());

				// If it could save it
				if (savingResult) {
					Log.getLog().info(
							labels.getString("s93") + textContent
									+ labels.getString("s94"));

					// Sets the green button to the editor
					MainWindow.getInstance().getEditorManager()
							.setGreenButton();

					// Gets the name
					int index = textContent.lastIndexOf("\\");
					if (index == -1)
						index = textContent.lastIndexOf("/");
					String name = textContent.substring(index + 1, textContent
							.length());

					// Sets the title
					MainWindow.getInstance().getEditorManager().getPane()
							.setTitleAt(
									MainWindow.getInstance()
											.getEditorManager().getPane()
											.getSelectedIndex(), name);

					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().setAbsolutePath(
									textContent);
					MainWindow.getInstance().getEditorManager().getPane()
							.setToolTipText(textContent);

					File fich = new File(MainWindow.getInstance()
							.getEditorManager().getSelectedEditor()
							.getAbsolutePath());
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().setLastChange(
									fich.lastModified());
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().setLastChange(
									fich.length());
				} else {
					
					// Updates the log
					Log.getLog()
							.info(labels.getString("s95") + textContent);
				}
			}
		}

		// Add the file to the project
		try {

			String file = textContent;

			if (file != null && file != " ") {

				TreePath path = MainWindow.getInstance().getExplorer()
						.getTree().getSelectionPath();
				DefaultMutableTreeNode filePath;
				ExplorerFile explorerFile;

				// Folder selected
				if (path != null) {

					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					explorerFile = (ExplorerFile) filePath.getUserObject();

					// File selected
					if (!explorerFile.isDirectory()) {
						filePath = MainWindow.getInstance().getExplorer()
								.getRoot().getNextNode();
						explorerFile = (ExplorerFile) filePath.getUserObject();
					}

				} else {

					// Nothing selected
					filePath = MainWindow.getInstance().getExplorer()
							.getRoot().getNextNode();
					explorerFile = (ExplorerFile) filePath.getUserObject();
				}

				// Gets the node name
				String name = "";
				int index = file.lastIndexOf("\\");
				if (index == -1)
					index = file.lastIndexOf("/");
				name = file.substring(index + 1, file.length());

				ExplorerFile fic = new ExplorerFile();
				fic.setPath(file);
				fic.setName(name);
				fic.setParent(explorerFile.getName());
				MainWindow.getInstance().getProjectConfiguration().addFile(
						fic);
				MainWindow.getInstance().getProjectConfiguration()
						.getFileAt(
								MainWindow.getInstance()
										.getProjectConfiguration()
										.getNumFilesFromList() - 1)
						.setIsOpened(true);

				DefaultMutableTreeNode d = new DefaultMutableTreeNode(fic);
				d.setAllowsChildren(false);
				filePath.add(d);
				MainWindow.getInstance().validate();
				MainWindow.getInstance().repaint();
				MainWindow.getInstance().getExplorer().getTreeModel()
						.reload();
				MainWindow.getInstance().getExplorer().expandTree();
				MainWindow.getInstance().getExplorer().getPopupMenu()
						.getRemoveFile().setEnabled(true);
				MainWindow.getInstance().getExplorer().getPopupMenu()
						.getDeleteFile().setEnabled(true);
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);

				// Updates the status bar
				MainWindow.getInstance().getStatusBar().setMessage(
						fic.getPath());
			}
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
