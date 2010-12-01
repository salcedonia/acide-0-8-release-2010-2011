package gui.menuBar.projectMenu.listeners;

import es.explorer.ExplorerFile;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguage;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * New project file menu item listener.											
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
public class NewProjectFileMenuItemListener implements ActionListener {

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
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Creates the file
		MainWindow.getInstance().getMenu().getFile().getNewFile().doClick();

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

		TextFile textFile = AcideIOFactory.getInstance().buildFile();
		String textContent = " ";

		if (MainWindow.getInstance().getFileEditorManager().getNumFileEditorPanels() == 0) {
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s89"));

		} else {

			MainWindow.getInstance().getMenu().setIsNPF(true);
			textContent = textFile.write();
			MainWindow.getInstance().getMenu().setIsNPF(false);

			if (textContent.equals(" ")) {
				MainWindow.getInstance().getFileEditorManager()
						.setSelectedFileEditorPanelAt(selectedEditorIndex);
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s92"));
			} else {

				boolean savingResult = textFile.save(textContent,
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getTextEditionAreaContent());

				// If it could save it
				if (savingResult) {
					AcideLog.getLog().info(
							labels.getString("s93") + textContent
									+ labels.getString("s94"));

					// Sets the green button to the editor
					MainWindow.getInstance().getFileEditorManager()
							.setGreenButton();

					// Gets the name
					int index = textContent.lastIndexOf("\\");
					if (index == -1)
						index = textContent.lastIndexOf("/");
					String name = textContent.substring(index + 1, textContent
							.length());

					// Sets the title
					MainWindow.getInstance().getFileEditorManager().getTabbedPane()
							.setTitleAt(
									MainWindow.getInstance()
											.getFileEditorManager().getTabbedPane()
											.getSelectedIndex(), name);

					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setAbsolutePath(
									textContent);
					MainWindow.getInstance().getFileEditorManager().getTabbedPane()
							.setToolTipText(textContent);

					File fich = new File(MainWindow.getInstance()
							.getFileEditorManager().getSelectedFileEditorPanel()
							.getAbsolutePath());
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setLastChange(
									fich.lastModified());
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setLastChange(
									fich.length());
				} else {
					
					// Updates the log
					AcideLog.getLog()
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
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
