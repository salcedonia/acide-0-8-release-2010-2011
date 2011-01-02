package gui.menuBar.projectMenu.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFile;
import es.text.TextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguageManager;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE -A Configurable IDE project menu new project file menu item listener.											
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
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
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
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

				TreePath path = MainWindow.getInstance().getExplorerPanel()
						.getTree().getSelectionPath();
				DefaultMutableTreeNode filePath;
				AcideProjectFile projectFile;

				// Folder selected
				if (path != null) {

					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					projectFile = (AcideProjectFile) filePath.getUserObject();

					// File selected
					if (!projectFile.isDirectory()) {
						filePath = MainWindow.getInstance().getExplorerPanel()
								.getRoot().getNextNode();
						projectFile = (AcideProjectFile) filePath.getUserObject();
					}

				} else {

					// Nothing selected
					filePath = MainWindow.getInstance().getExplorerPanel()
							.getRoot().getNextNode();
					projectFile = (AcideProjectFile) filePath.getUserObject();
				}

				// Gets the node name
				String name = "";
				int index = file.lastIndexOf("\\");
				if (index == -1)
					index = file.lastIndexOf("/");
				name = file.substring(index + 1, file.length());

				// Builds the project file
				projectFile = new AcideProjectFile();
				projectFile.setAbsolutePath(file);
				projectFile.setName(name);
				projectFile.setParent(projectFile.getName());
				
				// Adds the file to the project file list
				AcideProjectConfiguration.getInstance().addFile(
						projectFile);
				
				// Sets the new file open state to true
				AcideProjectConfiguration.getInstance()
						.getFileAt(
								AcideProjectConfiguration.getInstance()
										.getNumFilesFromList() - 1)
						.setIsOpened(true);

				DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(projectFile);
				defaultMutableTreeNode.setAllowsChildren(false);
				filePath.add(defaultMutableTreeNode);
				
				// Validates the MAIN WINDOW
				MainWindow.getInstance().validate();
				
				// Repaints the MAIN WINDOW
				MainWindow.getInstance().repaint();
				
				// Reloads the explorer panel tree
				MainWindow.getInstance().getExplorerPanel().getTreeModel()
						.reload();
				
				// Expands the explorer panel tree
				MainWindow.getInstance().getExplorerPanel().expandTree();
				
				// Enables the removes file menu item in the explorer panel popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getRemoveFile().setEnabled(true);
				
				// Enables the delete file menu item in the explorer panel popup menu
				MainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getDeleteFile().setEnabled(true);
				
				// The project configuration has been modified
				AcideProjectConfiguration.getInstance()
						.setIsModified(true);

				// Updates the status message in the status bar
				MainWindow.getInstance().getStatusBar().setStatusMessage(
						projectFile.getAbsolutePath());
			}
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
