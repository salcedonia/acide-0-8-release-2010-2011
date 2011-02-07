package gui.menuBar.projectMenu.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.project.AcideProjectFile;
import es.text.AcideTextFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguageManager;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;

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

		// Gets the selected file editor index
		int selectedFileEditorIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Calls to the file menu new file menu item action performed
		MainWindow.getInstance().getMenu().getFile().getNewFile().doClick();

		AcideTextFile textFile = AcideIOFactory.getInstance().buildFile();
		String filePath = " ";

		// If the project has no opened file editor panels
		if (MainWindow.getInstance().getFileEditorManager().getNumberOfFileEditorPanels() == 0) {
			
			// Updates the log
			AcideLog.getLog().info(AcideLanguageManager.getInstance().getLabels().getString("s89"));

		} else {

			MainWindow.getInstance().getMenu().setIsNewProjectFile(true);
			filePath = textFile.askSavingFileEditorFile();
			MainWindow.getInstance().getMenu().setIsNewProjectFile(false);

			if (filePath.equals(" ")) {
				
				// Set the selected file editor to the previous one
				MainWindow.getInstance().getFileEditorManager()
						.setSelectedFileEditorPanelAt(selectedFileEditorIndex);
				
				// Updates the log
				AcideLog.getLog().info(AcideLanguageManager.getInstance().getLabels().getString("s92"));
			} else {

				// Saves the file
				boolean savingResult = textFile.write(filePath,
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getTextEditionAreaContent());

				// If it could save it
				if (savingResult) {
					
					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels().getString("s93") + filePath
									+ AcideLanguageManager.getInstance().getLabels().getString("s94"));

					// Sets the green button to the editor
					MainWindow.getInstance().getFileEditorManager()
							.setGreenButton();

					// Gets the name
					int index = filePath.lastIndexOf("\\");
					if (index == -1)
						index = filePath.lastIndexOf("/");
					String name = filePath.substring(index + 1, filePath
							.length());

					// Sets the title
					MainWindow.getInstance().getFileEditorManager().getTabbedPane()
							.setTitleAt(
									MainWindow.getInstance()
											.getFileEditorManager().getTabbedPane()
											.getSelectedIndex(), name);

					// Sets the file editor panel absolute path
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setAbsolutePath(
									filePath);
					
					// Sets the file editor panel tool tip text
					MainWindow.getInstance().getFileEditorManager().getTabbedPane()
							.setToolTipText(filePath);

					// Builds the file to get the last changes
					File file = new File(MainWindow.getInstance()
							.getFileEditorManager().getSelectedFileEditorPanel()
							.getAbsolutePath());
					
					// Sets the last modification change
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setLastChange(
									file.lastModified());
					
					// Sets the last length change
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setLastChange(
									file.length());
				} else {
					
					// Updates the log
					AcideLog.getLog()
							.info(AcideLanguageManager.getInstance().getLabels().getString("s95") + filePath);
				}
			}
		}

		// Adds the new file to the explorer tree
		addFileToExplorerTree(filePath);
	}

	/**
	 * Adds the file to the explorer tree.
	 * 
	 * @param filePath path of the file to be added.
	 */
	public void addFileToExplorerTree(String filePath) {
		
		try {

			if (filePath != null && filePath != " ") {

				// Gets the current selection in the explorer tree
				TreePath currentSelection = MainWindow.getInstance().getExplorerPanel()
						.getTree().getSelectionPath();
				
				// Current node
				DefaultMutableTreeNode currentNode;
				
				// Current project file
				AcideProjectFile currentProjectFile;

				// Folder selected
				if (currentSelection != null) {

					// Gets the selected last path component
					currentNode = (DefaultMutableTreeNode) currentSelection
							.getLastPathComponent();
					
					// Transforms it into a project file
					currentProjectFile = (AcideProjectFile) currentNode.getUserObject();

					// If it is a file
					if (!currentProjectFile.isDirectory()) {
						
						// Gets the node parent
						currentNode = MainWindow.getInstance().getExplorerPanel()
								.getRoot().getNextNode();
						
						// Transforms it into a project file
						currentProjectFile = (AcideProjectFile) currentNode.getUserObject();
					}

				} else {

					// Nothing selected
					
					// Gets the node parent
					currentNode = MainWindow.getInstance().getExplorerPanel()
							.getRoot().getNextNode();
					
					// Transforms it into a project file
					currentProjectFile = (AcideProjectFile) currentNode.getUserObject();
				}

				// Gets the node name
				String fileName = "";
				int index = filePath.lastIndexOf("\\");
				if (index == -1)
					index = filePath.lastIndexOf("/");
				fileName = filePath.substring(index + 1, filePath.length());

				// Builds the new project file
				AcideProjectFile newProjectFile = new AcideProjectFile();
				
				// Sets the absolute path
				newProjectFile.setAbsolutePath(filePath);
				
				// Sets the name
				newProjectFile.setName(fileName);
				
				// Sets the parent
				newProjectFile.setParent(currentProjectFile.getName());
				
				// Adds the file to the project file list
				AcideProjectConfiguration.getInstance().addFile(
						newProjectFile);
				
				// Sets the new file open state to true
				AcideProjectConfiguration.getInstance()
						.getFileAt(
								AcideProjectConfiguration.getInstance()
										.getNumberOfFilesFromList() - 1)
						.setIsOpened(true);

				// Creates the new node to be added
				DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(newProjectFile);
				
				// Children are not allowed as it is a file
				newNode.setAllowsChildren(false);
				
				// Adds the node to the tree
				currentNode.add(newNode);
				
				// Validates the changes in the main window
				MainWindow.getInstance().validate();
				
				// Repaints the main window
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
						newProjectFile.getAbsolutePath());
			}
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
