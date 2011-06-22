package acide.gui.menuBar.projectMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.files.utils.AcideFileOperation;
import acide.files.utils.AcideFileTarget;
import acide.files.utils.AcideFileType;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE -A Configurable IDE project menu new project file menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideNewProjectFileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Creates a new NEW FILE tab in the file editor manager tabbed pane
		AcideMainWindow.getInstance().getMenu().getFileMenu()
				.getNewFileMenuItem().doClick();

		// Asks for the file path to the user
		String absolutePath = AcideFileManager.getInstance().askForFile(
				AcideFileOperation.SAVE, AcideFileTarget.FILES,
				AcideFileType.FILE, "", null);

		// If the user selected something
		if (absolutePath != null) {

			// Builds the file to check if it exists
			File file = new File(absolutePath);

			// If the file exists
			if (file.exists()) {

				// Asks to the user if he wants to overwrite it
				int returnValue = JOptionPane.showConfirmDialog(null,
						AcideLanguageManager.getInstance().getLabels()
								.getString("s954"), AcideLanguageManager
								.getInstance().getLabels().getString("s953"),
						JOptionPane.YES_NO_OPTION);

				// If it overwrites the file
				if (returnValue == JOptionPane.YES_OPTION) {

					// Saves the file
					saveFile(absolutePath);
				}

				// If it is no
				if (returnValue == JOptionPane.NO_OPTION) {

					// Closes the new file in the tabbed pane
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getTabbedPane()
							.remove(AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanelIndex());

					// Validates the changes in the tabbed pane
					AcideMainWindow.getInstance().getFileEditorManager()
							.getTabbedPane().validate();
				}
			} else {

				// Saves the file
				saveFile(absolutePath);
			}
		} else {

			// Closes the new file in the tabbed pane
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getTabbedPane()
					.remove(AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanelIndex());

			// Validates the changes in the tabbed pane
			AcideMainWindow.getInstance().getFileEditorManager()
					.getTabbedPane().validate();
		}
	}

	/**
	 * Saves the file and updates the ACIDE - A Configurable file editor tabbed
	 * pane.
	 * 
	 * @param absoluteFilePath
	 *            file to save.
	 */
	private void saveFile(String absoluteFilePath) {

		// Saves the file
		boolean result = AcideFileManager.getInstance().write(
				absoluteFilePath,
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getTextEditionAreaContent());

		// If it could save it
		if (result) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s93")
							+ absoluteFilePath
							+ AcideLanguageManager.getInstance().getLabels()
									.getString("s94"));

			// Sets the green button
			AcideMainWindow.getInstance().getFileEditorManager()
					.setGreenButton();

			// Gets the file name
			int lastIndexOfSlash = absoluteFilePath.lastIndexOf("\\");
			if (lastIndexOfSlash == -1)
				lastIndexOfSlash = absoluteFilePath.lastIndexOf("/");
			String fileName = absoluteFilePath.substring(lastIndexOfSlash + 1,
					absoluteFilePath.length());

			// Updates the name property
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getStyledDocument()
					.putProperty("name", fileName);

			// Updates the title
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getTabbedPane()
					.setTitleAt(
							AcideMainWindow.getInstance()
									.getFileEditorManager().getTabbedPane()
									.getSelectedIndex(), fileName);

			// Sets the file editor panel absolute path
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel()
					.setAbsolutePath(absoluteFilePath);

			// Sets the file editor panel tool tip text
			AcideMainWindow.getInstance().getFileEditorManager()
					.getTabbedPane().setToolTipText(absoluteFilePath);

			// Builds the file to get the last changes
			File file = new File(AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getAbsolutePath());

			// Sets the last modification change
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel()
					.setLastChange(file.lastModified());

			// Sets the last length change
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().setLastSize(file.length());

			// If the file does not belong to the ACIDE - A Configuration IDE
			// project configuration
			if (AcideProjectConfiguration.getInstance().getFileAt(
					absoluteFilePath) == null)
			
				// Adds the file to the explorer tree
				addFileToExplorerTree(absoluteFilePath);
			
			else {

				// The file is already opened in the ACIDE - A Configurable IDE
				// file editor
				AcideProjectConfiguration.getInstance()
						.getFileAt(absoluteFilePath).setIsOpened(true);

				// Selects the node in the ACIDE - A Configurable IDE explorer
				// tree
				AcideMainWindow.getInstance().getExplorerPanel()
						.selectTreeNodeFromFileEditor();
			}

			// Adds the new file to the recent files list
			AcideWorkbenchConfiguration.getInstance()
					.getRecentFilesConfiguration()
					.addRecentFileToList(absoluteFilePath);

		} else {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s95")
							+ absoluteFilePath);
		}
	}

	/**
	 * Adds the file to the explorer tree.
	 * 
	 * @param filePath
	 *            path of the file to be added.
	 */
	public void addFileToExplorerTree(String filePath) {

		try {

			if (filePath != null && filePath != " ") {

				// Gets the current selection in the explorer tree
				TreePath currentSelection = AcideMainWindow.getInstance()
						.getExplorerPanel().getTree().getSelectionPath();

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
					currentProjectFile = (AcideProjectFile) currentNode
							.getUserObject();

					// If it is a file
					if (!currentProjectFile.isDirectory()) {

						// Gets the node parent
						currentNode = AcideMainWindow.getInstance()
								.getExplorerPanel().getRoot().getNextNode();

						// Transforms it into a project file
						currentProjectFile = (AcideProjectFile) currentNode
								.getUserObject();
					}
				} else {

					// Nothing selected

					// Gets the node parent
					currentNode = AcideMainWindow.getInstance()
							.getExplorerPanel().getRoot().getNextNode();

					// Transforms it into a project file
					currentProjectFile = (AcideProjectFile) currentNode
							.getUserObject();
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

				// Sets the main file
				newProjectFile.setIsMainFile(AcideMainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.isMainFile());

				// Sets the compilable file
				newProjectFile.setIsCompilableFile(AcideMainWindow
						.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isCompilableFile());

				// Adds the file to the project file list
				AcideProjectConfiguration.getInstance().addFile(newProjectFile);

				// Sets the new file open state to true
				AcideProjectConfiguration
						.getInstance()
						.getFileAt(
								AcideProjectConfiguration.getInstance()
										.getNumberOfFilesFromList() - 1)
						.setIsOpened(true);

				// Creates the new node to be added
				DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(
						newProjectFile);

				// Children are not allowed as it is a file
				newNode.setAllowsChildren(false);

				// Adds the node to the tree
				currentNode.add(newNode);

				// Reloads the explorer panel tree
				AcideMainWindow.getInstance().getExplorerPanel().getTreeModel()
						.reload();

				// Expands the explorer panel tree
				AcideMainWindow.getInstance().getExplorerPanel().expandTree();

				// Enables the removes file menu item in the explorer panel
				// popup menu
				AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getRemoveFileMenuItem().setEnabled(true);

				// Enables the delete file menu item in the explorer panel popup
				// menu
				AcideMainWindow.getInstance().getExplorerPanel().getPopupMenu()
						.getDeleteFileMenuItem().setEnabled(true);

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject())

					// The project configuration has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);

				// Updates the status message in the status bar
				AcideMainWindow.getInstance().getStatusBar()
						.setStatusMessage(newProjectFile.getAbsolutePath());
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}
}
