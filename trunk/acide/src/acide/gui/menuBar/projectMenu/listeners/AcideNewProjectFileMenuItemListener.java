package acide.gui.menuBar.projectMenu.listeners;

import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.files.AcideFileManager;
import acide.files.project.AcideProjectFile;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * <p>
 * ACIDE -A Configurable IDE project menu new project file menu item listener.
 * </p>
 * <p>
 * There are four cases to be contemplated:
 * <ul>
 * <li>The new file belongs to the project and it is opened.</li>
 * <li>The new file belongs to the project and it is not opened.</li>
 * <li>The new file does not belong to the project and it is opened.</li>
 * <li>The new file does not belong to the project and it is not opened.</li>
 * </ul>
 * </p>
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

		// If the project has no opened file editor panels
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			String absoluteFilePath = null;
			String lastOpenedFileDirectory = null;

			// Creates the file chooser
			JFileChooser fileChooser = new JFileChooser();

			try {

				// Gets the ACIDE - A Configurable IDE last opened file
				// directory
				lastOpenedFileDirectory = AcideResourceManager.getInstance()
						.getProperty("lastOpenedFileDirectory");

				// Sets the current directory to the last opened file directory
				fileChooser.setCurrentDirectory(new File(
						lastOpenedFileDirectory));

				// Clears the previous selected files
				fileChooser.setSelectedFiles(new File[0]);

				// Disables the multiple selection of files
				fileChooser.setMultiSelectionEnabled(false);

				// Sets only files
				fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);

				// Ask to the user for saving the changes
				int returnValueSaveFile = fileChooser.showSaveDialog(null);

				// Gets the selected file
				File selectedFile = fileChooser.getSelectedFile();

				// Ask the user for saving it
				if (returnValueSaveFile == JFileChooser.APPROVE_OPTION) {

					// Gets the ACIDE - A Configurable IDE last opened
					// file
					// directory
					absoluteFilePath = fileChooser.getSelectedFile()
							.getAbsolutePath();

					// If the user selected something
					if (absoluteFilePath != null) {

						// Gets the name
						int index = absoluteFilePath.lastIndexOf("\\");
						if (index == -1)
							index = absoluteFilePath.lastIndexOf("/");
						String name = absoluteFilePath.substring(index + 1,
								absoluteFilePath.length());

						// Gets the overwritten index
						int overwrittenIndex = AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(name);

						// Gets the current file content
						String currentFileContent = AcideMainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getTextEditionAreaContent();

						// If exists
						if (selectedFile.exists()) {

							// Ask if are you sure about the operation
							int returnValueAreYouSure = JOptionPane
									.showConfirmDialog(
											null,
											AcideLanguageManager.getInstance()
													.getLabels()
													.getString("s954"),
											AcideLanguageManager.getInstance()
													.getLabels()
													.getString("s953"),
											JOptionPane.YES_NO_OPTION);

							// If it is ok
							if (returnValueAreYouSure == JOptionPane.YES_OPTION) {

								// If the overwritten file exists in the tabbed
								// pane
								if (overwrittenIndex != -1) {

									// Removes the tab in the file editor
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.getTabbedPane()
											.remove(AcideMainWindow
													.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanelIndex());

									// Validates the changes in the file editor
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getTabbedPane().validate();

									// Sets the tab that is overwritten from
									// the file
									// editor
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													overwrittenIndex);

									// Updates the components related to the
									// file panel
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.updateRelatedComponentsAt(
													overwrittenIndex);
								}

								// Saves the file
								boolean savingResult = AcideFileManager
										.getInstance().write(absoluteFilePath,
												currentFileContent);

								// If it could save it
								if (savingResult) {

									// Overwrites the opened file editor
									overwriteOpenedFileEditor(absoluteFilePath);
								} else {

									// Updates the log
									AcideLog.getLog().info(
											AcideLanguageManager.getInstance()
													.getLabels()
													.getString("s95")
													+ absoluteFilePath);
								}

							} else if (returnValueAreYouSure == JOptionPane.NO_OPTION) {

								// If the overwritten file exists in the tabbed
								// pane
								if (overwrittenIndex != -1) {

									// Removes the tab in the file editor
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.getTabbedPane()
											.remove(AcideMainWindow
													.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanelIndex());

									// Validates the changes in the file editor
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getTabbedPane().validate();

									// Removes the tab that is overwritten from
									// the file
									// editor
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													overwrittenIndex);

									// Updates its type

									// Updates the components related to the
									// file panel
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.updateRelatedComponentsAt(
													overwrittenIndex);
								}

								// Overwrites the opened file editor
								overwriteOpenedFileEditor(absoluteFilePath);
							}
						} else {

							// If the overwritten file exists in the tabbed
							// pane
							if (overwrittenIndex != -1) {

								// Removes the tab in the file editor
								AcideMainWindow
										.getInstance()
										.getFileEditorManager()
										.getTabbedPane()
										.remove(AcideMainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex());

								// Validates the changes in the file editor
								AcideMainWindow.getInstance()
										.getFileEditorManager().getTabbedPane()
										.validate();

								// Removes the tab that is overwritten from the
								// file
								// editor
								AcideMainWindow
										.getInstance()
										.getFileEditorManager()
										.setSelectedFileEditorPanelAt(
												overwrittenIndex);

								// Updates the components related to the file
								// panel
								AcideMainWindow
										.getInstance()
										.getFileEditorManager()
										.updateRelatedComponentsAt(
												overwrittenIndex);
							}

							// Saves the file
							boolean savingResult = AcideFileManager
									.getInstance().write(absoluteFilePath,
											currentFileContent);

							// If it could save it
							if (savingResult) {

								// Overwrites the opened file editor
								overwriteOpenedFileEditor(absoluteFilePath);
							} else {

								// Updates the log
								AcideLog.getLog().info(
										AcideLanguageManager.getInstance()
												.getLabels().getString("s95")
												+ absoluteFilePath);
							}
						}
					} else if (returnValueSaveFile == JFileChooser.CANCEL_OPTION) {

						// Cancel selection
						fileChooser.cancelSelection();

						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s308"));

						// Removes the tab in the file editor
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getTabbedPane()
								.remove(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex());

						// Validates the changes in the file editor
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().validate();
					}
				}

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

		} else {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s89"));
		}
	}

	/**
	 * <p>
	 * Overwrites an opened file editor with the new project file.
	 * </p>
	 * <p>
	 * If the selected file is opened in the tabbed pane, then updates its
	 * properties, whereas if it is not opened it updates its properties and
	 * adds it to the explorer tree.
	 * </p>
	 * 
	 * @param filePath
	 *            chosen file path.
	 */
	private void overwriteOpenedFileEditor(String filePath) {

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s307")
						+ filePath);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels().getString("s93")
						+ filePath
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s94"));

		// Gets the file name
		int lastIndexOfSlash = filePath.lastIndexOf("\\");
		if (lastIndexOfSlash == -1)
			lastIndexOfSlash = filePath.lastIndexOf("/");
		String fileName = filePath.substring(lastIndexOfSlash + 1,
				filePath.length());

		// Updates the name property
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getStyledDocument()
				.putProperty("name", fileName);

		// Sets the title
		AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.getTabbedPane()
				.setTitleAt(
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().getSelectedIndex(), fileName);

		// Sets the file editor panel absolute path
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().setAbsolutePath(filePath);

		// Sets the file editor panel tool tip text
		AcideMainWindow.getInstance().getFileEditorManager().getTabbedPane()
				.setToolTipText(filePath);

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
				.getSelectedFileEditorPanel().setLastChange(file.length());

		// Adds the new file to the recent files list
		AcideWorkbenchConfiguration.getInstance().getRecentFilesConfiguration()
				.addRecentFileToList(filePath);

		// Updates the ACIDE - A Configurable IDE last
		// opened
		// file
		// directory
		AcideResourceManager.getInstance().setProperty(
				"lastOpenedFileDirectory", filePath);

		// Gets the project file
		AcideProjectFile projectFile = AcideProjectConfiguration.getInstance()
				.getFileAt(filePath);

		// If the selected file belongs to the project
		if (projectFile != null) {

			// If the file is not opened
			if (!projectFile.isOpened()) {

				// The file now is opened
				projectFile.setIsOpened(true);

				// Creates the image icon to set
				ImageIcon imageIcon = null;

				// Updates its type
				switch (projectFile.getType()) {

				case NORMAL:

					// It is not a main file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setMainFile(false);

					// It is not a compilable file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setCompilableFile(false);
					break;
				case COMPILABLE:

					// It is not a main file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setMainFile(false);

					// It is a compilable file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setCompilableFile(true);

					// Creates the COMPILABLE image icon to set
					imageIcon = new ImageIcon(
							"./resources/icons/editor/compilable.png");

					break;
				case MAIN:

					// It is a main file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setMainFile(true);

					// It is a compilable file
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.setCompilableFile(true);

					// Creates the MAIN image icon to set
					imageIcon = new ImageIcon(
							"./resources/icons/editor/main.png");

					break;
				}

				// Sets the icon in the selected file editor panel
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getTabbedPane()
						.setIconAt(
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex(),
								imageIcon);

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// The project has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);
				}
			}
		} else {

			// Updates the explorer tree
			addFileToExplorerTree(filePath);

			// Updates the components related to the
			// file panel
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.updateRelatedComponentsAt(
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanelIndex());

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);
			}
		}
		
		// Updates the related components
		AcideMainWindow
				.getInstance()
				.getFileEditorManager()
				.updateRelatedComponentsAt(
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex());
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
