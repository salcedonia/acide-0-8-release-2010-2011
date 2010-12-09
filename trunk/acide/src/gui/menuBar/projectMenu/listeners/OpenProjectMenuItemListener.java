package gui.menuBar.projectMenu.listeners;

import es.configuration.lexicon.LexiconConfiguration;
import es.configuration.menu.MenuConfiguration;
import es.configuration.output.OutputConfiguration;
import es.configuration.toolBar.shellComandToolBar.ShellCommandList;
import es.explorer.ExplorerFile;
import es.text.ExtensionFilter;
import es.text.TextFile;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.menuMenu.gui.MenuConfigurationWindow;
import gui.menuBar.configurationMenu.toolBarMenu.gui.ToolBarConfigurationWindow;

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.undo.UndoableEdit;

import language.AcideLanguage;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * Open project menu item listener.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
public class OpenProjectMenuItemListener implements ActionListener {

	/**
	 * Search a file into a list of files
	 * 
	 * @param list
	 *            list of files
	 * @param fileName
	 *            file name to search for
	 * 
	 * @return the node of the tree if exists
	 */
	private DefaultMutableTreeNode searchDirectoryList(
			ArrayList<DefaultMutableTreeNode> list, String fileName) {

		int pos = 0;
		boolean found = false;

		while (pos < list.size() && !found) {

			DefaultMutableTreeNode temp = list.get(pos);
			ExplorerFile explorerFile = (ExplorerFile) temp.getUserObject();

			if (explorerFile.getName().equals(fileName)) {
				found = true;
				return (DefaultMutableTreeNode) list.get(pos);
			} else
				pos++;
		}
		return null;
	}

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
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		TextFile textFile = AcideIOFactory.getInstance().buildFile();

		boolean cancelOptionSelected = false;

		// Selects the extension for the project
		String[] ExtPide = new String[] { "acidePrj" };
		textFile.getFileChooser().addChoosableFileFilter(
				new ExtensionFilter(ExtPide, labels.getString("s328")));
		final String file;
		file = textFile.read();

		// If the file content is not empty
		if (file != null) {

			// If the project has been modified
			if (MainWindow.getInstance().getProjectConfiguration().isModified()) {

				// Do you want to save it?
				int chosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s657"), labels.getString("s953"),
						JOptionPane.YES_NO_CANCEL_OPTION);

				// If OK
				if (chosenOption == JOptionPane.OK_OPTION) {
					MainWindow.getInstance().getMenu().getProject()
							.getSaveProject().setEnabled(true);
					MainWindow.getInstance().getMenu().getProject()
							.getSaveProject().doClick();
				} else if (chosenOption == JOptionPane.CANCEL_OPTION)
					cancelOptionSelected = true;
			}

			// If the user does not select the cancel option
			if (!cancelOptionSelected) {

				// Puts the wait cursor
				MainWindow.getInstance().setCursor(
						Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				// Updates the status bar
				MainWindow.getInstance().getStatusBar().setMessage("");

				// Closes the previous project configuration
				closePreviousProjectConfiguration(labels);

				// Loads the file editor configuration
				loadNewProjectConfiguration(file);

				// Loads the language
				loadLanguage(labels);

				// Loads the main window configuration
				loadMainWindowConfiguration();

				// Loads the menu configuration
				loadMenuConfiguration(labels);

				// Loads the grammar configuration
				loadGrammarConfiguration(labels);

				// Loads the shell configuration
				loadShellConfiguration();

				// Loads the tool bar configuration
				loadToolBarConfiguration(labels);

				// Loads the explorer configuration
				loadExplorerConfiguration(labels);

				// Enables the show explorer panel menu item
				MainWindow.getInstance().getMenu().getView()
						.getShowExplorerPanel().setSelected(true);

				// Loads the file editor configuration
				loadFileEditorConfiguration(labels);

				// Loads the lexicon configuration
				loadLexiconConfiguration(labels);

				// The project has not been modified
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(false);

				// This is the first time that it is saved
				MainWindow.getInstance().getProjectConfiguration()
						.setFirstSave(true);

				// Enables the project menu
				MainWindow.getInstance().getMenu().enableProjectMenu();

				// Updates the MAIN WINDOW
				MainWindow.getInstance().validate();
				MainWindow.getInstance().repaint();

				// Sets the default cursor
				MainWindow.getInstance().setCursor(
						Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			}
		}
	}

	/**
	 * Loads the project explorer configuration.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public void loadExplorerConfiguration(ResourceBundle labels) {

		// Removes all the nodes in the tree
		MainWindow.getInstance().getExplorer().getRoot().removeAllChildren();

		// Creates the folder with the name of the project
		ExplorerFile explorerFile = new ExplorerFile();
		explorerFile.setPath(MainWindow.getInstance().getProjectConfiguration()
				.getName());
		explorerFile.setName(MainWindow.getInstance().getProjectConfiguration()
				.getName());
		explorerFile.setIsDirectory(true);
		explorerFile.setParent(null);
		MainWindow.getInstance().setTitle(
				labels.getString("s425")
						+ " - "
						+ MainWindow.getInstance().getProjectConfiguration()
								.getName());

		// Creates the explorer tree with the project as the root of it
		DefaultMutableTreeNode explorerTree = new DefaultMutableTreeNode(
				explorerFile);
		explorerTree.setAllowsChildren(true);
		MainWindow.getInstance().getExplorer().getRoot().add(explorerTree);
		ArrayList<DefaultMutableTreeNode> listdir = new ArrayList<DefaultMutableTreeNode>();

		// Adds the associated project files to the explorer
		for (int index = 0; index < MainWindow.getInstance()
				.getProjectConfiguration().getNumFilesFromList(); index++) {

			// Gets the file from the project configuration
			DefaultMutableTreeNode file = new DefaultMutableTreeNode(MainWindow
					.getInstance().getProjectConfiguration().getFileAt(index));

			// If it is directory
			if (MainWindow.getInstance().getProjectConfiguration()
					.getFileAt(index).isDirectory()) {

				// Can have files inside
				file.setAllowsChildren(true);
				listdir.add(file);
			} else
				// Can't have files inside
				file.setAllowsChildren(false);

			// If the file is in the same folder than the project folder
			if (MainWindow
					.getInstance()
					.getProjectConfiguration()
					.getFileAt(index)
					.getParent()
					.equals(MainWindow.getInstance().getProjectConfiguration()
							.getName())) {
				// Adds the file
				explorerTree.add(file);
			} else {

				// Searches for it in the tree structure
				DefaultMutableTreeNode fileAux = searchDirectoryList(listdir,
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(index).getParent());

				// Adds the file
				fileAux.add(file);
			}
		}

		// Updates the tree structure
		MainWindow.getInstance().getExplorer().getTreeModel().reload();
		MainWindow.getInstance().getExplorer().expandTree();

		// If there are files associated to the project
		if (MainWindow.getInstance().getProjectConfiguration()
				.getNumFilesFromList() > 0) {

			// Enables the remove file menu item
			MainWindow.getInstance().getExplorer().getPopupMenu()
					.getRemoveFile().setEnabled(true);

			// Enables the delete file menu item
			MainWindow.getInstance().getExplorer().getPopupMenu()
					.getDeleteFile().setEnabled(true);
		} else {

			// Disables the remove file menu item
			MainWindow.getInstance().getExplorer().getPopupMenu()
					.getRemoveFile().setEnabled(false);

			// Disables the delete file menu item
			MainWindow.getInstance().getExplorer().getPopupMenu()
					.getDeleteFile().setEnabled(false);
		}

		// Updates the MAIN WINDOW
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();

		// If the show explorer panel is selected
		if (!MainWindow.getInstance().getMenu().getView()
				.getShowExplorerPanel().isSelected())
			// Shows the explorer
			MainWindow.getInstance().getExplorer().showExplorer();
	}

	/**
	 * Loads the project language.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public void loadLanguage(ResourceBundle labels) {

		// Gets the language configuration
		String configurationLanguage = MainWindow.getInstance()
				.getProjectConfiguration().getLanguage();

		// SPANISH
		if (configurationLanguage.equals("spanish"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getSpanish().doClick();

		// ENGLISH
		if (configurationLanguage.equals("english"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getEnglish().doClick();

		// Updates the status bar
		MainWindow.getInstance().getStatusBar()
				.setLexiconMessage(labels.getString("s449") + " ");
	}

	/**
	 * Loads the project menu configuration.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public void loadMenuConfiguration(ResourceBundle labels) {

		String currentMenu = null;

		try {

			// Gets the menu configuration file path
			currentMenu = MainWindow.getInstance().getProjectConfiguration()
					.getMenu();

			// Loads the new menu item list
			MenuConfiguration.getInstance().setMenuElementList(
					MenuConfiguration.getInstance().loadMenuConfigurationFile(
							currentMenu));

			// Updates the RESOURCE MANAGER
			ResourceManager.getInstance().setProperty(
					"currentMenuConfiguration", currentMenu);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Gets the name
			String currentMenu2;
			int index = currentMenu.lastIndexOf("\\");
			if (index == -1)
				index = currentMenu.lastIndexOf("/");
			currentMenu2 = ".\\configuration\\menu\\"
					+ currentMenu.substring(index + 1, currentMenu.length());

			try {

				// Load the new menu item list
				MenuConfiguration.getInstance().setMenuElementList(
						MenuConfiguration.getInstance()
								.loadMenuConfigurationFile(currentMenu));

				// Updates the RESOURCE MANAGER
				ResourceManager.getInstance().setProperty(
						"currentMenuConfiguration", currentMenu2);

				// Error message
				JOptionPane
						.showMessageDialog(null, labels.getString("s956")
								+ currentMenu + labels.getString("s957")
								+ currentMenu2);

			} catch (Exception exception1) {

				try {

					// Loads the menu configuration
					MenuConfiguration.getInstance().setMenuElementList(
							MenuConfiguration.getInstance()
									.loadMenuConfigurationFile(currentMenu));
				} catch (Exception exception2) {

					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				}

				// Updates the RESOURCE MANAGER
				ResourceManager.getInstance().setProperty(
						"currentMenuConfiguration",
						"./configuration/menu/defaultAllOn.menuCfg");

				// Error message
				JOptionPane.showMessageDialog(null, labels.getString("s956")
						+ currentMenu + labels.getString("s959"));

				// Updates the log
				AcideLog.getLog().error(exception1.getMessage());
				exception1.printStackTrace();
			}
		}

		// Builds the menu
		MainWindow.getInstance().getMenu().buildMenu();

		// Updates the MAIN WINDOW
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();

		// Enables the save menu menu item
		MainWindow.getInstance().getMenu().getConfiguration().getMenu()
				.getSaveMenu().setEnabled(true);

		// The changes are saved
		MenuConfigurationWindow.setChangesAreSaved(true);
	}

	/**
	 * Loads the project grammar configuration.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public void loadGrammarConfiguration(ResourceBundle labels) {

		try {

			// Gets the grammar configuration
			String currentGrammar = MainWindow.getInstance()
					.getProjectConfiguration().getSyntacticConfiguration();

			// Gets the grammar name
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");
			String grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);

			// Updates the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setGrammarMessage(
							labels.getString("s248") + " " + grammarName);

			// Updates the RESOURCE MANAGER
			ResourceManager.getInstance().setProperty("currentGrammar",
					currentGrammar);
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s944"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
	}

	/**
	 * Loads the project shell configuration.
	 */
	public void loadShellConfiguration() {

		// Gets the output configuration
		OutputConfiguration.getInstance().load(
				MainWindow.getInstance().getProjectConfiguration()
						.getOutputConfiguration());

		// Resets the output
		// MainWindow.getInstance().getOutput().executeExitCommand();
		MainWindow.getInstance().getOutput().resetOutput();

		// Updates the MAIN WINDOW
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();
	}

	/**
	 * Loads the project tool bar configuration.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public void loadToolBarConfiguration(ResourceBundle labels) {

		String currentToolBarConfiguration = null;

		try {

			// Clears the list of shell commands
			ShellCommandList.clear();

			// Gets the tool bar configuration
			currentToolBarConfiguration = MainWindow.getInstance()
					.getProjectConfiguration().getToolBar();

			// Loads the command list from the configuration file
			ShellCommandList.loadList(currentToolBarConfiguration);

			// Updates the RESOURCE MANAGER
			ResourceManager.getInstance().setProperty(
					"currentToolBarConfiguration", currentToolBarConfiguration);
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Gets the name
			String currentToolBarConfiguration2;
			int index = currentToolBarConfiguration.lastIndexOf("\\");
			if (index == -1)
				index = currentToolBarConfiguration.lastIndexOf("/");

			// Updates the current tool bar configuration
			currentToolBarConfiguration2 = ".\\configuration\\toolbar\\"
					+ currentToolBarConfiguration.substring(index + 1,
							currentToolBarConfiguration.length());
			try {

				// Loads the command list from the configuration file
				ShellCommandList.loadList(currentToolBarConfiguration2);

				// Error message
				JOptionPane.showMessageDialog(null,
						labels.getString("s958") + currentToolBarConfiguration
								+ labels.getString("s957")
								+ currentToolBarConfiguration2);

				// Updates the RESOURCE MANAGER
				ResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration",
						currentToolBarConfiguration2);
			} catch (Exception exception1) {

				// Updates the log
				AcideLog.getLog().error(exception1.getMessage());
				exception1.printStackTrace();

				try {

					// Loads the command list from the configuration file
					ShellCommandList
							.loadList("./configuration/toolbar/default.TBcfg");
				} catch (Exception exception2) {

					// Updates the log
					AcideLog.getLog().error(exception2.getMessage());
					exception2.printStackTrace();
				}

				// Error message
				JOptionPane.showMessageDialog(null,
						labels.getString("s958") + currentToolBarConfiguration
								+ labels.getString("s959"));

				// Updates the RESOURCE MANAGER
				ResourceManager.getInstance().setProperty(
						"currentToolBarConfiguration",
						"./configuration/toolbar/default.TBcfg");
			}
		}

		// Builds the tool bar
		MainWindow.getInstance().buildToolBar();

		// Enables the save tool bar menu item
		MainWindow.getInstance().getMenu().getConfiguration().getToolBar()
				.getSaveToolBar().setEnabled(true);

		// The changes are saved
		ToolBarConfigurationWindow.setAreChangesSaved(true);

		// Updates the MAIN WINDOW
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();
	}

	/**
	 * Loads the file editor configuration from the configuration file.
	 * 
	 * @param configurationFilePath
	 *            configuration file path.
	 */
	public void loadNewProjectConfiguration(String configurationFilePath) {

		// Builds the text configuration file
		TextFile textConfigurationFile = AcideIOFactory.getInstance()
				.buildFile();

		// Loads the configuration file content
		String configurationFileContent = textConfigurationFile
				.load(configurationFilePath);

		// Updates the RESOURCE MANAGER
		ResourceManager.getInstance().setProperty("defaultAcideProject",
				configurationFilePath);

		// Sets the project path
		MainWindow.getInstance().getProjectConfiguration()
				.setPath(configurationFilePath);

		// Removes the previous associated files to the project
		MainWindow.getInstance().getProjectConfiguration().removeFiles();

		// Loads the project configuration
		MainWindow.getInstance().getProjectConfiguration()
				.load(configurationFileContent);
	}

	/**
	 * Loads the project main window configuration.
	 */
	public void loadMainWindowConfiguration() {
		
		// Is explorer panel showed?
		if (!MainWindow.getInstance().getProjectConfiguration()
				.isExplorerShowed())

			// Shows the explorer panel
			MainWindow.getInstance().getMenu().getView().getShowExplorerPanel()
					.doClick();

		// Is shell panel showed?
		if (!MainWindow.getInstance().getProjectConfiguration().isShellShowed())

			// Shows the shell panel
			MainWindow.getInstance().getMenu().getView().getShowShellWindow()
					.doClick();

		// MAIN WINDOW SIZE
		MainWindow.getInstance().setSize(
				MainWindow.getInstance().getProjectConfiguration()
						.getWindowWidth(),
				MainWindow.getInstance().getProjectConfiguration()
						.getWindowHeight());

		// MAIN WINDOW LOCATION
		MainWindow.getInstance().setLocation(
				MainWindow.getInstance().getProjectConfiguration().getPosX(),
				MainWindow.getInstance().getProjectConfiguration().getPosY());

		// VERTICAL SPLIT PANE
		MainWindow
				.getInstance()
				.getVerticalSplitPane()
				.setDividerLocation(
						MainWindow.getInstance().getProjectConfiguration()
								.getSplitPaneVerticalDividerLocation());

		// HORIZONTAL SPLIT PANE
		MainWindow
				.getInstance()
				.getHorizontalSplitPane()
				.setDividerLocation(
						MainWindow.getInstance().getProjectConfiguration()
								.getSplitPanelHorizontalDividerLocation());

		// Updates the MAIN WINDOW
		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();
	}

	/**
	 * Loads the project file editor configuration. SwingUtilities is used to 
	 * wait until the end of the execution of all the previous events so it
	 * can add all the editors properly and safety.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public void loadFileEditorConfiguration(final ResourceBundle labels) {

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				
				for (int index = 0; index < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); index++) {

					// Checks if the file really exists
					File file = new File(MainWindow.getInstance()
							.getProjectConfiguration().getFileAt(index)
							.getPath());

					// If the file is not a directory and exists
					if (!MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(index).isDirectory()
							&& file.exists()) {

						TextFile textFile = AcideIOFactory.getInstance()
								.buildFile();
						String text = null;
						text = textFile.load(MainWindow.getInstance()
								.getProjectConfiguration().getFileAt(index)
								.getPath());

						String fileName = null;
						String filePath = MainWindow.getInstance()
								.getProjectConfiguration().getFileAt(index)
								.getPath();

						// Gets the file name
						if (filePath != null) {

							int lastIndexOfSlash = filePath.lastIndexOf("\\");
							if (lastIndexOfSlash == -1)
								lastIndexOfSlash = filePath.lastIndexOf("/");
							fileName = filePath.substring(lastIndexOfSlash + 1,
									filePath.length());
						}

						// If the file was opened
						if (MainWindow.getInstance().getProjectConfiguration()
								.isDefaultProject()
								|| MainWindow.getInstance()
										.getProjectConfiguration()
										.getFileAt(index).isOpened()) {

							// Enables the file menu
							MainWindow.getInstance().getMenu().enableFileMenu();

							// Enables the edit menu
							MainWindow.getInstance().getMenu().enableEditMenu();

							// Updates the status bar
							MainWindow
									.getInstance()
									.getStatusBar()
									.setMessage(
											MainWindow.getInstance()
													.getProjectConfiguration()
													.getFileAt(index).getPath());

							// Check if it is a MAIN or COMPILABLE FILE
							int fileType = 0;

							// COMPILABLE
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(index)
									.isCompilableFile()) {

								fileType = 2;

								// Adds the <COMPILABLE> tag in the status bar
								MainWindow
										.getInstance()
										.getStatusBar()
										.setMessage(
												MainWindow
														.getInstance()
														.getProjectConfiguration()
														.getFileAt(index)
														.getPath()
														+ " <COMPILABLE>");
							}

							// MAIN
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(index)
									.isMainFile()) {

								fileType = 1;

								// Adds the <MAIN> tag in the status bar
								MainWindow
										.getInstance()
										.getStatusBar()
										.setMessage(
												MainWindow
														.getInstance()
														.getProjectConfiguration()
														.getFileAt(index)
														.getPath()
														+ " <MAIN>");
							}

							// Opens a new tab in the editor
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.newTab(fileName, filePath, text, true,
											fileType);

							// Checks if it is marked as a MAIN or COMPILABLE
							// FILE
							for (int i = 0; i < MainWindow.getInstance()
									.getFileEditorManager()
									.getNumFileEditorPanels(); i++) {

								if (MainWindow
										.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(i)
										.getAbsolutePath()
										.equals(MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(index).getPath())) {

									// IS COMPILABLE FILE?
									if (MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(index)
											.isCompilableFile())
										MainWindow.getInstance()
												.getFileEditorManager()
												.getFileEditorPanelAt(i)
												.setCompilerFile(true);

									// IS MAIN FILE?
									if (MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(index).isMainFile())
										MainWindow.getInstance()
												.getFileEditorManager()
												.getFileEditorPanelAt(i)
												.setMainFile(true);
								}
							}

							// UNDO REDO
							DefaultStyledDocument document = MainWindow
									.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getSyntaxDocument();

							document.addUndoableEditListener(new UndoableEditListener() {
								/*
								 * (non-Javadoc)
								 * 
								 * @see javax.swing.event.UndoableEditListener#
								 * undoableEditHappened
								 * (javax.swing.event.UndoableEditEvent)
								 */
								public void undoableEditHappened(
										UndoableEditEvent undoableEditEvent) {

									UndoableEdit edit = undoableEditEvent
											.getEdit();

									if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
											.getType() == DefaultDocumentEvent.EventType.CHANGE))) {

										// Sets the edit property over the
										// selected
										// editor undo manager
										MainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getUndoManager()
												.addEdit(
														undoableEditEvent
																.getEdit());
									}
								}
							});
						}

						// The project configuration has been modified
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(false);
					} else {

						// If the file does not exist
						if (!file.exists()) {

							// Error message
							JOptionPane.showMessageDialog(null,
									labels.getString("s970")
											+ MainWindow.getInstance()
													.getProjectConfiguration()
													.getFileAt(index).getPath()
											+ labels.getString("s971"),
									"Error", JOptionPane.ERROR_MESSAGE);

							// Removes the file from the project
							MainWindow.getInstance().getProjectConfiguration()
									.removeFileAt(index);

							// The project configuration has been modified
							MainWindow.getInstance().getProjectConfiguration()
									.setIsModified(true);
						}
					}
				}

				// Sets the selected editor
				if (MainWindow.getInstance().getProjectConfiguration()
						.getSelectedEditorIndex() != -1) {

					// Sets the selected file editor
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.setSelectedFileEditorPanelAt(
									MainWindow.getInstance()
											.getProjectConfiguration()
											.getSelectedEditorIndex());

					// Sets the focus in the edition area
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getActiveTextEditionArea().requestFocusInWindow();

					// Selects the tree node
					MainWindow.getInstance().getExplorer()
							.selectTreeNodeFromFileEditor();
					
					// Updates the status bar with the selected editor
					MainWindow.getInstance().getStatusBar().updatesStatusBarFromFileEditor();
				}
			}
		});
	}

	/**
	 * Closes the opened file editors and store the window parameters.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public void closePreviousProjectConfiguration(ResourceBundle labels) {

		// Saves previous windows and panels
		MainWindow.getInstance().getProjectConfiguration()
				.saveMainWindowParameters();

		// Gets the selected file editor index
		int selectedFileEditorIndex = MainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// Gets the opened file editors number
		int numFileEditors = MainWindow.getInstance().getFileEditorManager()
				.getNumFileEditorPanels();

		// Search for modified opened file editors
		for (int index = numFileEditors - 1; index >= 0; index--) {

			// Starts from the last opened file editor
			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(index);

			// If it is modified
			if (MainWindow.getInstance().getFileEditorManager().isRedButton()) {

				// Do you want to save it?
				int choosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s643"), labels.getString("s953"),
						JOptionPane.YES_NO_OPTION);

				// If OK
				if (choosenOption == JOptionPane.OK_OPTION) {

					// Saves the editor
					MainWindow.getInstance().getMenu().getFile().saveOrSaveAS();
				}
			}
		}

		// Sets the original selected opened file editor
		MainWindow.getInstance().getFileEditorManager()
				.setSelectedFileEditorPanelAt(selectedFileEditorIndex);

		// Close the editors
		for (int position = 0; position < numFileEditors; position++) {
			MainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(0);
			MainWindow.getInstance().getFileEditorManager().getTabbedPane()
					.remove(0);
			MainWindow.getInstance().getFileEditorManager().getTabbedPane()
					.validate();
		}
	}

	/**
	 * Loads the lexicon configuration and apply it to the opened file editors.
	 * 
	 * @param labels
	 *            labels to display in the selected language.
	 */
	public void loadLexiconConfiguration(ResourceBundle labels) {

		// Gets the lexical configuration from the project configuration
		ResourceManager.getInstance().setProperty(
				"languagePath",
				MainWindow.getInstance().getProjectConfiguration()
						.getLexicalConfiguration());

		// Loads the lexicon configuration
		LexiconConfiguration lexiconConfiguration = LexiconConfiguration
				.getInstance();
		lexiconConfiguration.load(MainWindow.getInstance()
				.getProjectConfiguration().getLexicalConfiguration());

		// Gets the opened file editor panel number
		int numFileEditorPanels = MainWindow.getInstance()
				.getFileEditorManager().getNumFileEditorPanels();

		// Resets all the opened files with the new lexicon configuration
		for (int index = 0; index < numFileEditorPanels; index++)
			MainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(index).resetDocument();

		// Loads the language
		loadLanguage(labels);

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		labels = language.getLabels();

		// Updates the status bar
		MainWindow
				.getInstance()
				.getStatusBar()
				.setLexiconMessage(
						labels.getString("s449") + " "
								+ lexiconConfiguration.getName());
	}
}
