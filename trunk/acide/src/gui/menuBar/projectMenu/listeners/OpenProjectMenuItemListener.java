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
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
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
			language.getLanguage(ResourceManager
					.getInstance().getProperty("language"));
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

				// If cancel
				if (chosenOption == JOptionPane.CANCEL_OPTION)
					cancelOptionSelected = true;

				// If yes
				if (chosenOption == JOptionPane.OK_OPTION) {
					MainWindow.getInstance().getMenu().getProject().getSaveProject()
							.setEnabled(true);
					MainWindow.getInstance().getMenu().getProject().getSaveProject()
							.doClick();
				}
			}

			// If ok
			if (!cancelOptionSelected) {

				// Puts the wait cursor
				MainWindow.getInstance().setCursor(Cursor
						.getPredefinedCursor(Cursor.WAIT_CURSOR));
				
				// Updates the status bar
				MainWindow.getInstance().getStatusBar().setMessage("");

				// Loads the file into a new thread
				Thread thread = new Thread() {
					/*
					 * (non-Javadoc)
					 * @see java.lang.Thread#run()
					 */
					public void run() {
						load(file);
					}
				};
				thread.start();

				// Loads the language
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
				MainWindow.getInstance().getStatusBar().setLexiconMessage(
						labels.getString("s449") + " ");

				// Loads the menu configuration
				String currentMenu = null;
				try {
					
					// Gets the menu configuration file path
					currentMenu = MainWindow.getInstance().getProjectConfiguration()
							.getMenu();
					
					// Loads the new menu item list
					MenuConfiguration.getInstance().setMenuElementList(MenuConfiguration
							.getInstance().loadMenuConfigurationFile(currentMenu));
					
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
							+ currentMenu.substring(index + 1, currentMenu
									.length());

					try {
						
						// Load the new menu item list
						MenuConfiguration.getInstance().setMenuElementList(MenuConfiguration
								.getInstance().loadMenuConfigurationFile(currentMenu));
						
						// Updates the RESOURCE MANAGER
						ResourceManager.getInstance().setProperty(
								"currentMenuConfiguration", currentMenu2);
						
						// Error message
						JOptionPane.showMessageDialog(null, labels
								.getString("s956")
								+ currentMenu
								+ labels.getString("s957")
								+ currentMenu2);
					} catch (Exception exception1) {

						try {
							MenuConfiguration.getInstance().setMenuElementList(MenuConfiguration
									.getInstance().loadMenuConfigurationFile(currentMenu));
						} catch (Exception exception2) {
							
							// Updates the log
							AcideLog.getLog().error(exception2.getMessage());
							exception2.printStackTrace();
						}
						
						// Updates the RESOURCE MANAGER
						ResourceManager
								.getInstance().setProperty("currentMenuConfiguration",
										"./configuration/menu/defaultAllOn.menuCfg");
						
						// Error message
						JOptionPane.showMessageDialog(null, labels
								.getString("s956")
								+ currentMenu + labels.getString("s959"));
						
						// Updates the log
						AcideLog.getLog().error(exception1.getMessage());
						exception1.printStackTrace();
					}
				}
				MainWindow.getInstance().getMenu().buildMenu();
				MainWindow.getInstance().validate();
				MainWindow.getInstance().repaint();
				MainWindow.getInstance().getMenu().getConfiguration().getMenu()
						.getSaveMenu().setEnabled(true);
				MenuConfigurationWindow.setChangesAreSaved(true);

				// Loads the grammar configuration
				try {

					String currentGrammar = MainWindow.getInstance()
							.getProjectConfiguration()
							.getSyntacticConfiguration();

					// Gets the grammar name
					int index = currentGrammar.lastIndexOf("\\");
					if (index == -1)
						index = currentGrammar.lastIndexOf("/");
					String grammarName = currentGrammar.substring(
							index + 1, currentGrammar.length() - 4);

					// Updates the status bar
					MainWindow.getInstance().getStatusBar().setGrammarMessage(
							labels.getString("s248") + " " + grammarName);
					
					// Updates the RESOURCE MANAGER
					ResourceManager.getInstance().setProperty("currentGrammar",
							currentGrammar);
				} catch (Exception exception) {

					// Error message
					JOptionPane.showMessageDialog(null, exception.getMessage(),
							labels.getString("s944"),
							JOptionPane.ERROR_MESSAGE);
					
					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}

				// Loads the shell configuration		
				OutputConfiguration.getInstance().load(MainWindow.getInstance()
						.getProjectConfiguration().getOutputConfiguration());
				
				MainWindow.getInstance().getOutput().executeExitCommand();
				MainWindow.getInstance().getOutput().resetOutput();

				// Loads the tool bar configuration
				String currentToolBarConfiguration = null;
				try {
					ShellCommandList.clear();
					currentToolBarConfiguration = MainWindow.getInstance()
							.getProjectConfiguration().getToolBar();
					ShellCommandList
							.loadList(currentToolBarConfiguration);
					
					// Updates the RESOURCE MANAGER
					ResourceManager.getInstance().setProperty(
							"currentToolBarConfiguration",
							currentToolBarConfiguration);
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
					
					// Gets the name
					String currentToolBarConfiguration2;
					int index = currentToolBarConfiguration
							.lastIndexOf("\\");
					if (index == -1)
						index = currentToolBarConfiguration
								.lastIndexOf("/");
					currentToolBarConfiguration2 = ".\\configuration\\toolbar\\"
							+ currentToolBarConfiguration.substring(
									index + 1, currentToolBarConfiguration
											.length());
					try {
						ShellCommandList
								.loadList(currentToolBarConfiguration2);
						JOptionPane.showMessageDialog(null, labels
								.getString("s958")
								+ currentToolBarConfiguration
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
							ShellCommandList
									.loadList("./configuration/toolbar/default.TBcfg");
						} catch (Exception exception2) {
							
							// Updates the log
							AcideLog.getLog().error(exception2.getMessage());
							exception2.printStackTrace();
						}
						
						JOptionPane.showMessageDialog(null, labels
								.getString("s958")
								+ currentToolBarConfiguration
								+ labels.getString("s959"));
						
						// Updates the RESOURCE MANAGER
						ResourceManager.getInstance().setProperty(
								"currentToolBarConfiguration",
								"./configuration/toolbar/default.TBcfg");
					}
				}
				
				MainWindow.getInstance().buildToolBar();
				MainWindow.getInstance().validate();
				MainWindow.getInstance().repaint();
				MainWindow.getInstance().getMenu().getConfiguration().getToolBar()
						.getSaveToolBar().setEnabled(true);
				ToolBarConfigurationWindow.setAreChangesSaved(true);

				// Loads the explorer configuration
				MainWindow.getInstance().getExplorer().getRoot().removeAllChildren();
				ExplorerFile explorerFile = new ExplorerFile();
				explorerFile.setPath(MainWindow.getInstance().getProjectConfiguration()
						.getName());
				explorerFile.setName(MainWindow.getInstance().getProjectConfiguration()
						.getName());
				explorerFile.setIsDirectory(true);
				explorerFile.setParent(null);
				MainWindow.getInstance().setTitle(labels.getString("s425") + " - "
						+ MainWindow.getInstance().getProjectConfiguration().getName());
				DefaultMutableTreeNode d = new DefaultMutableTreeNode(
						explorerFile);
				d.setAllowsChildren(true);
				MainWindow.getInstance().getExplorer().getRoot().add(d);
				ArrayList<DefaultMutableTreeNode> listdir = new ArrayList<DefaultMutableTreeNode>();
				
				for (int i = 0; i < MainWindow.getInstance().getProjectConfiguration()
						.getNumFilesFromList(); i++) {

					DefaultMutableTreeNode h = new DefaultMutableTreeNode(
							MainWindow.getInstance().getProjectConfiguration().getFileAt(
									i));
					if (MainWindow.getInstance().getProjectConfiguration().getFileAt(i)
							.isDirectory()) {
						h.setAllowsChildren(true);
						listdir.add(h);
					} else
						h.setAllowsChildren(false);

					if (MainWindow.getInstance().getProjectConfiguration().getFileAt(i)
							.getParent().equals(
									MainWindow.getInstance().getProjectConfiguration()
											.getName())) {
						d.add(h);
					} else {
						
						DefaultMutableTreeNode fh = searchDirectoryList(
								listdir, MainWindow.getInstance()
										.getProjectConfiguration()
										.getFileAt(i).getParent());

						fh.add(h);
					}
				}

				MainWindow.getInstance().getExplorer().getTreeModel().reload();
				MainWindow.getInstance().getExplorer().expandTree();
				if (MainWindow.getInstance().getProjectConfiguration()
						.getNumFilesFromList() > 0) {
					MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile()
							.setEnabled(true);
					MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile()
							.setEnabled(true);
				} else {
					MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile()
							.setEnabled(false);
					MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile()
							.setEnabled(false);
				}
				MainWindow.getInstance().validate();
				MainWindow.getInstance().repaint();

				if (!MainWindow.getInstance().getMenu().getView().getShowExplorerPanel()
						.isSelected())
					MainWindow.getInstance().getExplorer().showExplorer();

				MainWindow.getInstance().getMenu().getView().getShowExplorerPanel()
						.setSelected(true);
				MainWindow.getInstance().getProjectConfiguration().setIsModified(false);
				MainWindow.getInstance().getProjectConfiguration().setFirstSave(true);
				MainWindow.getInstance().getMenu().enableProjectMenu();

				// Sets the default cursor
				MainWindow.getInstance().setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			}
		}
	}

	/**
	 * Loads a project from a file.
	 * 
	 * @param file
	 *            file path.
	 */
	public void load(String file) {

		String fileContent = null;

		ResourceBundle labels = AcideLanguage.getInstance().getLabels();
		TextFile textFile = AcideIOFactory.getInstance().buildFile();

		// Sets the wait cursor
		Cursor cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
		MainWindow.getInstance().setCursor(cursor);

		// Saves previous windows and panels
		MainWindow.getInstance().getProjectConfiguration().saveMainWindowParameters();

		// Saves associated files to the project
		int selectedEditor = MainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanelIndex();
		
		int numEditors = MainWindow.getInstance().getFileEditorManager().getNumFileEditorPanels();
		MainWindow.getInstance().getFileEditorManager().setSelectedFileEditorPanelAt(numEditors - 1);
		
		// Search for modified editors
		for (int position = numEditors - 1; position >= 0; position--) {
			
			MainWindow.getInstance().getFileEditorManager().setSelectedFileEditorPanelAt(position);
			
			// If it is modified
			if (MainWindow.getInstance().getFileEditorManager().isRedButton()) {
				
				// Do you want to save it?
				int choosenOption = JOptionPane.showConfirmDialog(null, labels
						.getString("s643"), labels.getString("s953"),
						JOptionPane.YES_NO_OPTION);

				// If yes
				if (choosenOption == JOptionPane.OK_OPTION) {
					MainWindow.getInstance().getMenu().getFile().saveOrSaveAS();
				}
			}
		}
		
		// Sets the selected editor to the original
		MainWindow.getInstance().getFileEditorManager().setSelectedFileEditorPanelAt(selectedEditor);

		// Close the editors
		for (int position = 0; position < numEditors; position++) {
			MainWindow.getInstance().getFileEditorManager().setSelectedFileEditorPanelAt(0);
			MainWindow.getInstance().getFileEditorManager().getTabbedPane().remove(0);
			MainWindow.getInstance().getFileEditorManager().getTabbedPane().validate();
		}

		fileContent = textFile.load(file);

		// Updates the RESOURCE MANAGER
		ResourceManager.getInstance().setProperty("defaultAcideProject", file);
		MainWindow.getInstance().getProjectConfiguration().setPath(file);
		MainWindow.getInstance().getProjectConfiguration().removeFiles();
		MainWindow.getInstance().getProjectConfiguration().load(fileContent);

		// IS EXPLORER SHOWED?
		if (!MainWindow.getInstance().getProjectConfiguration().isExplorerShowed())
			MainWindow.getInstance().getMenu().getView().getShowExplorerPanel().doClick();

		// IS SHELL SHOWED?
		if (!MainWindow.getInstance().getProjectConfiguration().isShellShowed())
			MainWindow.getInstance().getMenu().getView().getShowShellWindow().doClick();

		// WINDOW PARAMETERS
		MainWindow.getInstance().setSize(MainWindow.getInstance().getProjectConfiguration()
				.getWindowWidth(), MainWindow.getInstance().getProjectConfiguration()
				.getWindowHeight());
		MainWindow.getInstance().setLocation(MainWindow.getInstance().getProjectConfiguration()
				.getPosX(), MainWindow.getInstance().getProjectConfiguration().getPosY());

		// LOCATION OF THE SPLIT PANE
		MainWindow.getInstance().getSplitPaneVertical().setDividerLocation(
				MainWindow.getInstance().getProjectConfiguration()
						.getSplitPaneVerticalDividerLocation());
		MainWindow.getInstance().getSplitPaneHorizontal().setDividerLocation(
				MainWindow.getInstance().getProjectConfiguration()
						.getSplitPanelHorizontalDividerLocation());

		MainWindow.getInstance().validate();
		MainWindow.getInstance().repaint();
		MainWindow.getInstance().setVisible(true);

		// Open files
		for (int position = 0; position < MainWindow.getInstance().getProjectConfiguration()
				.getNumFilesFromList(); position++) {

			// If it is a file
			if (!MainWindow.getInstance().getProjectConfiguration().getFileAt(position)
					.isDirectory()) {

				TextFile ff = AcideIOFactory.getInstance().buildFile();
				String text = ff.load(MainWindow.getInstance().getProjectConfiguration()
						.getFileAt(position).getPath());

				String name2 = null;
				String file2 = MainWindow.getInstance().getProjectConfiguration()
						.getFileAt(position).getPath();

				if (file2 != null) {

					// Gets the name
					int index = file2.lastIndexOf("\\");
					if (index == -1)
						index = file2.lastIndexOf("/");
					name2 = file2.substring(index + 1, file2.length());
				}

				if (MainWindow.getInstance().getProjectConfiguration().getFileAt(position)
						.isOpened()) {

					MainWindow.getInstance().getMenu().enableFileMenu();
					MainWindow.getInstance().getMenu().enableEditMenu();

					// Checks the type
					int type = 0;

					// Updates the status bar
					MainWindow.getInstance().getStatusBar().setMessage(
							MainWindow.getInstance().getProjectConfiguration().getFileAt(
									position).getPath());

					// COMPILABLE FILE
					if (MainWindow.getInstance().getProjectConfiguration().getFileAt(position)
							.isCompilableFile()) {
						type = 2;

						MainWindow.getInstance().getStatusBar().setMessage(
								MainWindow.getInstance().getProjectConfiguration()
										.getFileAt(position).getPath()
										+ " <COMPILABLE>");
					}

					// MAIN FILE
					if (MainWindow.getInstance().getProjectConfiguration().getFileAt(position)
							.isMainFile()) {
						type = 1;

						MainWindow.getInstance().getStatusBar().setMessage(
								MainWindow.getInstance().getProjectConfiguration()
										.getFileAt(position).getPath()
										+ " <MAIN>");
					}

					// Creates a new tab
					MainWindow.getInstance().getFileEditorManager().newTab(name2, file2,
							text, true, type);
					
					// Updates the main window
					MainWindow.getInstance().validate();
					MainWindow.getInstance().repaint();

					// UNDO REDO MANAGER
					int numEditor = MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanelIndex();
					DefaultStyledDocument document = MainWindow.getInstance()
							.getFileEditorManager().getSelectedFileEditorPanel()
							.getSyntaxDocument();

					document.addUndoableEditListener(new UndoableEditListener() {
						/*
						 * (non-Javadoc)
						 * 
						 * @seejavax.swing.event.UndoableEditListener#
						 * undoableEditHappened
						 * (javax.swing.event.UndoableEditEvent)
						 */
						@Override
						public void undoableEditHappened(
								UndoableEditEvent undoableEditEvent) {

							UndoableEdit edit = undoableEditEvent.getEdit();
							
							if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
									.getType() == DefaultDocumentEvent.EventType.CHANGE))) {
								
								// Gets the selected editor index
								int selectedEditorIndex = MainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanelIndex();
								
								// Set the edit property over the selected editor undo manager
								MainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditorIndex)
								.getUndoManager().addEdit(undoableEditEvent
										.getEdit());
							}
						}
					});

					// Sets the caret in the first position of the editor
					numEditor = MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanelIndex();
					MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(numEditor)
							.getActiveTextEditionArea().setCaretPosition(0);
				}
			}
		}

		// Updates the RESOURCE MANAGER
		ResourceManager.getInstance().setProperty("languagePath", MainWindow.getInstance()
				.getProjectConfiguration().getLexicalConfiguration());

		LexiconConfiguration lexiconConfiguration = LexiconConfiguration.getInstance();
		lexiconConfiguration.load(MainWindow.getInstance().getProjectConfiguration()
				.getLexicalConfiguration());

		// Resets all the opened files with the new lexicon configuration
		int numEditors2 = MainWindow.getInstance().getFileEditorManager()
				.getNumFileEditorPanels();
		for (int i = 0; i < numEditors2; i++)
			MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(i)
					.resetDocument();

		// Loads language
		String languageSelected = MainWindow.getInstance().getProjectConfiguration()
				.getLanguage();

		// SPANISH
		if (languageSelected.equals("spanish"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getSpanish().doClick();
		// ENGLISH
		if (languageSelected.equals("english"))
			MainWindow.getInstance().getMenu().getConfiguration().getLanguage()
					.getEnglish().doClick();

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
		labels = language.getLabels();
		
		// Updates the status bar
		MainWindow.getInstance().getStatusBar().setLexiconMessage(
				labels.getString("s449") + " " + lexiconConfiguration.getName());

		// Sets the default cursor
		cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
		MainWindow.getInstance().setCursor(cursor);
	}
}
