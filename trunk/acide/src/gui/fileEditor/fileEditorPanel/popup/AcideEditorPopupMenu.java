package gui.fileEditor.fileEditorPanel.popup;

import es.explorer.ExplorerFile;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Editor panel popup menu of ACIDE - A Configurable IDE.
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
 * @see JPopupMenu																													
 ***********************************************************************/
public class AcideEditorPopupMenu extends JPopupMenu {

	/**
	 * Class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the icon of the copy menu item.
	 */
	private final static String COPY = "./resources/icons/menu/edit/copy.png";
	/**
	 * Image file for the icon of the paste menu item.
	 */
	private final static String PASTE = "./resources/icons/menu/edit/paste.png";
	/**
	 * Image file for the icon of the cut menu item.
	 */
	private final static String CUT = "./resources/icons/menu/edit/cut.png";
	/**
	 * Image file for the icon of the select all menu item.
	 */
	private final static String SELECT_ALL = "./resources/icons/menu/edit/selectAll.png";
	/**
	 * Image file for the icon of the add file menu item.
	 */
	private static final String ADD_FILE = "./resources/icons/menu/project/addFile.png";
	/**
	 * Image file for the icon of the delete file menu item.
	 */
	private static final String DELETE_FILE = "./resources/icons/menu/project/deleteFile.png";
	/**
	 * Image file for the icon of the set main menu item.
	 */
	private static final String SET_MAIN = "./resources/icons/menu/project/setMain.png";
	/**
	 * Image file for the icon of the unset main menu item.
	 */
	private static final String UNSET_MAIN = "./resources/icons/menu/project/unsetMain.png";
	/**
	 * Image file for the icon of the set compilable menu item.
	 */
	private static final String SET_COMPILABLE = "./resources/icons/menu/project/setCompilable.png";
	/**
	 * Image file for the icon of the unset compilable menu item.
	 */
	private static final String UNSET_COMPILABLE = "./resources/icons/menu/project/unsetCompilable.png";
	/**
	 * Image file for the icon of the print file menu item.
	 */
	private final static String PRINT_FILE = "./resources/icons/menu/file/printFile.png";
	/**
	 * Add file menu item.
	 */
	private JMenuItem _addFile;
	/**
	 * Delete file menu item.
	 */
	private JMenuItem _deleteFile;
	/**
	 * Remove file menu item.
	 */
	private JMenuItem _removeFile;
	/**
	 * Select all menu item.
	 */
	private JMenuItem _selectAll;
	/**
	 * Copy menu item.
	 */
	private JMenuItem _copy;
	/**
	 * Paste menu item.
	 */
	private JMenuItem _paste;
	/**
	 * Cut menu item.
	 */
	private JMenuItem _cut;
	/**
	 * Set compilable menu item.
	 */
	private JMenuItem _setCompilable;
	/**
	 * Unset compilable menu item.
	 */
	private JMenuItem _unsetCompilable;
	/**
	 * Set main menu item.
	 */
	private JMenuItem _setMain;
	/**
	 * Unset main menu item.
	 */
	private JMenuItem _unsetMain;
	/**
	 * Print menu item.
	 */
	private JMenuItem _print;

	/**
	 * Class constructor.
	 */
	public AcideEditorPopupMenu() {

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
		final ResourceBundle labels = language.getLabels();

		_copy = new JMenuItem(labels.getString("s187"), new ImageIcon(COPY));
		_copy.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Updates the log
				AcideLog.getLog().info(labels.getString("s99"));

				// COPY
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
						.copy();
			}
		});
		add(_copy);

		// CUT
		_cut = new JMenuItem(labels.getString("s188"), new ImageIcon(CUT));
		_cut.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Updates the log
				AcideLog.getLog().info(labels.getString("s97"));

				// CUT
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
						.cut();
			}
		});
		add(_cut);

		// PASTE
		_paste = new JMenuItem(labels.getString("s189"), new ImageIcon(PASTE));
		_paste.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Updates the log
				AcideLog.getLog().info(labels.getString("s98"));

				// PASTE
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
						.paste();
			}
		});
		add(_paste);

		// SELECT ALL
		_selectAll = new JMenuItem(labels.getString("s191"), new ImageIcon(
				SELECT_ALL));
		_selectAll.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
						.setCaretPosition(0);
				int length = MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
						.getText().length();
				MainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex()).getActiveTextEditionArea()
						.setSelectionEnd(length);
			}
		});
		add(_selectAll);
		addSeparator();

		// ADD FILE
		_addFile = new JMenuItem(labels.getString("s17"), new ImageIcon(
				ADD_FILE));
		_addFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				try {

					String file = "";
					file = MainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(
									MainWindow.getInstance().getFileEditorManager()
											.getSelectedFileEditorPanelIndex())
							.getAbsolutePath();

					if (file != null) {

						TreePath path = MainWindow.getInstance().getExplorer()
								.getTree().getSelectionPath();
						DefaultMutableTreeNode filePath;
						ExplorerFile fc;

						// Folder selected
						if (path != null) {
							filePath = (DefaultMutableTreeNode) path
									.getLastPathComponent();
							fc = (ExplorerFile) filePath.getUserObject();

							// File selected
							if (!fc.isDirectory()) {
								filePath = MainWindow.getInstance()
										.getExplorer().getRoot().getNextNode();
								fc = (ExplorerFile) filePath.getUserObject();
							}

						} else {
							filePath = MainWindow.getInstance().getExplorer()
									.getRoot().getNextNode();
							fc = (ExplorerFile) filePath.getUserObject();
						}

						// Gets the name
						String name = "";
						int index = file.lastIndexOf("\\");
						if (index == -1)
							index = file.lastIndexOf("/");
						name = file.substring(index + 1, file.length());

						ExplorerFile fic = new ExplorerFile();
						fic.setPath(file);
						fic.setName(name);

						boolean isAdded = false;
						for (int i = 0; i < MainWindow.getInstance()
								.getProjectConfiguration()
								.getNumFilesFromList(); i++) {
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(i)
									.getPath().equals(fic.getPath())) {
								isAdded = true;
							}
						}

						if (!isAdded) {

							fic.setParent(MainWindow.getInstance()
									.getProjectConfiguration().getName());
							MainWindow.getInstance().getProjectConfiguration()
									.addFile(fic);
							MainWindow
									.getInstance()
									.getProjectConfiguration()
									.getFileAt(
											MainWindow.getInstance()
													.getProjectConfiguration()
													.getNumFilesFromList() - 1)
									.setIsOpened(true);
							DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(
									fic);
							defaultMutableTreeNode.setAllowsChildren(false);
							filePath.add(defaultMutableTreeNode);
							MainWindow.getInstance().validate();
							MainWindow.getInstance().repaint();
							MainWindow.getInstance().getExplorer()
									.getTreeModel().reload();
							MainWindow.getInstance().getExplorer().expandTree();
							MainWindow.getInstance().getExplorer()
									.getPopupMenu().getRemoveFile()
									.setEnabled(true);
							MainWindow.getInstance().getExplorer()
									.getPopupMenu().getDeleteFile()
									.setEnabled(true);
							MainWindow.getInstance().getProjectConfiguration()
									.setIsModified(true);
						}
					}
				} catch (Exception exception) {
					
					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
			}
		});

		// REMOVE FILE
		_removeFile = new JMenuItem(labels.getString("s618"));
		_removeFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Are you sure?
				int chosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s623"));

				// If ok
				if (chosenOption == JOptionPane.OK_OPTION) {

					ExplorerFile explorerFile = new ExplorerFile();
					int posExplorer = -1;
					int selectedEditorIndex = MainWindow.getInstance()
							.getFileEditorManager().getSelectedFileEditorPanelIndex();

					for (int position1 = 0; position1 < MainWindow.getInstance()
							.getProjectConfiguration().getNumFilesFromList(); position1++) {

						if (MainWindow
								.getInstance()
								.getProjectConfiguration()
								.getFileAt(position1)
								.getPath()
								.equals(MainWindow.getInstance()
										.getFileEditorManager().getSelectedFileEditorPanel()
										.getAbsolutePath())) {
							
							explorerFile = MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(position1);
							
							for (int position2 = 0; position2 < MainWindow.getInstance()
									.getProjectConfiguration()
									.getNumFilesFromList() + 1; position2++) {
								
								if (MainWindow.getInstance().getExplorer()
										.getTree().getPathForRow(position2)
										.getLastPathComponent().toString()
										.equals(explorerFile.getLastPathComponent())) {

									posExplorer = position2;
								}
							}
						}
					}

					// Gets the selected tree node
					TreePath currentSelection = MainWindow.getInstance()
							.getExplorer().getTree().getPathForRow(posExplorer);

					// Something selected
					if (currentSelection != null) {

						DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
								.getLastPathComponent());
						ExplorerFile p = (ExplorerFile) currentNode
								.getUserObject();

						// Is not a directory
						if (!p.isDirectory()) {

							// Gets the parent
							MutableTreeNode parent = (MutableTreeNode) (currentNode
									.getParent());

							// Has parent
							if (parent != null) {

								// Removes the node parent
								MainWindow.getInstance().getExplorer()
										.getTreeModel()
										.removeNodeFromParent(currentNode);

								// Searches for the file into the project configuration file list
								int posProjectList = -1;

								for (int position = 0; position < MainWindow.getInstance()
										.getProjectConfiguration()
										.getNumFilesFromList(); position++) {

									if (MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(position).getPath()
											.equals(p.getPath())) {
										posProjectList = position;
									}
								}

								// Removes the file
								MainWindow.getInstance()
										.getProjectConfiguration()
										.removeFileAt(posProjectList);
								
								// Updates the status bar
								MainWindow.getInstance().getStatusBar()
										.setMessage("");
								
								// The project has been modified
								MainWindow.getInstance()
										.getProjectConfiguration()
										.setIsModified(true);

								// If the file has been modified
								if (MainWindow.getInstance().getFileEditorManager()
										.isRedButton(selectedEditorIndex)) {

									// Do you want to save it?
									chosenOption = JOptionPane.showConfirmDialog(
											null, labels.getString("s643"),
											labels.getString("s953"),
											JOptionPane.YES_NO_OPTION);

									// If ok
									if (chosenOption == JOptionPane.OK_OPTION) {
										MainWindow.getInstance().getMenu()
												.getFile().getSaveFile()
												.setEnabled(true);
										MainWindow.getInstance().getMenu()
												.getFile().getSaveFile()
												.doClick();
									}
								}

								// Closes the tab
								MainWindow.getInstance().getFileEditorManager()
										.getTabbedPane().remove(selectedEditorIndex);
								
								// No more tabs
								if (MainWindow.getInstance().getFileEditorManager()
										.getTabbedPane().getTabCount() == 0) {
									
									// Disables the file and edit menu item
									MainWindow.getInstance().getMenu()
											.disableFileMenu();
									MainWindow.getInstance().getMenu()
											.disableEditMenu();
								}

								return;
							}
						}

					}

					// IF THERE ARE MORE FILE IN THE PROJECT CONFIGURATION
					if (MainWindow.getInstance().getProjectConfiguration()
							.getNumFilesFromList() > 0) {
						
						// UPDATES THE EXPLORER POPUP MENU 
						MainWindow.getInstance().getExplorer().getPopupMenu()
								.getRemoveFile().setEnabled(true);
						MainWindow.getInstance().getExplorer().getPopupMenu()
								.getDeleteFile().setEnabled(true);
					} else {
						
						// UPDATES THE EXPLORER POPUP MENU
						MainWindow.getInstance().getExplorer().getPopupMenu()
								.getRemoveFile().setEnabled(false);
						MainWindow.getInstance().getExplorer().getPopupMenu()
								.getDeleteFile().setEnabled(false);
					}
				}
			}
		});
		add(_removeFile);

		// DELETE FILE
		_deleteFile = new JMenuItem(labels.getString("s950"), new ImageIcon(
				DELETE_FILE));
		_deleteFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
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

				int chosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s951"));
				if (chosenOption == JOptionPane.OK_OPTION) {

					// DEFAULT PROJECT
					if (MainWindow.getInstance().getProjectConfiguration()
							.isDefaultProject()) {

						// DELETE THE FILE
						String fileRemove = MainWindow.getInstance()
								.getFileEditorManager().getSelectedFileEditorPanel()
								.getAbsolutePath();
						File file = new File(fileRemove);
						file.delete();

						// REMOVES THE TAB IN THE EDITOR
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.getTabbedPane()
								.remove(MainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex());

						// SET THE STATUS BAR
						MainWindow.getInstance().getStatusBar().setMessage("");

					} else {

						// NOT DEFAULT PROJECT
						ExplorerFile explorerFile = new ExplorerFile();
						
						int posExplorer = -1;
						int selectedEditor = MainWindow.getInstance()
								.getFileEditorManager().getSelectedFileEditorPanelIndex();
						
						for (int position1 = 0; position1 < MainWindow.getInstance()
								.getProjectConfiguration()
								.getNumFilesFromList(); position1++) {

							if (MainWindow
									.getInstance()
									.getProjectConfiguration()
									.getFileAt(position1)
									.getPath()
									.equals(MainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getAbsolutePath())) {

								explorerFile = MainWindow.getInstance()
										.getProjectConfiguration().getFileAt(position1);
								for (int position2 = 0; position2 < MainWindow.getInstance()
										.getProjectConfiguration()
										.getNumFilesFromList() + 1; position2++) {

									if (MainWindow.getInstance().getExplorer()
											.getTree().getPathForRow(position2)
											.getLastPathComponent().toString()
											.equals(explorerFile.getLastPathComponent())) {

										posExplorer = position2;
									}
								}
							}
						}

						// GET THE SELECTED NODE
						TreePath currentSelection = MainWindow.getInstance()
								.getExplorer().getTree().getPathForRow(posExplorer);

						// BELONGS TO THE PROJECT
						if (currentSelection != null) {

							DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
									.getLastPathComponent());
							ExplorerFile p = (ExplorerFile) currentNode
									.getUserObject();

							// IS NOT A DIRECTORY
							if (!p.isDirectory()) {

								// GET THE PARENT
								MutableTreeNode parent = (MutableTreeNode) (currentNode
										.getParent());

								// HAS PARENT
								if (parent != null) {

									// REMOVE THE PARENT
									MainWindow.getInstance().getExplorer()
											.getTreeModel()
											.removeNodeFromParent(currentNode);

									int posProjectList = -1;
									
									for (int position = 0; position < MainWindow
											.getInstance()
											.getProjectConfiguration()
											.getNumFilesFromList(); position++)
										if (MainWindow.getInstance()
												.getProjectConfiguration()
												.getFileAt(position).getPath()
												.equals(p.getPath()))
											posProjectList = position;

									ExplorerFile f = MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(posProjectList);
									String fileRemove = f.getPath();
									
									// REMOVE THE FILE FROM THE PROJECT CONFIGURATION
									MainWindow.getInstance()
											.getProjectConfiguration()
											.removeFileAt(posProjectList);

									File file = new File(fileRemove);
									file.delete();

									// Updates the status bar
									MainWindow.getInstance().getStatusBar()
											.setMessage("");
									
									// THE PROJECT HAS BEEN MODIFIED
									MainWindow.getInstance()
											.getProjectConfiguration()
											.setIsModified(true);
									MainWindow.getInstance().getFileEditorManager()
											.getTabbedPane().remove(selectedEditor);

									return;
								}
							}
						} else {

							// NOT BELONGS TO THE PROJECT
							String fileRemove = MainWindow.getInstance()
									.getFileEditorManager().getSelectedFileEditorPanel()
									.getAbsolutePath();
							
							// DELETE THE FILE
							File file = new File(fileRemove);
							file.delete();
							
							// CLOSES THE TAB
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.remove(MainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanelIndex());
							
							// Updates the status bar
							MainWindow.getInstance().getStatusBar()
									.setMessage("");
						}

						// IF THERE ARE MORE FILES IN THE PROJECT CONFIGURATION
						if (MainWindow.getInstance().getProjectConfiguration()
								.getNumFilesFromList() > 0) {
							
							// UPDATES THE EXPLORER POPUP MENU
							MainWindow.getInstance().getExplorer()
									.getPopupMenu().getRemoveFile()
									.setEnabled(true);
							MainWindow.getInstance().getExplorer()
									.getPopupMenu().getDeleteFile()
									.setEnabled(true);
						} else {
							
							// UPDATES THE EXPLORER POPUP MENU
							MainWindow.getInstance().getExplorer()
									.getPopupMenu().getRemoveFile()
									.setEnabled(false);
							MainWindow.getInstance().getExplorer()
									.getPopupMenu().getDeleteFile()
									.setEnabled(false);
						}
					}
				}
			}
		});
		add(_deleteFile);
		addSeparator();

		// SET COMPILABLE
		_setCompilable = new JMenuItem(labels.getString("s254"), new ImageIcon(
				SET_COMPILABLE));
		_setCompilable.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getFileEditorManager().setCompilableFile();
			}
		});
		add(_setCompilable);

		// UNSET COMPILABLE
		_unsetCompilable = new JMenuItem(labels.getString("s255"),
				new ImageIcon(UNSET_COMPILABLE));
		_unsetCompilable.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getFileEditorManager()
						.unsetCompilableFile();
			}
		});
		add(_unsetCompilable);

		// SET MAIN
		_setMain = new JMenuItem(labels.getString("s256"), new ImageIcon(
				SET_MAIN));
		_setMain.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getFileEditorManager().setMainFile();
			}
		});
		add(_setMain);

		// UNSET MAIN
		_unsetMain = new JMenuItem(labels.getString("s952"), new ImageIcon(
				UNSET_MAIN));
		_unsetMain.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getFileEditorManager().unsetMainFile();
			}
		});
		add(_unsetMain);
		addSeparator();

		// PRINT MENU
		_print = new JMenuItem(labels.getString("s624"), new ImageIcon(
				PRINT_FILE));
		_print.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				MainWindow.getInstance().getMenu().getFile().getPrintFile()
						.setEnabled(true);
				MainWindow.getInstance().getMenu().getFile().getPrintFile()
						.doClick();
			}
		});
		add(_print);
	}

	/**
	 * Returns the cut menu item.
	 * 
	 * @return the cut menu item.
	 */
	public JMenuItem getCut() {
		return _cut;
	}

	/**
	 * Returns the copy menu item.
	 * 
	 * @return the copy menu item.
	 */
	public JMenuItem getCopy() {
		return _copy;
	}

	/**
	 * Returns the paste menu item.
	 * 
	 * @return the paste menu item.
	 */
	public JMenuItem getPaste() {
		return _paste;
	}

	/**
	 * Returns the add file menu item.
	 * 
	 * @return the add file menu item.
	 */
	public JMenuItem getAddFile() {
		return _addFile;
	}

	/**
	 * Returns the remove file menu item.
	 * 
	 * @return the remove file menu item.
	 */
	public JMenuItem getRemoveFile() {
		return _removeFile;
	}

	/**
	 * Returns the set compilable menu item.
	 * 
	 * @return the set compilable menu item.
	 */
	public JMenuItem getSetCompilable() {
		return _setCompilable;
	}

	/**
	 * Returns the unset compilable menu item.
	 * 
	 * @return the unset compilable menu item.
	 */
	public JMenuItem getUnsetCompilable() {
		return _unsetCompilable;
	}

	/**
	 * Returns the set main menu item.
	 * 
	 * @return the set main menu item.
	 */
	public JMenuItem getSetMain() {
		return _setMain;
	}

	/**
	 * Returns the unset main menu item.
	 * 
	 * @return the unset main menu item.
	 */
	public JMenuItem getUnsetMain() {
		return _unsetMain;
	}
}
