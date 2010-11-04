package gui.editor;

import es.explorer.ExplorerFile;
import gui.MainWindow;

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

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;

/**
 * Editor popup menu.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class EditorPopupMenu extends JPopupMenu{

	/**
	 * serialVersionUID.
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
	 * Constructor of the class.
	 */
	public EditorPopupMenu(){
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager
					.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS TO DISPLAY
		final ResourceBundle labels = language.getLabels();
		
		_copy = new JMenuItem(labels.getString("s187"), new ImageIcon(COPY));
		_copy.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				// GET THE LANGUAGE
				Language language = Language.getInstance();
				try {
					language.getLanguage(PropertiesManager
							.getProperty("language"));
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				
				// GET THE LABELS
				ResourceBundle labels = language.getLabels();
				
				// UPDATES THE LOG
				Log.getLog().info(labels.getString("s99"));
				
				// COPY
				MainWindow.getInstance().getEditorBuilder()
						.getEditorAt(
								MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
						.getEditor().copy();
			}
		});
		add(_copy);
		
		// CUT
		_cut = new JMenuItem(labels.getString("s188"), new ImageIcon(CUT));
		_cut.addActionListener(new ActionListener() {
			
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				// GET THE LANGUAGE
				Language language = Language.getInstance();
				try {
					language.getLanguage(PropertiesManager
							.getProperty("language"));
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				
				// GET THE LABELS
				ResourceBundle labels = language.getLabels();
				
				// UPDATES THE LOG
				Log.getLog().info(labels.getString("s97"));
				
				// CUT
				MainWindow.getInstance().getEditorBuilder()
						.getEditorAt(
								MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
						.getEditor().cut();
			}
		});
		add(_cut);
		
		// PASTE
		_paste = new JMenuItem(labels.getString("s189"), new ImageIcon(PASTE));
		_paste.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				// GET THE LANGUAGE
				Language language = Language.getInstance();
				
				try {
					language.getLanguage(PropertiesManager
							.getProperty("language"));
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				
				// GET THE LABELS
				ResourceBundle labels = language.getLabels();
				
				// UPDATES THE LOG
				Log.getLog().info(labels.getString("s98"));
				
				// PASTE
				MainWindow.getInstance().getEditorBuilder()
						.getEditorAt(
								MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
						.getEditor().paste();
			}
		});
		add(_paste);
		
		// SELECT ALL
		_selectAll = new JMenuItem(labels.getString("s191"), new ImageIcon(SELECT_ALL));
		_selectAll.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				MainWindow.getInstance().getEditorBuilder()
						.getEditorAt(
								MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
						.getEditor().setCaretPosition(0);
				int length = MainWindow.getInstance()
						.getEditorBuilder()
						.getEditorAt(
								MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
						.getEditor().getText().length();
				MainWindow.getInstance().getEditorBuilder()
						.getEditorAt(
								MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
						.getEditor().setSelectionEnd(length);
			}
		});
		add(_selectAll);
		addSeparator();
		
		// ADD FILE
		_addFile = new JMenuItem(labels.getString("s17"), new ImageIcon(ADD_FILE));
		_addFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				
				try {

					String file = "";
					file = MainWindow.getInstance().getEditorBuilder()
							.getEditorAt(MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
							.getAbsolutePath();
					
					if (file != null) {
						
						TreePath path = MainWindow.getInstance().getExplorer().getTree().getSelectionPath();
						DefaultMutableTreeNode filePath;
						ExplorerFile fc;
						
						// FOLDER SELECTED
						if (path != null) {
							filePath = (DefaultMutableTreeNode) path
									.getLastPathComponent();
							fc = (ExplorerFile) filePath.getUserObject();

							// FILE SELECTED
							if (!fc.isDirectory()) {
								filePath = MainWindow.getInstance().getExplorer().getRoot().getNextNode();
								fc = (ExplorerFile) filePath.getUserObject();
							}

						} else {
							filePath = MainWindow.getInstance().getExplorer().getRoot().getNextNode();
							fc = (ExplorerFile) filePath.getUserObject();
						}

						// GET THE NAME
						String name = "";
						int index = file.lastIndexOf("\\");
						if (index == -1)
							index = file.lastIndexOf("/");	
						name = file.substring(index + 1, file.length());
					
						ExplorerFile fic = new ExplorerFile();
						fic.setPath(file);
						fic.setName(name);

						boolean isAdded = false;
						for (int i = 0; i < MainWindow.getInstance().getProjectConfiguration()
								.getNumFilesFromList(); i++) {
							if (MainWindow.getInstance().getProjectConfiguration().getFileAt(i).getPath()
									.equals(fic.getPath())) {
								isAdded = true;
							}
						}

						if (!isAdded) {

							fic.setParent(MainWindow.getInstance().getProjectConfiguration().getName());
							MainWindow.getInstance().getProjectConfiguration().addFile(fic);
							MainWindow.getInstance().getProjectConfiguration()
									.getFileAt(
											MainWindow.getInstance().getProjectConfiguration()
													.getNumFilesFromList() - 1)
									.setIsOpened(true);
							DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(fic);
							defaultMutableTreeNode.setAllowsChildren(false);
							filePath.add(defaultMutableTreeNode);
							MainWindow.getInstance().validate();
							MainWindow.getInstance().repaint();
							MainWindow.getInstance().getExplorer().getTreeModel().reload();
							MainWindow.getInstance().getExplorer().expandTree();
							MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile().setEnabled(true);
							MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile().setEnabled(true);
							MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
						}
					}
				} catch (Exception ex) {
					ex.printStackTrace();
				}
			}
		});
		
		// REMOVE FILE
		_removeFile = new JMenuItem(labels.getString("s618"));
		_removeFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				
				// GET THE LANGUAGE
				Language language = Language.getInstance();
				
				try {
					language.getLanguage(PropertiesManager.getProperty("language"));
				} catch (Exception e) {
					e.printStackTrace();
				}
				
				// GET THE LABELS
				ResourceBundle labels = language.getLabels();
				
				// ASK THE USER
				int chosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s623"));
				
				// IF OK
				if (chosenOption == JOptionPane.OK_OPTION) {

					
					ExplorerFile f = new ExplorerFile();
					int y = -1;
					int selectedEditor = MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex();
					for (int j = 0; j < MainWindow.getInstance().getProjectConfiguration()
							.getNumFilesFromList(); j++) {

						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(j)
								.getPath()
								.equals(MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
										.getAbsolutePath())) {
							f = MainWindow.getInstance().getProjectConfiguration().getFileAt(j);
							for (int m = 0; m < MainWindow.getInstance().getProjectConfiguration()
									.getNumFilesFromList() + 1; m++) {
								if (MainWindow.getInstance().getExplorer().getTree().getPathForRow(m)
										.getLastPathComponent().toString()
										.equals(f.getLastPathComponent())) {

									y = m;
								}
							}
						}
					}

					TreePath currentSelection = MainWindow.getInstance().getExplorer().getTree()
							.getPathForRow(y);

					if (currentSelection != null) {
						
						DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
								.getLastPathComponent());
						ExplorerFile p = (ExplorerFile) currentNode.getUserObject();
						
						if (!p.isDirectory()) {
							
							MutableTreeNode parent = (MutableTreeNode) (currentNode
									.getParent());
							
							if (parent != null) {
								
								MainWindow.getInstance().getExplorer().getTreeModel()
										.removeNodeFromParent(currentNode);
								
								int cont = -1;
								
								for (int j = 0; j < MainWindow.getInstance().getProjectConfiguration()
										.getNumFilesFromList(); j++) {
									
									if (MainWindow.getInstance().getProjectConfiguration().getFileAt(j)
											.getPath().equals(p.getPath())) {
										cont = j;
									}
								}
								
								MainWindow.getInstance().getProjectConfiguration().removeFileAt(cont);
								MainWindow.getInstance().getStatusBar().setMessage("");
								MainWindow.getInstance().getProjectConfiguration().setIsModified(true);

								// ASK FOR SAVING IT THE FILE HAS BEEN MODIFIED
								if (MainWindow.getInstance().getEditorBuilder().isRedButton(selectedEditor)) {
									
									int opt = JOptionPane.showConfirmDialog(null,
											labels.getString("s643"),
											labels.getString("s953"),
											JOptionPane.YES_NO_OPTION);

									if (opt == JOptionPane.OK_OPTION) {
										MainWindow.getInstance().getMenu().getFile().getSaveFile().setEnabled(true);
										MainWindow.getInstance().getMenu().getFile().getSaveFile().doClick();
									}
								}
								
								// CLOSE
								MainWindow.getInstance().getEditorBuilder().getPane().remove(selectedEditor);
								if (MainWindow.getInstance().getEditorBuilder().getPane().getTabCount() == 0) {
									MainWindow.getInstance().getMenu().disableFileMenu();
									MainWindow.getInstance().getMenu().disableEditMenu();
								}

								return;
							}
						}

					}
					
					if (MainWindow.getInstance().getProjectConfiguration().getNumFilesFromList() > 0) {
						MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile().setEnabled(true);
						MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile().setEnabled(true);
					} else {
						MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile().setEnabled(false);
						MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile().setEnabled(false);
					}
				} 
			}
		});
		add(_removeFile);
		
		// DELETE FILE
		_deleteFile = new JMenuItem(labels.getString("s950"), new ImageIcon(DELETE_FILE));
		_deleteFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				
				// GET THE LANGUAGE
				Language language = Language.getInstance();
				
				try {
					language.getLanguage(PropertiesManager.getProperty("language"));
				} catch (Exception e) {
					e.printStackTrace();
				}
				
				// GET THE LABELS
				ResourceBundle labels = language.getLabels();
				
				int chosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s951"));
				if (chosenOption == JOptionPane.OK_OPTION) {

					// DEFAULT PROJECT
					if (MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
						
						// DELETE THE FILE
						String fileRemove = MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
								.getAbsolutePath();
						File file = new File(fileRemove);
						file.delete();
						
						// REMOVES THE TAB IN THE EDITOR
						MainWindow.getInstance().getEditorBuilder().getPane()
								.remove(MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex());
						
						// SET THE STATUS BAR
						MainWindow.getInstance().getStatusBar().setMessage("");
						
					} else {

						// NOT DEFAULT PROJECT		
						ExplorerFile ff = new ExplorerFile();
						int y = -1;
						int selectedEditor = MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex();
						for (int j = 0; j < MainWindow.getInstance().getProjectConfiguration()
								.getNumFilesFromList(); j++) {

							if (MainWindow.getInstance().getProjectConfiguration()
									.getFileAt(j)
									.getPath()
									.equals(MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
											.getAbsolutePath())) {

								ff = MainWindow.getInstance().getProjectConfiguration().getFileAt(j);
								for (int m = 0; m < MainWindow.getInstance().getProjectConfiguration()
										.getNumFilesFromList() + 1; m++) {

									if (MainWindow.getInstance().getExplorer().getTree().getPathForRow(m)
											.getLastPathComponent().toString()
											.equals(ff.getLastPathComponent())) {

										y = m;
									}
								}
							}
						}

						TreePath currentSelection = MainWindow.getInstance().getExplorer().getTree()
								.getPathForRow(y);

						// BELONGS TO THE PROJECT
						if (currentSelection != null) {
							
							DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
									.getLastPathComponent());
							ExplorerFile p = (ExplorerFile) currentNode.getUserObject();
							
							if (!p.isDirectory()) {
								
								MutableTreeNode parent = (MutableTreeNode) (currentNode
										.getParent());
								
								if (parent != null) {
									
									MainWindow.getInstance().getExplorer().getTreeModel()
											.removeNodeFromParent(currentNode);
								
									int cont = -1;
									for (int j = 0; j < MainWindow.getInstance().getProjectConfiguration()
											.getNumFilesFromList(); j++)		
										if (MainWindow.getInstance().getProjectConfiguration().getFileAt(j)
												.getPath().equals(p.getPath()))
											cont = j;
									
									ExplorerFile f = MainWindow.getInstance().getProjectConfiguration()
											.getFileAt(cont);
									String fileRemove = f.getPath();
									MainWindow.getInstance().getProjectConfiguration().removeFileAt(cont);
									
									File fi = new File(fileRemove);
									fi.delete();

									MainWindow.getInstance().getStatusBar().setMessage("");
									MainWindow.getInstance().getProjectConfiguration().setIsModified(true);
									MainWindow.getInstance().getEditorBuilder().getPane().remove(selectedEditor);

									return;
								}
							}
						} else {
							
							// NOT BELONGS TO THE PROJECT
							String fileRemove = MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditor().getAbsolutePath();
							File fi = new File(fileRemove);
							fi.delete();
							MainWindow.getInstance().getEditorBuilder().getPane()
									.remove(MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex());
							MainWindow.getInstance().getStatusBar().setMessage("");
						}
						
						if (MainWindow.getInstance().getProjectConfiguration().getNumFilesFromList() > 0) {
							MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile().setEnabled(true);
							MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile().setEnabled(true);
						} else {
							MainWindow.getInstance().getExplorer().getPopupMenu().getRemoveFile().setEnabled(false);
							MainWindow.getInstance().getExplorer().getPopupMenu().getDeleteFile().setEnabled(false);
						}
					}
				} 
			}
		});
		add(_deleteFile);
		addSeparator();
		
		// SET COMPILABLE
		_setCompilable = new JMenuItem(labels.getString("s254"), new ImageIcon(SET_COMPILABLE));
		_setCompilable.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow.getInstance().getEditorBuilder().setCompilableFile();
			}
		});
		add(_setCompilable);

		// UNSET COMPILABLE
		_unsetCompilable = new JMenuItem(labels.getString("s255"), new ImageIcon(UNSET_COMPILABLE));
		_unsetCompilable.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow.getInstance().getEditorBuilder().unsetCompilableFile();
			}
		});
		add(_unsetCompilable);

		// SET MAIN
		_setMain = new JMenuItem(labels.getString("s256"), new ImageIcon(SET_MAIN));
		_setMain.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow.getInstance().getEditorBuilder().setMainFile();
			}
		});
		add(_setMain);

		// UNSET MAIN
		_unsetMain = new JMenuItem(labels.getString("s952"), new ImageIcon(UNSET_MAIN));
		_unsetMain.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {		
				MainWindow.getInstance().getEditorBuilder().unsetMainFile();
			}
		});
		add(_unsetMain);
		addSeparator();
		
		// PRINT MENU
		_print = new JMenuItem(labels.getString("s624"), new ImageIcon(PRINT_FILE));
		_print.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow.getInstance().getMenu().getFile().getPrintFile().setEnabled(true);
				MainWindow.getInstance().getMenu().getFile().getPrintFile().doClick();
			}
		});
		add(_print);
	}
	
	/**
	 * Returns the cut menu item.
	 * 
	 * @return The cut menu item.
	 */
	public JMenuItem getCut() {
		return _cut;
	}

	/**
	 * Returns the copy menu item.
	 * 
	 * @return The copy menu item.
	 */
	public JMenuItem getCopy() {
		return _copy;
	}

	/**
	 * Returns the paste menu item.
	 * 
	 * @return The paste menu item.
	 */
	public JMenuItem getPaste() {
		return _paste;
	}

	/**
	 * Returns the add file menu item.
	 * 
	 * @return the add fil menu item.
	 */
	public JMenuItem getAddFile() {
		return _addFile;
	}

	/**
	 * Returns the remove file menu item.
	 * 
	 * @return The remove file menu item.
	 */
	public JMenuItem getRemoveFile() {
		return _removeFile;
	}

	/**
	 * Returns the set compilable menu item.
	 * 
	 * @return The set compilable menu item.
	 */
	public JMenuItem getSetCompilable() {
		return _setCompilable;
	}

	/**
	 * Returns the unset compilable menu item.
	 * 
	 * @return The unset compilable menu item.
	 */
	public JMenuItem getUnsetCompilable() {
		return _unsetCompilable;
	}

	/**
	 * Returns the set main menu item.
	 * 
	 * @return The set main menu item.
	 */
	public JMenuItem getSetMain() {
		return _setMain;
	}

	/**
	 * Returns the unset main menu item.
	 * 
	 * @return The unset main menu item.
	 */
	public JMenuItem getUnsetMain() {
		return _unsetMain;
	}
}
