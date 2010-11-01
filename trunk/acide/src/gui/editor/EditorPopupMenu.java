package gui.editor;

import es.explorer.ExplorerFile;
import gui.MainWindow;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

import org.apache.log4j.Logger;

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
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();
	
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
		
		_copy = new JMenuItem(labels.getString("s187"));
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
				_logger.info(labels.getString("s99"));
				
				MainWindow.getInstance().getEditorBuilder()
						.getEditorAt(
								MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
						.getEditor().copy();
			}
		});
		add(_copy);
		
		// CUT
		_cut = new JMenuItem(labels.getString("s188"));
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
				_logger.info(labels.getString("s97"));
				
				MainWindow.getInstance().getEditorBuilder()
						.getEditorAt(
								MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
						.getEditor().cut();
			}
		});
		add(_cut);
		
		// PASTE
		_paste = new JMenuItem(labels.getString("s189"));
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
				_logger.info(labels.getString("s98"));
				
				MainWindow.getInstance().getEditorBuilder()
						.getEditorAt(
								MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex())
						.getEditor().paste();
			}
		});
		add(_paste);
		
		// SELECT ALL
		_selectAll = new JMenuItem(labels.getString("s191"));
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
		_addFile = new JMenuItem(labels.getString("s17"));
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

					Toolkit toolkit = Toolkit.getDefaultToolkit();

					ExplorerFile f = new ExplorerFile();
					int y = -1;
					int editor = MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex();
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
								toolkit.beep();
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
								if (MainWindow.getInstance().getEditorBuilder().isRedButton(editor)) {
									
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
								MainWindow.getInstance().getEditorBuilder().getPane().remove(editor);
								if (MainWindow.getInstance().getEditorBuilder().getPane().getTabCount() == 0) {
									MainWindow.getInstance().getMenu().disableFileMenu();
									MainWindow.getInstance().getMenu().disableEditMenu();
								}

								return;
							}
							toolkit.beep();
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
		_deleteFile = new JMenuItem(labels.getString("s950"));
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
				MainWindow mainWindow = MainWindow.getInstance();

				int chosenOption = JOptionPane.showConfirmDialog(null,
						labels.getString("s951"));
				if (chosenOption == JOptionPane.OK_OPTION) {

					Toolkit toolkit = Toolkit.getDefaultToolkit();
					String prj = null;
					try {
						prj = PropertiesManager.getProperty("defaultAcideProject");
					} catch (Exception e1) {
						e1.printStackTrace();
					}

					// No project
					if ((prj.equals("./configuration/default.acidePrj") && mainWindow
							.getProjectConfiguration().getName().equals(""))) {
						
						String fileRemove = mainWindow.getEditorBuilder().getSelectedEditor()
								.getAbsolutePath();
						File fi = new File(fileRemove);
						fi.delete();
						mainWindow.getEditorBuilder().getPane()
								.remove(mainWindow.getEditorBuilder().getSelectedEditorIndex());
						mainWindow.getStatusBar().setMessage("");
						toolkit.beep();
					} else {

						ExplorerFile ff = new ExplorerFile();
						int y = -1;
						int editor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
						for (int j = 0; j < mainWindow.getProjectConfiguration()
								.getNumFilesFromList(); j++) {

							if (mainWindow.getProjectConfiguration()
									.getFileAt(j)
									.getPath()
									.equals(mainWindow.getEditorBuilder().getSelectedEditor()
											.getAbsolutePath())) {

								ff = mainWindow.getProjectConfiguration().getFileAt(j);
								for (int m = 0; m < mainWindow.getProjectConfiguration()
										.getNumFilesFromList() + 1; m++) {

									if (mainWindow.getExplorer().getTree().getPathForRow(m)
											.getLastPathComponent().toString()
											.equals(ff.getLastPathComponent())) {

										y = m;
									}
								}
							}
						}

						TreePath currentSelection = mainWindow.getExplorer().getTree()
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
									mainWindow.getExplorer().getTreeModel()
											.removeNodeFromParent(currentNode);
									toolkit.beep();
									int cont = -1;
									for (int j = 0; j < mainWindow.getProjectConfiguration()
											.getNumFilesFromList(); j++) {
										if (mainWindow.getProjectConfiguration().getFileAt(j)
												.getPath().equals(p.getPath())) {
											System.out.println(mainWindow
													.getProjectConfiguration()
													.getFileAt(j).getPath());
											cont = j;
										}
									}
									ExplorerFile f = mainWindow.getProjectConfiguration()
											.getFileAt(cont);
									String fileRemove = f.getPath();
									mainWindow.getProjectConfiguration().removeFileAt(cont);
									
									File fi = new File(fileRemove);
									fi.delete();

									mainWindow.getStatusBar().setMessage("");
									mainWindow.getProjectConfiguration().setIsModified(true);
									mainWindow.getEditorBuilder().getPane().remove(editor);

									return;
								}
								toolkit.beep();
							}
						} else {
							
							// NOT BELONGS TO THE PROJECT
							String fileRemove = mainWindow.getEditorBuilder()
									.getSelectedEditor().getAbsolutePath();
							File fi = new File(fileRemove);
							fi.delete();
							mainWindow.getEditorBuilder().getPane()
									.remove(mainWindow.getEditorBuilder().getSelectedEditorIndex());
							mainWindow.getStatusBar().setMessage("");
							toolkit.beep();
						}
						
						if (mainWindow.getProjectConfiguration().getNumFilesFromList() > 0) {
							mainWindow.getExplorer().getPopupMenu().getRemoveFile().setEnabled(true);
							mainWindow.getExplorer().getPopupMenu().getDeleteFile().setEnabled(true);
						} else {
							mainWindow.getExplorer().getPopupMenu().getRemoveFile().setEnabled(false);
							mainWindow.getExplorer().getPopupMenu().getDeleteFile().setEnabled(false);
						}
					}
				} 
			}
		});
		add(_deleteFile);
		addSeparator();
		
		// SET COMPILABLE
		_setCompilable = new JMenuItem(labels.getString("s254"));
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
		_unsetCompilable = new JMenuItem(labels.getString("s255"));
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
		_setMain = new JMenuItem(labels.getString("s256"));
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
		_unsetMain = new JMenuItem(labels.getString("s952"));
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
		_print = new JMenuItem(labels.getString("s624"));
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
