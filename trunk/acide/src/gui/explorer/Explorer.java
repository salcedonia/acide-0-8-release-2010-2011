package gui.explorer;

import javax.swing.*;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.Document;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.tree.*;
import javax.swing.undo.UndoableEdit;

import language.Language;

import operations.configuration.ExplorerFile;
import operations.listeners.AcideKeyboardListener;
import operations.listeners.AcideKeyboardListener2;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

import es.text.ValidExtensions;
import es.text.TextFile;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.*;
import java.util.ResourceBundle;

import gui.MainWindow;
import gui.editor.EditorBuilder;

/**
 * 
 */
public class Explorer extends JPanel {

	/**
	 * SerialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private JTree _tree;
	/**
	 * 
	 */
	private DefaultMutableTreeNode _root;
	/**
	 * 
	 */
	private DefaultTreeModel _treeModel;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private JPopupMenu _popup;
	/**
	 * 
	 */
	private JMenuItem _saveProject;
	/**
	 * 
	 */
	private JMenuItem _addFile;
	/**
	 * 
	 */
	private JMenuItem _removeFile;
	/**
	 * 
	 */
	private JMenuItem _newFile;
	/**
	 * 
	 */
	private JMenuItem _addFolder;
	/**
	 * 
	 */
	private JMenuItem _removeFolder;
	/**
	 * 
	 */
	private int _explorerSize;
	/**
	 * 
	 */
	private JMenuItem _deleteFile;
	/**
	 * 
	 */
	private JMenuItem _setMain;
	/**
	 * 
	 */
	private JMenuItem _setCompilable;
	/**
	 * 
	 */
	private JMenuItem _unsetMain;
	/**
	 * 
	 */
	private JMenuItem _unsetCompilable;

	/**
	 * Constructor of the class.
	 */
	public Explorer() {
		final ResourceBundle labels = Language.getInstance().getLabels();
		try {
			_logger.info(labels.getString("s324"));
			initPopup();
			_root = new DefaultMutableTreeNode("raiz");
			_treeModel = new DefaultTreeModel(_root);
			_treeModel.setAsksAllowsChildren(true);
			setLayout(new BorderLayout());
			_tree = new JTree(_treeModel);
			_tree.setRootVisible(false);
			_tree.getSelectionModel().setSelectionMode(
					TreeSelectionModel.SINGLE_TREE_SELECTION);
			_tree.setShowsRootHandles(true);
			_tree.setCellRenderer(new ExtendedTreeCellRenderer());

			_tree.addMouseListener(new ExplorerDoubleClick());
			_tree.addKeyListener(new ExporerEnterKey());
			_tree.addKeyListener(new AcideKeyboardListener());
			_tree.addKeyListener(new AcideKeyboardListener2());
			
			try {
				Cursor cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
				_tree.setCursor(cursor);

			} catch (HeadlessException e1) {
				e1.printStackTrace();
			}
		} catch (Exception e) {
			_logger.info(labels.getString("s325"));
			e.printStackTrace();
		}
		_logger.info(labels.getString("s326"));
		add(new JScrollPane((JTree) _tree), "Center");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.Component#getPreferredSize()
	 */
	public Dimension getPreferredSize() {
		return new Dimension(200, 120);
	}

	/**
	 * 
	 */
	public void initPopup() {

		final ResourceBundle labels = Language.getInstance().getLabels();
		_logger.info(labels.getString("s324"));
		_popup = new JPopupMenu();
		JMenuItem newProject = new JMenuItem(labels.getString("s14"));
		newProject.addActionListener(new ActionListener() {
			
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow.getInstance().getMenu().getProject().getNewProject().doClick();
			}

		});
		_popup.add(newProject);

		JMenuItem openProj = new JMenuItem(labels.getString("s15"));
		openProj.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow.getInstance().getMenu().getProject().getOpenProject().doClick();
			}
		});
		_popup.add(openProj);
		_saveProject = new JMenuItem(labels.getString("s16"));
		_saveProject.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getSaveProject().setEnabled(true);
				mainWindow.getMenu().getProject().getSaveProject().doClick();
			}
		});
		_popup.add(_saveProject);
		_popup.addSeparator();

		_newFile = new JMenuItem(labels.getString("s947"));
		_newFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getNewProjectFile().setEnabled(true);
				mainWindow.getMenu().getProject().getNewProjectFile().doClick();
			}
		});
		_popup.add(_newFile);

		_addFile = new JMenuItem(labels.getString("s17"));
		_addFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getAddFile().setEnabled(true);
				mainWindow.getMenu().getProject().getAddFile().doClick();
			}
		});
		_popup.add(_addFile);

		_removeFile = new JMenuItem(labels.getString("s618"));
		_removeFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getRemoveFile().setEnabled(true);
				mainWindow.getMenu().getProject().getRemoveFile().doClick();
			}
		});
		_popup.add(_removeFile);

		_deleteFile = new JMenuItem(labels.getString("s950"));
		_deleteFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getDeleteFile().setEnabled(true);
				mainWindow.getMenu().getProject().getDeleteFile().doClick();
			}
		});
		_popup.add(_deleteFile);

		_popup.addSeparator();
		_setCompilable = new JMenuItem(labels.getString("s254"));
		_setCompilable.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getSetCompilable().setEnabled(true);
				mainWindow.getMenu().getProject().getSetCompilable().doClick();
			}
		});
		_popup.add(_setCompilable);

		_unsetCompilable = new JMenuItem(labels.getString("s255"));
		_unsetCompilable.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getUnsetCompilable().setEnabled(true);
				mainWindow.getMenu().getProject().getUnsetCompilable().doClick();
			}
		});
		_popup.add(_unsetCompilable);

		_setMain = new JMenuItem(labels.getString("s256"));
		_setMain.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getSetMain().setEnabled(true);
				mainWindow.getMenu().getProject().getSetMain().doClick();
			}
		});
		_popup.add(_setMain);

		_unsetMain = new JMenuItem(labels.getString("s952"));
		_unsetMain.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getUnsetMain().setEnabled(true);
				mainWindow.getMenu().getProject().getUnsetMain().doClick();
			}
		});
		_popup.add(_unsetMain);
		//

		_popup.addSeparator();
		_removeFolder = new JMenuItem(labels.getString("s220"));
		_removeFolder.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getRemoveFolder().setEnabled(true);
				mainWindow.getMenu().getProject().getRemoveFolder().doClick();
			}
		});

		_addFolder = new JMenuItem(labels.getString("s219"));
		_addFolder.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getAddFolder().setEnabled(true);
				mainWindow.getMenu().getProject().getAddFolder().doClick();
			}
		});
		_popup.add(_addFolder);
		_popup.add(_removeFolder);
	}

	/**
	 * 
	 * @param nodo
	 * @param f
	 */
	public void buildTree(DefaultMutableTreeNode nodo, File f) {

		ValidExtensions ev = ValidExtensions.getInstance();

		if (!f.isDirectory()) {
			if (ev.isValidExtension(f.getName())) {
				DefaultMutableTreeNode hijo = new DefaultMutableTreeNode(f);
				nodo.add(hijo);
			}
		} else {
			DefaultMutableTreeNode hijo = new DefaultMutableTreeNode(f);
			nodo.add(hijo);
			File fList[] = f.listFiles();
			for (int i = 0; i < fList.length; i++)
				buildTree(hijo, fList[i]);
		}
	}

	/**
	 * 
	 */
	class ExplorerDoubleClick extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
		 */
		@SuppressWarnings("static-access")
		public void mouseClicked(MouseEvent evt) {
			
			MainWindow mainWindow = MainWindow.getInstance();
			TreePath path = _tree.getPathForLocation(evt.getX(), evt.getY());
			String filePath;
			
			if (path != null) {
				mainWindow.getStatusBar().setMessage(
						path.getLastPathComponent().toString());
				// status
				DefaultMutableTreeNode d = (DefaultMutableTreeNode) path
						.getLastPathComponent();
				Object nodo = d.getUserObject();
				ExplorerFile fc = (ExplorerFile) nodo;
				fc.getPath();

				for (int i = 0; i < mainWindow.getProjectConfiguration()
						.getNumFilesFromList(); i++) {

					if (mainWindow.getProjectConfiguration().getFile(i).getPath()
							.equals(fc.getPath()))
						if (!mainWindow.getProjectConfiguration().getFile(i)
								.isDirectory()) {
							if (mainWindow.getProjectConfiguration().getFile(i)
									.isSetFile())
								if (mainWindow.getProjectConfiguration().getFile(i)
										.isMainFile())
									mainWindow.getStatusBar().setMessage(
											mainWindow.getProjectConfiguration()
													.getFile(i).getName()
													+ " <MAIN>");
								else
									mainWindow.getStatusBar().setMessage(
											mainWindow.getProjectConfiguration()
													.getFile(i).getName()
													+ " <COMPILABLE>");
							else
								mainWindow.getStatusBar().setMessage(
										mainWindow.getProjectConfiguration().getFile(i)
												.getName());
						} else {
							filePath = path.getLastPathComponent().toString();
							mainWindow.getStatusBar().setMessage(filePath);
						}
				}
				
				for (int i = 0; i < mainWindow.getEditorBuilder().getNumEditors(); i++) {
					if (mainWindow.getEditorBuilder().getEditorAt(i).getPath()
							.equals(fc.getPath())) {
						mainWindow.getEditorBuilder().setSelectedEditorAt(i);
					}
				}

				mainWindow.validate();
				mainWindow.repaint();
			}
			if (evt.getClickCount() > 1) {
				path = _tree.getPathForLocation(evt.getX(), evt.getY());

				if (path != null) {
					DefaultMutableTreeNode d = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					Object treeNode = d.getUserObject();
					ExplorerFile fc = (ExplorerFile) treeNode;

					boolean isOpened = false;
					int s = -1;
					for (int j = 0; j < mainWindow.getEditorBuilder().getNumEditors(); j++) {
						if (mainWindow.getEditorBuilder().getEditorAt(j).getPath()
								.equals(fc.getPath())) {
							isOpened = true;
							s = j;
						}
					}
					if (!isOpened) {
						if (!fc.isDirectory()) {
							TextFile fd = new TextFile();
							EditorBuilder ce = mainWindow.getEditorBuilder();
							String s2 = null;
							s2 = fd.load(fc.getPath());

							int j = -1;
							for (int z = 0; z < mainWindow.getProjectConfiguration()
									.getNumFilesFromList(); z++) {
								if (mainWindow.getProjectConfiguration().getFile(z)
										.getPath().equals(fc.getPath()))
									j = z;
							}

							// Check the type
							int t = 0;
							if (mainWindow.getProjectConfiguration().getFile(j)
									.isSetFile())
								t = 2;
							if (mainWindow.getProjectConfiguration().getFile(j)
									.isMainFile())
								t = 1;

							ce.newTab(fc.getPath(), fc.getPath(), s2, true, t);
							// UNDO REDO
							mainWindow.getMenu().enableFileMenu();
							mainWindow.getMenu().enableEditMenu();
							int numeditor = mainWindow.getEditorBuilder()
									.getSelectedEditorIndex();
							Document doc = mainWindow.getEditorBuilder()
									.getEditorAt(numeditor).getEditor()
									.getDocument();
							doc.addUndoableEditListener(new UndoableEditListener() {
								/*
								 * (non-Javadoc)
								 * @see javax.swing.event.UndoableEditListener#undoableEditHappened(javax.swing.event.UndoableEditEvent)
								 */
								public void undoableEditHappened(
										UndoableEditEvent evt) {
									MainWindow mainWindow = MainWindow.getInstance();
									UndoableEdit edit = evt.getEdit();
									if (edit instanceof DefaultDocumentEvent
											&& ((DefaultDocumentEvent) edit)
													.getType() == DefaultDocumentEvent.EventType.CHANGE) {
										return;
									} else {
										mainWindow.getMenu().getEdit().getUndoManager()
												.addEdit(evt.getEdit());
									}
								}
							});
							// Caret in the first position of the text
							numeditor = mainWindow.getEditorBuilder()
									.getSelectedEditorIndex();
							mainWindow.getEditorBuilder().getEditorAt(numeditor)
									.getEditor().setCaretPosition(0);

							for (int z = 0; z < mainWindow.getProjectConfiguration()
									.getFileListSize(); z++) {
								if (mainWindow.getProjectConfiguration().getFile(z)
										.getPath().equals(fc.getPath())) {
									mainWindow.getProjectConfiguration().getFile(z)
											.setOpened(true);
								}
							}
							mainWindow.getProjectConfiguration().setModified(true);
						}

					} else {
						mainWindow.getEditorBuilder().setSelectedEditorAt(s);
					}
				}

			}
		}
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent arg0) {
			if (arg0.isPopupTrigger()) {
				_popup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
			}
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
		 */
		public void mouseReleased(MouseEvent arg0) {
			
			if (arg0.isPopupTrigger()){

				MainWindow mainWindow = MainWindow.getInstance();
				String prj = null;
				try {
					prj = PropertiesManager.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					_saveProject.setEnabled(false);
					_newFile.setEnabled(false);
					_addFile.setEnabled(false);
					_removeFile.setEnabled(false);
					_deleteFile.setEnabled(false);
					_setMain.setEnabled(false);
					_unsetMain.setEnabled(false);
					_setCompilable.setEnabled(false);
					_unsetCompilable.setEnabled(false);
					_addFolder.setEnabled(false);
					_removeFolder.setEnabled(false);
				} else {
					_saveProject.setEnabled(false);
					_newFile.setEnabled(true);
					_addFile.setEnabled(true);
					_removeFile.setEnabled(false);
					_deleteFile.setEnabled(false);
					_setMain.setEnabled(false);
					_unsetMain.setEnabled(false);
					_setCompilable.setEnabled(false);
					_unsetCompilable.setEnabled(false);
					_addFolder.setEnabled(true);
					_removeFolder.setEnabled(false);

					if (mainWindow.getProjectConfiguration().isModified()) {
						_saveProject.setEnabled(true);
					}

					TreePath path = mainWindow.getExplorer().getTree()
							.getSelectionPath();
					DefaultMutableTreeNode filePath;
					ExplorerFile fc;
					if (path != null) {

						filePath = (DefaultMutableTreeNode) path
								.getLastPathComponent();
						fc = (ExplorerFile) filePath.getUserObject();

						if (!fc.isDirectory()) {
							_removeFile.setEnabled(true);
							_deleteFile.setEnabled(true);
							if (!fc.isMainFile())
								_setMain.setEnabled(true);
							if (fc.isMainFile())
								_unsetMain.setEnabled(true);
							if (!fc.isSetFile()
									|| (fc.isSetFile() && fc.isMainFile()))
								_setCompilable.setEnabled(true);
							if (fc.isSetFile() && !fc.isMainFile())
								_unsetCompilable.setEnabled(true);
						} else {
							_removeFolder.setEnabled(true);
						}
					}
				}
				_popup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
			}
		}
	}

	/**
	 * 
	 */
	class ExporerEnterKey extends KeyAdapter {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
		@SuppressWarnings("static-access")
		public void keyPressed(KeyEvent evt) {

			if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
				
				TreePath path = _tree.getSelectionPath();
				MainWindow mainWindow = MainWindow.getInstance();
				
				if (path != null) {
					String filePath = path.getLastPathComponent().toString();
					mainWindow.getStatusBar().setMessage(filePath);
					DefaultMutableTreeNode d = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					Object nodo = d.getUserObject();
					ExplorerFile fc = (ExplorerFile) nodo;

					if (!fc.isDirectory()) {
						TextFile fd = new TextFile();
						EditorBuilder ce = mainWindow.getEditorBuilder();
						String s = null;
						s = fd.load(fc.getPath());

						int j = -1;
						for (int z = 0; z < mainWindow.getProjectConfiguration()
								.getNumFilesFromList(); z++) {
							if (mainWindow.getProjectConfiguration().getFile(z)
									.getPath().equals(fc.getPath()))
								j = z;
						}

						// Check the type
						int t = 0;
						if (mainWindow.getProjectConfiguration().getFile(j).isSetFile()) {
							t = 2;
							// Status
							mainWindow.getStatusBar().setMessage(
									filePath + " <COMPILABLE>");
						}
						if (mainWindow.getProjectConfiguration().getFile(j).isMainFile()) {
							t = 1;
							// Status
							mainWindow.getStatusBar().setMessage(filePath + " <MAIN>");
						}

						ce.newTab(fc.getPath(), fc.getPath(), s, true, t);
						
						// UNDO REDO
						mainWindow.getMenu().getFile().enableSaveFileAs();
						int numeditor = mainWindow.getEditorBuilder()
								.getSelectedEditorIndex();
						Document doc = mainWindow.getEditorBuilder()
								.getEditorAt(numeditor).getEditor()
								.getDocument();
						doc.addUndoableEditListener(new UndoableEditListener() {
							
							/*
							 * (non-Javadoc)
							 * @see javax.swing.event.UndoableEditListener#undoableEditHappened(javax.swing.event.UndoableEditEvent)
							 */
							public void undoableEditHappened(
									UndoableEditEvent evt) {
								MainWindow mainWindow = MainWindow.getInstance();
								UndoableEdit edit = evt.getEdit();
								if (edit instanceof DefaultDocumentEvent
										&& ((DefaultDocumentEvent) edit)
												.getType() == DefaultDocumentEvent.EventType.CHANGE) {
									return;
								} else {
									mainWindow.getMenu().getEdit().getUndoManager()
											.addEdit(evt.getEdit());
								}
							}
						});

						// Caret in the first position of the text
						numeditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
						mainWindow.getEditorBuilder().getEditorAt(numeditor).getEditor()
								.setCaretPosition(0);
					}
				}
			}
		}
	}

	/**
	 * 
	 * @return
	 */
	public DefaultMutableTreeNode getRoot() {
		return _root;
	}

	/**
	 * 
	 * @return
	 */
	public JTree getTree() {
		return _tree;
	}

	/**
	 * 
	 * @param tree
	 */
	public void setTree(JTree tree) {
		_tree = tree;
	}

	/**
	 * 
	 * @param root
	 */
	public void setRoot(DefaultMutableTreeNode root) {
		_root = root;
	}

	/**
	 * 
	 * @return
	 */
	public DefaultTreeModel getTreeModel() {
		return _treeModel;
	}

	/**
	 * 
	 * @param treeModel
	 */
	public void setTreeModel(DefaultMutableTreeNode treeModel) {
		_treeModel = new DefaultTreeModel(treeModel);
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getAddFile() {
		return _addFile;
	}

	/**
	 * 
	 */
	public void setEnabledAddFile() {
		_addFile.setEnabled(true);
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveProj() {
		return _saveProject;
	}

	/**
	 * 
	 */
	public void setEnabledSaveProj() {
		_saveProject.setEnabled(true);
	}

	/**
	 * 
	 */
	public void setEnabledRemoveFile() {
		_removeFile.setEnabled(true);
	}

	/**
	 * 
	 */
	public void setEnabledDeleteFile() {
		_deleteFile.setEnabled(true);
	}

	/**
	 * 
	 */
	public void disposeExplorer() {

		MainWindow mainWindow = MainWindow.getInstance();
		_explorerSize = mainWindow.getSplitPaneVertical().getDividerLocation();
		mainWindow.getSplitPaneVertical().setDividerLocation(0);
		mainWindow.getSplitPaneVertical().getLeftComponent().setVisible(false);
	}

	/**
	 * 
	 */
	public void showExplorer() {

		MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.getSplitPaneVertical().setDividerLocation(this._explorerSize);
		mainWindow.getSplitPaneVertical().getLeftComponent().setVisible(true);
	}

	/**
	 * 
	 */
	public void expandTree() {
		
		int row = 0;
		while (row < getTree().getRowCount()) {
			getTree().expandRow(row);
			row++;
		}
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getRemoveFile() {
		return _removeFile;
	}

	/**
	 * 
	 * @return
	 */
	public JComponent getDeleteFile() {
		return _deleteFile;
	}

	/**
	 * 
	 * @return
	 */
	public int getExplorerSize() {
		return _explorerSize;
	}

	/**
	 * 
	 * @param explorerSize
	 */
	public void setExplorerSize(int explorerSize) {
		_explorerSize = explorerSize;
	}
}

/**
 * 
 */
class ExtendedTreeCellRenderer extends DefaultTreeCellRenderer {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	static Icon ICON_COMPILABLE = new ImageIcon("./resources/icons/explorer/compilable.png");
	/**
	 * 
	 */
	static Icon ICON_MAIN = new ImageIcon("./resources/icons/explorer/main.png");
	/**
	 * 
	 */
	static Icon ICON_FOLDER = new ImageIcon("./resources/icons/explorer/folder.png");
	/**
	 * 
	 */
	static Icon ICON_DEFAULT = new ImageIcon("./resources/icons/explorer/default.png");

	/*
	 * (non-Javadoc)
	 * @see javax.swing.tree.DefaultTreeCellRenderer#getTreeCellRendererComponent(javax.swing.JTree, java.lang.Object, boolean, boolean, boolean, int, boolean)
	 */
	public Component getTreeCellRendererComponent(JTree tree, Object value,
			boolean sel, boolean expanded, boolean leaf, int row,
			boolean hasFocus) {

		super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf,
				row, hasFocus);

		DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
		ExplorerFile fc;

		try {
			fc = (ExplorerFile) node.getUserObject();

			setText(fc.getName());
			setIcon(ICON_DEFAULT);
			if (fc.isDirectory())
				setIcon(ICON_FOLDER);
			if (fc.isSetFile())
				setIcon(ICON_COMPILABLE);
			if (fc.isMainFile())
				setIcon(ICON_MAIN);

		} catch (RuntimeException e) {

		}
		return this;
	}
}
