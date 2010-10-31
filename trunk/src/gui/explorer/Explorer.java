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
import operations.listeners.AcideKeyboardListenerForMenus;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

import es.text.ValidExtensions;
import es.text.TextFile;

import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.*;
import java.util.ResourceBundle;

import gui.MainWindow;
import gui.editor.EditorBuilder;
import gui.splashScreen.SplashScreen;

/**
 * Explorer of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class Explorer extends JPanel {

	/**
	 * SerialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Tree of folders and files.
	 */
	private JTree _tree;
	/**
	 * Tree model for display the files.
	 */
	private DefaultMutableTreeNode _root;
	/**
	 * Tree model for display the directories.
	 */
	private DefaultTreeModel _treeModel;
	/**
	 * Popup menu for the explorer.
	 */
	private PopupMenuExplorer _popup;
	/**
	 * Explorer size.
	 */
	private int _explorerSize;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();
	
	/**
	 * Constructor of the class.
	 */
	public Explorer() {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager
					.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		SplashScreen.setProgressBar(22);

		// GET THE LABELS
		final ResourceBundle labels = language.getLabels();
		
		setLayout(new BorderLayout());
		
		try {
			
			_logger.info(labels.getString("s324"));
			initPopup();
			_root = new DefaultMutableTreeNode("raiz");
			_treeModel = new DefaultTreeModel(_root);
			_treeModel.setAsksAllowsChildren(true);
			_tree = new JTree(_treeModel);
			_tree.setRootVisible(false);
			_tree.getSelectionModel().setSelectionMode(
					TreeSelectionModel.SINGLE_TREE_SELECTION);
			_tree.setShowsRootHandles(true);
			_tree.setCellRenderer(new ExtendedTreeCellRenderer());

			// LISTENERS
			_tree.addMouseListener(new ExplorerDoubleClick());
			_tree.addKeyListener(new ExporerEnterKey());
			_tree.addKeyListener(new AcideKeyboardListener());
			_tree.addKeyListener(new AcideKeyboardListenerForMenus());
			
			// SET THE CURSOR
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

	/**
	 * Initialize the explorer popup.
	 */
	public void initPopup(){
		_popup = new PopupMenuExplorer();	
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
	 * Build the tree.
	 * 
	 * @param node Node to add to the tree.
	 * @param file File to add.
	 */
	public void buildTree(DefaultMutableTreeNode node, File file) {

		ValidExtensions validExtensions = ValidExtensions.getInstance();

		if (!file.isDirectory()) {
			if (validExtensions.isValidExtension(file.getName())) {
				DefaultMutableTreeNode son = new DefaultMutableTreeNode(file);
				node.add(son);
			}
		} else {
			DefaultMutableTreeNode son = new DefaultMutableTreeNode(file);
			node.add(son);
			File fList[] = file.listFiles();
			for (int i = 0; i < fList.length; i++)
				buildTree(son, fList[i]);
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
	 * Return the tree.
	 * 
	 * @return The tree.
	 */
	public JTree getTree() {
		return _tree;
	}

	/**
	 * Set a new value to the tree,
	 * 
	 * @param tree New value to set.
	 */
	public void setTree(JTree tree) {
		_tree = tree;
	}

	/**
	 * Set a new value to the root.
	 * 
	 * @param root New value to set.
	 */
	public void setRoot(DefaultMutableTreeNode root) {
		_root = root;
	}

	/**
	 * Returns the tree model.
	 * 
	 * @return The tree model.
	 */
	public DefaultTreeModel getTreeModel() {
		return _treeModel;
	}

	/**
	 * Set a new value to the tree model.
	 * 
	 * @param treeModel New value to set.
	 */
	public void setTreeModel(DefaultMutableTreeNode treeModel) {
		_treeModel = new DefaultTreeModel(treeModel);
	}

	/**
	 * Close the explorer.
	 */
	public void disposeExplorer() {

		_explorerSize = MainWindow.getInstance().getSplitPaneVertical().getDividerLocation();
		MainWindow.getInstance().getSplitPaneVertical().setDividerLocation(0);
		MainWindow.getInstance().getSplitPaneVertical().getLeftComponent().setVisible(false);
	}

	/**
	 * Show the explorer.
	 */
	public void showExplorer() {

		MainWindow.getInstance().getSplitPaneVertical().setDividerLocation(_explorerSize);
		MainWindow.getInstance().getSplitPaneVertical().getLeftComponent().setVisible(true);
	}

	/**
	 * Expands the tree.
	 */
	public void expandTree() {
		
		int row = 0;
		while (row < getTree().getRowCount()) {
			getTree().expandRow(row);
			row++;
		}
	}

	/**
	 * Returns the explorer size.
	 * 
	 * @return The explorer size.
	 */
	public int getExplorerSize() {
		return _explorerSize;
	}

	/**
	 * Set a new value to the explorer size.
	 * 
	 * @param explorerSize New value to set.
	 */
	public void setExplorerSize(int explorerSize) {
		_explorerSize = explorerSize;
	}
	
	/**
	 * Returns the popup menu of the explorer.
	 * 
	 * @return The popup menu of the explorer.
	 */
	public PopupMenuExplorer getPopupMenu(){
		return _popup;
	}
	
	/**
	 * Explorer double click mouse adapter.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class ExplorerDoubleClick extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent evt) {
			
			MainWindow mainWindow = MainWindow.getInstance();
			TreePath path = _tree.getPathForLocation(evt.getX(), evt.getY());
			String filePath;
			
			if (path != null) {
				mainWindow.getStatusBar().setMessage(
						path.getLastPathComponent().toString());
				
				DefaultMutableTreeNode d = (DefaultMutableTreeNode) path
						.getLastPathComponent();
				Object nodo = d.getUserObject();
				ExplorerFile fc = (ExplorerFile) nodo;
				fc.getPath();

				for (int i = 0; i < mainWindow.getProjectConfiguration()
						.getNumFilesFromList(); i++) {

					if (mainWindow.getProjectConfiguration().getFileAt(i).getPath()
							.equals(fc.getPath()))
						if (!mainWindow.getProjectConfiguration().getFileAt(i)
								.isDirectory()) {
							if (mainWindow.getProjectConfiguration().getFileAt(i)
									.isCompilableFile())
								if (mainWindow.getProjectConfiguration().getFileAt(i)
										.isMainFile())
									mainWindow.getStatusBar().setMessage(
											mainWindow.getProjectConfiguration()
													.getFileAt(i).getName()
													+ " <MAIN>");
								else
									mainWindow.getStatusBar().setMessage(
											mainWindow.getProjectConfiguration()
													.getFileAt(i).getName()
													+ " <COMPILABLE>");
							else
								mainWindow.getStatusBar().setMessage(
										mainWindow.getProjectConfiguration().getFileAt(i)
												.getName());
						} else {
							filePath = path.getLastPathComponent().toString();
							mainWindow.getStatusBar().setMessage(filePath);
						}
				}
				
				for (int i = 0; i < mainWindow.getEditorBuilder().getNumEditors(); i++) {
					if (mainWindow.getEditorBuilder().getEditorAt(i).getAbsolutePath()
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
						if (mainWindow.getEditorBuilder().getEditorAt(j).getAbsolutePath()
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
								if (mainWindow.getProjectConfiguration().getFileAt(z)
										.getPath().equals(fc.getPath()))
									j = z;
							}

							// CHECK THE TYPE
							int t = 0;
							
							// COMPILABLE FILE
							if (mainWindow.getProjectConfiguration().getFileAt(j)
									.isCompilableFile())
								t = 2;

							// MAIN FILE
							if (mainWindow.getProjectConfiguration().getFileAt(j)
									.isMainFile())
								t = 1;

							// OPEN THE TAB IN THE EDITOR
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
							
							// SET THE CARET IN THE FIRST POSITION OF THE ACTIVE EDITOR
							numeditor = mainWindow.getEditorBuilder()
									.getSelectedEditorIndex();
							mainWindow.getEditorBuilder().getEditorAt(numeditor)
									.getEditor().setCaretPosition(0);

							for (int z = 0; z < mainWindow.getProjectConfiguration()
									.getFileListSize(); z++) {
								if (mainWindow.getProjectConfiguration().getFileAt(z)
										.getPath().equals(fc.getPath())) {
									mainWindow.getProjectConfiguration().getFileAt(z)
											.setIsOpened(true);
								}
							}
							mainWindow.getProjectConfiguration().setIsModified(true);
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

				String project = null;
				try {
					project = PropertiesManager.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				
				// DEFAULT PROJECT
				if ((project.equals("./configuration/default.acidePrj") && MainWindow.getInstance()
						.getProjectConfiguration().getName().equals(""))) {
					
					_popup.getSaveProject().setEnabled(false);
					_popup.getNewFile().setEnabled(false);
					_popup.getAddFile().setEnabled(false);
					_popup.getRemoveFile().setEnabled(false);
					_popup.getDeleteFile().setEnabled(false);
					_popup.getSetMain().setEnabled(false);
					_popup.getUnsetMain().setEnabled(false);
					_popup.getSetCompilable().setEnabled(false);
					_popup.getUnsetCompilable().setEnabled(false);
					_popup.getAddFolder().setEnabled(false);
					_popup.getRemoveFolder().setEnabled(false);
				} else {
					_popup.getSaveProject().setEnabled(false);
					_popup.getNewFile().setEnabled(true);
					_popup.getAddFile().setEnabled(true);
					_popup.getRemoveFile().setEnabled(false);
					_popup.getDeleteFile().setEnabled(false);
					_popup.getSetMain().setEnabled(false);
					_popup.getUnsetMain().setEnabled(false);
					_popup.getSetCompilable().setEnabled(false);
					_popup.getUnsetCompilable().setEnabled(false);
					_popup.getAddFolder().setEnabled(true);
					_popup.getRemoveFolder().setEnabled(false);

					if (MainWindow.getInstance().getProjectConfiguration().isModified())
						_popup.getSaveProject().setEnabled(true);

					TreePath path = MainWindow.getInstance().getExplorer().getTree()
							.getSelectionPath();
					
					DefaultMutableTreeNode filePath;
					ExplorerFile fc;
					
					if (path != null) {

						filePath = (DefaultMutableTreeNode) path
								.getLastPathComponent();
						fc = (ExplorerFile) filePath.getUserObject();

						if (!fc.isDirectory()) {
							_popup.getRemoveFile().setEnabled(true);
							_popup.getDeleteFile().setEnabled(true);
							if (!fc.isMainFile())
								_popup.getSetMain().setEnabled(true);
							if (fc.isMainFile())
								_popup.getUnsetMain().setEnabled(true);
							if (!fc.isCompilableFile()
									|| (fc.isCompilableFile() && fc.isMainFile()))
								_popup.getSetCompilable().setEnabled(true);
							if (fc.isCompilableFile() && !fc.isMainFile())
								_popup.getUnsetCompilable().setEnabled(true);
						} else {
							_popup.getRemoveFolder().setEnabled(true);
						}
					}
				}
				_popup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
			}
		}
	}

	/**
	 * Explorer enter key adapter.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class ExporerEnterKey extends KeyAdapter {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
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
							if (mainWindow.getProjectConfiguration().getFileAt(z)
									.getPath().equals(fc.getPath()))
								j = z;
						}

						int t = 0;
						
						// COMPILABLE FILE
						if (mainWindow.getProjectConfiguration().getFileAt(j).isCompilableFile()) {
							t = 2;
							mainWindow.getStatusBar().setMessage(
									filePath + " <COMPILABLE>");
						}
						
						// MAIN FILE
						if (mainWindow.getProjectConfiguration().getFileAt(j).isMainFile()) {
							t = 1;
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

						// CARET IN THE FIRST POSITION OF THE EDITOR
						numeditor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
						mainWindow.getEditorBuilder().getEditorAt(numeditor).getEditor()
								.setCaretPosition(0);
					}
				}
			}
		}
	}
}

/**
 * Explorer extended tree cell renderer.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class ExtendedTreeCellRenderer extends DefaultTreeCellRenderer {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the compilable icon.
	 */
	private static Icon ICON_COMPILABLE = new ImageIcon("./resources/icons/explorer/compilable.png");
	/**
	 * Image file for the main icon.
	 */
	private static Icon ICON_MAIN = new ImageIcon("./resources/icons/explorer/main.png");
	/**
	 * Image file for the folder icon.
	 */
	private static Icon ICON_FOLDER = new ImageIcon("./resources/icons/explorer/folder.png");
	/**
	 * Image file for the default icon.
	 */
	private static Icon ICON_DEFAULT = new ImageIcon("./resources/icons/explorer/default.png");

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
		ExplorerFile explorerFile;

		try {
			explorerFile = (ExplorerFile) node.getUserObject();

			setText(explorerFile.getName());
			setIcon(ICON_DEFAULT);
			if (explorerFile.isDirectory())
				setIcon(ICON_FOLDER);
			if (explorerFile.isCompilableFile())
				setIcon(ICON_COMPILABLE);
			if (explorerFile.isMainFile())
				setIcon(ICON_MAIN);

		} catch (RuntimeException e) {

		}
		return this;
	}
}
