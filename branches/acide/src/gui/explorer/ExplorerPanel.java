package gui.explorer;

import javax.swing.*;
import javax.swing.tree.*;

import language.Language;

import operations.listeners.AcideKeyboardListener;
import operations.listeners.AcideKeyboardListenerForMenus;
import operations.log.Log;

import properties.PropertiesManager;

import es.text.ValidExtensions;
import java.awt.*;
import java.io.*;
import java.util.ResourceBundle;

import gui.explorer.listeners.ExplorerPanelClickMouseListener;
import gui.explorer.listeners.ExplorerPanelDoubleClickMouseListener;
import gui.explorer.listeners.ExplorerPanelKeyboardListener;
import gui.explorer.listeners.ExplorerPanelPopupMenuListener;
import gui.explorer.popup.ExplorerPanelPopupMenu;
import gui.explorer.utils.ExtendedTreeCellRenderer;
import gui.mainWindow.MainWindow;

/************************************************************************																
 * Explorer panel of ACIDE - A Configurable IDE										
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
 * @see JPanel																													
 ***********************************************************************/
public class ExplorerPanel extends JPanel {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Tree of folders and files
	 */
	private JTree _tree;
	/**
	 * Tree model for display the files
	 */
	private DefaultMutableTreeNode _root;
	/**
	 * Tree model for display the directories
	 */
	private DefaultTreeModel _treeModel;
	/**
	 * Popup menu for the explorer
	 */
	private ExplorerPanelPopupMenu _popup;
	/**
	 * Explorer size
	 */
	private int _explorerSize;

	/**
	 * Class constructor
	 */
	public ExplorerPanel() {

		// Gets the language
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Sets the layout
		setLayout(new BorderLayout());

		try {

			// Updates the log
			Log.getLog().info(labels.getString("s324"));

			// POPUP MENU
			buildPopupMenu();

			// Builds the tree
			_root = new DefaultMutableTreeNode("raiz");
			_treeModel = new DefaultTreeModel(_root);
			_treeModel.setAsksAllowsChildren(true);
			_tree = new JTree(_treeModel);
			_tree.setRootVisible(false);
			_tree.getSelectionModel().setSelectionMode(
					TreeSelectionModel.SINGLE_TREE_SELECTION);
			_tree.setShowsRootHandles(true);
			_tree.setCellRenderer(new ExtendedTreeCellRenderer());

			// Listeners
			_tree.addMouseListener(new ExplorerPanelClickMouseListener());
			_tree.addMouseListener(new ExplorerPanelPopupMenuListener());
			_tree.addMouseListener(new ExplorerPanelDoubleClickMouseListener());
			_tree.addKeyListener(new ExplorerPanelKeyboardListener());
			_tree.addKeyListener(new AcideKeyboardListener());
			_tree.addKeyListener(new AcideKeyboardListenerForMenus());

			// Sets the hand cursor
			try {
				Cursor cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
				_tree.setCursor(cursor);
			} catch (HeadlessException exception) {
				
				// Updates the log
				Log.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		} catch (Exception exception) {

			// Updates the log
			Log.getLog().info(labels.getString("s325"));
			exception.printStackTrace();
		}

		// Updates the log
		Log.getLog().info(labels.getString("s326"));
		add(new JScrollPane((JTree) _tree), "Center");
	}

	/**
	 * Builds the explorer POPUP menu
	 * 
	 * @see ExplorerPanelPopupMenu
	 */
	public void buildPopupMenu() {
		_popup = new ExplorerPanelPopupMenu();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.Component#getPreferredSize()
	 */
	@Override
	public Dimension getPreferredSize() {
		return new Dimension(200, 120);
	}

	/**
	 * Builds the explorer tree
	 * 
	 * @param node
	 *            node to add to the tree
	 * @param file
	 *            file to add
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
	 * Returns the explorer tree root
	 * 
	 * @return the explorer tree root
	 */
	public DefaultMutableTreeNode getRoot() {
		return _root;
	}

	/**
	 * Returns the explorer tree
	 * 
	 * @return the explorer tree
	 */
	public JTree getTree() {
		return _tree;
	}

	/**
	 * Sets a new value to the tree
	 * 
	 * @param tree
	 *            new value to set
	 */
	public void setTree(JTree tree) {
		_tree = tree;
	}

	/**
	 * Sets a new value to the root
	 * 
	 * @param root
	 *            new value to set
	 */
	public void setRoot(DefaultMutableTreeNode root) {
		_root = root;
	}

	/**
	 * Returns the tree model
	 * 
	 * @return the tree model
	 */
	public DefaultTreeModel getTreeModel() {
		return _treeModel;
	}

	/**
	 * Set a new value to the tree model
	 * 
	 * @param treeModel
	 *            new value to set
	 */
	public void setTreeModel(DefaultMutableTreeNode treeModel) {
		_treeModel = new DefaultTreeModel(treeModel);
	}

	/**
	 * Closes the explorer panel
	 */
	public void disposeExplorer() {

		_explorerSize = MainWindow.getInstance().getSplitPaneVertical()
				.getDividerLocation();
		MainWindow.getInstance().getSplitPaneVertical().setDividerLocation(0);
		MainWindow.getInstance().getSplitPaneVertical().getLeftComponent()
				.setVisible(false);
	}

	/**
	 * Show the explorer panel
	 */
	public void showExplorer() {

		MainWindow.getInstance().getSplitPaneVertical()
				.setDividerLocation(_explorerSize);
		MainWindow.getInstance().getSplitPaneVertical().getLeftComponent()
				.setVisible(true);
	}

	/**
	 * Expands the tree
	 */
	public void expandTree() {

		int row = 0;
		while (row < getTree().getRowCount()) {
			getTree().expandRow(row);
			row++;
		}
	}

	/**
	 * Returns the explorer size
	 * 
	 * @return the explorer size
	 */
	public int getExplorerSize() {
		return _explorerSize;
	}

	/**
	 * Sets a new value to the explorer size
	 * 
	 * @param explorerSize
	 *            new value to set
	 */
	public void setExplorerSize(int explorerSize) {
		_explorerSize = explorerSize;
	}

	/**
	 * Returns the popup menu of the explorer
	 * 
	 * @return the popup menu of the explorer
	 * @see ExplorerPanelPopupMenu
	 */
	public ExplorerPanelPopupMenu getPopupMenu() {
		return _popup;
	}
}