package gui.explorerPanel;

import javax.swing.*;
import javax.swing.tree.*;

import language.AcideLanguage;

import operations.listeners.AcideKeyboardListener;
import operations.listeners.AcideKeyboardListenerForMenus;
import operations.log.AcideLog;
import resources.ResourceManager;


import es.text.ValidExtensions;
import java.awt.*;
import java.io.*;
import java.util.ResourceBundle;

import gui.explorerPanel.listeners.AcideExplorerPanelClickMouseListener;
import gui.explorerPanel.listeners.AcideExplorerPanelDoubleClickMouseListener;
import gui.explorerPanel.listeners.AcideExplorerPanelFocusListener;
import gui.explorerPanel.listeners.AcideExplorerPanelKeyboardListener;
import gui.explorerPanel.listeners.AcideExplorerPanelPopupMenuListener;
import gui.explorerPanel.popup.AcideExplorerPanelPopupMenu;
import gui.explorerPanel.utils.ExtendedTreeCellRenderer;
import gui.mainWindow.MainWindow;

/************************************************************************																
 * ACIDE - A Configurable IDE explorer panel.
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
public class AcideExplorerPanel extends JPanel {

	/**
	 * ACIDE - A Configurable IDE explorer panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE explorer panel tree of folders and files.
	 */
	private JTree _tree;
	/**
	 * ACIDE - A Configurable IDE explorer panel tree model for display the files.
	 */
	private DefaultMutableTreeNode _root;
	/**
	 * ACIDE - A Configurable IDE explorer panel tree model for display the directories.
	 */
	private DefaultTreeModel _treeModel;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu.
	 */
	private AcideExplorerPanelPopupMenu _popupMenu;
	/**
	 * ACIDE - A Configurable IDE explorer panel size.
	 */
	private int _explorerSize;

	/**
	 * Creates a new ACIDE - A Configurable IDE explorer panel.
	 */
	public AcideExplorerPanel() {

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

		// Sets the layout
		setLayout(new BorderLayout());

		try {

			// Updates the log
			AcideLog.getLog().info(labels.getString("s324"));

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
			_tree.addFocusListener(new AcideExplorerPanelFocusListener());		
			_tree.addMouseListener(new AcideExplorerPanelClickMouseListener());
			_tree.addMouseListener(new AcideExplorerPanelPopupMenuListener());
			_tree.addMouseListener(new AcideExplorerPanelDoubleClickMouseListener());
			_tree.addKeyListener(new AcideExplorerPanelKeyboardListener());
			_tree.addKeyListener(new AcideKeyboardListener());
			_tree.addKeyListener(new AcideKeyboardListenerForMenus());

			// Sets the hand cursor
			try {
				Cursor cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
				_tree.setCursor(cursor);
			} catch (HeadlessException exception) {
				
				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().info(labels.getString("s325"));
			exception.printStackTrace();
		}

		// Updates the log
		AcideLog.getLog().info(labels.getString("s326"));
		add(new JScrollPane((JTree) _tree), "Center");
	}

	/**
	 * Builds the ACIDE - A Configurable IDE explorer panel popup menu.
	 * @see AcideExplorerPanelPopupMenu
	 */
	public void buildPopupMenu() {
		_popupMenu = new AcideExplorerPanelPopupMenu();
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
	 * Builds the ACIDE - A Configurable IDE explorer panel tree.
	 * 
	 * @param node
	 *            node to add to the tree.
	 * @param file
	 *            file to add.
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
	 * Returns the ACIDE - A Configurable IDE explorer panel tree root.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel tree root.
	 */
	public DefaultMutableTreeNode getRoot() {
		return _root;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel tree.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel tree.
	 */
	public JTree getTree() {
		return _tree;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE explorer panel tree.
	 * 
	 * @param tree
	 *            new value to set.
	 */
	public void setTree(JTree tree) {
		_tree = tree;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE explorer panel root.
	 * 
	 * @param root
	 *            new value to set.
	 */
	public void setRoot(DefaultMutableTreeNode root) {
		_root = root;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel tree model.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel tree model.
	 */
	public DefaultTreeModel getTreeModel() {
		return _treeModel;
	}

	/**
	 * Set a new value to the ACIDE - A Configurable IDE explorer panel tree model.
	 * 
	 * @param treeModel
	 *            new value to set.
	 */
	public void setTreeModel(DefaultMutableTreeNode treeModel) {
		_treeModel = new DefaultTreeModel(treeModel);
	}

	/**
	 * Closes the ACIDE - A Configurable IDE explorer panel.
	 */
	public void disposeExplorer() {

		_explorerSize = MainWindow.getInstance().getSplitPaneVertical()
				.getDividerLocation();
		MainWindow.getInstance().getSplitPaneVertical().setDividerLocation(0);
		MainWindow.getInstance().getSplitPaneVertical().getLeftComponent()
				.setVisible(false);
	}

	/**
	 * Shows the ACIDE - A Configurable IDE explorer panel.
	 */
	public void showExplorer() {

		MainWindow.getInstance().getSplitPaneVertical()
				.setDividerLocation(_explorerSize);
		MainWindow.getInstance().getSplitPaneVertical().getLeftComponent()
				.setVisible(true);
	}

	/**
	 * Expands the ACIDE - A Configurable IDE explorer panel tree.
	 */
	public void expandTree() {

		int row = 0;
		while (row < getTree().getRowCount()) {
			getTree().expandRow(row);
			row++;
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel size.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel size.
	 */
	public int getExplorerSize() {
		return _explorerSize;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE explorer panel size.
	 * 
	 * @param explorerSize
	 *            new value to set.
	 */
	public void setExplorerSize(int explorerSize) {
		_explorerSize = explorerSize;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE explorer panel popup menu.
	 * 
	 * @return the ACIDE - A Configurable IDE explorer panel popup menu.
	 * @see AcideExplorerPanelPopupMenu
	 */
	public AcideExplorerPanelPopupMenu getPopupMenu() {
		return _popupMenu;
	}
}