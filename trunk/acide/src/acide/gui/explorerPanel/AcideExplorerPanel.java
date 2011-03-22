/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package acide.gui.explorerPanel;

import javax.swing.*;
import javax.swing.tree.*;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import acide.configuration.lexicon.validExtensions.AcideValidExtensionsManager;
import acide.configuration.project.AcideProjectConfiguration;
import acide.files.project.AcideProjectFile;
import java.awt.*;
import java.io.*;
import java.util.ArrayList;
import acide.gui.explorerPanel.listeners.AcideExplorerPanelClickMouseListener;
import acide.gui.explorerPanel.listeners.AcideExplorerPanelDoubleClickMouseListener;
import acide.gui.explorerPanel.listeners.AcideExplorerPanelFocusListener;
import acide.gui.explorerPanel.listeners.AcideExplorerPanelKeyboardListener;
import acide.gui.explorerPanel.listeners.AcideExplorerPanelPopupMenuListener;
import acide.gui.explorerPanel.popup.AcideExplorerPanelPopupMenu;
import acide.gui.explorerPanel.utils.AcideExplorerTreeCellRenderer;
import acide.gui.listeners.AcideStatusBarKeyboardListener;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE explorer panel.
 * 
 * @version 0.8
 * @see JPanel
 */
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
	 * ACIDE - A Configurable IDE explorer panel tree model for display the
	 * files.
	 */
	private DefaultMutableTreeNode _root;
	/**
	 * ACIDE - A Configurable IDE explorer panel tree model for display the
	 * directories.
	 */
	private DefaultTreeModel _treeModel;
	/**
	 * ACIDE - A Configurable IDE explorer panel popup menu.
	 */
	private AcideExplorerPanelPopupMenu _popupMenu;
	/**
	 * ACIDE - A Configurable IDE explorer panel size.
	 */
	private int _size;

	/**
	 * Creates a new ACIDE - A Configurable IDE explorer panel.
	 */
	public AcideExplorerPanel() {

		// Sets the layout
		setLayout(new BorderLayout());

		try {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s324"));

			// Creates the popup menu
			buildPopupMenu();

			// Builds the tree
			_root = new DefaultMutableTreeNode("raiz");

			// Creates the tree model
			_treeModel = new DefaultTreeModel(_root);

			// Sets asks allows children as true
			_treeModel.setAsksAllowsChildren(true);

			// Creates the tree
			_tree = new JTree(_treeModel);

			// The root is not visible
			_tree.setRootVisible(false);

			// Sets the single selection mode
			_tree.getSelectionModel().setSelectionMode(
					TreeSelectionModel.SINGLE_TREE_SELECTION);

			// Shows the root handles
			_tree.setShowsRootHandles(true);

			// Sets the cell renderer
			_tree.setCellRenderer(new AcideExplorerTreeCellRenderer());

			// Sets the ACIDE - A Configurable IDE explorer panel listeners
			setListeners();

			try {

				// Sets the hand cursor
				_tree.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
			} catch (HeadlessException exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s325"));
			exception.printStackTrace();
		}

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s326"));

		// Adds the scroll pane
		add(new JScrollPane((JTree) _tree), BorderLayout.CENTER);
	}

	/**
	 * Sets the ACIDE - A Configurable IDE explorer panel listeners.
	 */
	private void setListeners() {
		
		// Sets the ACIDE - A Configurable IDE explorer panel focus listener
		_tree.addFocusListener(new AcideExplorerPanelFocusListener());
		
		// Sets the ACIDE - A Configurable IDE explorer panel click mouse listener
		_tree.addMouseListener(new AcideExplorerPanelClickMouseListener());
		
		// Sets the ACIDE - A Configurable IDE explorer panel popup menu listener
		_tree.addMouseListener(new AcideExplorerPanelPopupMenuListener());
		
		// Sets the ACIDE - A Configurable IDE explorer double click listener
		_tree.addMouseListener(new AcideExplorerPanelDoubleClickMouseListener());
		
		// Sets the ACIDE - A Configurable IDE explorer panel keyboard listener
		_tree.addKeyListener(new AcideExplorerPanelKeyboardListener());
		
		// Sets the ACIDE - A Configurable IDE status bar keyboard listener
		_tree.addKeyListener(new AcideStatusBarKeyboardListener());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE explorer panel popup menu.
	 * 
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

		if (!file.isDirectory()) {
			if (AcideValidExtensionsManager.getInstance().isValidExtension(
					file.getName())) {
				DefaultMutableTreeNode son = new DefaultMutableTreeNode(file);
				node.add(son);
			}
		} else {
			DefaultMutableTreeNode son = new DefaultMutableTreeNode(file);
			node.add(son);
			File fileList[] = file.listFiles();
			for (int index = 0; index < fileList.length; index++)
				buildTree(son, fileList[index]);
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
	 * Set a new value to the ACIDE - A Configurable IDE explorer panel tree
	 * model.
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
	public void disposeExplorerPanel() {

		// Updates the explorer panel size
		_size = AcideMainWindow.getInstance().getVerticalSplitPane()
				.getDividerLocation();

		// Sets the vertical split pane divider location in the main window as 0
		AcideMainWindow.getInstance().getVerticalSplitPane()
				.setDividerLocation(0);

		// Sets the left component of the split pane in the main window as not
		// visible
		AcideMainWindow.getInstance().getVerticalSplitPane().getLeftComponent()
				.setVisible(false);
	}

	/**
	 * Shows the ACIDE - A Configurable IDE explorer panel.
	 */
	public void showExplorerPanel() {

		// Updates the explorer panel size
		AcideMainWindow.getInstance().getVerticalSplitPane()
				.setDividerLocation(_size);

		// Sets the left component of the split pane in the main window as
		// visible
		AcideMainWindow.getInstance().getVerticalSplitPane().getLeftComponent()
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
		return _size;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE explorer panel size.
	 * 
	 * @param explorerSize
	 *            new value to set.
	 */
	public void setExplorerSize(int explorerSize) {
		_size = explorerSize;
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

	/**
	 * Selects the explorer tree node that matches with the currently selected
	 * file editor panel.
	 */
	public void selectTreeNodeFromFileEditor() {

		// If it is not the default project
		if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

			// Editor selected?
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel() != null) {

				// Creates the ACIDE - A Configurable IDE project file
				AcideProjectFile file = new AcideProjectFile();
				int fileIndex = -1;

				// Searches for the file in the explorer tree
				for (int index1 = 0; index1 < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index1++) {

					// Does the file belong to the project?
					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index1)
							.getAbsolutePath()
							.equals(AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanel()
									.getAbsolutePath())) {

						// Gets the explorer file from the project configuration
						file = AcideProjectConfiguration.getInstance()
								.getFileAt(index1);

						for (int index2 = 0; index2 < AcideProjectConfiguration
								.getInstance().getNumberOfFilesFromList() + 1; index2++) {

							if (AcideMainWindow.getInstance()
									.getExplorerPanel().getTree()
									.getPathForRow(index2)
									.getLastPathComponent().toString()
									.equals(file.getLastPathComponent())) {

								// Gets the index
								fileIndex = index2;
							}
						}
					}
				}

				// Gets the current selection in the tree
				TreePath currentSelection = AcideMainWindow.getInstance()
						.getExplorerPanel().getTree().getPathForRow(fileIndex);

				// Selects the node in the explorer tree
				AcideMainWindow.getInstance().getExplorerPanel().getTree()
						.setSelectionPath(currentSelection);
			}
		}
	}

	/**
	 * Searches for a file into the list of files. If finds it then it returns
	 * the node itself and null in other case.
	 * 
	 * @param directoryList
	 *            list of files.
	 * @param fileName
	 *            file name to search for.
	 * @return the file itself if it exists, and null in the opposite case.
	 */
	public DefaultMutableTreeNode searchDirectoryList(
			final ArrayList<DefaultMutableTreeNode> directoryList,
			final String fileName) {

		int index = 0;

		boolean found = false;

		while (index < directoryList.size() && !found) {

			DefaultMutableTreeNode node = directoryList.get(index);
			AcideProjectFile file = (AcideProjectFile) node.getUserObject();

			if (file.getName().equals(fileName)) {
				found = true;
				return directoryList.get(index);
			} else
				index++;
		}
		return null;
	}
}