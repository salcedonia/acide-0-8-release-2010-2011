package gui.explorerPanel.utils;

import java.awt.Component;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

import es.explorer.ExplorerFile;

/************************************************************************																
 * Explorer tree cell renderer. Handles the graphics and the general behavior of
 * the explorer tree.									
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
 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan Jos� Ortiz S�nchez										
 *         </ul>														
 *         <ul>															
 *         Delf�n Rup�rez Ca�as											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Mart�n L�zaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo G�mez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see DefaultTreeCellRenderer																													
 ***********************************************************************/
public class ExtendedTreeCellRenderer extends DefaultTreeCellRenderer {

	/**
	 * Extended tree cell renderer class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Compilable file image icon.
	 */
	private static Icon ICON_COMPILABLE = new ImageIcon(
			"./resources/icons/explorer/compilable.png");
	/**
	 * Main file image icon.
	 */
	private static Icon ICON_MAIN = new ImageIcon(
			"./resources/icons/explorer/main.png");
	/**
	 * Folder image icon.
	 */
	private static Icon ICON_FOLDER = new ImageIcon(
			"./resources/icons/explorer/folder.png");
	/**
	 * Default file image icon.
	 */
	private static Icon ICON_DEFAULT = new ImageIcon(
			"./resources/icons/explorer/default.png");

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.tree.DefaultTreeCellRenderer#getTreeCellRendererComponent
	 * (javax.swing.JTree, java.lang.Object, boolean, boolean, boolean, int,
	 * boolean)
	 */
	@Override
	public Component getTreeCellRendererComponent(JTree tree, Object value,
			boolean sel, boolean expanded, boolean leaf, int row,
			boolean hasFocus) {

		super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf,
				row, hasFocus);

		DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
		ExplorerFile explorerFile;
		
		try {
			
			// Get the info from the node
			explorerFile = (ExplorerFile) node.getUserObject();
			
			// Set the tree node text
			setText(explorerFile.getName());
			
			// Set the tree node icon
			setIcon(ICON_DEFAULT);
			if (explorerFile.isDirectory())
				setIcon(ICON_FOLDER);
			if (explorerFile.isCompilableFile())
				setIcon(ICON_COMPILABLE);
			if (explorerFile.isMainFile())
				setIcon(ICON_MAIN);

		} catch (RuntimeException exception) {
			
			// Updates the log
			//Log.getLog().error(exception.getMessage());
			//exception.printStackTrace();
		}
		return this;
	}
}