package gui.explorer.listeners;

import es.explorer.ExplorerFile;
import gui.explorer.ExplorerPanel;
import gui.mainWindow.MainWindow;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/************************************************************************
 * Explorer panel popup menu listener
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
 * @see MouseAdapter
 ***********************************************************************/
public class ExplorerPanelPopupMenuListener extends MouseAdapter{

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		maybeShowPopup(mouseEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(MouseEvent mouseEvent) {
		maybeShowPopup(mouseEvent);
	}

	/**
	 * Shows the popup menu
	 * 
	 * @param mouseEvent
	 *            mouse event
	 */
	private void maybeShowPopup(MouseEvent mouseEvent) {

		if (mouseEvent.isPopupTrigger()) {

			ExplorerPanel explorerPanel = MainWindow.getInstance().getExplorer();
			
			// DEFAULT PROJECT
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {
		
				explorerPanel.getPopupMenu().getSaveProject().setEnabled(false);
				explorerPanel.getPopupMenu().getNewFile().setEnabled(false);
				explorerPanel.getPopupMenu().getAddFile().setEnabled(false);
				explorerPanel.getPopupMenu().getRemoveFile().setEnabled(false);
				explorerPanel.getPopupMenu().getDeleteFile().setEnabled(false);
				explorerPanel.getPopupMenu().getSetMain().setEnabled(false);
				explorerPanel.getPopupMenu().getUnsetMain().setEnabled(false);
				explorerPanel.getPopupMenu().getSetCompilable().setEnabled(false);
				explorerPanel.getPopupMenu().getUnsetCompilable().setEnabled(false);
				explorerPanel.getPopupMenu().getAddFolder().setEnabled(false);
				explorerPanel.getPopupMenu().getRemoveFolder().setEnabled(false);
			} else {
				explorerPanel.getPopupMenu().getSaveProject().setEnabled(false);
				explorerPanel.getPopupMenu().getNewFile().setEnabled(true);
				explorerPanel.getPopupMenu().getAddFile().setEnabled(true);
				explorerPanel.getPopupMenu().getRemoveFile().setEnabled(false);
				explorerPanel.getPopupMenu().getDeleteFile().setEnabled(false);
				explorerPanel.getPopupMenu().getSetMain().setEnabled(false);
				explorerPanel.getPopupMenu().getUnsetMain().setEnabled(false);
				explorerPanel.getPopupMenu().getSetCompilable().setEnabled(false);
				explorerPanel.getPopupMenu().getUnsetCompilable().setEnabled(false);
				explorerPanel.getPopupMenu().getAddFolder().setEnabled(true);
				explorerPanel.getPopupMenu().getRemoveFolder().setEnabled(false);

				if (MainWindow.getInstance().getProjectConfiguration()
						.isModified())
					explorerPanel.getPopupMenu().getSaveProject().setEnabled(true);

				TreePath path = MainWindow.getInstance().getExplorer()
						.getTree().getSelectionPath();

				DefaultMutableTreeNode filePath;
				ExplorerFile explorerFile;

				if (path != null) {

					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					explorerFile = (ExplorerFile) filePath.getUserObject();

					if (!explorerFile.isDirectory()) {
						explorerPanel.getPopupMenu().getRemoveFile().setEnabled(true);
						explorerPanel.getPopupMenu().getDeleteFile().setEnabled(true);
						if (!explorerFile.isMainFile())
							explorerPanel.getPopupMenu().getSetMain().setEnabled(true);
						if (explorerFile.isMainFile())
							explorerPanel.getPopupMenu().getUnsetMain().setEnabled(true);
						if (!explorerFile.isCompilableFile()
								|| (explorerFile.isCompilableFile() && explorerFile
										.isMainFile()))
							explorerPanel.getPopupMenu().getSetCompilable().setEnabled(true);
						if (explorerFile.isCompilableFile() && !explorerFile.isMainFile())
							explorerPanel.getPopupMenu().getUnsetCompilable().setEnabled(true);
					} else {
						explorerPanel.getPopupMenu().getRemoveFolder().setEnabled(true);
					}
				}
			}
			
			// Shows the popup menu
			explorerPanel.getPopupMenu().show(mouseEvent.getComponent(), mouseEvent.getX(), mouseEvent.getY());
		}
	}
}
