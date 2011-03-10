package acide.gui.listeners;

import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

/**																
 * ACIDE keyboard listener for menus.											
 *					
 * @version 0.8	
 * @see KeyAdapter																													
 */
public class AcideKeyboardListenerForMenus extends KeyAdapter {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyPressed(KeyEvent keyEvent) {

		AcideMainWindow.getInstance().getMenu().getFileMenu().getSaveFileAsMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getFileMenu().getSaveFileMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getFileMenu().getSaveAllFilesMenuItem().setEnabled(true);

		AcideMainWindow.getInstance().getMenu().getEditMenu().getUndoMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getEditMenu().getRedoMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getEditMenu().getCopyMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getEditMenu().getPasteMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getEditMenu().getCutMenuItem().setEnabled(true);

		AcideMainWindow.getInstance().getMenu().getProjectMenu().getSaveProjectMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getRemoveFileMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getDeleteFileMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getRemoveFolderMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getSetMainMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getUnsetMainMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getSetCompilableMenuItem().setEnabled(true);
		AcideMainWindow.getInstance().getMenu().getProjectMenu().getUnsetCompilableMenuItem().setEnabled(true);
	}
}
