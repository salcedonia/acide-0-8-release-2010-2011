package gui.listeners;

import gui.mainWindow.MainWindow;

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

		MainWindow.getInstance().getMenu().getFile().getSaveFileAs().setEnabled(true);
		MainWindow.getInstance().getMenu().getFile().getSaveFile().setEnabled(true);
		MainWindow.getInstance().getMenu().getFile().getSaveAllFiles().setEnabled(true);

		MainWindow.getInstance().getMenu().getEdit().getUndo().setEnabled(true);
		MainWindow.getInstance().getMenu().getEdit().getRedo().setEnabled(true);
		MainWindow.getInstance().getMenu().getEdit().getCopy().setEnabled(true);
		MainWindow.getInstance().getMenu().getEdit().getPaste().setEnabled(true);
		MainWindow.getInstance().getMenu().getEdit().getCut().setEnabled(true);

		MainWindow.getInstance().getMenu().getProject().getSaveProject().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getRemoveFile().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getDeleteFile().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getRemoveFolder().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getSetMain().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getUnsetMain().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getSetCompilable().setEnabled(true);
		MainWindow.getInstance().getMenu().getProject().getUnsetCompilable().setEnabled(true);
	}
}
