package operations.listeners;

import gui.MainWindow;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

/**
 * Listener for the Keyboard events in the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class AcideKeyboardListenerForMenus extends KeyAdapter {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	public void keyPressed(KeyEvent evt) {

		MainWindow.getInstance().getMenu().getFile().getSaveFileAs().setEnabled(true);
		MainWindow.getInstance().getMenu().getFile().getSaveFile().setEnabled(true);
		MainWindow.getInstance().getMenu().getFile().getSaveAllFiles().setEnabled(true);

		MainWindow.getInstance().getMenu().getEdit().getUndo().setEnabled(true);
		MainWindow.getInstance().getMenu().getEdit().getRepeat().setEnabled(true);
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
