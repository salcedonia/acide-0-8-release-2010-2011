package operations.listeners;

import gui.MainWindow;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

/**
 * 
 */
public class AcideKeyboardListener2 extends KeyAdapter {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
	 */
	public void keyPressed(KeyEvent evt) {

		MainWindow mainWindow = MainWindow.getInstance();

		mainWindow.getMenu().getFile().getSaveFileAs().setEnabled(true);
		mainWindow.getMenu().getFile().getSaveFile().setEnabled(true);
		mainWindow.getMenu().getFile().getSaveAllFiles().setEnabled(true);

		mainWindow.getMenu().getEdit().getUndo().setEnabled(true);
		mainWindow.getMenu().getEdit().getRepeat().setEnabled(true);
		mainWindow.getMenu().getEdit().getCopy().setEnabled(true);
		mainWindow.getMenu().getEdit().getPaste().setEnabled(true);
		mainWindow.getMenu().getEdit().getCut().setEnabled(true);

		mainWindow.getMenu().getProject().getSaveProject().setEnabled(true);
		mainWindow.getMenu().getProject().getRemoveFile().setEnabled(true);
		mainWindow.getMenu().getProject().getDeleteFile().setEnabled(true);
		mainWindow.getMenu().getProject().getRemoveFolder().setEnabled(true);
		mainWindow.getMenu().getProject().getSetMain().setEnabled(true);
		mainWindow.getMenu().getProject().getUnsetMain().setEnabled(true);
		mainWindow.getMenu().getProject().getSetCompilable().setEnabled(true);
		mainWindow.getMenu().getProject().getUnsetCompilable().setEnabled(true);
	}
}
