package operations.listeners;

import gui.MainWindow;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/**
 * 
 */
public class AcideWindowListener extends WindowAdapter {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent)
	 */
	public void windowClosing(WindowEvent arg0) {
		
		MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.setEnabled(true);
		mainWindow.setAlwaysOnTop(true);
		mainWindow.setAlwaysOnTop(false);
	}
}
