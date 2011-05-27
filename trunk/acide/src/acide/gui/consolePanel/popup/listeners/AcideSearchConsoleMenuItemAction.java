package acide.gui.consolePanel.popup.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import acide.gui.mainWindow.AcideMainWindow;

/**
 * ACIDE - A Configurable IDE console panel popup menu search console menu item
 * action listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class AcideSearchConsoleMenuItemAction implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Performs the search console menu item action listener
		AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
				.getConsoleMenu().getSearchConsoleMenuItem().doClick();
	}
}
