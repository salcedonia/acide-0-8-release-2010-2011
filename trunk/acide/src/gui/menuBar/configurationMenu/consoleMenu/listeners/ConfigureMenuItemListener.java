package gui.menuBar.configurationMenu.consoleMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import operations.factory.AcideGUIFactory;

/**																
 * ACIDE - A Configurable IDE configure menu item listener.											
 *					
 * @version 0.8
 * @see ActionListener																														
 */
public class ConfigureMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {
		AcideGUIFactory.getInstance().buildAcideConsoleConfigurationWindow();
	}
}
