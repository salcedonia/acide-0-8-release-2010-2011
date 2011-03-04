package gui.menuBar.configurationMenu.languageMenu.listeners;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**																
 * ACIDE - A Configurable IDE language menu Spanish menu item listener.
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class SpanishMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {		
		MainWindow.getInstance().getMenu().getConfiguration().getLanguage().changeLanguage("spanish");
	}
}
