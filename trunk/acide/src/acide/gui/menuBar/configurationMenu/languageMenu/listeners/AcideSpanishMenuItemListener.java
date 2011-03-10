package acide.gui.menuBar.configurationMenu.languageMenu.listeners;

import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**																
 * ACIDE - A Configurable IDE language menu Spanish menu item listener.
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class AcideSpanishMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {		
		AcideMainWindow.getInstance().getMenu().getConfigurationMenu().getLanguageMenu().changeLanguage("spanish");
	}
}
