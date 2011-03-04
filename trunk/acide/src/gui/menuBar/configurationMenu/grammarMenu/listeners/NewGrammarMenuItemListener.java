package gui.menuBar.configurationMenu.grammarMenu.listeners;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import operations.factory.AcideGUIFactory;

/**																
 * ACIDE - A Configurable IDE new grammar menu item listener.
 *					
 * @version 0.8	
 * @see ActionListener																													
 */
public class NewGrammarMenuItemListener implements ActionListener{
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {
		AcideGUIFactory.getInstance().buildAcideGrammarConfigurationWindow(false);
	}
}
