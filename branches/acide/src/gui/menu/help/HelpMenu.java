package gui.menu.help;

import es.configuration.menu.MenuConfiguration;
import gui.menu.help.listeners.ShowAboutUsListener;
import gui.menu.help.listeners.ShowHelpListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import operations.log.Log;

import language.Language;
import properties.PropertiesManager;

/************************************************************************																
 * Help menu of ACIDE - A Configurable IDE											
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
 *           									
 ************************************************************************
 * @author <ul>															
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>			
 *         <li><b>Version 0.1-0.6:</b>									
 *         <ul>															
 *         Diego Cardiel Freire											
 *         </ul>														
 *         <ul>															
 *         Juan José Ortiz Sánchez										
 *         </ul>														
 *         <ul>															
 *         Delfín Rupérez Cañas											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.7:</b>										
 *         <ul>															
 *         Miguel Martín Lázaro											
 *         </ul>														
 *         </li>														
 *         <li><b>Version 0.8:</b>										
 *         <ul>															
 *         Javier Salcedo Gómez											
 *         </ul>														
 *         </li>														
 *         </ul>														
 ************************************************************************																	
 * @version 0.8	
 * @see JMenu																													
 ***********************************************************************/
public class HelpMenu extends JMenu {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the help icon
	 */
	private final static String HELP = "./resources/icons/menu/help/help.png";
	/**
	 * Image file for the about us icon
	 */
	private final static String ABOUT_US = "./resources/icons/menu/help/aboutUs.png";
	/**
	 * Show help menu item
	 */
	private JMenuItem _showHelp;
	/**
	 * Show about us menu item
	 */
	private JMenuItem _showAboutUs;

	/**
	 * Class constructor
	 */
	public HelpMenu() {

		// MENU ITEM
		_showHelp = new JMenuItem(new ImageIcon(HELP));
		_showAboutUs = new JMenuItem(new ImageIcon(ABOUT_US));

		setLanguageLabels();
	}

	/**
	 * Sets the labels to display in the selected language
	 */
	public void setLanguageLabels() {

		// Gets the language
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// SHOW HELP
		_showHelp.setText(labels.getString("s38"));
		_showHelp.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,
				ActionEvent.CTRL_MASK));

		// SHOW ABOUT US
		_showAboutUs.setText(labels.getString("s39"));
	}

	/**
	 * Builds the help menu
	 */
	public void buildMenu() {

		removeAll();

		if (MenuConfiguration.getShowHelp())
			add(_showHelp);
		if (MenuConfiguration.getShowHelp()
				&& MenuConfiguration.getShowAboutUs())
			addSeparator();
		if (MenuConfiguration.getShowAboutUs())
			add(_showAboutUs);
	}

	/**
	 * Sets the menu item Listeners
	 */
	public void setListeners() {

		// SHOW ABOUT US
		_showAboutUs.addActionListener(new ShowAboutUsListener());

		// SHOW HELP
		_showHelp.addActionListener(new ShowHelpListener());
	}

	/**
	 * Returns the about us menu item
	 * 
	 * @return the about us menu item
	 */
	public JMenuItem getShowAboutUs() {
		return _showAboutUs;
	}

	/**
	 * Sets a new value to the show about us menu item
	 * 
	 * @param showAboutUs
	 *            new value to set
	 */
	public void setShowAboutUs(JMenuItem showAboutUs) {
		_showAboutUs = showAboutUs;
	}

	/**
	 * Returns the show help menu item
	 * 
	 * @return the show help menu item
	 */
	public JMenuItem getShowHelp() {
		return _showHelp;
	}

	/**
	 * Sets a new value to the show help menu item
	 * 
	 * @param showHelp
	 *            new value to set
	 */
	public void setShowHelp(JMenuItem showHelp) {
		_showHelp = showHelp;
	}
}
