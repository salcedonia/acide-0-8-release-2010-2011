package gui.menuBar.helpMenu;

import es.configuration.menu.MenuConfiguration;
import gui.menuBar.helpMenu.listeners.ShowAboutUsMenuItemListener;
import gui.menuBar.helpMenu.listeners.ShowHelpMenuItemListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import operations.log.AcideLog;
import resources.ResourceManager;

import language.AcideLanguage;

/************************************************************************																
 * Help menu of ACIDE - A Configurable IDE.											
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
	 * Help menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Show help menu item name.
	 */
	public final static String SHOW_HELP_NAME = "Show Help";
	/**
	 * Show about us menu item name.
	 */
	public final static String SHOW_ABOUT_US_NAME = "Show About Us";
	/**
	 * Show help menu item image icon.
	 */
	private final static ImageIcon SHOW_HELP_IMAGE = new ImageIcon("./resources/icons/menu/help/help.png");
	/**
	 * Show about us menu item image icon.
	 */
	private final static ImageIcon SHOW_ABOUT_US_IMAGE = new ImageIcon("./resources/icons/menu/help/aboutUs.png");
	/**
	 * Show help menu item.
	 */
	private JMenuItem _showHelp;
	/**
	 * Show about us menu item.
	 */
	private JMenuItem _showAboutUs;

	/**
	 * Creates a new help menu.
	 */
	public HelpMenu() {

		// MENU ITEM
		_showHelp = new JMenuItem(SHOW_HELP_IMAGE);
		_showAboutUs = new JMenuItem(SHOW_ABOUT_US_IMAGE);

		setLanguageLabels();
	}

	/**
	 * Sets the labels to display in the selected language.
	 */
	public void setLanguageLabels() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
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
	 * Builds the help menu.
	 */
	public void buildMenu() {

		removeAll();

		// SHOW HELP
		if (MenuConfiguration.getInstance().getIsDisplayed(SHOW_HELP_NAME))
			add(_showHelp);
		
		// SEPARATOR
		if (MenuConfiguration.getInstance().getIsDisplayed(SHOW_HELP_NAME)
				&& MenuConfiguration.getInstance().getIsDisplayed(SHOW_ABOUT_US_NAME))
			addSeparator();
		
		// SHOW ABOUT US
		if (MenuConfiguration.getInstance().getIsDisplayed(SHOW_ABOUT_US_NAME))
			add(_showAboutUs);
	}

	/**
	 * Sets the help menu item listeners.
	 */
	public void setListeners() {

		// SHOW ABOUT US
		_showAboutUs.addActionListener(new ShowAboutUsMenuItemListener());

		// SHOW HELP
		_showHelp.addActionListener(new ShowHelpMenuItemListener());
	}

	/**
	 * Returns the about us menu item.
	 * 
	 * @return the about us menu item.
	 */
	public JMenuItem getShowAboutUs() {
		return _showAboutUs;
	}

	/**
	 * Sets a new value to the show about us menu item.
	 * 
	 * @param showAboutUs
	 *            new value to set.
	 */
	public void setShowAboutUs(JMenuItem showAboutUs) {
		_showAboutUs = showAboutUs;
	}

	/**
	 * Returns the show help menu item.
	 * 
	 * @return the show help menu item.
	 */
	public JMenuItem getShowHelp() {
		return _showHelp;
	}

	/**
	 * Sets a new value to the show help menu item.
	 * 
	 * @param showHelp
	 *            new value to set.
	 */
	public void setShowHelp(JMenuItem showHelp) {
		_showHelp = showHelp;
	}
}
