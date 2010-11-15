package gui.menu.view;

import es.configuration.menu.MenuConfiguration;
import gui.menu.view.listeners.ShowExplorerPanelListener;
import gui.menu.view.listeners.ShowLogTabListener;
import gui.menu.view.listeners.ShowOutputPanelListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import operations.log.Log;

import language.Language;

import properties.PropertiesManager;

/************************************************************************																
 * View menu of ACIDE - A Configurable IDE											
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
 ***********************************************************************/
public class ViewMenu extends JMenu {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the show log tab menu item icon
	 */
	private final static String SHOW_LOG = "./resources/icons/menu/view/showLog.png";
	/**
	 * Image file for the show explorer panel check box menu item icon
	 */
	private final static String SHOW_EXPLORER = "./resources/icons/menu/view/showExplorer.png";
	/**
	 * Image file for the show output panel check box menu item icon
	 */
	private final static String SHOW_SHELL_WINDOWS = "./resources/icons/menu/view/showShellWindows.png";
	/**
	 * Show log tab menu item
	 */
	private JMenuItem _showLogTab;
	/**
	 * Show explorer panel check box menu item
	 */
	private JCheckBoxMenuItem _showExplorerPanel;
	/**
	 * Show output panel check box menu item 
	 */
	private JCheckBoxMenuItem _showOutputPanel;
	/**
	 * Shell size
	 */
	private int _shellSize;

	/**
	 * Class constructor
	 */
	public ViewMenu(){
		
		// MENU ITEM
		_showLogTab = new JMenuItem(new ImageIcon(SHOW_LOG));
		_showExplorerPanel = new JCheckBoxMenuItem(new ImageIcon(SHOW_EXPLORER));
		_showExplorerPanel.setSelected(true);		
		_showOutputPanel = new JCheckBoxMenuItem(new ImageIcon(SHOW_SHELL_WINDOWS));
		_showOutputPanel.setSelected(true);

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

		// SHOW LOG TAB
		_showLogTab.setText(labels.getString("s28"));
		_showLogTab.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		
		// SHOW EXPLORER PANEL
		_showExplorerPanel.setText(labels.getString("s221"));
		
		// SHOW OUTPUT PANEL
		_showOutputPanel.setText(labels.getString("s223"));
	}
	
	/**
	 * Builds the view menu
	 */
	public void buildMenu(){
		
		removeAll();
		
		if (MenuConfiguration.getShowLog())
			add(_showLogTab);
		if (MenuConfiguration.getShowBrowser())
			add(_showExplorerPanel);
		if (MenuConfiguration.getShowShellWindow())
			add(_showOutputPanel);
	}
	
	/**
	 * Sets the menu item Listeners
	 */
	public void setListeners(){
		
		// SHOW LOG 
		_showLogTab.addActionListener(new ShowLogTabListener());
		
		// SHOW BROWSER
		_showExplorerPanel.addActionListener(new ShowExplorerPanelListener());
		
		// SHOW SHELL WINDOWS
		_showOutputPanel.addActionListener(new ShowOutputPanelListener());
	}
	
	/**
	 * Returns the show log tab menu item
	 * 
	 * @return the show log tab menu item
	 */
	public JMenuItem getShowLogTab() {
		return _showLogTab;
	}

	/**
	 * Sets a new value to the show log tab menu item
	 * 
	 * @param showLogTab new value to set
	 */
	public void setShowLogTab(JMenuItem showLogTab) {
		_showLogTab = showLogTab;
	}
	
	/**
	 * Returns the show explorer panel check box menu item
	 *  
	 * @return the show explorer panel check box menu item
	 */
	public JCheckBoxMenuItem getShowExplorerPanel() {
		return _showExplorerPanel;
	}

	/**
	 * Sets a new value to the show explorer panel check box menu item
	 * 
	 * @param showExplorerPanel new value to set
	 */
	public void setShowExplorerPanel(JCheckBoxMenuItem showExplorerPanel) {
		_showExplorerPanel = showExplorerPanel;
	}

	/**
	 * Returns the show output panel check box menu item
	 * 
	 * @return the show output panel check box menu item
	 */
	public JCheckBoxMenuItem getShowShellWindow() {
		return _showOutputPanel;
	}

	/**
	 * Sets the show output panel check box menu item
	 * 
	 * @param showOutputPanel new value to set
	 */
	public void setShowShellWindowCBox(JCheckBoxMenuItem showOutputPanel) {
		_showOutputPanel = showOutputPanel;
	}
	
	/**
	 * Returns the show output panel menu item
	 * 
	 * @return the show output panel menu item
	 */
	public JMenuItem getShowOutputPanel() {
		return _showOutputPanel;
	}
	
	/**
	 * Returns the shell size
	 * 
	 * @return the shell size
	 */
	public int getShellSize(){
		return _shellSize;
	}
	
	/**
	 * Sets a new value to the shell size
	 * 
	 * @param shellSize new value to set
	 */
	public void setShellSize(int shellSize){
		_shellSize = shellSize;
	}
}