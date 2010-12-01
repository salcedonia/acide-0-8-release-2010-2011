package gui.menuBar.configurationMenu.outputMenu;

import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;
import es.configuration.menu.MenuConfiguration;
import gui.menuBar.configurationMenu.outputMenu.listeners.ConfigureMenuItemListener;
import gui.menuBar.configurationMenu.outputMenu.listeners.ExternalCommandMenuItemListener;
import gui.menuBar.configurationMenu.outputMenu.listeners.ShellDisplayOptionsMenuItemListener;

/************************************************************************																
 * Output menu of ACIDE - A Configurable IDE.											
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
public class OutputMenu extends JMenu {

	/**
	 * Output menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Configure menu item name.
	 */
	public static final String CONFIGURE_NAME = "Configure";
	/**
	 * External command menu item name.
	 */
	public static final String EXTERNAL_COMMAND_NAME = "External Command";
	/**
	 * Shell display options menu item name.
	 */
	public static final String SHELL_DISPLAY_OPTIONS_NAME = "Shell Display Options";
	/**
	 * Configure menu item.
	 */
	private JMenuItem _configure;
	/**
	 * External command item.
	 */
	private JMenuItem _externalCommand;
	/**
	 * Shell display options.
	 */
	private JMenuItem _shellDisplayOptions;
	
	/**
	 * Creates a new output menu.
	 */
	public OutputMenu(){
				
		// MENU ITEM
		_configure = new JMenuItem();
		_externalCommand = new JMenuItem();
		_shellDisplayOptions = new JMenuItem();
		
		setLanguageLabels();
	}
	
	/**
	 * Sets the language labels in the selected language.
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
		ResourceBundle labels = language.getLabels();
		
		// CONFIGURE
		_configure.setText(labels.getString("s333"));
		
		// CONFIGURE
		_externalCommand.setText(labels.getString("s341"));
		
		// SHELL DISPLAY OPTIONS
		_shellDisplayOptions.setText(labels.getString("s977"));
	}
	
	/**
	 * Builds the output menu.
	 */
	public void buildMenu() {
		
		removeAll();
		
		// CONFIGURE MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(CONFIGURE_NAME))
			add(_configure);
		
		// EXTERNAL COMMAND
		if (MenuConfiguration.getInstance().getIsDisplayed(EXTERNAL_COMMAND_NAME))
			add(_externalCommand);
		
		// SHELL DISPLAY OPTIONS
		if(MenuConfiguration.getInstance().getIsDisplayed(SHELL_DISPLAY_OPTIONS_NAME))
			add(_shellDisplayOptions);
	}
	
	/**
	 * Sets the menu item listeners.
	 */
	public void setListeners(){
		
		// CONFIGURE
		_configure.addActionListener(new ConfigureMenuItemListener());
		
		// EXTERNAL COMMAND
		_externalCommand.addActionListener(new ExternalCommandMenuItemListener());
		
		// SHELL DISPLAY OPTIONS
		_shellDisplayOptions.addActionListener(new ShellDisplayOptionsMenuItemListener());
	}
	
	/**
	 * Returns the external command menu item.
	 * 
	 * @return the external command menu item.
	 */
	public JMenuItem getExternalCommand() {
		return _externalCommand;
	}

	/**
	 * Sets a new value to the external command menu item.
	 * 
	 * @param externalCommand new value to set.
	 */ 
	public void setExternalCommand(JMenuItem externalCommand) {
		_externalCommand = externalCommand;
	}
	
	/**
	 * Returns the configure menu item.
	 * 
	 * @return the configure menu item.
	 */
	public JMenuItem getConfigure() {
		return _configure;
	}

	/**
	 * Sets a new value to the configure menu item.
	 * 
	 * @param configure new value to set
	 */
	public void setConfigure(JMenuItem configure) {
		_configure = configure;
	}
	
	/**
	 * Returns the shell display options menu item.
	 * 
	 * @return the shell display options menu item.
	 */
	public JMenuItem getShellDisplayOptions(){
		return _shellDisplayOptions;
	}
	
	/**
	 * Sets a new value to the shell display options menu item.
	 * 
	 * @param configure new value to set
	 */
	public void setShellDisplayOptions(JMenuItem shellDisplayOptions) {
		_shellDisplayOptions = shellDisplayOptions;
	}
}