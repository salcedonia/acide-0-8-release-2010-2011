package gui.menu.configuration.output;

import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;
import es.configuration.menu.MenuConfiguration;
import gui.menu.configuration.output.listeners.ConfigureListener;
import gui.menu.configuration.output.listeners.ExternalCommandListener;

/************************************************************************																
 * Output menu of ACIDE - A Configurable IDE											
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
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Configure menu item
	 */
	private JMenuItem _configure;
	/**
	 * External command item
	 */
	private JMenuItem _externalCommand;
	
	/**
	 * Class constructor
	 */
	public OutputMenu(){
				
		// MENU ITEM
		_configure = new JMenuItem();
		_externalCommand = new JMenuItem();
		
		setLanguageLabels();
	}
	
	/**
	 * Sets the language labels in the selected language
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
		ResourceBundle labels = language.getLabels();
		
		// CONFIGURE
		_configure.setText(labels.getString("s333"));
		
		// CONFIGURE
		_externalCommand.setText(labels.getString("s341"));
	}
	
	/**
	 * Builds the output menu
	 */
	public void buildMenu() {
		
		removeAll();
		
		if (MenuConfiguration.getConfigure())
			add(_configure);
		if (MenuConfiguration.getExternalCommand())
			add(_externalCommand);
	}
	
	/**
	 * Sets the menu item listeners
	 */
	public void setListeners(){
		
		// CONFIGURE
		_configure.addActionListener(new ConfigureListener());
		
		// EXTERNAL COMMAND
		_externalCommand.addActionListener(new ExternalCommandListener());
	}
	
	/**
	 * Returns the external command menu item
	 * 
	 * @return the external command menu item
	 */
	public JMenuItem getExternalCommand() {
		return _externalCommand;
	}

	/**
	 * Sets a new value to the external command menu item
	 * 
	 * @param externalCommand new value to set
	 */ 
	public void setExternalCommand(JMenuItem externalCommand) {
		_externalCommand = externalCommand;
	}
	
	/**
	 * Returns the configure menu item
	 * 
	 * @return the configure menu item
	 */
	public JMenuItem getConfigure() {
		return _configure;
	}

	/**
	 * Sets a new value to the configure menu item
	 * 
	 * @param configure new value to set
	 */
	public void setConfigure(JMenuItem configure) {
		_configure = configure;
	}
}