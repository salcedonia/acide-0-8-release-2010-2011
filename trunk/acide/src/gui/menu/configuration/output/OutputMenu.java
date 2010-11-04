package gui.menu.configuration.output;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import es.configuration.menu.MenuConfiguration;

import language.Language;

import operations.factory.GUIFactory;
import properties.PropertiesManager;

/**
 * Output menu of the the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class OutputMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Configure menu item.
	 */
	private JMenuItem _configure;
	/**
	 * External command item.
	 */
	private JMenuItem _externalCommand;
	
	/**
	 * Constructor of the class.
	 */
	public OutputMenu(){
				
		// MENU ITEM
		_configure = new JMenuItem();
		_externalCommand = new JMenuItem();
		
		setLanguageLabels();
	}
	
	/**
	 * Set the language labels in the selected language.
	 */
	public void setLanguageLabels() {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		// CONFIGURE
		_configure.setText(labels.getString("s333"));
		
		// CONFIGURE
		_externalCommand.setText(labels.getString("s341"));
	}
	
	/**
	 * Builds the menu.
	 */
	public void buildMenu() {
		
		removeAll();
		
		if (MenuConfiguration.getConfigure())
			add(_configure);
		if (MenuConfiguration.getExternalCommand())
			add(_externalCommand);
	}
	
	/**
	 * Set the listeners
	 */
	public void setListeners(){
		
		// CONFIGURE
		_configure.addActionListener(new ConfigureListener());
		
		// EXTERNAL COMMAND
		_externalCommand.addActionListener(new ExternalCommandListener());
	}
	
	/**
	 * Returns the external command menu item.
	 * 
	 * @return The external command menu item.
	 */
	public JMenuItem getExternalCommand() {
		return _externalCommand;
	}

	/**
	 * Set a new value to the external command menu item.
	 * 
	 * @param externalCommand New value to set.
	 */ 
	public void setExternalCommand(JMenuItem externalCommand) {
		_externalCommand = externalCommand;
	}
	
	/**
	 * Returns the configure menu item.
	 * 
	 * @return The configure menu item.
	 */
	public JMenuItem getConfigure() {
		return _configure;
	}

	/**
	 * Set a new value to the configure menu item.
	 * 
	 * @param configure New value to set.
	 */
	public void setConfigure(JMenuItem configure) {
		_configure = configure;
	}
}

/**
 * Configure menu item listener.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class ConfigureListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		GUIFactory.getInstance().buildOutputGUI();
	}
}

/**
 * External command menu item listener.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
class ExternalCommandListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		GUIFactory.getInstance().buildExternalCommandGUI();
	}
}
