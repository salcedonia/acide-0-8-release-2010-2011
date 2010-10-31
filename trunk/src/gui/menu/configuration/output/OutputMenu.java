package gui.menu.configuration.output;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.Language;

import operations.configuration.MenuConfiguration;
import operations.factory.GUIFactory;
import properties.PropertiesManager;

/**
 * 
 */
public class OutputMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private JMenuItem _configure;
	/**
	 * 
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
	 * 
	 */
	public void setLanguageLabels() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		ResourceBundle labels = language.getLabels();
		
		// CONFIGURE
		_configure.setText(labels.getString("s333"));
		
		// CONFIGURE
		_externalCommand.setText(labels.getString("s341"));
	}
	
	/**
	 * 
	 */
	public void buildMenu() {
		
		removeAll();
		
		if (MenuConfiguration.getConfigure())
			add(_configure);
		if (MenuConfiguration.getExternalCommand())
			add(_externalCommand);
	}
	
	/**
	 * 
	 */
	public void setListeners(){
		
		// CONFIGURE
		_configure.addActionListener(new ConfigureListener());
		
		// EXTERNAL COMMAND
		_externalCommand.addActionListener(new ExternalCommandListener());
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getExternalCommand() {
		return _externalCommand;
	}

	/**
	 * 
	 * @param externalCommand
	 */
	public void setExternalCommand(JMenuItem externalCommand) {
		_externalCommand = externalCommand;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getConfigure() {
		return _configure;
	}

	/**
	 * 
	 * @param configure
	 */
	public void setConfigure(JMenuItem configure) {
		_configure = configure;
	}
}

/**
 * 
 */
class ConfigureListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		GUIFactory.getInstance().generaOutputGUI();
	}
}

/**
 * 
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
