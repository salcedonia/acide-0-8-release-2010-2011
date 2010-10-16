package gui.menu.help;

import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import operations.configuration.MenuConfiguration;
import operations.factory.GUIFactory;

import language.Language;
import properties.PropertiesManager;

/**
 * 
 */
public class HelpMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private final static String HELP_URL = "http://www.fdi.ucm.es/profesor/fernan/ACIDE/index.html";
	/**
	 * 
	 */
	private final static String HELP = "./resources/icons/menu/help/help.png";
	/**
	 * 
	 */
	private final static String ABOUT_US = "./resources/icons/menu/help/aboutUs.png";
	/**
	 * 
	 */
	private JMenuItem _showHelp;
	/**
	 * 
	 */
	private JMenuItem _showAboutUs;
	
	/**
	 * Constructor of the class.
	 */
	public HelpMenu(){
				
		// MENU ITEM
		_showHelp = new JMenuItem(new ImageIcon(HELP));
		_showAboutUs = new JMenuItem(new ImageIcon(ABOUT_US));
		
		setLanguageLabels();
	}

	/**
	 * 
	 */
	public void setLanguageLabels(){
		
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}

		final ResourceBundle labels = language.getLabels();
		
		// SHOW HELP
		_showHelp.setText(labels.getString("s38"));
		_showHelp.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,
				ActionEvent.CTRL_MASK));
		
		// SHOW ABOUT US
		_showAboutUs.setText(labels.getString("s39"));	
	}
	
	/**
	 * 
	 */
	public void buildMenu(){
		
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
	 * 
	 */
	public void setListeners(){
		
		// SHOW ABOUT US
		_showAboutUs.addActionListener(new ShowAboutUsListener());
		
		// SHOW HELP
		_showHelp.addActionListener(new ShowHelpListener());
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getAboutUs() {
		return _showAboutUs;
	}

	/**
	 * 
	 * @param aboutUs
	 */
	public void setAboutUs(JMenuItem aboutUs) {
		_showAboutUs = aboutUs;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getShowHelp() {
		return _showHelp;
	}

	/**
	 * 
	 * @param showHelp
	 */
	public void setShowHelp(JMenuItem showHelp) {
		_showHelp = showHelp;
	}
	
	/**
	 * 
	 */
	class ShowAboutUsListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			
			GUIFactory.getInstance().buildAboutUs();
		}
	}

	/**
	 * 
	 */
	class ShowHelpListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			
			//GUIFactory.getInstance().buildHelp();
			try {
				Desktop.getDesktop().browse(new URI(HELP_URL));
			} catch (IOException e) {
				e.printStackTrace();
			} catch (URISyntaxException e) {
				e.printStackTrace();
			}
		}
	}
}


