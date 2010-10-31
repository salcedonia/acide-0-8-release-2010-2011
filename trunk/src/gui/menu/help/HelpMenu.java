package gui.menu.help;

import gui.MainWindow;

import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import operations.configuration.MenuConfiguration;
import operations.factory.GUIFactory;

import language.Language;
import properties.PropertiesManager;

/**
 * Help menu of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class HelpMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Help URL of the help file in Spanish.
	 */
	private final static String SPANISH_HELP_URL = "./resources/help/userGuideSpanish.doc";
	/**
	 * Help URL of the help file in English.
	 */
	private final static String ENGLISH_HELP_URL = "./resources/help/userGuideEnglish.doc";
	/**
	 * Image file for the help icon.
	 */
	private final static String HELP = "./resources/icons/menu/help/help.png";
	/**
	 * Image file for the about us icon.
	 */
	private final static String ABOUT_US = "./resources/icons/menu/help/aboutUs.png";
	/**
	 * Show help menu item.
	 */
	private JMenuItem _showHelp;
	/**
	 * Show about us menu item.
	 */
	private JMenuItem _showAboutUs;

	/**
	 * Constructor of the class.
	 */
	public HelpMenu() {

		// MENU ITEM
		_showHelp = new JMenuItem(new ImageIcon(HELP));
		_showAboutUs = new JMenuItem(new ImageIcon(ABOUT_US));

		setLanguageLabels();
	}

	/**
	 * Set the labels to display in the selected language.
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
		final ResourceBundle labels = language.getLabels();

		// SHOW HELP
		_showHelp.setText(labels.getString("s38"));
		_showHelp.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,
				ActionEvent.CTRL_MASK));

		// SHOW ABOUT US
		_showAboutUs.setText(labels.getString("s39"));
	}

	/**
	 * Builds the menu adding the menu items to the menu.
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
	 * Set the listeners for the menu items.
	 */
	public void setListeners() {

		// SHOW ABOUT US
		_showAboutUs.addActionListener(new ShowAboutUsListener());

		// SHOW HELP
		_showHelp.addActionListener(new ShowHelpListener());
	}

	/**
	 * Return the about us menu item.
	 * 
	 * @return Returns the about us menu item.
	 */
	public JMenuItem getShowAboutUs() {
		return _showAboutUs;
	}

	/**
	 * Set a new value to the show about us menu item.
	 * 
	 * @param showAboutUs
	 *            New value to set.
	 */
	public void setShowAboutUs(JMenuItem showAboutUs) {
		_showAboutUs = showAboutUs;
	}

	/**
	 * Returns the show help menu item.
	 * 
	 * @return The show help menu item.
	 */
	public JMenuItem getShowHelp() {
		return _showHelp;
	}

	/**
	 * Set a new value to the show help menu item.
	 * 
	 * @param showHelp
	 *            New value to set.
	 */
	public void setShowHelp(JMenuItem showHelp) {
		_showHelp = showHelp;
	}

	/**
	 * Show about us menu item listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class ShowAboutUsListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent arg0) {

			GUIFactory.getInstance().buildAboutUs();
		}
	}

	/**
	 * Show help menu item listener.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class ShowHelpListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent arg0) {

			String path = "";
			
			try {

				if (MainWindow.getInstance().getProjectConfiguration()
						.getLanguage().equals("0"))
					// SPANISH USER GUIDE
					path = SPANISH_HELP_URL;
				else
					// ENGLISH USER'S GUIDE
					path = ENGLISH_HELP_URL;
				
				Desktop.getDesktop().open(new File(path));	
					
			} catch (Exception ex) {
				
				// GET THE LANGUAGE
				Language language = Language.getInstance();

				try {
					language.getLanguage(PropertiesManager.getProperty("language"));
				} catch (Exception e) {
					e.printStackTrace();
				}

				// GET THE LABELS
				final ResourceBundle labels = language.getLabels();
				
				// HELP GUIDE NOT FOUND
				JOptionPane.showMessageDialog(null, labels.getString("s969") + path);
			}
			// GUIFactory.getInstance().buildHelp();
			/*
			 * try { Desktop.getDesktop().browse(new URI(HELP_URL)); } catch
			 * (IOException e) { e.printStackTrace(); } catch
			 * (URISyntaxException e) { e.printStackTrace(); }
			 */
		}
	}
}
