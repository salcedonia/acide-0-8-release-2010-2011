package gui.menu.configuration.menu;

import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;
import es.configuration.menu.MenuConfiguration;
import gui.menu.configuration.menu.listeners.LoadMenuListener;
import gui.menu.configuration.menu.listeners.ModifyMenuListener;
import gui.menu.configuration.menu.listeners.NewMenuListener;
import gui.menu.configuration.menu.listeners.SaveAsMenuListener;
import gui.menu.configuration.menu.listeners.SaveMenuListener;

/************************************************************************
 * Menu menu of ACIDE - A Configurable IDE
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
public class MenuMenu extends JMenu {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * New menu menu item
	 */
	private JMenuItem _newMenu;
	/**
	 * Load menu menu item
	 */
	private JMenuItem _loadMenu;
	/**
	 * Modify menu menu item
	 */
	private JMenuItem _modifyMenu;
	/**
	 * Save menu menu item
	 */
	private JMenuItem _saveMenu;
	/**
	 * Save as menu menu item
	 */
	private JMenuItem _saveAsMenu;

	/**
	 * Class constructor
	 */
	public MenuMenu() {

		// MENU ITEM
		_newMenu = new JMenuItem();
		_loadMenu = new JMenuItem();
		_modifyMenu = new JMenuItem();
		_saveMenu = new JMenuItem();
		_saveMenu.setEnabled(false);
		_saveAsMenu = new JMenuItem();

		setLanguageLabels();
	}

	/**
	 * Sets the language labels to display in the selected language
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

		// NEW
		_newMenu.setText(labels.getString("s275"));

		// LOAD
		_loadMenu.setText(labels.getString("s276"));

		// MODIFY
		_modifyMenu.setText(labels.getString("s277"));

		// SAVE
		_saveMenu.setText(labels.getString("s278"));

		// SAVE AS
		_saveAsMenu.setText(labels.getString("s279"));
	}

	/**
	 * Builds the menu menu
	 */
	public void buildMenu() {

		removeAll();

		if (MenuConfiguration.getNewMenu())
			add(_newMenu);
		if (MenuConfiguration.getLoadMenu())
			add(_loadMenu);
		if (MenuConfiguration.getModifyMenu())
			add(_modifyMenu);
		if (MenuConfiguration.getSaveMenu())
			add(_saveMenu);
		if (MenuConfiguration.getSaveAsMenu())
			add(_saveAsMenu);
	}

	/**
	 * Set the menu item listeners
	 */
	public void setListeners() {

		// NEW MENU
		_newMenu.addActionListener(new NewMenuListener());

		// LOAD MENU
		_loadMenu.addActionListener(new LoadMenuListener());

		// MODIFY MENU
		_modifyMenu.addActionListener(new ModifyMenuListener());

		// SAVE MENU
		_saveMenu.addActionListener(new SaveMenuListener());

		// SAVE AS MENU
		_saveAsMenu.addActionListener(new SaveAsMenuListener());
	}

	/**
	 * Returns the load menu menu item
	 * 
	 * @return the load menu menu item
	 */
	public JMenuItem getLoadMenu() {
		return _loadMenu;
	}

	/**
	 * Sets a new value to the load menu menu item
	 * 
	 * @param loadMenu
	 *            new value to set
	 */
	public void setLoadMenu(JMenuItem loadMenu) {
		_loadMenu = loadMenu;
	}

	/**
	 * Returns the modify menu menu item
	 * 
	 * @return the modify menu menu item
	 */
	public JMenuItem getModifyMenu() {
		return _modifyMenu;
	}

	/**
	 * Sets a new value to the modify menu menu item
	 * 
	 * @param modifyMenu
	 *            new value to set
	 */
	public void setModifyMenu(JMenuItem modifyMenu) {
		_modifyMenu = modifyMenu;
	}

	/**
	 * Returns the new menu menu item
	 * 
	 * @return the new menu menu item
	 */
	public JMenuItem getNewMenu() {
		return _newMenu;
	}

	/**
	 * Sets a new value to the new menu menu item
	 * 
	 * @param newMenu
	 *            new value to set
	 */
	public void setNewMenu(JMenuItem newMenu) {
		_newMenu = newMenu;
	}

	/**
	 * Returns the save as menu menu item
	 * 
	 * @return the save as menu menu item
	 */
	public JMenuItem getSaveAsMenu() {
		return _saveAsMenu;
	}

	/**
	 * Sets a new value to the save as menu menu item
	 * 
	 * @param saveAsMenu
	 *            new value to set
	 */
	public void setSaveAsMenu(JMenuItem saveAsMenu) {
		_saveAsMenu = saveAsMenu;
	}

	/**
	 * Returns the save menu menu item
	 * 
	 * @return the save menu menu item
	 */
	public JMenuItem getSaveMenu() {
		return _saveMenu;
	}

	/**
	 * Sets a new value to the save menu menu item
	 * 
	 * @param saveMenu
	 *            new value to set
	 */
	public void setSaveMenu(JMenuItem saveMenu) {
		_saveMenu = saveMenu;
	}
}
