package gui.menuBar.configurationMenu.toolBarMenu;

import es.configuration.menu.MenuConfiguration;
import gui.menuBar.configurationMenu.toolBarMenu.listeners.LoadToolBarMenuItemListener;
import gui.menuBar.configurationMenu.toolBarMenu.listeners.ModifyToolBarMenuItemListener;
import gui.menuBar.configurationMenu.toolBarMenu.listeners.NewToolBarMenuItemListener;
import gui.menuBar.configurationMenu.toolBarMenu.listeners.SaveAsToolBaMenuItemrListener;
import gui.menuBar.configurationMenu.toolBarMenu.listeners.SaveToolBarMenuItemListener;

import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************																
 * Tool bar menu of ACIDE - A Configurable IDE.
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
public class ToolBarMenu extends JMenu {

	/**
	 * Tool bar menu class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * New tool bar menu item name.
	 */
	public static final String NEW_TOOLBAR_NAME = "New Toolbar";
	/**
	 * Load tool bar menu item name.
	 */
	public static final String LOAD_TOOLBAR_NAME = "Load Toolbar";
	/**
	 * Modify tool bar menu item name.
	 */
	public static final String MODIFY_TOOLBAR_NAME = "Modify Toolbar";
	/**
	 * Save tool bar menu item name.
	 */
	public static final String SAVE_TOOLBAR_NAME = "Save Toolbar";
	/**
	 * Save tool bar as menu item name.
	 */
	public static final String SAVE_TOOLBAR_AS_NAME = "Save Toolbar As";
	/**
	 * New tool bar menu item.
	 */
	private JMenuItem _newToolBar;
	/**
	 * Load tool bar menu item.
	 */
	private JMenuItem _loadToolBar;
	/**
	 * Modify tool bar menu item.
	 */
	private JMenuItem _modifyToolBar;
	/**
	 * Save tool bar menu item.
	 */
	private JMenuItem _saveToolBar;
	/**
	 * Save as tool menu item.
	 */
	private JMenuItem _saveAsToolBar;
	
	/**
	 * Creates a new tool bar menu.
	 */
	public ToolBarMenu(){
				
		// MENU ITEM
		_newToolBar = new JMenuItem();
		_loadToolBar = new JMenuItem();	
		_modifyToolBar = new JMenuItem();
		_saveToolBar = new JMenuItem();
		_saveToolBar.setEnabled(false);
		_saveAsToolBar = new JMenuItem();
		
		setLanguageLabels();
	}

	/**
	 * Sets the language labels to display in the selected language.
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
		
		// NEW
		_newToolBar.setText(labels.getString("s280"));
		
		// LOAD
		_loadToolBar.setText(labels.getString("s281"));
		
		// MODIFY
		_modifyToolBar.setText(labels.getString("s282"));
		
		// SAVE
		_saveToolBar.setText(labels.getString("s283"));
		
		// SAVE AS
		_saveAsToolBar.setText(labels.getString("s284"));
	}
	
	/**
	 * Sets the tool bar menu item listeners.
	 */
	public void setListeners(){
		
		// NEW
		_newToolBar.addActionListener(new NewToolBarMenuItemListener());
		
		// LOAD
		_loadToolBar.addActionListener(new LoadToolBarMenuItemListener());
		
		// MODIFY
		_modifyToolBar.addActionListener(new ModifyToolBarMenuItemListener());	
		
		// SAVE
		_saveToolBar.addActionListener(new SaveToolBarMenuItemListener());
		
		// SAVE AS
		_saveAsToolBar.addActionListener(new SaveAsToolBaMenuItemrListener());
	}

	/**
	 * Builds the tool bar menu.
	 */
	public void buildMenu(){
		
		removeAll();
		
		// NEW TOOL BAR MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(NEW_TOOLBAR_NAME))
			add(_newToolBar);
		
		// LOAD TOOL BAR MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(LOAD_TOOLBAR_NAME))
			add(_loadToolBar);
		
		// MODIFY TOOL BAR MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(MODIFY_TOOLBAR_NAME))
			add(_modifyToolBar);
		
		// SAVE TOOL BAR MENU
		if (MenuConfiguration.getInstance().getIsDisplayed(SAVE_TOOLBAR_NAME))
			add(_saveToolBar);
		
		// SAVE AS TOOL BAR
		if (MenuConfiguration.getInstance().getIsDisplayed(SAVE_TOOLBAR_AS_NAME))
			add(_saveAsToolBar);
	}
	
	/**
	 * Returns the load tool bar menu item.
	 * 
	 * @return the load tool bar menu item.
	 */
	public JMenuItem getLoadToolBar() {
		return _loadToolBar;
	}

	/**
	 * Sets a new value to the load tool bar menu item.
	 * 
	 * @param loadToolBar new value to set.
	 */
	public void setLoadToolBar(JMenuItem loadToolBar) {
		_loadToolBar = loadToolBar;
	}
	
	/**
	 * Returns the modify tool bar menu item.
	 * 
	 * @return the modify tool bar menu item.
	 */
	public JMenuItem getModifyToolBar() {
		return _modifyToolBar;
	}

	/**
	 * Sets a new value to the modify tool bar menu item.
	 * 
	 * @param modifyToolBar new value to set.
	 */
	public void setModifyToolBar(JMenuItem modifyToolBar) {
		_modifyToolBar = modifyToolBar;
	}
	
	/**
	 * Returns the new tool bar menu item.
	 * 
	 * @return the new tool bar menu item.
	 */
	public JMenuItem getNewToolBar() {
		return _newToolBar;
	}

	/**
	 * Sets a new value to the new tool bar menu item.
	 * 
	 * @param newToolBar new value to set.
	 */
	public void setNewToolBar(JMenuItem newToolBar) {
		_newToolBar = newToolBar;
	}
	
	/**
	 * Returns the save as tool bar menu item.
	 * 
	 * @return the save as tool bar menu item.
	 */
	public JMenuItem getSaveAsToolBar() {
		return _saveAsToolBar;
	}

	/**
	 * Sets a new value to the save as tool bar menu item
	 * 
	 * @param saveAsToolBar new value to set
	 */
	public void setSaveAsToolBar(JMenuItem saveAsToolBar) {
		_saveAsToolBar = saveAsToolBar;
	}
	
	/**
	 * Returns the save tool bar menu item
	 * 
	 * @return the save tool bar menu item
	 */
	public JMenuItem getSaveToolBar() {
		return _saveToolBar;
	}

	/**
	 * Sets a new value to the save tool bar menu item.
	 * 
	 * @param saveToolBar new value to set.
	 */
	public void setSaveToolBar(JMenuItem saveToolBar) {
		_saveToolBar = saveToolBar;
	}
}