package gui.menu.configuration.toolBar;

import es.configuration.menu.MenuConfiguration;
import gui.menu.configuration.toolBar.listeners.LoadToolBarListener;
import gui.menu.configuration.toolBar.listeners.ModifyToolBarListener;
import gui.menu.configuration.toolBar.listeners.NewToolBarListener;
import gui.menu.configuration.toolBar.listeners.SaveAsToolBarListener;
import gui.menu.configuration.toolBar.listeners.SaveToolBarListener;

import java.util.ResourceBundle;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import language.Language;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * Tool bar menu of ACIDE - A Configurable IDE											
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
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * New tool bar menu item
	 */
	private JMenuItem _newToolBar;
	/**
	 * Load tool bar menu item
	 */
	private JMenuItem _loadToolBar;
	/**
	 * Modify tool bar menu item
	 */
	private JMenuItem _modifyToolBar;
	/**
	 * Save tool bar menu item
	 */
	private JMenuItem _saveToolBar;
	/**
	 * Save as tool menu item
	 */
	private JMenuItem _saveAsToolBar;
	
	/**
	 * Class constructor
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
	 * Sets the menu item listeners
	 */
	public void setListeners(){
		
		// NEW
		_newToolBar.addActionListener(new NewToolBarListener());
		
		// LOAD
		_loadToolBar.addActionListener(new LoadToolBarListener());
		
		// MODIFY
		_modifyToolBar.addActionListener(new ModifyToolBarListener());	
		
		// SAVE
		_saveToolBar.addActionListener(new SaveToolBarListener());
		
		// SAVE AS
		_saveAsToolBar.addActionListener(new SaveAsToolBarListener());
	}

	/**
	 * Builds the tool bar menu
	 */
	public void buildMenu(){
		
		removeAll();
		
		if (MenuConfiguration.getNewToolBar())
			add(_newToolBar);
		if (MenuConfiguration.getLoadToolBar())
			add(_loadToolBar);
		if (MenuConfiguration.getModifyToolBar())
			add(_modifyToolBar);
		if (MenuConfiguration.getSaveToolBar())
			add(_saveToolBar);
		if (MenuConfiguration.getSaveAsToolBar())
			add(_saveAsToolBar);
	}
	
	/**
	 * Returns the load tool bar menu item
	 * 
	 * @return the load tool bar menu item
	 */
	public JMenuItem getLoadToolBar() {
		return _loadToolBar;
	}

	/**
	 * Sets a new value to the load tool bar menu item
	 * 
	 * @param loadToolBar new value to set
	 */
	public void setLoadToolBar(JMenuItem loadToolBar) {
		_loadToolBar = loadToolBar;
	}
	
	/**
	 * Returns the modify tool bar menu item
	 * 
	 * @return the modify tool bar menu item
	 */
	public JMenuItem getModifyToolBar() {
		return _modifyToolBar;
	}

	/**
	 * Sets a new value to the modify tool bar menu item
	 * 
	 * @param modifyToolBar new value to set
	 */
	public void setModifyToolBar(JMenuItem modifyToolBar) {
		_modifyToolBar = modifyToolBar;
	}
	
	/**
	 * Returns the new tool bar menu item
	 * 
	 * @return the new tool bar menu item
	 */
	public JMenuItem getNewToolBar() {
		return _newToolBar;
	}

	/**
	 * Sets a new value to the new tool bar menu item
	 * 
	 * @param newToolBar new value to set
	 */
	public void setNewToolBar(JMenuItem newToolBar) {
		_newToolBar = newToolBar;
	}
	
	/**
	 * Returns the save as tool bar menu item
	 * 
	 * @return the save as tool bar menu item
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
	 * Sets a new value to the save tool bar menu item
	 * 
	 * @param saveToolBar new value to set
	 */
	public void setSaveToolBar(JMenuItem saveToolBar) {
		_saveToolBar = saveToolBar;
	}
}