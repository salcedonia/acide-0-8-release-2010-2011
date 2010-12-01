package es.configuration.menu;

/************************************************************************																
 * Menu item information of ACIDE - A Configurable IDE. It is used to 
 * be stored into an ArrayList which contents all the menu items.
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
public class MenuItemInformation {

	/**
	 * Menu item name.
	 */
	private String _name;
	/**
	 * Menu item flag that indicates if the menu element
	 * has to be displayed in the ACIDE - A Configurable IDE menu bar.
	 */
	private boolean _isDisplayed;
	
	/**
	 * Creates a new menu element information with a new name and isDisplayed flag
	 * given as parameters.
	 * 
	 * @param name new name.
	 * @param isDisplayed new flag value.
	 */
	public MenuItemInformation(String name, boolean isDisplayed){
		_name = name;
		_isDisplayed = isDisplayed;
	}

	/**
	 * Returns the menu item information name.
	 * 
	 * @return the menu item information name.
	 */
	public String getName() {
		return _name;
	}

	/**
	 * Sets a new value to the menu item information name.
	 * 
	 * @param name new value to set.
	 */
	public void setName(String name) {
		_name = name;
	}

	/**
	 * Returns the menu item information is displayed flag.
	 * 
	 * @return the menu item information is displayed flag.
	 */
	public boolean getIsDisplayed() {
		return _isDisplayed;
	}

	/**
	 * Sets a new value to the menu item information is displayed flag.
	 * 
	 * @param isDisplayed new value to set.
	 */
	public void setIsDisplayed(boolean isDisplayed) {
		_isDisplayed = isDisplayed;
	}
}
