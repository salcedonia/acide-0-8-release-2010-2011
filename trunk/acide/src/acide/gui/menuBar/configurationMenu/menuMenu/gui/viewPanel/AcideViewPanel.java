/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package acide.gui.menuBar.configurationMenu.menuMenu.gui.viewPanel;

import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.HashMap;

import javax.swing.JCheckBox;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.menu.AcideMenuItemInformation;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.viewMenu.AcideViewMenu;

/**
 * ACIDE - A Configurable IDE view menu panel.
 * 
 * @version 0.8
 * @see JPanel
 */
public class AcideViewPanel extends JPanel {

	/**
	 * ACIDE - A Configurable IDE view menu panel serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE view menu panel view menu instance.
	 */
	private AcideViewMenu _viewMenu = AcideMainWindow.getInstance().getMenu()
			.getViewMenu();
	/**
	 * ACIDE - A Configurable IDE file menu panel components.
	 */
	private HashMap<String, JCheckBox> _components = new HashMap<String, JCheckBox>();
	
	/**
	 * Creates a new ACIDE - A Configurable IDE view menu panel.
	 */
	public AcideViewPanel() {

		super(new GridLayout(0, 2));

		// Builds and adds the components to the panel
		initComponents();
	}

	/**
	 * Builds and adds the components to the ACIDE - A Configurable IDE view
	 * menu panel.
	 */
	public void initComponents() {

		for (int index = 0; index < _viewMenu.getItemCount(); index++) {

			JMenuItem menuItem = null;
			try {

				// Gets the menu item from the view menu
				menuItem = _viewMenu.getItem(index);

				// If it is not a separator
				if (menuItem != null) {

					// Puts the component in the hash map
					_components.put(menuItem.getName(),
							new JCheckBox(menuItem.getText()));

					// Adds the component to the panel
					add(_components.get(menuItem.getName()));
				}
			} catch (ClassCastException exception) {

				// If the file JMenu has a menu instead of a JMenuItem

				// Gets the menu from the file menu
				JMenu menu = (JMenu) _viewMenu.getMenuComponent(index);

				// If it is not a separator
				if (menu != null) {

					// Puts the component in the hash map
					_components.put(menu.getName(),
							new JCheckBox(menu.getText()));

					// Adds the component to the panel
					add(_components.get(menu.getName()));
				}
			}
		}
	}

	/**
	 * Sets the check box values from the menu item list of the menu
	 * configuration.
	 */
	public void setCheckBoxesFromMenuItemList() {

		for (int index = 0; index < _viewMenu.getItemCount(); index++) {

			JMenuItem menuItem = null;
			try {

				// Gets the menu item from the file menu
				menuItem = _viewMenu.getItem(index);

				// If it is not a separator
				if (menuItem != null) {

					// Updates the menu item check box state with the menu
					// configuration
					_components.get(menuItem.getName()).setSelected(
							AcideMenuConfiguration.getInstance()
									.getIsDisplayed(menuItem.getName()));
				}
			} catch (ClassCastException exception) {

				// If the file JMenu has a menu instead of a JMenuItem

				// Gets the menu from the file menu
				JMenu menu = (JMenu) _viewMenu.getMenuComponent(index);

				// If it is not a separator
				if (menu != null) {

					// Updates the menu check box state with the menu
					// configuration
					_components.get(menu.getName()).setSelected(
							AcideMenuConfiguration.getInstance()
									.getIsDisplayed(menu.getName()));
				}
			}
		}
	}

	/**
	 * Adds the view menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addViewMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {

		for (int index = 0; index < _viewMenu.getItemCount(); index++) {

			JMenuItem menuItem = null;
			try {

				// Gets the menu item from the view menu
				menuItem = _viewMenu.getItem(index);

				// If it is not a separator
				if (menuItem != null) {

					// Adds the menu item information to the menu item list
					menuItemList.add(new AcideMenuItemInformation(menuItem
							.getName(), _components.get(menuItem.getName())
							.isSelected()));
				}
			} catch (ClassCastException exception) {

				// If the file JMenu has a menu instead of a JMenuItem

				// Gets the menu from the view menu
				JMenu menu = (JMenu) _viewMenu.getMenuComponent(index);

				// If it is not a separator
				if (menu != null) {

					// Adds the menu information to the menu item list
					menuItemList.add(new AcideMenuItemInformation(menu
							.getName(), _components.get(menu.getName())
							.isSelected()));
				}
			}
		}
	}

	/**
	 * Marks as selected all the ACIDE - A Configurable IDE view menu panel
	 * components.
	 */
	public void selectAll() {

		for (int index = 0; index < _viewMenu.getItemCount(); index++) {

			JMenuItem menuItem = null;
			try {

				// Gets the menu item from the file menu
				menuItem = _viewMenu.getItem(index);

				// If it is not a separator
				if (menuItem != null) {

					// Sets the menu item check box as selected
					_components.get(menuItem.getName()).setSelected(true);
				}
			} catch (ClassCastException exception) {

				// If the file JMenu has a menu instead of a JMenuItem

				// Gets the menu from the view menu
				JMenu menu = (JMenu) _viewMenu.getMenuComponent(index);

				// If it is not a separator
				if (menu != null) {

					// Sets the menu check box as selected
					_components.get(menu.getName()).setSelected(true);
				}
			}
		}
	}

	/**
	 * Marks as selected none of the ACIDE - A Configurable IDE view menu panel
	 * components.
	 */
	public void selectNone() {

		for (int index = 0; index < _viewMenu.getItemCount(); index++) {

			JMenuItem menuItem = null;
			try {

				// Gets the menu item from the view menu
				menuItem = _viewMenu.getItem(index);

				// If it is not a separator
				if (menuItem != null) {

					// Sets the menu item check box as selected
					_components.get(menuItem.getName()).setSelected(false);
				}
			} catch (ClassCastException exception) {

				// If the file JMenu has a menu instead of a JMenuItem

				// Gets the menu from the view menu
				JMenu menu = (JMenu) _viewMenu.getMenuComponent(index);

				// If it is not a separator
				if (menu != null) {

					// Sets the menu check box as selected
					_components.get(menu.getName()).setSelected(false);
				}
			}
		}
	}
}
