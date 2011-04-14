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
package acide.gui.menuBar.configurationMenu.menuMenu.gui.editPanel;

import java.awt.GridLayout;
import java.util.ArrayList;

import javax.swing.JCheckBox;
import javax.swing.JPanel;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.menu.AcideMenuItemInformation;
import acide.gui.menuBar.editMenu.AcideEditMenu;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE edit menu panel.
 * 
 * @version 0.8
 * @see JPanel
 */
public class AcideEditMenuPanel extends JPanel {

	/**
	 * ACIDE - A Configurable IDE edit menu panel serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE edit menu panel undo check box.
	 */
	private JCheckBox _undoCheckBox;
	/**
	 * ACIDE - A Configurable IDE edit menu panel redo check box.
	 */
	private JCheckBox _redoCheckBox;
	/**
	 * ACIDE - A Configurable IDE edit menu panel copy check box.
	 */
	private JCheckBox _copyCheckBox;
	/**
	 * ACIDE - A Configurable IDE edit menu panel paste check box.
	 */
	private JCheckBox _pasteCheckBox;
	/**
	 * ACIDE - A Configurable IDE edit menu panel cut check box.
	 */
	private JCheckBox _cutCheckBox;
	/**
	 * ACIDE - A Configurable IDE edit menu panel select all
	 * check box.
	 */
	private JCheckBox _selectAllCheckBox;
	/**
	 * ACIDE - A Configurable IDE edit menu panel go to line check
	 * box.
	 */
	private JCheckBox _goToLineCheckBox;
	/**
	 * ACIDE - A Configurable IDE edit menu panel search check box.
	 */
	private JCheckBox _searchCheckBox;
	/**
	 * ACIDE - A Configurable IDE edit menu panel replace check box.
	 */
	private JCheckBox _replaceCheckBox;

	/**
	 * Creates a new ACIDE - A Configurable IDE edit menu panel.
	 */
	public AcideEditMenuPanel() {

		super(new GridLayout(0, 2));

		// Builds the panel components
		buildComponents();

		// Sets the listeners of the panel components
		setListeners();

		// Adds the components to the panel
		addComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE edit menu panel.
	 */
	public void addComponents() {

		// Adds the undo check box to the panel
		add(_undoCheckBox);

		// Adds the redo check box to the panel
		add(_redoCheckBox);

		// Adds the copy check box to the panel
		add(_copyCheckBox);

		// Adds the paste check box to the panel
		add(_pasteCheckBox);

		// Adds the cut check box to the panel
		add(_cutCheckBox);

		// Adds the select all check box to the panel
		add(_selectAllCheckBox);

		// Adds the go to line check box to the panel
		add(_goToLineCheckBox);

		// Adds the search check box to the panel
		add(_searchCheckBox);

		// Adds the replace check box to the panel
		add(_replaceCheckBox);
	}

	/**
	 * Creates the ACIDE - A Configurable IDE edit menu panel components.
	 */
	public void buildComponents() {

		// Creates the undo check box
		_undoCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s21"));

		// Creates the redo check box
		_redoCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s22"));

		// Creates the copy check box
		_copyCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s23"));

		// Creates the paste check box
		_pasteCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s25"));

		// Creates the cut check box
		_cutCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s24"));

		// Creates the select all check box
		_selectAllCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s190"));

		// Creates the go to line check box
		_goToLineCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s222"));

		// Creates the search check box
		_searchCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s26"));

		// Creates the replace check box
		_replaceCheckBox = new JCheckBox(AcideLanguageManager.getInstance()
				.getLabels().getString("s27"));
	}

	/**
	 * Sets the listener for the ACIDE - A Configurable IDE edit menu panel
	 * components.
	 */
	public void setListeners() {

	}

	/**
	 * Sets the check box values from the menu item list of the menu
	 * configuration.
	 */
	public void setCheckBoxesFromMenuItemList() {

		// Updates the undo check box state
		_undoCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.UNDO_NAME));

		// Updates the redo check box state
		_redoCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.REDO_NAME));

		// Updates the copy check box state
		_copyCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.COPY_NAME));

		// Updates the paste check box state
		_pasteCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.PASTE_NAME));

		// Updates the cut check box state
		_cutCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.CUT_NAME));

		// Updates the select all check box state
		_selectAllCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.SELECT_ALL_NAME));

		// Updates the go to line check box state
		_goToLineCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.GO_TO_LINE_NAME));

		// Updates the search check box state
		_searchCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.SEARCH_NAME));

		// Updates the replace check box state
		_replaceCheckBox.setSelected(AcideMenuConfiguration.getInstance()
				.getIsDisplayed(AcideEditMenu.REPLACE_NAME));
	}

	/**
	 * Adds the edit menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addEditMenuInformation(
			ArrayList<AcideMenuItemInformation> menuItemList) {

		// Adds the undo menu information to the menu item list
		menuItemList.add(new AcideMenuItemInformation(AcideEditMenu.UNDO_NAME,
				_undoCheckBox.isSelected()));

		// Adds the redo menu information to the menu item list
		menuItemList.add(new AcideMenuItemInformation(AcideEditMenu.REDO_NAME,
				_redoCheckBox.isSelected()));

		// Adds the copy menu information to the menu item list
		menuItemList.add(new AcideMenuItemInformation(AcideEditMenu.COPY_NAME,
				_copyCheckBox.isSelected()));

		// Adds the paste menu information to the menu item list
		menuItemList.add(new AcideMenuItemInformation(AcideEditMenu.PASTE_NAME,
				_pasteCheckBox.isSelected()));

		// Adds the cut menu information to the menu item list
		menuItemList.add(new AcideMenuItemInformation(AcideEditMenu.CUT_NAME,
				_cutCheckBox.isSelected()));

		// Adds the select all menu information to the menu item list
		menuItemList.add(new AcideMenuItemInformation(
				AcideEditMenu.SELECT_ALL_NAME, _selectAllCheckBox
						.isSelected()));

		// Adds the go to line menu information to the menu item list
		menuItemList.add(new AcideMenuItemInformation(
				AcideEditMenu.GO_TO_LINE_NAME, _goToLineCheckBox.isSelected()));

		// Adds the search menu information to the menu item list
		menuItemList.add(new AcideMenuItemInformation(
				AcideEditMenu.SEARCH_NAME, _searchCheckBox.isSelected()));

		// Adds the replace menu information to the menu item list
		menuItemList.add(new AcideMenuItemInformation(
				AcideEditMenu.REPLACE_NAME, _replaceCheckBox.isSelected()));
	}

	/**
	 * Marks as selected all the ACIDE - A Configurable IDE edit menu panel
	 * components.
	 */
	public void selectAll() {

		// Sets the undo check box as selected
		_undoCheckBox.setSelected(true);

		// Sets the redo check box as selected
		_redoCheckBox.setSelected(true);

		// Sets the copy check box as selected
		_copyCheckBox.setSelected(true);

		// Sets the paste check box as selected
		_pasteCheckBox.setSelected(true);

		// Sets the cut check box as selected
		_cutCheckBox.setSelected(true);

		// Sets the select all check box as selected
		_selectAllCheckBox.setSelected(true);

		// Sets the go to line check box as selected
		_goToLineCheckBox.setSelected(true);

		// Sets the search check box as selected
		_searchCheckBox.setSelected(true);

		// Sets the replace check box as selected
		_replaceCheckBox.setSelected(true);
	}

	/**
	 * Marks as selected none of the ACIDE - A Configurable IDE edit menu panel
	 * components.
	 */
	public void selectNone() {

		// Sets the undo check box as not selected
		_undoCheckBox.setSelected(false);

		// Sets the redo check box as not selected
		_redoCheckBox.setSelected(false);

		// Sets the copy check box as not selected
		_copyCheckBox.setSelected(false);

		// Sets the paste check box as not selected
		_pasteCheckBox.setSelected(false);

		// Sets the cut check box as not selected
		_cutCheckBox.setSelected(false);

		// Sets the select all check box as not selected
		_selectAllCheckBox.setSelected(false);

		// Sets the go to line check box as not selected
		_goToLineCheckBox.setSelected(false);

		// Sets the search check box as not selected
		_searchCheckBox.setSelected(false);

		// Sets the replace check box as not selected
		_replaceCheckBox.setSelected(false);
	}
}
