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
package acide.gui.menuBar.configurationMenu.fileEditor;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.gui.menuBar.configurationMenu.fileEditor.listeners.AcideAutomaticIndentMenuItemListener;
import acide.gui.menuBar.configurationMenu.fileEditor.listeners.AcideFileEditorDisplayOptionsMenuItemListener;
import acide.gui.menuBar.configurationMenu.fileEditor.listeners.AcideLineWrappingMenuItemListener;
import acide.gui.menuBar.configurationMenu.fileEditor.listeners.AcideMaximumLinesToConsoleMenuItemListener;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE file editor menu.
 * 
 * @version 0.8
 * @see JMenu
 */
public class AcideFileEditorMenu extends JMenu {

	/**
	 * ACIDE - A Configurable IDE file editor menu serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE file editor menu file editor display options
	 * menu item name.
	 */
	public static final String FILE_EDITOR_DISPLAY_OPTIONS_NAME = "File Editor Display Options";
	/**
	 * ACIDE - A Configurable IDE file editor menu automatic indent menu item
	 * name.
	 */
	public static final String AUTOMATIC_INDENT_NAME = "Automatic Indent";
	/**
	 * ACIDE - A Configurable IDE file editor menu line wrapping menu item name.
	 */
	public static final String LINE_WRAPPING_NAME = "Line Wrapping";
	/**
	 * ACIDE - A Configurable IDE file editor menu maximum lines to console menu
	 * item name.
	 */
	public static final String MAXIMUM_LINES_TO_CONSOLE_NAME = "Maximum Lines To Console";
	/**
	 * ACIDE - A Configurable IDE file editor menu file editor display options
	 * menu item image icon.
	 */
	private final static ImageIcon CONSOLE_DISPLAY_OPTIONS_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/fileEditor/fileEditorDisplayOptions.png");
	/**
	 * ACIDE - A Configurable IDE file editor menu automatic indent menu item
	 * image icon.
	 */
	private final static ImageIcon AUTOMATIC_INDENT_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/fileEditor/automaticIndent.png");
	/**
	 * ACIDE - A Configurable IDE file editor menu line wrapping menu item image
	 * icon.
	 */
	private final static ImageIcon LINE_WRAPPING_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/fileEditor/lineWrapping.png");
	/**
	 * ACIDE - A Configurable IDE file editor menu maximum lines to console menu
	 * item image icon.
	 */
	private final static ImageIcon MAXIMUM_LINES_TO_CONSOLE_IMAGE = new ImageIcon(
			"./resources/icons/menu/configuration/fileEditor/maximumLinesToConsole.png");
	/**
	 * ACIDE - A Configurable IDE file editor menu file editor display options
	 * menu item.
	 */
	private JMenuItem _fileEditorDisplayOptionsMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor menu automatic indent menu item.
	 */
	private JCheckBoxMenuItem _automaticIndentCheckBoxMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor menu maximum lines to console menu
	 * item.
	 */
	private JMenuItem _maximumLinesToConsoleMenuItem;
	/**
	 * ACIDE - A Configurable IDE file editor menu line wrapping menu item.
	 */
	private JCheckBoxMenuItem _lineWrappingCheckBoxMenuItem;

	/**
	 * Creates a new ACIDE - A Configurable IDE file editor menu.
	 */
	public AcideFileEditorMenu() {

		// Builds the menu components
		buildComponents();

		// Adds the components to the menu
		addComponents();

		// Sets the text of the file editor menu components
		setTextOfMenuComponets();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE file editor menu
	 */
	private void addComponents() {

		// Adds the file editor display options menu item to the menu
		add(_fileEditorDisplayOptionsMenuItem);

		// Adds the automatic indent check box menu item to the menu
		add(_automaticIndentCheckBoxMenuItem);

		// Adds the line wrapping check box menu item to the menu
		add(_lineWrappingCheckBoxMenuItem);

		// Adds the maximum lines to console menu item to the menu
		add(_maximumLinesToConsoleMenuItem);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE file editor menu components.
	 */
	private void buildComponents() {

		// Creates the file editor display options menu item
		_fileEditorDisplayOptionsMenuItem = new JMenuItem(
				CONSOLE_DISPLAY_OPTIONS_IMAGE);

		// Sets the file editor display options menu item name
		_fileEditorDisplayOptionsMenuItem
				.setName(FILE_EDITOR_DISPLAY_OPTIONS_NAME);

		// Creates the automatic indent check box menu item
		_automaticIndentCheckBoxMenuItem = new JCheckBoxMenuItem(
				AUTOMATIC_INDENT_IMAGE);

		// Sets the automatic indent check box menu item name
		_automaticIndentCheckBoxMenuItem.setName(AUTOMATIC_INDENT_NAME);

		// Creates the maximum lines to console menu item
		_maximumLinesToConsoleMenuItem = new JMenuItem(
				MAXIMUM_LINES_TO_CONSOLE_IMAGE);

		// Creates the maximum lines to console menu item name
		_maximumLinesToConsoleMenuItem.setName(MAXIMUM_LINES_TO_CONSOLE_NAME);

		// Creates the line wrapping check box menu item
		_lineWrappingCheckBoxMenuItem = new JCheckBoxMenuItem(
				LINE_WRAPPING_IMAGE);

		// Sets the line wrapping check box menu item accelerator
		_lineWrappingCheckBoxMenuItem.setAccelerator(KeyStroke.getKeyStroke(
				KeyEvent.VK_A, ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// Sets the line wrapping check box menu item name
		_lineWrappingCheckBoxMenuItem.setName(LINE_WRAPPING_NAME);
	}

	/**
	 * Sets the text of the ACIDE - A Configurable IDE file editor menu
	 * components with the labels in the selected language to display.
	 */
	public void setTextOfMenuComponets() {

		// Sets the file editor display options menu item text
		_fileEditorDisplayOptionsMenuItem.setText(AcideLanguageManager
				.getInstance().getLabels().getString("s1041"));

		// Sets the automatic indent check box menu item text
		_automaticIndentCheckBoxMenuItem.setText(AcideLanguageManager
				.getInstance().getLabels().getString("s1097"));

		// Sets the line wrapping check box menu item text
		_lineWrappingCheckBoxMenuItem.setText(AcideLanguageManager
				.getInstance().getLabels().getString("s2014"));

		// Sets the maximum lines to console menu item text
		_maximumLinesToConsoleMenuItem.setText(AcideLanguageManager
				.getInstance().getLabels().getString("s2007"));
	}

	/**
	 * Updates the ACIDE - A Configurable IDE file editor menu components
	 * visibility with the menu configuration.
	 */
	public void updateComponentsVisibility() {

		// Sets the file editor display options menu item to visible or not
		// visible
		_fileEditorDisplayOptionsMenuItem
				.setVisible(AcideMenuConfiguration.getInstance()
						.getIsDisplayed(FILE_EDITOR_DISPLAY_OPTIONS_NAME));

		// Sets the automatic indent check box menu item to visible or not
		// visible
		_automaticIndentCheckBoxMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(AUTOMATIC_INDENT_NAME));

		// Sets the line wrapping check box menu item to visible or not
		// visible
		_lineWrappingCheckBoxMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(LINE_WRAPPING_NAME));

		// Sets the maximum lines to console menu item to visible or not
		// visible
		_maximumLinesToConsoleMenuItem.setVisible(AcideMenuConfiguration
				.getInstance().getIsDisplayed(MAXIMUM_LINES_TO_CONSOLE_NAME));
	}

	/**
	 * Sets ACIDE - A Configurable IDE file editor menu item listeners.
	 */
	public void setListeners() {

		// Sets the file editor display options menu item action listener
		_fileEditorDisplayOptionsMenuItem
				.addActionListener(new AcideFileEditorDisplayOptionsMenuItemListener());

		// Sets the line wrapping check box menu item action listener
		_lineWrappingCheckBoxMenuItem
				.addActionListener(new AcideLineWrappingMenuItemListener());

		// Sets the automatic indent check box menu item action listener
		_automaticIndentCheckBoxMenuItem
				.addActionListener(new AcideAutomaticIndentMenuItemListener());

		// Sets the maximum lines to console menu item action listener
		_maximumLinesToConsoleMenuItem
				.addActionListener(new AcideMaximumLinesToConsoleMenuItemListener());
	}

	/**
	 * Configures the ACIDE - A Configurable IDE file editor menu.
	 */
	public void configure() {

		// Sets the automatic check box menu item state
		_automaticIndentCheckBoxMenuItem
				.setSelected(AcideWorkbenchConfiguration.getInstance()
						.getFileEditorConfiguration().getAutomaticIndent());

		// Sets the line wrapping check box menu item state
		_lineWrappingCheckBoxMenuItem.setSelected(AcideWorkbenchConfiguration
				.getInstance().getFileEditorConfiguration().getLineWrapping());
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor menu file editor
	 * display options menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor menu file editor
	 *         display options menu item.
	 */
	public JMenuItem getFileEditorDisplayOptionsMenuItem() {
		return _fileEditorDisplayOptionsMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor menu file editor line
	 * wrapping check box menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor menu file editor line
	 *         wrapping check box menu item.
	 */
	public JCheckBoxMenuItem getLineWrappingCheckBoxMenuItem() {
		return _lineWrappingCheckBoxMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor menu file editor
	 * automatic indent check box menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor menu file editor
	 *         automatic indent check box menu item.
	 */
	public JCheckBoxMenuItem getAutomaticIndentCheckBoxMenuItem() {
		return _automaticIndentCheckBoxMenuItem;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE file editor menu maximum lines to
	 * console menu item.
	 * 
	 * @return the ACIDE - A Configurable IDE file editor menu maximum lines to
	 *         console menu item.
	 */
	public JMenuItem getMaximumLinesToConsoleMenuItem() {
		return _maximumLinesToConsoleMenuItem;
	}
}
