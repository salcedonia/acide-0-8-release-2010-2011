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
package acide.gui.menuBar.configurationMenu.menuMenu.gui;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;

import acide.configuration.menu.AcideMenuConfiguration;
import acide.configuration.menu.AcideMenuItemInformation;
import acide.configuration.project.AcideProjectConfiguration;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.configurationPanel.AcideConfigurationMenuPanel;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.editPanel.AcideEditMenuPanel;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.filePanel.AcideFileMenuPanel;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.helpPanel.AcideHelpPanel;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.projectPanel.AcideProjectPanel;
import acide.gui.menuBar.configurationMenu.menuMenu.gui.viewPanel.AcideViewPanel;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE menu configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideMenuConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE menu configuration window class serial version
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE menu configuration window image icon.
	 */
	private static final String WINDOW_ICON = "./resources/images/icon.png";
	/**
	 * ACIDE - A Configurable IDE menu configuration window tabbed pane.
	 */
	private JTabbedPane _tabbedPane;
	/**
	 * ACIDE - A Configurable IDE menu configuration window file panel.
	 */
	private AcideFileMenuPanel _fileMenuPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window edit panel.
	 */
	private AcideEditMenuPanel _editMenuPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window project panel.
	 */
	private AcideProjectPanel _projectMenuPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window view panel.
	 */
	private AcideViewPanel _viewMenuPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window configuration panel.
	 */
	private AcideConfigurationMenuPanel _configurationMenuPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window help panel.
	 */
	private AcideHelpPanel _helpMenuPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration window select all button.
	 */
	private JButton _selectAllButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration window select none button.
	 */
	private JButton _selectNoneButton;
	/**
	 * Flag that indicates if the changes are saved or not.
	 */
	private static boolean _changesAreSaved;
	/**
	 * Flag that indicates if the window is used for modifying an existent menu
	 * configuration or to create a new one.
	 */
	private boolean _forModifying;

	/**
	 * Creates a new menu configuration window.
	 * 
	 * @param forModifying
	 *            indicates if the window is used for modify the menu
	 *            configuration or for create it.
	 */
	public AcideMenuConfigurationWindow(boolean forModifying) {

		// Stores the flag
		_forModifying = forModifying;

		// The changes are saved
		_changesAreSaved = true;

		// Builds the window components
		buildComponents();

		// Sets the listeners of the window components
		setListeners();

		// Adds the components to the window
		addComponents();

		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE menu configuration window
	 * components.
	 */
	private void buildComponents() {

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s531"));

		// Creates the tabbed pane
		_tabbedPane = new JTabbedPane();

		// Creates the file menu panel
		_fileMenuPanel = new AcideFileMenuPanel();

		// Adds the file menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s500"), _fileMenuPanel);

		// Creates the edit menu panel
		_editMenuPanel = new AcideEditMenuPanel();

		// Adds the edit menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s501"), _editMenuPanel);

		// Creates the project menu panel
		_projectMenuPanel = new AcideProjectPanel();

		// Adds the project menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s502"), _projectMenuPanel);

		// Creates the view menu panel
		_viewMenuPanel = new AcideViewPanel();

		// Adds the view menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s503"), _viewMenuPanel);

		// Creates the configuration menu panel
		_configurationMenuPanel = new AcideConfigurationMenuPanel();

		// Adds the configuration menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s504"), _configurationMenuPanel);

		// Creates the help menu panel
		_helpMenuPanel = new AcideHelpPanel();

		// Adds the help menu panel to the tabbed pane
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s506"), _helpMenuPanel);

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the accept button
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s507"));

		// Sets the accept button tool tip text
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s508"));

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s509"));

		// Sets the cancel button tool tip text
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s510"));

		// Creates the select all button
		_selectAllButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s515"));

		// Sets the select all button tool tip text
		_selectAllButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s516"));

		// Creates the select none button
		_selectNoneButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s517"));

		// Sets the select none button tool tip text
		_selectNoneButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s518"));

		// If the window is for modifying the menu items
		if (_forModifying)
			// Sets the check boxes from the menu item list
			setCheckBoxesFromMenuItemList();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE menu configuration window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the title
		if (_forModifying) {

			String currentMenuConfiguration = null;

			try {

				// Gets the the ACIDE - A Configurable IDE current menu
				// configuration
				currentMenuConfiguration = AcideResourceManager.getInstance()
						.getProperty("currentMenuConfiguration");

				// Gets the name
				int index = currentMenuConfiguration.lastIndexOf("\\");
				if (index == -1)
					index = currentMenuConfiguration.lastIndexOf("/");
				currentMenuConfiguration = currentMenuConfiguration.substring(
						index + 1, currentMenuConfiguration.length() - 8);
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());

				// Displays an error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s295"), JOptionPane.ERROR_MESSAGE);
			}

			// Sets the window title
			setTitle(AcideLanguageManager.getInstance().getLabels()
					.getString("s532")
					+ " - " + currentMenuConfiguration);
		} else
			// Sets the window title
			setTitle(AcideLanguageManager.getInstance().getLabels()
					.getString("s298"));

		// Sets the window icon
		setIconImage(new ImageIcon(WINDOW_ICON).getImage());

		// The window is not resizable
		setResizable(false);

		// Packs the window components
		pack();

		// Centers the window
		setLocationRelativeTo(null);

		// Displays the window
		setVisible(true);

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s530"));
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE menu configuration
	 * window with the layout.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new BorderLayout());

		// Adds the main panel to the window
		add(_tabbedPane, BorderLayout.NORTH);

		// Adds the select all button to the button panel
		_buttonPanel.add(_selectAllButton);

		// Adds the select none button to the button panel
		_buttonPanel.add(_selectNoneButton);

		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);

		// Adds the button panel to the window
		add(_buttonPanel, BorderLayout.CENTER);
	}

	/**
	 * Sets the listeners to the ACIDE - A Configurable IDE menu configuration
	 * window components.
	 */
	private void setListeners() {

		// Sets the accept button action listener
		_acceptButton.addActionListener(new AcceptButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// When the escape key is pressed down performs the cancel button action
		_cancelButton.registerKeyboardAction(new EscapeKeyAction(),
				"EscapeKey",
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// Sets the select all button action listener
		_selectAllButton.addActionListener(new SelectAllButtonAction());

		// Sets the select none button action listener
		_selectNoneButton.addActionListener(new SelectNoneButtonAction());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());
	}

	/**
	 * Returns the changes are saved flag.
	 * 
	 * @return the changes saved flag.
	 */
	public static boolean getChangesAreSaved() {
		return _changesAreSaved;
	}

	/**
	 * Sets a new value to the changes are saved flag.
	 * 
	 * @param changesAreSaved
	 *            new value to set.
	 */
	public static void setChangesAreSaved(boolean changesAreSaved) {
		_changesAreSaved = changesAreSaved;
	}

	/**
	 * Sets the check box values from the menu item list of the menu
	 * configuration.
	 */
	public void setCheckBoxesFromMenuItemList() {

		// Sets the check box from menu item list in the file menu list
		_fileMenuPanel.setCheckBoxesFromMenuItemList();

		// Sets the check box from menu item list in the edit menu list
		_editMenuPanel.setCheckBoxesFromMenuItemList();

		// Sets the check box from menu item list in the project menu list
		_projectMenuPanel.setCheckBoxesFromMenuItemList();

		// Sets the check box from menu item list in the view menu list
		_viewMenuPanel.setCheckBoxesFromMenuItemList();

		// Sets the check box from menu item list in the configuration menu list
		_configurationMenuPanel.setCheckBoxesFromMenuItemList();

		// Sets the check box from menu item list in the help menu list
		_helpMenuPanel.setCheckBoxesFromMenuItemList();
	}

	/**
	 * Builds the menu item information list with the check box values.
	 * 
	 * @return the menu item information list with the check box values.
	 */
	public ArrayList<AcideMenuItemInformation> buildMenuItemInformationList() {

		// Creates the menu item list
		ArrayList<AcideMenuItemInformation> menuItemList = new ArrayList<AcideMenuItemInformation>();

		// Adds the file menu information
		_fileMenuPanel.addFileMenuInformation(menuItemList);

		// Adds the edit menu information
		_editMenuPanel.addEditMenuInformation(menuItemList);

		// Adds the project menu information
		_projectMenuPanel.addProjectMenuInformation(menuItemList);

		// Adds the view menu information
		_viewMenuPanel.addViewMenuInformation(menuItemList);

		// Adds the configuration menu information
		_configurationMenuPanel.addConfigurationMenuInformation(menuItemList);

		// Adds the help menu information
		_helpMenuPanel.addHelpMenuInformation(menuItemList);

		return menuItemList;
	}

	/**
	 * Closes the ACIDE - A Configurable IDE menu configuration window.
	 */
	private void closeWindow() {

		// Enables the main window again
		AcideMainWindow.getInstance().setEnabled(true);

		// Closes the window
		dispose();

		// Brings the main window to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);

		// But not permanently
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration window select none button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SelectNoneButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Selects none in the file menu panel
			_fileMenuPanel.selectNone();

			// Selects edit in the file menu panel
			_editMenuPanel.selectNone();

			// Selects project in the file menu panel
			_projectMenuPanel.selectNone();

			// Selects view in the file menu panel
			_viewMenuPanel.selectNone();

			// Selects configuration in the file menu panel
			_configurationMenuPanel.selectNone();

			// Selects help in the file menu panel
			_helpMenuPanel.selectNone();
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration window select all button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SelectAllButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Selects all in the file menu panel
			_fileMenuPanel.selectAll();

			// Selects all in the edit menu panel
			_editMenuPanel.selectAll();

			// Selects all in the project menu panel
			_projectMenuPanel.selectAll();

			// Selects all in the view menu panel
			_viewMenuPanel.selectAll();

			// Selects all in the configuration menu panel
			_configurationMenuPanel.selectAll();

			// Selects all in the help menu panel
			_helpMenuPanel.selectAll();
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration window cancel button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s520"));

			// Closes the window
			closeWindow();
		}

	}

	/**
	 * ACIDE - A Configurable IDE menu configuration window accept button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Creates the menu item information list
			ArrayList<AcideMenuItemInformation> menuItemList = buildMenuItemInformationList();

			// Stores the new menu item information list
			AcideMenuConfiguration.getInstance().setMenuElementList(
					menuItemList);

			// Picks the new name
			String newName = "";
			if (_forModifying)
				newName = "./configuration/menu/lastModified.menuConfig";
			else
				newName = "./configuration/menu/newMenu.menuConfig";

			// Saves the new configuration
			AcideMenuConfiguration.getInstance().saveMenuConfigurationFile(
					newName);

			try {

				// Gets the the ACIDE - A Configurable IDE current menu
				// configuration
				String currentMenuConfiguration = AcideResourceManager
						.getInstance().getProperty("currentMenuConfiguration");

				if (_changesAreSaved) {

					if (!currentMenuConfiguration
							.endsWith("lastModified.menuConfig")
							&& !currentMenuConfiguration
									.endsWith("newMenu.menuConfig")) {

						// Updates the the ACIDE - A Configurable IDE previous
						// menu
						// configuration
						AcideResourceManager.getInstance().setProperty(
								"previousMenuConfiguration",
								currentMenuConfiguration);
					}
				}

				// Updates the the ACIDE - A Configurable IDE current menu
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentMenuConfiguration", newName);

				// Builds the menu
				AcideMainWindow.getInstance().getMenu()
						.updateComponentsVisibility();

				// Enables the save menu item in the configuration menu
				AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
						.getMenuMenu().getSaveMenuMenuItem().setEnabled(true);

				// Validates the changes in the main window
				AcideMainWindow.getInstance().validate();

				// Repaints the main window
				AcideMainWindow.getInstance().repaint();

				// The changes are not saved
				_changesAreSaved = false;

				// Closes the window
				closeWindow();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s519"));

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject())
					// The project has been modified
					AcideProjectConfiguration.getInstance().setIsModified(true);

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());

				// Displays an error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s292"), JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration window escape key action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class EscapeKeyAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Closes the window
			closeWindow();
		}
	}
}
