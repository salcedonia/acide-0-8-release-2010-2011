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
package acide.gui.menuBar.configurationMenu.toolBarMenu.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;

import acide.configuration.toolBar.AcideToolBarConfiguration;
import acide.configuration.toolBar.consolePanelToolBar.AcideConsolePanelToolBarButtonConf;
import acide.configuration.toolBar.externalAppsToolBar.AcideExternalAppsToolBarButtonConf;
import acide.gui.listeners.AcideStatusBarKeyboardListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.consolePanel.AcideConsolePanelConfigurationPanel;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.consolePanel.utils.AcideConsolePanelConfigurationPanelTableModel;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.externalApps.AcideExternalAppsConfigurationPanel;
import acide.gui.menuBar.configurationMenu.toolBarMenu.gui.externalApps.utils.AcideExternalAppsConfigurationPanelTableModel;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE tool bar configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideToolBarConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE tool bar configuration window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window tabbed pane.
	 */
	private JTabbedPane _tabbedPane;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window console panel
	 * configuration panel.
	 */
	private AcideConsolePanelConfigurationPanel _consolePanelConfigurationPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window external
	 * applications configuration panel.
	 */
	private AcideExternalAppsConfigurationPanel _externalAppsConfigurationPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window flag that
	 * indicates if the changes are saved.
	 */
	private static boolean _areChangesSaved;
	/**
	 * ACIDE - A Configurable IDE tool bar configuration window for modifying
	 * flag.
	 */
	private boolean _forModifying;

	/**
	 * Creates a new ACIDE - A Configurable IDE tool bar configuration window.
	 * 
	 * @param forModifying
	 *            indicates if the ACIDE - A Configurable IDE tool bar
	 *            configuration window is for modifying an existing tool bar
	 *            configuration or for creating a new one.
	 */
	public AcideToolBarConfigurationWindow(boolean forModifying) {

		super();

		// Stores the flag
		_forModifying = forModifying;

		// The changes are saved
		_areChangesSaved = true;

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s132"));

		// Builds the window components
		buildComponents();

		// Adds the components to the window
		addComponents();

		// Sets the action listeners of the window componens
		setListeners();

		// Sets the window configuration
		setWindowConfiguration();

		// Sets the data into the tables from the tool bar configuration.
		setDataFromConfiguration();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE tool bar configuration window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle();

		// Sets the window icon image
		setIconImage(ICON.getImage());

		// The window does not do anything on close
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

		// The window is not resizable
		setResizable(false);

		// Packs the window components
		pack();

		// Centers the location
		setLocationRelativeTo(null);

		// Shows the window
		setVisible(true);

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s207"));
	}

	/**
	 * Sets the ACIDE - A Configurable IDE tool bar configuration window title.
	 */
	private void setTitle() {

		// If the window is used for modifying the tool bar configuration
		if (_forModifying) {

			try {

				// Gets the ACIDE - A Configurable IDE current tool bar
				// configuration
				String currentToolBarConfiguration = AcideResourceManager
						.getInstance().getProperty(
								"currentToolBarConfiguration");

				// Gets the name
				String name = "";
				int lastIndexOfSlash = currentToolBarConfiguration
						.lastIndexOf("\\");
				if (lastIndexOfSlash == -1)
					lastIndexOfSlash = currentToolBarConfiguration
							.lastIndexOf("/");
				name = currentToolBarConfiguration.substring(
						lastIndexOfSlash + 1,
						currentToolBarConfiguration.length() - 6);

				// Sets the window title for modifying
				setTitle(AcideLanguageManager.getInstance().getLabels()
						.getString("s147")
						+ " - " + name);

			} catch (Exception exception) {

				// Displays an error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s295"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}
		} else {

			// Sets the window title by default
			setTitle(AcideLanguageManager.getInstance().getLabels()
					.getString("s910"));
		}
	}

	/**
	 * Sets the data into the tables from the ACIDE - A Configurable IDE tool
	 * bar configuration.
	 */
	private void setDataFromConfiguration() {

		// If the window is used for modifying the tool bar configuration
		if (_forModifying) {

			try {

				// Gets the current tool bar configuration
				String currentToolBarConfiguration = AcideResourceManager
						.getInstance().getProperty(
								"currentToolBarConfiguration");

				// Loads the console panel tool bar configuration temporal list
				AcideToolBarConfiguration.getInstance()
						.getConsolePanelToolBarConfiguration()
						.loadTemporalList(currentToolBarConfiguration);

				// Loads the external applications tool bar configuration
				// temporal list
				AcideToolBarConfiguration.getInstance()
						.getExternalAppsToolBarConfiguration()
						.loadTemporalList(currentToolBarConfiguration);

				// Updates the console panel configuration panel table model
				// with the data
				((AcideConsolePanelConfigurationPanelTableModel) _consolePanelConfigurationPanel
						.getTable().getModel())
						.setItems(AcideToolBarConfiguration.getInstance()
								.getConsolePanelToolBarConfiguration()
								.getTemporalList());

				// Updates the external application configuration panel table
				// model with the data
				((AcideExternalAppsConfigurationPanelTableModel) _externalAppsConfigurationPanel
						.getTable().getModel())
						.setItems(AcideToolBarConfiguration.getInstance()
								.getExternalAppsToolBarConfiguration()
								.getTemporalList());

			} catch (Exception exception) {

				// Displays an error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s269"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}
		} else {

			// Creates the console panel configuration panel table model with
			// empty data
			((AcideConsolePanelConfigurationPanelTableModel) _consolePanelConfigurationPanel
					.getTable().getModel())
					.setItems(new ArrayList<AcideConsolePanelToolBarButtonConf>());

			// Creates the external application configuration panel table model
			// with empty data
			((AcideExternalAppsConfigurationPanelTableModel) _externalAppsConfigurationPanel
					.getTable().getModel())
					.setItems(new ArrayList<AcideExternalAppsToolBarButtonConf>());
		}
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE tool bar
	 * configuration window with the layout.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new BorderLayout());

		// Adds the tabbed pane to the window
		add(_tabbedPane, BorderLayout.CENTER);

		// Adds the button panel to the window
		add(_buttonPanel, BorderLayout.SOUTH);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE tool bar configuration window
	 * components.
	 */
	private void buildComponents() {

		// Creates the tabbed pane
		_tabbedPane = new JTabbedPane();

		// Creates the console panel configuration panel
		_consolePanelConfigurationPanel = new AcideConsolePanelConfigurationPanel(
				this);

		// Adds the tab with the console panel configuration panel
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s1075"), _consolePanelConfigurationPanel);

		// Creates the external applications configuration panel
		_externalAppsConfigurationPanel = new AcideExternalAppsConfigurationPanel(
				this);

		// Adds the tab with the external applications configuration panel
		_tabbedPane.addTab(AcideLanguageManager.getInstance().getLabels()
				.getString("s1076"), _externalAppsConfigurationPanel);

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the accept button
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s154"));

		// Sets the accept button tool tip text
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s155"));

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s162"));

		// Sets the cancel button tool tip text
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s163"));

		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
	}

	/**
	 * Sets the listeners of the ACIDE - A Configurable IDE tool bar
	 * configuration window components.
	 */
	private void setListeners() {

		// Sets the accept button action listener
		_acceptButton.addActionListener(new AcceptButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// Sets the window closing listener
		addWindowListener(new AcideToolBarConfigurationWindowClosingListener());

		// Puts the escape key in the input map of the window
		getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, false),
				"EscapeKey");

		// Puts the escape key in the action map of the window
		getRootPane().getActionMap().put("EscapeKey", new EscapeKeyAction());

		// Adds the status bar keyboards listener to all the main window
		// components
		AcideStatusBarKeyboardListener statusBarKeyListener = new AcideStatusBarKeyboardListener();

		Component[] components = getContentPane().getComponents();
		for (Component component : components) {
			component.addKeyListener(statusBarKeyListener);
		}
	}

	/**
	 * Applies the changes to the tool bar configuration and the tool bar panel.
	 */
	private void applyChanges() {

		// Sets the console panel tool bar configuration final list
		AcideToolBarConfiguration
				.getInstance()
				.getConsolePanelToolBarConfiguration()
				.setFinalList(
						(((AcideConsolePanelConfigurationPanelTableModel) _consolePanelConfigurationPanel
								.getTable().getModel()).getItems()));

		// Sets the external applications tool bar configuration final list
		AcideToolBarConfiguration
				.getInstance()
				.getExternalAppsToolBarConfiguration()
				.setFinalList(
						(((AcideExternalAppsConfigurationPanelTableModel) _externalAppsConfigurationPanel
								.getTable().getModel()).getItems()));

		// Sets the new tool bar configuration name
		String newName = "";

		if (_forModifying)
			newName = "./configuration/toolbar/lastModified.toolbarConfig";
		else
			newName = "./configuration/toolbar/newToolBar.toolbarConfig";

		// Saves the console panel tool bar configuration final list
		// into the new tool bar configuration
		AcideToolBarConfiguration.getInstance()
				.getConsolePanelToolBarConfiguration().saveFinalList(newName);

		// Saves the external applications tool bar configuration final list
		// into the new tool bar configuration
		AcideToolBarConfiguration.getInstance()
				.getExternalAppsToolBarConfiguration().saveFinalList(newName);

		try {

			// Gets the ACIDE - A Configurable IDE current tool bar
			// configuration
			String currentToolBarConfiguration = AcideResourceManager
					.getInstance().getProperty("currentToolBarConfiguration");

			if (_areChangesSaved) {

				if (!currentToolBarConfiguration
						.endsWith("lastModified.toolbarConfig")
						&& !currentToolBarConfiguration
								.endsWith("newToolBar.toolbarConfig")) {

					// Updates the ACIDE - A Configurable IDE
					// previous
					// tool bar configuration
					AcideResourceManager.getInstance().setProperty(
							"previousToolBarConfiguration",
							currentToolBarConfiguration);
				}
			}

			// Updates the ACIDE - A Configurable IDE current tool
			// bar configuration
			AcideResourceManager.getInstance().setProperty(
					"currentToolBarConfiguration", newName);

			// Builds the tool bar
			AcideMainWindow.getInstance().buildToolBarPanel();

			// Enables the save tool bar menu option
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getToolBarMenu().getSaveToolBarMenuItem().setEnabled(true);

			// The changes are not saved
			_areChangesSaved = false;

			// Validates the changes in the main window
			AcideMainWindow.getInstance().validate();

			// Repaints the main window
			AcideMainWindow.getInstance().repaint();

			// Closes the tool bar configuration window
			closeWindow();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s170"));

		} catch (Exception exception) {

			// Displays an error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s909"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}

	/**
	 * Closes the ACIDE - A Configurable IDE tool bar configuration window.
	 */
	public void closeWindow() {

		// Enables the main window
		AcideMainWindow.getInstance().setEnabled(true);

		// Closes the window
		dispose();

		// Brings it to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);

		// But not always
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
	}

	/**
	 * If there are changes in the window, asks to the user if he wants to save
	 * them. If not, closes the window.
	 */
	private void askForSaving() {

		// If there have been changes ask for saving the changes
		if (_consolePanelConfigurationPanel.areThereChanges()
				|| _externalAppsConfigurationPanel.areThereChanges()) {

			// Asks the user if wants to save the changes
			int returnValue = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s996"), AcideLanguageManager
							.getInstance().getLabels().getString("s995"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If it is not cancel or closed option
			if (returnValue != JOptionPane.CANCEL_OPTION
					&& returnValue != JOptionPane.CLOSED_OPTION) {

				// If it is ok
				if (returnValue == JOptionPane.YES_OPTION)

					// Applies the changes
					applyChanges();
				else
					// Performs the cancel button action
					_cancelButton.doClick();
			}

		} else {

			// Closes the tool bar configuration window
			closeWindow();
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE tool bar configuration window are
	 * change saved flag.
	 * 
	 * @return the ACIDE - A Configurable IDE tool bar configuration window are
	 *         change saved flag.
	 */
	public static boolean areChangesSaved() {
		return _areChangesSaved;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE tool bar configuration
	 * window are change saved flag.
	 * 
	 * @param areChangesSaved
	 *            new value to set.
	 */
	public static void setAreChangesSaved(boolean areChangesSaved) {
		_areChangesSaved = areChangesSaved;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE console panel configuration panel.
	 * 
	 * @return the ACIDE - A Configurable IDE console panel configuration panel.
	 */
	public AcideConsolePanelConfigurationPanel getConsolePanel() {
		return _consolePanelConfigurationPanel;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE external applications
	 * configuration panel.
	 * 
	 * @return the ACIDE - A Configurable IDE external applications
	 *         configuration panel.
	 */
	public AcideExternalAppsConfigurationPanel getExternalAppsConfigurationPanel() {
		return _externalAppsConfigurationPanel;
	}

	/**
	 * ACIDE - A Configurable IDE tool bar configuration window accept button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Asks for saving the changes if any
			askForSaving();
		}
	}

	/**
	 * ACIDE - A Configurable IDE tool bar configuration window cancel button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s164"));

			// Closes the tool bar configuration window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE tool bar configuration window escape key
	 * action.
	 * 
	 * @version 0.8
	 * @see AbstractAction
	 */
	class EscapeKeyAction extends AbstractAction {

		/**
		 * Escape key action serial version UID.
		 */
		private static final long serialVersionUID = 1L;

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Performs the accept button action
			_acceptButton.doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE tool bar configuration window closing
	 * listener.
	 * 
	 * @version 0.8
	 * @see WindowAdapter
	 */
	class AcideToolBarConfigurationWindowClosingListener extends WindowAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent
		 * )
		 */
		@Override
		public void windowClosing(WindowEvent windowEvent) {

			// Asks for saving the changes if any
			askForSaving();
		}
	}
}
