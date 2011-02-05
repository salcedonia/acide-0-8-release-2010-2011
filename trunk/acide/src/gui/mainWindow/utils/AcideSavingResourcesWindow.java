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
package gui.mainWindow.utils;

import es.configuration.project.workbench.AcideWorkbenchManager;
import gui.mainWindow.MainWindow;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE saving resources window.
 * 
 * It is used when the user decides to exit the application whether clicking on
 * the menu option or in the closing window button.
 * 
 * Creates a JList with the modified file editor panels which have not been
 * saved yet.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideSavingResourcesWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE saving resources window class serial version
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE saving resources window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE saving resources window panel which contains
	 * the list panel and the main button panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE saving resources window panel which contains
	 * the select and deselect buttons.
	 */
	private JPanel _mainButtonPanel;
	/**
	 * ACIDE - A Configurable IDE saving resources window panel which contains
	 * the accept and cancel buttons.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE saving resources window message label.
	 */
	private JLabel _messageLabel;
	/**
	 * ACIDE - A Configurable IDE saving resources window scroll panel which
	 * contains the list.
	 */
	private JScrollPane _scrollPanel;
	/**
	 * ACIDE - A Configurable IDE saving resources window list component.
	 */
	private JList _listComponent;
	/**
	 * ACIDE - A Configurable IDE saving resources window select all button.
	 */
	private JButton _selectAllButton;
	/**
	 * ACIDE - A Configurable IDE saving resources window deselect all button.
	 */
	private JButton _deselectAllButton;
	/**
	 * ACIDE - A Configurable IDE saving resources window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE saving resources window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE saving resources window resource list.
	 */
	private Vector<String> _resourceList;

	/**
	 * Creates a new ACIDE - A Configurable IDE saving resources window.
	 * 
	 * @param resourceList
	 */
	public AcideSavingResourcesWindow(Vector<String> resourceList) {

		super();

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		try {
			language.getLanguage(AcideResourceManager.getInstance()
					.getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		// Updates the log
		AcideLog.getLog().info(labels.getString("s1012"));

		// Sets the layout
		setLayout(new BorderLayout());

		// Disables the main window
		MainWindow.getInstance().setEnabled(false);

		// Gets the resource list
		_resourceList = resourceList;

		// MAIN PANEL
		_mainPanel = new JPanel(new GridBagLayout());

		// MESSAGE LABEL
		_messageLabel = new JLabel(labels.getString("s1014"));

		// LIST
		_listComponent = new JList(resourceList);

		// Multiple selection
		_listComponent
				.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

		// List selection listener
		_listComponent.addListSelectionListener(new ListSelectionListener(){

			/*
			 * (non-Javadoc)
			 * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
			 */
			@Override
			public void valueChanged(ListSelectionEvent arg0) {
				
				// If there is a selected value in the list at least, the accept button is enabled
				_acceptButton.setEnabled(_listComponent.getSelectedIndex() != -1);
			}	
		});
		
		// SCROLL PANE
		_scrollPanel = new JScrollPane(_listComponent);

		// MAIN BUTTON PANEL
		_mainButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// SELECT ALL BUTTON
		_selectAllButton = new JButton(labels.getString("s1015"));
		_selectAllButton.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Select all the items
				_listComponent.setSelectionInterval(0, _listComponent
						.getModel().getSize() - 1);
			}
		});

		// DESELECT ALL BUTTON
		_deselectAllButton = new JButton(labels.getString("s1016"));
		_deselectAllButton.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Deselects all
				_listComponent.getSelectionModel().clearSelection();
			}
		});

		// ACCEPT BUTTON
		_acceptButton = new JButton(labels.getString("s918"));
		_acceptButton.setEnabled(false);
		_acceptButton.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Checks the list component
				for (int index = 0; index < _listComponent
				.getModel().getSize(); index++) {

					// If the component is selected
					if (_listComponent.getSelectionModel().isSelectedIndex(
							index)) {

						// Updates the selected file editor panel
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.setSelectedFileEditorPanelAt(
										MainWindow
												.getInstance()
												.getFileEditorManager()
												.getIndexOfFileEditorPanel(
														_resourceList
																.get(index)));

						// Saves the file editor
						MainWindow.getInstance().getMenu().getFile()
								.saveOrSaveAS();
					}
				}

				// Save the rest of the parameters in the workbench configuration
				AcideWorkbenchManager.getInstance().saveRestOfWorkbenchConfiguration();
				
				// Closes the window
				dispose();

				// Enables the main window
				MainWindow.getInstance().setEnabled(true);

				// Closes the main window
				System.exit(0);
			}
		});

		// CANCEL BUTTON
		_cancelButton = new JButton(labels.getString("s919"));
		_cancelButton.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Closes the window
				dispose();

				// Enables the main window
				MainWindow.getInstance().setEnabled(true);

				// Closes the main window
				System.exit(0);
			}
		});

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel.add(_messageLabel, constraints);
		constraints.gridy = 1;
		_mainPanel.add(_scrollPanel, constraints);
		_mainButtonPanel.add(_selectAllButton);
		_mainButtonPanel.add(_deselectAllButton);
		constraints.gridy = 2;
		_mainPanel.add(_mainButtonPanel, constraints);
		add(_mainPanel, BorderLayout.CENTER);

		_buttonPanel.add(_acceptButton);
		_buttonPanel.add(_cancelButton);
		add(_buttonPanel, BorderLayout.SOUTH);

		// FRAME
		setTitle(labels.getString("s1013"));
		setIconImage(ICON.getImage());
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE saving resources window resource
	 * list.
	 * 
	 * @return the ACIDE - A Configurable IDE saving resources window resource
	 *         list.
	 */
	public Vector<String> getResourceList() {
		return _resourceList;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE saving resources
	 * window resource list.
	 * 
	 * @param resourceList
	 *            new value to set.
	 */
	public void setResourceList(Vector<String> resourceList) {
		_resourceList = resourceList;
	}
}
