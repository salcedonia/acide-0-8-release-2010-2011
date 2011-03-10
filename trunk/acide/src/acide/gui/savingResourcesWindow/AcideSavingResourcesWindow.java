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
package acide.gui.savingResourcesWindow;

import acide.configuration.project.workbench.AcideWorkbenchManager;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

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
	 * @param resourceList contains the list with the absolute paths of the files to be
	 * saved in the window.
	 */
	public AcideSavingResourcesWindow(Vector<String> resourceList) {

		super();

		// Updates the log
		AcideLog.getLog().info(AcideLanguageManager.getInstance().getLabels().getString("s1012"));

		// Sets the layout
		setLayout(new BorderLayout());

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);

		// Gets the resource list
		_resourceList = resourceList;

		// MAIN PANEL
		_mainPanel = new JPanel(new GridBagLayout());

		// MESSAGE LABEL
		_messageLabel = new JLabel(AcideLanguageManager.getInstance().getLabels().getString("s1014"));

		// LIST COMPONENT
		_listComponent = new JList(resourceList);

		// Multiple selection is allowed on the list component
		_listComponent
				.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

		// SCROLL PANE
		_scrollPanel = new JScrollPane(_listComponent);

		// MAIN BUTTON PANEL
		_mainButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// SELECT ALL BUTTON
		_selectAllButton = new JButton(AcideLanguageManager.getInstance().getLabels().getString("s1015"));
		
		// DESELECT ALL BUTTON
		_deselectAllButton = new JButton(AcideLanguageManager.getInstance().getLabels().getString("s1016"));
		
		// ACCEPT BUTTON
		_acceptButton = new JButton(AcideLanguageManager.getInstance().getLabels().getString("s918"));
		_acceptButton.setEnabled(false);
		
		// CANCEL BUTTON
		_cancelButton = new JButton(AcideLanguageManager.getInstance().getLabels().getString("s42"));
		
		// Sets the listeners of the window components
		setListeners();
		
		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		
		// MAIN PANEL
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

		// BUTTON PANEL
		_buttonPanel.add(_acceptButton);
		_buttonPanel.add(_cancelButton);
		add(_buttonPanel, BorderLayout.SOUTH);

		// FRAME
		setTitle(AcideLanguageManager.getInstance().getLabels().getString("s1013"));
		setIconImage(ICON.getImage());
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);
	}

	private void setListeners() {
		
		// LIST COMPONENT
		_listComponent.addListSelectionListener(new ListComponentSelectionListener());
		
		// SELECT ALL BUTTON
		_selectAllButton.addActionListener(new SelectAllButtonAction());
		
		// DESELECT ALL BUTTON
		_deselectAllButton.addActionListener(new DeselectAllButtonAction());
		
		// ACCEPT BUTTON
		_acceptButton.addActionListener(new AcceptButtonAction());
		
		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonAction());
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
	
	/**
	 * ACIDE - A Configurable IDE saving resources window accept button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonAction implements ActionListener{
		
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
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.setSelectedFileEditorPanelAt(
									AcideMainWindow
											.getInstance()
											.getFileEditorManager()
											.getIndexOfFileEditorPanel(
													_resourceList
															.get(index)));

					// Saves the file editor
					AcideMainWindow.getInstance().getMenu().getFileMenu()
							.saveOrSaveAS();
				}
			}

			// Save the rest of the parameters in the workbench configuration
			AcideWorkbenchManager.getInstance().saveRestOfWorkbenchConfiguration();
			
			// Closes the window
			dispose();

			// Enables the main window
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the main window
			System.exit(0);
		}
	}
	
	/**
	 * ACIDE - A Configurable IDE saving resources window cancel button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonAction implements ActionListener{
		
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
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the main window
			System.exit(0);
		}
	}
	
	/**
	 * ACIDE - A Configurable IDE saving resources window select all button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SelectAllButtonAction implements ActionListener{
		
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
	}
	
	/**
	 * ACIDE - A Configurable IDE saving resources window deselect all button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class DeselectAllButtonAction implements ActionListener{
		
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
	}
	
	/**
	 * ACIDE - A Configurable IDE saving resources window list component selection listener.
	 * 
	 * @version 0.8
	 * @see ListSelectionListener
	 */
	class ListComponentSelectionListener implements ListSelectionListener{
		
		/*
		 * (non-Javadoc)
		 * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
		 */
		@Override
		public void valueChanged(ListSelectionEvent arg0) {
			
			// If there is a selected value in the list at least, the accept button is enabled
			_acceptButton.setEnabled(_listComponent.getSelectedIndex() != -1);
		}
	}
}
