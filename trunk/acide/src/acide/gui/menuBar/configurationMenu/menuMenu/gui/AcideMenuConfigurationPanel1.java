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

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE menu configuration panel.
 * 
 * @version 0.8
 */
public class AcideMenuConfigurationPanel1 extends JPanel {

	/**
	 * ACIDE - A Configurable IDE menu configuration panel serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel non displayed items
	 * label.
	 */
	private JLabel _nonDisplayedItemsLabel;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel displayed items
	 * label.
	 */
	private JLabel _displayedItemsLabel;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel non displayed items
	 * list.
	 */
	private JList _nonDisplayedItemsList;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel displayed items list.
	 */
	private JList _displayedItemsList;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel non displayed items
	 * list scroll panel.
	 */
	private JScrollPane _nonDisplayedItemsListScrollPane;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel displayed items list
	 * scroll pane.
	 */
	private JScrollPane _displayedItemsListScrollPane;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel display selected
	 * items button.
	 */
	private JButton _displaySelectedItemsButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel display all items
	 * button.
	 */
	private JButton _displayAllItemsButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel non display selected
	 * items button.
	 */
	private JButton _nonDisplaySelectedItemsButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel non display all items
	 * button.
	 */
	private JButton _nonDisplayAllItemsButton;
	/**
	 * ACIDE - A Configurable IDE menu configuration panel menu item to generate
	 * the panel.
	 */
	private JMenu _menu;

	/**
	 * Creates a new ACIDE - A Configurable IDE menu configuration panel.
	 * 
	 * @param menu
	 *            menu item.
	 */
	public AcideMenuConfigurationPanel1(JMenu menu) {

		super(new GridBagLayout());

		// Stores the menu
		_menu = menu;

		// Builds the panel components
		buildComponents();

		// Sets the listener for the panel components
		setListeners();

		// Adds the components to the panel
		addComponents();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE menu configuration panel
	 * components.
	 */
	private void buildComponents() {

		// Creates the non displayed items label
		_nonDisplayedItemsLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s1073"));

		// Creates the displayed items label
		_displayedItemsLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s1074"));

		// Builds the lists
		buildLists();

		// Creates the button panel
		_buttonPanel = new JPanel(new GridBagLayout());

		// Creates the display selected items button
		_displaySelectedItemsButton = new JButton(">");

		// Sets the display selected items button as not enabled
		_displaySelectedItemsButton.setEnabled(false);

		// Creates the display all items button
		_displayAllItemsButton = new JButton(">>");

		// Creates the non display selected items button
		_nonDisplaySelectedItemsButton = new JButton("<");

		// Sets the non display selected items button as not selected
		_nonDisplaySelectedItemsButton.setEnabled(false);

		// Creates the non display all items button
		_nonDisplayAllItemsButton = new JButton("<<");

		GridBagConstraints constraints = new GridBagConstraints();
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the display selected items button to the button panel
		_buttonPanel.add(_displaySelectedItemsButton, constraints);

		constraints.gridy = 1;

		// Adds the display all items button to the button panel
		_buttonPanel.add(_displayAllItemsButton, constraints);

		constraints.gridy = 2;

		// Adds the non display selected items button to the button panel
		_buttonPanel.add(_nonDisplaySelectedItemsButton, constraints);

		constraints.gridy = 3;

		// Adds the non display all items button to the button panel
		_buttonPanel.add(_nonDisplayAllItemsButton, constraints);
	}

	/**
	 * Builds the lists with the menu items.
	 */
	private void buildLists() {

		// Creates the displayed items list model
		DefaultListModel displayedItemslistModel = new DefaultListModel();

		for (int index = 0; index < _menu.getItemCount(); index++) {

			// If it is not a separator
			if (_menu.getItem(index) != null)

				// Adds the menu item to the displayed items list model
				displayedItemslistModel.addElement(_menu.getItem(index)
						.getText());
		}

		// Creates the non displayed items list
		_nonDisplayedItemsList = new JList(new DefaultListModel());

		// Creates the non displayed items list scroll pane
		_nonDisplayedItemsListScrollPane = new JScrollPane(
				_nonDisplayedItemsList);

		// Creates the displayed items list with the displayed items list model
		_displayedItemsList = new JList(displayedItemslistModel);

		// Creates the displayed items list scroll pane
		_displayedItemsListScrollPane = new JScrollPane(_displayedItemsList);

		// No selected item in the displayed items list
		_displayedItemsList.setSelectedIndex(-1);

		// No selected item in the non displayed items list
		_nonDisplayedItemsList.setSelectedIndex(-1);
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE menu configuration
	 * panel with the layout.
	 */
	private void addComponents() {

		GridBagConstraints constraints = new GridBagConstraints();
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the non displayed items label to the panel
		add(_nonDisplayedItemsLabel, constraints);

		constraints.gridy = 1;

		// Adds the non displayed items list scroll pane to the panel
		add(_nonDisplayedItemsListScrollPane, constraints);

		constraints.gridx = 1;

		// Adds the button panel to the panel
		add(_buttonPanel, constraints);

		constraints.gridx = 2;
		constraints.gridy = 0;

		// Adds the displayed items label to the panel
		add(_displayedItemsLabel, constraints);

		constraints.gridy = 1;

		// Adds the displayed items list scroll pane
		add(_displayedItemsListScrollPane, constraints);
	}

	/**
	 * Sets the listeners for the ACIDE - A Configurable IDE menu configuration
	 * panel components.
	 */
	private void setListeners() {

		// Sets the displayed items list listener
		_displayedItemsList
				.addListSelectionListener(new DisplayedItemsListListener());

		// Sets the non displayed items list listener
		_nonDisplayedItemsList
				.addListSelectionListener(new NonDisplayedItemsListListener());

		// Sets the display selected items button action listener
		_displaySelectedItemsButton
				.addActionListener(new DisplaySelectedItemsButtonAction());

		// Sets the display all items button action listener
		_displayAllItemsButton
				.addActionListener(new DisplayAllItemsButtonAction());

		// Sets the non display selected items button action listener
		_nonDisplaySelectedItemsButton
				.addActionListener(new NonDisplaySelectedItemsButtonAction());

		// Sets the non display all items button action listener
		_nonDisplayAllItemsButton
				.addActionListener(new NonDisplayAllItemsButtonAction());
	}

	/**
	 * Updates both lists with the ACIDE - A Configurable IDE configuration.
	 */
	public void updateListsWithMenuConfiguration() {

	}

	/**
	 * ACIDE - A Configurable IDE menu configuration panel displayed selected
	 * items button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class DisplaySelectedItemsButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			Object[] selectedItems = _nonDisplayedItemsList.getSelectedValues();

			for (int index = 0; index < selectedItems.length; index++) {

				((DefaultListModel) _displayedItemsList.getModel())
						.addElement(selectedItems[index]);
				((DefaultListModel) _nonDisplayedItemsList.getModel())
						.removeElement(selectedItems[index]);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration panel displayed all items
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class DisplayAllItemsButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			for (int index = 0; index < ((DefaultListModel) _nonDisplayedItemsList
					.getModel()).getSize(); index++) {

				((DefaultListModel) _displayedItemsList.getModel())
						.addElement(((DefaultListModel) _nonDisplayedItemsList
								.getModel()).elementAt(index));
			}
			((DefaultListModel) _nonDisplayedItemsList.getModel())
					.removeAllElements();
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration panel non displayed
	 * selected items button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class NonDisplaySelectedItemsButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			Object[] selectedItems = _displayedItemsList.getSelectedValues();

			for (int index = 0; index < selectedItems.length; index++) {

				((DefaultListModel) _nonDisplayedItemsList.getModel())
						.addElement(selectedItems[index]);
				((DefaultListModel) _displayedItemsList.getModel())
						.removeElement(selectedItems[index]);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration panel non displayed all
	 * items button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class NonDisplayAllItemsButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			for (int index = 0; index < ((DefaultListModel) _displayedItemsList
					.getModel()).getSize(); index++) {

				((DefaultListModel) _nonDisplayedItemsList.getModel())
						.addElement(((DefaultListModel) _displayedItemsList
								.getModel()).elementAt(index));
			}
			((DefaultListModel) _displayedItemsList.getModel())
					.removeAllElements();
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration panel displayed items list
	 * listener.
	 * 
	 * @version 0.8
	 * @see ListSelectionListener
	 */
	class DisplayedItemsListListener implements ListSelectionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * javax.swing.event.ListSelectionListener#valueChanged(javax.swing.
		 * event.ListSelectionEvent)
		 */
		@Override
		public void valueChanged(ListSelectionEvent event) {

			if (event.getValueIsAdjusting() == false) {

				if (_displayedItemsList.getSelectedIndex() == -1) {
					// No selection, disable fire button.
					_nonDisplaySelectedItemsButton.setEnabled(false);

				} else {
					// Selection, enable the fire button.
					_nonDisplaySelectedItemsButton.setEnabled(true);
				}
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu configuration panel non displayed items
	 * list listener.
	 * 
	 * @version 0.8
	 * @see ListSelectionListener
	 */
	class NonDisplayedItemsListListener implements ListSelectionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * javax.swing.event.ListSelectionListener#valueChanged(javax.swing.
		 * event.ListSelectionEvent)
		 */
		@Override
		public void valueChanged(ListSelectionEvent event) {
			if (event.getValueIsAdjusting() == false) {

				if (_nonDisplayedItemsList.getSelectedIndex() == -1) {
					// No selection, disable fire button.
					_displaySelectedItemsButton.setEnabled(false);

				} else {
					// Selection, enable the fire button.
					_displaySelectedItemsButton.setEnabled(true);
				}
			}
		}
	}
}
