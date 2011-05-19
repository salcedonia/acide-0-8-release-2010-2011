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
package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.predetermineWindow;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.configuration.workbench.lexiconAssigner.AcideLexiconAssigner;
import acide.files.AcideFileExtensionFilterManager;
import acide.files.AcideFileManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.predetermineWindow.utils.AcidePredetermineLexiconWindowTableModel;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE default lexicons window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideDefaultLexiconsWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE default lexicons window serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE default lexicons window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE default lexicons window file editor configuration panel.
	 */
	private JPanel _fileEditorConfigurationPanel;
	/**
	 * ACIDE - A Configurable IDE default lexicons window table.
	 */
	private JTable _table;
	/**
	 * ACIDE - A Configurable IDE default lexicons window table scroll pane.
	 */
	private JScrollPane _tableScrollPane;
	/**
	 * ACIDE - A Configurable IDE default lexicons window table button panel.
	 */
	private JPanel _tableButtonPanel;
	/**
	 * ACIDE - A Configurable IDE default lexicons window console configuration
	 * panel.
	 */
	private JPanel _consoleConfigurationPanel;
	/**
	 * ACIDE - A Configurable IDE default lexicons window console lexicon label.
	 */
	private JLabel _consoleLexiconLabel;
	/**
	 * ACIDE - A Configurable IDE default lexicons window console lexicon text
	 * field.
	 */
	private JTextField _consoleLexiconTextField;
	/**
	 * ACIDE - A Configurable IDE default lexicons window examine console
	 * lexicon button.
	 */
	private JButton _examineConsoleLexiconButton;
	/**
	 * ACIDE - A Configurable IDE default lexicons window add lexicon assigner
	 * button.
	 */
	private JButton _addLexiconAssignerButton;
	/**
	 * ACIDE - A Configurable IDE default lexicons window remove lexicon
	 * assigner button.
	 */
	private JButton _removeLexiconAssignerButton;
	/**
	 * ACIDE - A Configurable IDE default lexicons window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE default lexicons window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE default lexicons window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE default lexicons window apply lexicon to
	 * console check box.
	 */
	private JCheckBox _applyLexiconToConsoleCheckBox;
	/**
	 * ACIDE - A Configurable IDE default lexicons window are there changes
	 * flag.
	 */
	private boolean _areThereChanges;

	/**
	 * Creates a new ACIDE - A Configurable IDE default lexicons window.
	 */
	public AcideDefaultLexiconsWindow() {

		super();

		// There are no changes yet
		_areThereChanges = false;

		// Builds the window components
		buildComponents();

		// Adds the components to the window
		addComponents();

		// Sets the listeners of the window components
		setListeners();

		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE default lexicons window components.
	 */
	private void buildComponents() {

		// Creates the file editor configuration panel
		_fileEditorConfigurationPanel = new JPanel(new BorderLayout());

		// Sets the file editor configuration panel border
		_fileEditorConfigurationPanel.setBorder(BorderFactory
				.createTitledBorder(AcideLanguageManager.getInstance()
						.getLabels().getString("s2001")));
		
		// Builds the table
		buildTable();

		// Creates the table button panel
		_tableButtonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));

		// Creates the console configuration panel
		_consoleConfigurationPanel = new JPanel(new GridBagLayout());

		// Sets the console configuration panel border
		_consoleConfigurationPanel.setBorder(BorderFactory
				.createTitledBorder(AcideLanguageManager.getInstance()
						.getLabels().getString("s2002")));
		
		// Creates the console lexicon label
		_consoleLexiconLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s1098"));

		// Creates the console lexicon text field
		_consoleLexiconTextField = new JTextField(AcideWorkbenchConfiguration
				.getInstance().getLexiconAssignerConfiguration()
				.getConsoleLexiconConfiguration(), 50);

		// Creates the examine console lexicon button
		_examineConsoleLexiconButton = new JButton("...");

		// Creates the add lexicon assigner button
		_addLexiconAssignerButton = new JButton(AcideLanguageManager
				.getInstance().getLabels().getString("s1083"));

		// Creates the remove lexicon assigner button
		_removeLexiconAssignerButton = new JButton(AcideLanguageManager
				.getInstance().getLabels().getString("s1084"));

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the accept button
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s367"));

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s369"));

		// Creates the apply lexicon to console check box
		_applyLexiconToConsoleCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s2000"));

		// Sets its selected state
		_applyLexiconToConsoleCheckBox.setSelected(AcideWorkbenchConfiguration
				.getInstance().getLexiconAssignerConfiguration()
				.getApplyLexiconToConsole());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE default lexicons window table.
	 */
	private void buildTable() {

		// Creates the table
		_table = new JTable(new AcidePredetermineLexiconWindowTableModel(this));

		// Sets the data into the table
		((AcidePredetermineLexiconWindowTableModel) _table.getModel())
				.setItems(AcideWorkbenchConfiguration.getInstance()
						.getLexiconAssignerConfiguration().getList());

		// Sets the single selection in the table
		_table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		// The columns width are not equal
		_table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);

		// Creates the table scroll panel
		_tableScrollPane = new JScrollPane(_table);

		// Sets its preferred size
		_tableScrollPane.setPreferredSize(new Dimension(750, 250));

		// Sets the table columns width
		setTableColumnsWidth();
	}

	/**
	 * Sets the width of each one of the table columns.
	 */
	public void setTableColumnsWidth() {

		// Gets the scroll pane viewport
		JViewport scroll = (JViewport) _table.getParent();

		// Gets the scroll pane viewport width
		int width = scroll.getPreferredSize().width;

		int columnWidth = 0;
		TableColumnModel columnModel = _table.getColumnModel();
		TableColumn tableColumn;

		for (int index = 0; index < _table.getColumnCount(); index++) {

			tableColumn = columnModel.getColumn(index);

			// One different width per each different column
			switch (index) {
			case 0:
				columnWidth = (10 * width) / 100;
				break;
			case 1:
				columnWidth = (45 * width) / 100;
				break;
			case 2:
				columnWidth = (45 * width) / 100;
				break;
			}

			// Sets the table column preferred size
			tableColumn.setPreferredWidth(columnWidth);
		}
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE default lexicons
	 * window.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new BorderLayout());

		// Builds the file editor configuration panel
		buildMainPanel();

		// Adds the file editor configuration panel to the window
		add(_fileEditorConfigurationPanel, BorderLayout.NORTH);

		// Builds the console configuration panel
		buildConsoleConfigurationPanel();

		// Adds the console configuration panel to the window
		add(_consoleConfigurationPanel, BorderLayout.CENTER);

		// Builds the button panel
		buildButtonPanel();

		// Adds the button panel to the window
		add(_buttonPanel, BorderLayout.SOUTH);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE default lexicons window console
	 * configuration panel.
	 */
	private void buildConsoleConfigurationPanel() {

		GridBagConstraints constraints = new GridBagConstraints();
		constraints.anchor = GridBagConstraints.WEST;
		constraints.fill = GridBagConstraints.NONE;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the apply lexicon to console check box to the console
		// configuration panel
		_consoleConfigurationPanel.add(_applyLexiconToConsoleCheckBox,
				constraints);

		constraints.gridy = 1;

		// Adds the console lexicon label to the console configuration panel
		_consoleConfigurationPanel.add(_consoleLexiconLabel, constraints);

		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 1;

		// Adds the console lexicon text field to the console configuration
		// panel
		_consoleConfigurationPanel.add(_consoleLexiconTextField, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 2;

		// Adds the examine console lexicon button to the console configuration
		// panel
		_consoleConfigurationPanel.add(_examineConsoleLexiconButton,
				constraints);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE default lexicons window button
	 * panel.
	 */
	private void buildButtonPanel() {

		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE default lexicons window file editor configuration panel.
	 */
	private void buildMainPanel() {

		// Builds the table button panel
		buildTableButtonPanel();

		// Adds the table button panel to the file editor configuration panel
		_fileEditorConfigurationPanel.add(_tableButtonPanel, BorderLayout.NORTH);

		// Adds the table scroll pane to the file editor configuration panel
		_fileEditorConfigurationPanel.add(_tableScrollPane, BorderLayout.CENTER);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE default lexicons window file type
	 * list button panel.
	 */
	private void buildTableButtonPanel() {

		// Adds the add file type button to the file type list button panel
		_tableButtonPanel.add(_addLexiconAssignerButton);

		// Adds the remove file type button to the file type list button panel
		_tableButtonPanel.add(_removeLexiconAssignerButton);
	}

	/**
	 * Sets the listeners for the ACIDE - A Configurable IDE default lexicons
	 * window components.
	 */
	private void setListeners() {

		// Sets the accept button action listener
		_acceptButton.addActionListener(new AcceptButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// When the escape key is pressed the executes the cancel button action
		_cancelButton.registerKeyboardAction(new CancelButtonAction(),
				"EscapeKey", KeyStroke.getKeyStroke(
						java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// Sets the add file type button action listener
		_addLexiconAssignerButton
				.addActionListener(new AddLexiconAssignerButtonAction());

		// Sets the remove file type button action listener
		_removeLexiconAssignerButton
				.addActionListener(new RemoveLexiconAssignerButtonAction());

		_examineConsoleLexiconButton
				.addActionListener(new ExamineConsoleLexiconButtonAction());

		// Sets the table mouse listener
		_table.addMouseListener(new TableMouseListener());

		// Sets the window closing listener
		addWindowListener(new WindowClosingListener());
	}

	/**
	 * Sets the ACIDE - A Configurable IDE default lexicons window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s1081"));

		// Sets the window icon image
		setIconImage(ICON.getImage());

		// Does not anything on window closing
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

		// Sets the window not resizable
		setResizable(false);

		// Packs the window components
		pack();

		// Centers the window
		setLocationRelativeTo(null);

		// Sets the window visible
		setVisible(true);

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);
	}

	/**
	 * Applies the changes in the ACIDE - A Configurable IDE lexicon assigner
	 * manager.
	 */
	private void applyChanges() {

		// Resets the lexicon assigner manager
		AcideWorkbenchConfiguration
				.getInstance()
				.getLexiconAssignerConfiguration()
				.setList(
						((AcidePredetermineLexiconWindowTableModel) _table
								.getModel()).getItems());

		// Updates the console lexicon configuration in the lexicon assigner
		// configuration
		AcideWorkbenchConfiguration
				.getInstance()
				.getLexiconAssignerConfiguration()
				.setConsoleLexiconConfiguration(
						_consoleLexiconTextField.getText());

		// Updates the apply lexicon to console flag in the lexicon assigner
		// configuration
		AcideWorkbenchConfiguration
				.getInstance()
				.getLexiconAssignerConfiguration()
				.setApplyLexiconToConsole(
						_applyLexiconToConsoleCheckBox.isSelected());

	}

	/**
	 * Closes the ACIDE - A Configurable IDE default lexicons window.
	 */
	private void closeWindow() {

		// Set the main window enabled again
		AcideMainWindow.getInstance().setEnabled(true);

		// Closes the window
		dispose();

		// Brings the main window to the front
		AcideMainWindow.getInstance().setAlwaysOnTop(true);

		// But not permanently
		AcideMainWindow.getInstance().setAlwaysOnTop(false);
	}

	/**
	 * Create the context menu for the cell in the table.
	 * 
	 * @param rowIndex
	 *            row index.
	 * @param columnIndex
	 *            column index.
	 * 
	 * @return the context menu for the cell in the table.
	 */
	private JPopupMenu createContextMenu(final int rowIndex,
			final int columnIndex) {

		// Creates the context menu
		JPopupMenu contextMenu = new JPopupMenu();

		// Creates the load image menu item
		JMenuItem loadLexiconConfigurationMenuItem = new JMenuItem(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1091"));
		loadLexiconConfigurationMenuItem
				.addActionListener(new ActionListener() {

					/*
					 * (non-Javadoc)
					 * 
					 * @see
					 * java.awt.event.ActionListener#actionPerformed(java.awt
					 * .event. ActionEvent)
					 */
					@Override
					public void actionPerformed(ActionEvent actionEvent) {

						// Selects the extension for the project
						String[] extensions = new String[] { "xml" };

						// Adds the filter to the file chooser
						AcideFileManager
								.getInstance()
								.getFileChooser()
								.addChoosableFileFilter(
										new AcideFileExtensionFilterManager(
												extensions,
												AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s327")));

						// Asks the the file to the user
						String absolutePath = AcideFileManager.getInstance()
								.askForOpenFile("./configuration/lexicon/");

						if (absolutePath != null) {

							// Updates the table with the absolute path
							_table.getModel().setValueAt(absolutePath,
									rowIndex, columnIndex);
						}
					}
				});
		contextMenu.add(loadLexiconConfigurationMenuItem);

		return contextMenu;
	}

	/**
	 * Shows the ACIDE - A Configurable IDE add lexicon assigner window.
	 */
	private void showAcideAddLexiconAssignerWindow() {

		// Shows the add lexicon assigner window
		new AcideAddLexiconAssignerWindow(this);
	}

	/**
	 * Returns the are there changes flag.
	 * 
	 * @return the are there changes flag.
	 */
	public boolean getAreThereChanges() {
		return _areThereChanges;
	}

	/**
	 * Sets a new value to the are there changes flag.
	 * 
	 * @param areThereChanges
	 *            new value to set.
	 */
	public void setAreThereChanges(boolean areThereChanges) {
		_areThereChanges = areThereChanges;
	}

	/**
	 * Returns the table.
	 * 
	 * @return the table.
	 */
	public JTable getTable() {
		return _table;
	}

	/**
	 * Adds a new lexicon assigner to the table.
	 * 
	 * @param lexiconAssigner
	 *            new lexicon assigner to add.
	 */
	public void addLexiconAssigner(AcideLexiconAssigner lexiconAssigner) {

		// Adds the selected row to the model
		((AcidePredetermineLexiconWindowTableModel) _table.getModel())
				.addItem(lexiconAssigner);

		// Updates the table model
		((AcidePredetermineLexiconWindowTableModel) _table.getModel())
				.fireTableRowsInserted(0,
						((AcidePredetermineLexiconWindowTableModel) _table
								.getModel()).getRowCount());

		// Selects the new row in the table
		_table.setRowSelectionInterval(_table.getRowCount() - 1,
				_table.getRowCount() - 1);

		// Scrolls automatically to see the new row, without doing it
		// automatically
		_table.scrollRectToVisible(_table.getCellRect(_table.getRowCount() - 1,
				0, true));

		// There are changes
		_areThereChanges = true;
	}

	/**
	 * ACIDE - A Configurable IDE default lexicons window accept button action
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

			// Applies the changes
			applyChanges();

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE default lexicons window cancel button action
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
		public void actionPerformed(ActionEvent actionEvent) {

			// Closes the window
			closeWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE default lexicons window window listener.
	 * 
	 * @version 0.8
	 * @see WindowAdapter
	 */
	class WindowClosingListener extends WindowAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent
		 * )
		 */
		public void windowClosing(WindowEvent windowEvent) {

			boolean isCancelSelected = false;

			// If there are changes
			if (_areThereChanges) {

				// Asks the user if wants to save the changes
				int returnValue = JOptionPane.showConfirmDialog(null,
						AcideLanguageManager.getInstance().getLabels()
								.getString("s1068"), AcideLanguageManager
								.getInstance().getLabels().getString("s1067"),
						JOptionPane.YES_NO_CANCEL_OPTION);

				// If it is not the cancel or the closed option
				if (returnValue != JOptionPane.CANCEL_OPTION
						&& returnValue != JOptionPane.CLOSED_OPTION) {

					// If it is yes
					if (returnValue == JOptionPane.YES_OPTION)

						// Applies the changes
						applyChanges();

					// If it is no
					else if (returnValue == JOptionPane.NO_OPTION)
						// Performs the cancel button action
						_cancelButton.doClick();
				} else
					isCancelSelected = true;
			}

			if (!isCancelSelected) {

				// Closes the window
				closeWindow();
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE default lexicons window add lexicon assigner
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AddLexiconAssignerButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		public void actionPerformed(ActionEvent actionEvent) {

			// Shows the add lexicon assigner window
			showAcideAddLexiconAssignerWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE default lexicons window remove lexicon
	 * assigner button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class RemoveLexiconAssignerButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// If there is a selected row
			if (_table.getSelectedRow() != -1) {

				// Gets the selected row
				int selectedRow = _table.getSelectedRow();

				// Gets the last row index
				int lastRow = _table.getRowCount() - 1;

				// Removes the selected row from the model
				((AcidePredetermineLexiconWindowTableModel) _table.getModel())
						.removeItem(_table.getSelectedRow());

				// Updates the table model
				((AcidePredetermineLexiconWindowTableModel) _table.getModel())
						.fireTableRowsDeleted(
								0,
								((AcidePredetermineLexiconWindowTableModel) _table
										.getModel()).getRowCount());

				// If there is more than on element
				if (lastRow > 0) {

					// If the selected row is not the last row in the table
					if (selectedRow < lastRow)
						// Selects the following row in the table
						_table.setRowSelectionInterval(selectedRow, selectedRow);
					else
						// Selects the last row in the table
						_table.setRowSelectionInterval(lastRow - 1, lastRow - 1);
				}

				// There are changes
				_areThereChanges = true;
			} else {

				// Displays an error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s156"),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s157"), JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE default lexicons window table mouse listener.
	 * 
	 * @version 0.8
	 * @see MouseAdapter
	 */
	class TableMouseListener extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent )
		 */
		@Override
		public void mousePressed(MouseEvent mouseEvent) {
			maybeShowPopup(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseReleased(MouseEvent mouseEvent) {
			maybeShowPopup(mouseEvent);
		}

		/**
		 * Shows the popup menu only for the 4th column in the table.
		 * 
		 * @param mouseEvent
		 *            mouse event
		 */
		private void maybeShowPopup(MouseEvent mouseEvent) {

			if (mouseEvent.isPopupTrigger() && _table.isEnabled()) {

				// Gets the point
				Point point = new Point(mouseEvent.getX(), mouseEvent.getY());
				int column = _table.columnAtPoint(point);
				int row = _table.rowAtPoint(point);

				// Translate table index to model index
				int modelColumn = _table
						.getColumn(_table.getColumnName(column))
						.getModelIndex();

				if (row >= 0 && row < _table.getRowCount()) {

					// Creates popup menu
					JPopupMenu contextMenu = createContextMenu(row, modelColumn);

					// And show it
					if (contextMenu != null
							&& contextMenu.getComponentCount() > 0) {

						// Only if this is the 3rd column
						if (modelColumn == 2)
							contextMenu.show(_table, point.x, point.y);
					}
				}
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE default lexicons window examine console
	 * lexicon button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ExamineConsoleLexiconButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Selects the extension for the project
			String[] extensions = new String[] { "xml" };

			// Adds the filter to the file chooser
			AcideFileManager
					.getInstance()
					.getFileChooser()
					.addChoosableFileFilter(
							new AcideFileExtensionFilterManager(extensions,
									AcideLanguageManager.getInstance()
											.getLabels().getString("s327")));

			// Asks the the file to the user
			String absolutePath = AcideFileManager.getInstance()
					.askForOpenFile("./configuration/lexicon/");

			if (absolutePath != null) {

				// There are changes
				_areThereChanges = true;

				// Updates the console lexicon text field
				_consoleLexiconTextField.setText(absolutePath);
			}
		}
	}
}
