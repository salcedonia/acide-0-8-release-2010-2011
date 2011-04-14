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
package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.panels.reserverdWords;

import java.awt.Checkbox;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.AbstractButton;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;

import acide.configuration.lexicon.tokens.AcideLexiconTokenType;
import acide.configuration.lexicon.tokens.AcideLexiconTokenTypeManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.utils.AcideCellColorEditor;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.utils.AcideColorCellRenderer;
import acide.gui.menuBar.configurationMenu.lexiconMenu.utils.LexiconTableModel;
import acide.gui.menuBar.configurationMenu.lexiconMenu.utils.LexiconTableSorter;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE reserved words panel.
 * 
 * @version 0.8
 */
public class AcideReservedWordsPanel extends JPanel {

	/**
	 * ACIDE - A Configurable IDE reserved words panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE reserved words panel color palette button
	 * image icon.
	 */
	private final static ImageIcon COLOR_PALETTE_IMAGE = new ImageIcon(
			"./resources/icons/buttons/colorPalette.png");
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window case sensitive
	 * check box.
	 */
	private Checkbox _caseSensitiveCheckBox;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window color palette
	 * button.
	 */
	private JButton _colorPaletteButton;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window font type combo
	 * box.
	 */
	private JComboBox _fontTypeComboBox;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window case sensitive
	 * label.
	 */
	private JLabel _caseSensitiveLabel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window word label.
	 */
	private JLabel _reservedWordLabel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window color label.
	 */
	private JLabel _colorLabel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window preview label.
	 */
	private JLabel _previewLabel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window font type label.
	 */
	private JLabel _fontTypeLabel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window preview text
	 * field.
	 */
	private JTextField _previewTextField;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window word text field.
	 */
	private JTextField _reservedWordTextField;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window modify button.
	 */
	private JButton _modifyButton;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window apply for all
	 * delimiters button.
	 */
	private JButton _applyForAllDelimitersButton;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window quit button.
	 */
	private JButton _quitButton;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window add button.
	 */
	private JButton _addButton;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window table.
	 */
	private JTable _table;
	/**
	 * Table column names.
	 */
	private String[] _tableColumns;
	/**
	 * Table data.
	 */
	private Object[][] _tableData;
	/**
	 * Table sorter.
	 */
	private LexiconTableSorter _tableSorter;
	/**
	 * Table model.
	 */
	private LexiconTableModel _tableModel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window are there changes
	 * flag.
	 */
	private boolean _areThereChanges;

	/**
	 * Creates a new ACIDE - A Configurable IDE reserved words panel.
	 */
	public AcideReservedWordsPanel() {

		super();

		// There are no changes yet
		_areThereChanges = false;

		// Builds the word list columns
		_tableColumns = new String[3];
		_tableColumns[0] = AcideLanguageManager.getInstance().getLabels()
				.getString("s374");
		_tableColumns[1] = AcideLanguageManager.getInstance().getLabels()
				.getString("s443");
		_tableColumns[2] = AcideLanguageManager.getInstance().getLabels()
				.getString("s375");

		// Builds the panel components
		buildComponents();

		// Sets the listeners of the window components
		setListeners();

		// Adds the components to the panel
		addComponents();
	}

	/**
	 * Builds the panel components.
	 */
	private void buildComponents() {
		
		// Creates the case sensitive label
		_caseSensitiveLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s431"), JLabel.CENTER);

		// Creates the case sensitive check box
		_caseSensitiveCheckBox = new Checkbox();

		// Creates the reserved word label
		_reservedWordLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s389"), JLabel.CENTER);

		// Creates the reserved word text field
		_reservedWordTextField = new JTextField();
		
		// Sets the reserved word text field tool tip text
		_reservedWordTextField.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s390"));

		// Creates the color label
		_colorLabel = new JLabel(AcideLanguageManager.getInstance().getLabels()
				.getString("s391"), JLabel.CENTER);

		// Creates the preview label
		_previewLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s392"), JLabel.CENTER);

		// Creates the preview text field
		_previewTextField = new JTextField();
		
		// Sets the preview text field tool tip text
		_previewTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s393"));
		
		// It is not editable
		_previewTextField.setEditable(false);
		
		// Sets its horizontal alignment as center
		_previewTextField.setHorizontalAlignment(JTextField.CENTER);
		
		// Sets its font
		_previewTextField.setFont(new Font(_reservedWordLabel.getFont()
				.getFontName(), Font.PLAIN, _reservedWordLabel.getFont()
				.getSize()));
		
		// Sets its foreground color
		_previewTextField.setForeground(Color.BLACK);
		
		// Sets its text
		_previewTextField.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s394"));

		// Creates the font type label
		_fontTypeLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s395"), JLabel.CENTER);

		// Creates the font type combo box
		_fontTypeComboBox = new JComboBox();
		
		// Adds the plain font to the font type combo box
		_fontTypeComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s396"));
		
		// Adds the italic font to the font type combo box
		_fontTypeComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s397"));
		
		// Adds the bold font to the font type combo box
		_fontTypeComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s398"));
		
		// Adds the italic + bold font to the font type combo box
		_fontTypeComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s399"));
		
		// It is not enabled
		_fontTypeComboBox.setEnabled(true);
		
		// Sets its tool tip text
		_fontTypeComboBox.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s400"));

		// Creates the color palette button
		_colorPaletteButton = new JButton(COLOR_PALETTE_IMAGE);

		// Creates the add button 
		_addButton = new JButton(AcideLanguageManager.getInstance().getLabels()
				.getString("s385"));
		
		// Sets the add button vertical text position as center
		_addButton.setVerticalTextPosition(AbstractButton.CENTER);
		
		// Sets the add button horizontal text position as leading
		_addButton.setHorizontalTextPosition(AbstractButton.LEADING);
		
		// Sets the add button mnemonic
		_addButton.setMnemonic(KeyEvent.VK_A);
		
		// Sets the add button tool tip text
		_addButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s386"));

		// Creates the quit button
		_quitButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s387"));
		
		// Sets the quit button vertical text position as center
		_quitButton.setVerticalTextPosition(AbstractButton.CENTER);
		
		// Sets the quit button horizontal text position as leading
		_quitButton.setHorizontalTextPosition(AbstractButton.LEADING);
		
		// Sets the quit button mnemonic
		_quitButton.setMnemonic(KeyEvent.VK_Q);
		
		// Sets the quit button tool tip text
		_quitButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s388"));

		// Creates the modify button
		_modifyButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s436"));
		
		// Sets the modify button vertical text position as center
		_modifyButton.setVerticalTextPosition(AbstractButton.CENTER);
		
		// Sets the modify button horizontal text position as leading
		_modifyButton.setHorizontalTextPosition(AbstractButton.LEADING);
		
		// Sets the modify button vertical mnemonic
		_modifyButton.setMnemonic(KeyEvent.VK_M);
		
		// Sets the modify button vertical tool tip text
		_modifyButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s437"));

		// Creates the apply for all delimiters button
		_applyForAllDelimitersButton = new JButton(AcideLanguageManager
				.getInstance().getLabels().getString("s438"));
		
		// Sets the apply for all delimiters button vertical text position as center
		_applyForAllDelimitersButton
				.setVerticalTextPosition(AbstractButton.CENTER);
		
		// Sets the apply for all delimiters button horizontal text position as leading
		_applyForAllDelimitersButton
				.setHorizontalTextPosition(AbstractButton.LEADING);
		
		// // Sets the apply for all delimiters button mnemonic
		_applyForAllDelimitersButton.setMnemonic(KeyEvent.VK_S);
		
		// Sets the apply for all delimiters button tool tip text
		_applyForAllDelimitersButton.setToolTipText(AcideLanguageManager
				.getInstance().getLabels().getString("s439"));
	}

	/**
	 * Adds the components to the panel with the layout.
	 */
	private void addComponents() {
		
		// Sets the layout
		setLayout(new GridBagLayout());
		
		// Adds the components to the panel with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 0;
		
		// Adds the reserved word label to the panel
		add(_reservedWordLabel, constraints);
		
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the reserved word text field to the panel
		add(_reservedWordTextField, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 1;
		
		// Adds the case sensitive label to the panel
		add(_caseSensitiveLabel, constraints);
		
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the case sensitive check box to the panel
		add(_caseSensitiveCheckBox, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 2;
		
		// Adds the color label to the panel
		add(_colorLabel, constraints);
		
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the color palette button to the panel
		add(_colorPaletteButton, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 3;
		
		// Adds the font type label to the panel
		add(_fontTypeLabel, constraints);
		
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the font type combo box to the panel
		add(_fontTypeComboBox, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 4;
		
		// Adds the preview label to the panel
		add(_previewLabel, constraints);
		
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the preview text field to the panel
		add(_previewTextField, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 5;
		
		// Adds the add button to the panel
		add(_addButton, constraints);

		constraints.gridx = 1;
		
		// Adds the quit button to the panel
		add(_quitButton, constraints);

		constraints.gridy = 6;
		constraints.gridx = 0;
		
		// Adds the apply for all delimiters button to the panel
		add(_applyForAllDelimitersButton, constraints);

		constraints.gridx = 1;
		
		// Adds the modify button to the panel
		add(_modifyButton, constraints);
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// Sets the font type combo box action listener
		_fontTypeComboBox.addActionListener(new FontTypeComboBoxAction());

		// Sets the color palette button action listener
		_colorPaletteButton.addActionListener(new ColorPaletteButtonAction());

		// Sets the apply for all delimiters button action listener
		_applyForAllDelimitersButton
				.addActionListener(new ApplyForAllDelimitersButtonAction());

		// Sets the modify button action listener
		_modifyButton.addActionListener(new ModifyButtonAction());

		// Sets the quit button action listener
		_quitButton.addActionListener(new QuitButtonAction());

		// Sets the add button action listener
		_addButton.addActionListener(new AddButtonAction());

		// When the enter key is pressed down the enter key action is performed
		_reservedWordTextField.registerKeyboardAction(new EnterKeyAction(),
				KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, false),
				JComponent.WHEN_FOCUSED);

		// Sets the reserved word text field key listener
		_reservedWordTextField
				.addKeyListener(new ReservedWordTextFieldListener());
	}

	/**
	 * Builds the table with the token type list in the lexicon configuration.
	 * 
	 * @return the table with the token type list in the lexicon configuration.
	 */
	public JScrollPane buildTable() {

		// Creates the table data
		createTableData();

		// Creates the table model
		_tableModel = new LexiconTableModel();

		// Updates the table model
		_tableModel.setValues(_tableColumns, _tableData);

		// Creates the table sorter
		_tableSorter = new LexiconTableSorter(_tableModel);

		// Creates the table with the table sorter
		_table = new JTable(_tableSorter);

		// Adds the table mouse listener
		_table.addMouseListener(new TableMouseListener());

		// Adds the table key listener
		_table.addKeyListener(new TableKeyListener());

		// Adds the reserved word table sorter
		_tableSorter.addTableHeaderMouseListeners(_table);

		// Sets the table renderer
		_table.setDefaultRenderer(Color.class, new AcideColorCellRenderer(true));
		
		// Sets the table editor
		_table.setDefaultEditor(Color.class, new AcideCellColorEditor());

		// Single selection mode allowed
		_table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		// Sets the table size
		_table.setPreferredScrollableViewportSize(new Dimension(310, 200));

		// Returns the scroll pane with the table on it
		return new JScrollPane(_table);
	}

	/**
	 * Updates the form fields with the data of the selected row in the table.
	 */
	public void updateFieldsWithRowSelectedInTable() {

		// Gets the table selected row
		int selectedRow = _table.getSelectedRow();

		// Gets the table selected column
		int selectedColumn = 0;
		for (int index = 0; index < _table.getColumnCount(); index++) {
			if (_table.getColumnName(index) == AcideLanguageManager
					.getInstance().getLabels().getString("s374")) {
				selectedColumn = index;
			}
		}

		// If there is a selected row in the table
		if (selectedRow != -1) {

			// Gets the selected value in the table
			String value = (String) _table.getValueAt(selectedRow,
					selectedColumn);

			// Gets the token type from the selected row in the table
			AcideLexiconTokenType tokenType = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getLexiconConfiguration().getTokenTypeManager()
					.getTokenAt(value);

			// Sets the value in the reserved word text field
			_reservedWordTextField.setText(value);

			// Sets the value in the preview text field
			_previewTextField.setText(value);

			// Sets the foreground color in the preview text field
			_previewTextField.setForeground(tokenType.getColor());

			// Sets the case sensitive check box state
			_caseSensitiveCheckBox.setState(tokenType.isCaseSensitive());

			switch (tokenType.getFontStyle()) {

			case Font.PLAIN:

				// Sets the plain font style in the preview text field
				_previewTextField.setFont(new Font(_previewTextField.getFont()
						.getFontName(), Font.PLAIN, _previewTextField.getFont()
						.getSize()));

				// Sets the selected index in the font type combo box
				_fontTypeComboBox.setSelectedIndex(0);

				break;
			case Font.ITALIC:

				// Sets the italic font style in the preview text field
				_previewTextField.setFont(new Font(_previewTextField.getFont()
						.getFontName(), Font.ITALIC, _previewTextField
						.getFont().getSize()));

				// Sets the selected index in the font type combo box
				_fontTypeComboBox.setSelectedIndex(1);
				break;
			case Font.BOLD:

				// Sets the bold font style in the preview text field
				_previewTextField.setFont(new Font(_previewTextField.getFont()
						.getFontName(), Font.BOLD, _previewTextField.getFont()
						.getSize()));

				// Sets the selected index in the font type combo box
				_fontTypeComboBox.setSelectedIndex(2);
				break;
			case Font.BOLD + Font.ITALIC:

				// Sets the bold + italic font style in the preview text field
				_previewTextField.setFont(new Font(_previewTextField.getFont()
						.getFontName(), Font.BOLD + Font.ITALIC,
						_previewTextField.getFont().getSize()));

				// Sets the selected index in the font type combo box
				_fontTypeComboBox.setSelectedIndex(3);
				break;
			}
		}
	}

	/**
	 * Calculates the table total size.
	 * 
	 * @return the table total size.
	 */
	public int calculateTableTotalSize() {
		int totalSize = 0;
		for (int index = 0; index < AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel()
				.getLexiconConfiguration().getTokenTypeManager().getSize(); index++) {
			totalSize = totalSize
					+ AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getLexiconConfiguration().getTokenTypeManager()
							.getTokenType(index).getTokenListSize();
		}
		return totalSize;
	}

	/**
	 * Creates the table data.
	 */
	public void createTableData() {

		// Gets the token type manager from the lexicon configuration
		AcideLexiconTokenTypeManager tokenTypeManager = AcideMainWindow
				.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getLexiconConfiguration()
				.getTokenTypeManager();

		// Calculates the total size
		int totalSize = calculateTableTotalSize();

		// Creates the data
		_tableData = new Object[totalSize][3];
		int aux = 0;

		for (int i = 0; i < tokenTypeManager.getSize(); i++) {

			for (int j = 0; j < tokenTypeManager.getTokenType(i)
					.getTokenListSize(); j++) {

				// Sets the name
				_tableData[aux][0] = tokenTypeManager.getTokenType(i).getToken(
						j);
				String color = "";

				int colorAux = tokenTypeManager.getTokenType(i).getColor()
						.getRed();

				if (Integer.toHexString(colorAux).length() == 1) {
					color += "0" + Integer.toHexString(colorAux);
				} else {
					color += Integer.toHexString(colorAux);
				}

				colorAux = tokenTypeManager.getTokenType(i).getColor()
						.getGreen();
				if (Integer.toHexString(colorAux).length() == 1) {
					color += "0" + Integer.toHexString(colorAux);
				} else {
					color += Integer.toHexString(colorAux);
				}
				colorAux = tokenTypeManager.getTokenType(i).getColor()
						.getBlue();
				if (Integer.toHexString(colorAux).length() == 1) {
					color += "0" + Integer.toHexString(colorAux);
				} else {
					color += Integer.toHexString(colorAux);
				}

				String value = "";

				// Gets the is italic flag
				boolean isItalic = false;

				// Gets the is bold flag
				boolean isBold = false;

				switch (tokenTypeManager.getTokenType(i).getFontStyle()) {

				case Font.PLAIN:
					isItalic = false;
					isBold = false;
					break;
				case Font.ITALIC:
					isItalic = true;
					isBold = false;
					break;
				case Font.BOLD:
					isItalic = false;
					isBold = true;
					break;
				case Font.BOLD + Font.ITALIC:
					isItalic = true;
					isBold = true;
					break;
				}

				if (!tokenTypeManager.getTokenType(i).getToken(j)
						.equalsIgnoreCase("<")) {
					if (!isItalic && !isBold)
						value = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER>"
								+ tokenTypeManager.getTokenType(i).getToken(j)
								+ "</CENTER></div></body></html>";
					else if (isItalic && !isBold)
						value = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><I>"
								+ tokenTypeManager.getTokenType(i).getToken(j)
								+ "</I></CENTER></div></body></html>";
					else if (!isItalic && isBold)
						value = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><B>"
								+ tokenTypeManager.getTokenType(i).getToken(j)
								+ "</B></CENTER></div></body></html>";
					else if (isItalic && isBold)
						value = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><B><I>"
								+ tokenTypeManager.getTokenType(i).getToken(j)
								+ "</I></B></CENTER></div></body></html>";
				} else {
					if (!isItalic && !isBold)
						value = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER>" + "&lt;"
								+ "</CENTER></div></body></html>";
					else if (isItalic && !isBold)
						value = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><I>" + "&lt;"
								+ "</I></CENTER></div></body></html>";
					else if (!isItalic && isBold)
						value = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><B>" + "&lt;"
								+ "</B></CENTER></div></body></html>";
					else if (isItalic && isBold)
						value = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><B><I>" + "&lt;"
								+ "</I></B></CENTER></div></body></html>";
				}

				// Sets the preview
				_tableData[aux][1] = value;

				// Sets the color
				_tableData[aux][2] = tokenTypeManager.getTokenType(i)
						.getColor();
				aux++;
			}
		}
	}

	/**
	 * Updates the table model and the table sorter with the new data.
	 */
	private void updatesTableModelAndSorter() {

		// Updates the model
		_tableModel.setValues(_tableColumns, _tableData);

		// Notifies to the table sorter about the changes in the model
		_tableSorter.setModel(_tableModel);

		// Gets the column from the table sorter
		int column = _tableSorter.getColumn();

		// If it is valid
		if (column != -1)

			// Sort the table by the column
			_tableSorter.sortByColumn(column);

		// Updates the changes in the table
		_tableSorter.fireTableDataChanged();
	}

	/**
	 * Returns the reserved word label.
	 * 
	 * @return the reserved word label.
	 */
	public JLabel getReservedWordLabel() {
		return _reservedWordLabel;
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
	 * ACIDE - A Configurable IDE reserved words panel add button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AddButtonAction implements ActionListener {
		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// If the reserved word is not empty
			if (!_reservedWordTextField.getText().matches("")) {

				// Creates the new token type
				AcideLexiconTokenType newTokenType = new AcideLexiconTokenType();
				newTokenType.setColor(_previewTextField.getForeground());
				newTokenType.setFontStyle(_previewTextField.getFont()
						.getStyle());
				newTokenType.setToken(_reservedWordTextField.getText());
				newTokenType.setName();
				newTokenType
						.setCaseSensitive(_caseSensitiveCheckBox.getState());

				// Adds the new token type to the list
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getLexiconConfiguration()
						.getTokenTypeManager()
						.insertTokenType(newTokenType,
								_reservedWordTextField.getText());

				// Creates the table data
				createTableData();

				// Updates the table model and sorter with the data
				updatesTableModelAndSorter();

				// Selects the new reserved word in the table
				_table.setRowSelectionInterval(_table.getRowCount() - 1,
						_table.getRowCount() - 1);

				// Updates the form fields with the selected row data
				updateFieldsWithRowSelectedInTable();

				// Scrolls automatically to see the new row, without doing
				// it
				// automatically
				_table.scrollRectToVisible(_table.getCellRect(
						_table.getRowCount() - 1, 0, true));

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s417")
								+ _reservedWordTextField.getText());
				
				// There are changes in the panel
				_areThereChanges = true;
				
			} else
				// Displays an error message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s1002"), "Error",
						JOptionPane.ERROR_MESSAGE);
		}
	}

	/**
	 * ACIDE - A Configurable IDE reserved words panel quit button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class QuitButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Gets the selected row in the table
			int selectedRow = _table.getSelectedRow();

			// Gets the last row index
			int lastRow = _table.getRowCount() - 1;

			// Gets the selected column in the table
			int selectedColumn = 0;
			for (int index = 0; index < _table.getColumnCount(); index++) {
				if (_table.getColumnName(index) == AcideLanguageManager
						.getInstance().getLabels().getString("s374")) {
					selectedColumn = index;
				}
			}

			// If there is a row selected
			if (selectedRow != -1) {

				// Gets the value from the selected row in the table
				String value = (String) _table.getValueAt(selectedRow,
						selectedColumn);

				// Removes the token from the token type list
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getLexiconConfiguration()
						.getTokenTypeManager().removeTokenAs(value);

				// Creates the table data
				createTableData();

				// Updates the table model and sorter with the data
				updatesTableModelAndSorter();

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

				// Updates the form fields with the selected row data
				updateFieldsWithRowSelectedInTable();

				// There are changes in the panel
				_areThereChanges = true;
				
				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s419")
								+ value);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE reserved words panel apply for all delimiters
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ApplyForAllDelimitersButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			for (int index = 0; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getLexiconConfiguration().getDelimitersManager().getSize(); index++) {

				// Gets the delimiter value
				String value = AcideMainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.getLexiconConfiguration().getDelimitersManager()
						.getDelimiterAt(index);

				// Removes the token from the list
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getLexiconConfiguration()
						.getTokenTypeManager().removeTokenAs(value);

				// Creates the new token type
				AcideLexiconTokenType newTokenType = new AcideLexiconTokenType();
				newTokenType.setColor(_previewTextField.getForeground());
				newTokenType.setFontStyle(_previewTextField.getFont()
						.getStyle());
				newTokenType.setToken(value);
				newTokenType.setName();
				newTokenType
						.setCaseSensitive(_caseSensitiveCheckBox.getState());

				// Inserts the new token type in the list
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getLexiconConfiguration()
						.getTokenTypeManager()
						.insertTokenType(newTokenType, value);
			}

			// Creates the table data
			createTableData();

			// Updates the table model and sorter with the data
			updatesTableModelAndSorter();

			// Clears the reserved word text field
			_reservedWordTextField.setText("");

			// Clears the preview text field
			_previewTextField.setText("");
			
			// There are changes in the panel
			_areThereChanges = true;
		}
	}

	/**
	 * ACIDE - A Configurable IDE reserved words panel modify button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ModifyButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Gets the selected row in the table
			int selectedRow = _table.getSelectedRow();

			// Gets the selected column in the table
			int selectedColumn = 0;
			for (int index = 0; index < _table.getColumnCount(); index++) {
				if (_table.getColumnName(index) == AcideLanguageManager
						.getInstance().getLabels().getString("s374")) {
					selectedColumn = index;
				}
			}

			// If there is a selected row in the table
			if (selectedRow != -1) {

				// Gets the value from the table
				String value = (String) _table.getValueAt(selectedRow,
						selectedColumn);

				// Removes the token from the token type list
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getLexiconConfiguration()
						.getTokenTypeManager().removeTokenAs(value);

				// Creates the new token to replace the old one
				AcideLexiconTokenType tokenType = new AcideLexiconTokenType();
				tokenType.setColor(_previewTextField.getForeground());
				tokenType.setFontStyle(_previewTextField.getFont().getStyle());
				tokenType.setCaseSensitive(_caseSensitiveCheckBox.getState());
				tokenType.setToken(value);
				tokenType.setName();

				// Replaces the old one
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getLexiconConfiguration()
						.getTokenTypeManager()
						.insertTokenType(tokenType,
								_reservedWordTextField.getText());

				// Creates the table data
				createTableData();
				
				// Updates the table model and sorter with the data
				updatesTableModelAndSorter();
				
				// Selects the new reserved word in the table
				_table.setRowSelectionInterval(_table.getRowCount() - 1,
					_table.getRowCount() - 1);

				// Updates the form fields with the selected row data
				updateFieldsWithRowSelectedInTable();

				// Clears the reserved word text field
				_reservedWordTextField.setText("");

				// Clears the preview text field
				_previewTextField.setText("");
				
				// There are changes in the panel
				_areThereChanges = true;
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE reserved words panel font type combo box
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class FontTypeComboBoxAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Gets the font from the reserved word label
			Font font = _reservedWordLabel.getFont();

			// Gets the font type combo box selected item
			String selectedItem = (String) _fontTypeComboBox.getSelectedItem();

			if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s413")))

				// Sets the font in the preview text field
				_previewTextField.setFont(new Font(font.getFontName(),
						Font.PLAIN, font.getSize()));
			else if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s414")))

				// Sets the font in the preview text field
				_previewTextField.setFont(new Font(font.getFontName(),
						Font.ITALIC, font.getSize()));
			else if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s415")))

				// Sets the font in the preview text field
				_previewTextField.setFont(new Font(font.getFontName(),
						Font.BOLD, font.getSize()));
			else if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s416")))

				// Sets the font in the preview text field
				_previewTextField.setFont(new Font(font.getFontName(),
						Font.BOLD + Font.ITALIC, font.getSize()));

			// If the reserved word text field is not empty
			if (_reservedWordTextField.getText() != "")

				// Sets the text in the preview text field
				_previewTextField.setText(_reservedWordTextField.getText());
		}
	}

	/**
	 * ACIDE - A Configurable IDE reserved words panel color palette button
	 * action.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ColorPaletteButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Ask for the color to the user
			Color color = JColorChooser.showDialog(null, AcideLanguageManager
					.getInstance().getLabels().getString("s992"),
					_previewTextField.getForeground());

			// If the user has selected any
			if (color != null) {

				// Updates the preview text color
				_previewTextField.setForeground(color);
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE reserved words panel enter key action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class EnterKeyAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Adds the word to the table
			_addButton.doClick();
		}
	}

	/**
	 * ACIDE - A Configurable IDE reserved words panel reserved word text field
	 * key listener.
	 * 
	 * @version 0.8
	 * @see KeyAdapter
	 */
	class ReservedWordTextFieldListener extends KeyAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyReleased(KeyEvent keyEvent) {

			// Updates the preview text field with the value in the reserved
			// word text field
			_previewTextField.setText(_reservedWordTextField.getText());
		}
	}

	/**
	 * ACIDE - A Configurable IDE reserved words panel table mouse listener.
	 * 
	 * @version 0.8
	 * @see MouseAdapter
	 */
	class TableMouseListener extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseClicked(MouseEvent mouseEvent) {

			// Updates the form fields with the selected row data
			updateFieldsWithRowSelectedInTable();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent )
		 */
		@Override
		public void mousePressed(MouseEvent mouseEvent) {

			// Updates the form fields with the selected row data
			updateFieldsWithRowSelectedInTable();
		}
	}

	/**
	 * ACIDE - A Configurable IDE reserved words panel table key listener.
	 * 
	 * @version 0.8
	 * @see KeyListener
	 */
	class TableKeyListener implements KeyListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyReleased(KeyEvent keyEvent) {

			// Updates the form fields with the selected row data
			updateFieldsWithRowSelectedInTable();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyTyped(KeyEvent keyEvent) {

			// Updates the form fields with the selected row data
			updateFieldsWithRowSelectedInTable();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyPressed(KeyEvent keyEvent) {

			// Updates the form fields with the selected row data
			updateFieldsWithRowSelectedInTable();
		}
	}
}
