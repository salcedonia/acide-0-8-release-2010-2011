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
package gui.menuBar.configurationMenu.lexiconMenu.gui.panels;

import gui.menuBar.configurationMenu.lexiconMenu.gui.utils.CellColorEditor;
import gui.menuBar.configurationMenu.lexiconMenu.gui.utils.ColorCellRenderer;
import gui.menuBar.configurationMenu.lexiconMenu.utils.LexiconTableModel;
import gui.menuBar.configurationMenu.lexiconMenu.utils.LexiconTableSorter;

import java.awt.Checkbox;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ResourceBundle;

import javax.swing.AbstractButton;
import javax.swing.BorderFactory;
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
import javax.swing.border.TitledBorder;

import operations.lexicon.DelimiterList;
import operations.lexicon.TokenType;
import operations.lexicon.TokenTypeList;
import operations.log.AcideLog;
import resources.AcideResourceManager;

import language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE reserved words panel.
 * 
 * @version 0.8
 */
public class AcideReservedWordsPanel extends JPanel{

	/**
	 * ACIDE - A Configurable IDE reserved words panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE reserved words panel color palette button image icon.
	 */
	private final static ImageIcon COLOR_PALETTE_IMAGE = new ImageIcon(
			"./resources/icons/buttons/colorPalette.png");
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window case sensitive check box.
	 */
	private Checkbox _caseSensitiveCheckBox;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window color palette button.
	 */
	private JButton _colorPaletteButton;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window font type combo box.
	 */
	private JComboBox _fontTypeComboBox;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window case sensitive label.
	 */
	private final JLabel _caseSensitiveLabel;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window word label.
	 */
	private final JLabel _reservedWordLabel;
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
	 * ACIDE - A Configurable IDE lexicon configuration window preview text field.
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
	 * ACIDE - A Configurable IDE lexicon configuration window set delimiters button.
	 */
	private JButton _setDelimitersButton;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window quit button.
	 */
	private JButton _quitButton;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window add button.
	 */
	private JButton _addButton;
	/**
	 * Word list table.
	 */
	private JTable _reservedWordTable;
	/**
	 * Reserved word table columns.
	 */
	private String[] _reservedWordTableColumns;
	/**
	 * Sorter table for the word list table.
	 */
	private LexiconTableSorter _reservedWordTableSorter;
	
	/**
	 * Creates a new ACIDE - A Configurable IDE reserved words panel.
	 */
	public AcideReservedWordsPanel(){
		
		super();
		
		// Gets the labels
		final ResourceBundle _labels = AcideLanguageManager.getInstance().getLabels();
		
		// Builds the word list columns
		_reservedWordTableColumns = new String[3];
		_reservedWordTableColumns[0] = _labels.getString("s374");
		_reservedWordTableColumns[1] = _labels.getString("s443");
		_reservedWordTableColumns[2] = _labels.getString("s375");
		
		// Sets the layout
		setLayout(new GridBagLayout());
		
		// Sets a border
		setBorder(BorderFactory.createTitledBorder(null,
				_labels.getString("s428"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		
		// CASE SENSITIVE
		_caseSensitiveLabel = new JLabel(_labels.getString("s431"),
				JLabel.CENTER);
		_caseSensitiveCheckBox = new Checkbox();

		// WORD
		_reservedWordLabel = new JLabel(_labels.getString("s389"),
				JLabel.CENTER);
		_reservedWordTextField = new JTextField();
		_reservedWordTextField.setToolTipText(_labels.getString("s390"));

		// COLOR
		_colorLabel = new JLabel(_labels.getString("s391"), JLabel.CENTER);

		// PREVIEW
		_previewLabel = new JLabel(_labels.getString("s392"), JLabel.CENTER);
		_previewTextField = new JTextField();
		_previewTextField.setToolTipText(_labels.getString("s393"));
		_previewTextField.setEditable(false);
		_previewTextField.setHorizontalAlignment(JTextField.CENTER);
		_previewTextField.setFont(new Font(_reservedWordLabel.getFont()
				.getFontName(), Font.PLAIN, _reservedWordLabel.getFont()
				.getSize()));
		_previewTextField.setForeground(Color.BLACK);
		_previewTextField.setText(_labels.getString("s394"));

		// TYPE FONT
		_fontTypeLabel = new JLabel(_labels.getString("s395"), JLabel.CENTER);
		_fontTypeComboBox = new JComboBox();
		_fontTypeComboBox.addItem(_labels.getString("s396"));
		_fontTypeComboBox.addItem(_labels.getString("s397"));
		_fontTypeComboBox.addItem(_labels.getString("s398"));
		_fontTypeComboBox.addItem(_labels.getString("s399"));
		_fontTypeComboBox.setEnabled(true);
		_fontTypeComboBox.setToolTipText(_labels.getString("s400"));
		
		// Listeners
		_reservedWordTextField.addKeyListener(new KeyListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyTyped(KeyEvent keyEvent) {
				// previewTextField.setText(palabraField.getText());
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyPressed(KeyEvent keyEvent) {
				// previewTextField.setText(palabraField.getText());
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent keyEvent) {
				_previewTextField.setText(_reservedWordTextField.getText());
			}

		});

		// Adds the enter key listener to the reserved word text
		ActionListener actionListener = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				_addButton.doClick();
			}
		};
		KeyStroke keystroke = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0,
				false);
		_reservedWordTextField.registerKeyboardAction(actionListener,
				keystroke, JComponent.WHEN_FOCUSED);
		
		_fontTypeComboBox.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				Font font = _reservedWordLabel.getFont();
				String selectedItem = (String) _fontTypeComboBox
						.getSelectedItem();

				if (selectedItem.equals(_labels.getString("s413")))
					_previewTextField.setFont(new Font(font.getFontName(),
							Font.PLAIN, font.getSize()));
				else if (selectedItem.equals(_labels.getString("s414")))
					_previewTextField.setFont(new Font(font.getFontName(),
							Font.ITALIC, font.getSize()));
				else if (selectedItem.equals(_labels.getString("s415")))
					_previewTextField.setFont(new Font(font.getFontName(),
							Font.BOLD, font.getSize()));
				else if (selectedItem.equals(_labels.getString("s416")))
					_previewTextField.setFont(new Font(font.getFontName(),
							Font.BOLD + Font.ITALIC, font.getSize()));
				_previewTextField.setText(_reservedWordTextField.getText());
			}
		});

		// COLOR PALETTE BUTTON
		_colorPaletteButton = new JButton(COLOR_PALETTE_IMAGE);
		_colorPaletteButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Ask for the color to the user
				Color color = JColorChooser.showDialog(null,
						_labels.getString("s992"), _previewTextField.getForeground());

				// If the user has selected any
				if (color != null) {
					
					// Updates the preview text color
					_previewTextField.setForeground(color);
				}
			}
		});
		
		// ADD BUTTON
		_addButton = new JButton(_labels.getString("s385"));
		_addButton.setVerticalTextPosition(AbstractButton.CENTER);
		_addButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_addButton.setMnemonic(KeyEvent.VK_A);
		_addButton.setToolTipText(_labels.getString("s386"));
		_addButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Gets the language
				AcideLanguageManager language = AcideLanguageManager.getInstance();

				try {
					language.getLanguage(AcideResourceManager.getInstance().getProperty(
							"language"));
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();
				
				// If the reserved word is not empty
				if (!_reservedWordTextField.getText().matches("")) {
					
					TokenType tokenType = new TokenType();
					tokenType.setColor(_previewTextField.getForeground());
					tokenType.setItalic(_previewTextField.getFont().isItalic());
					tokenType.setBold(_previewTextField.getFont().isBold());
					tokenType.setToken(_reservedWordTextField.getText());
					tokenType.setName();
					tokenType.setCaseSensitive(_caseSensitiveCheckBox
							.getState());

					TokenTypeList tokenTypeList = TokenTypeList.getInstance();
					tokenTypeList.insertTokenType(tokenType,
							_reservedWordTextField.getText());

					int num = 0;
					for (int i = 0; i < tokenTypeList.getSize(); i++) {
						num = num
								+ tokenTypeList.getTokenType(i)
										.getTokenListSize();
					}

					Object[][] data = new Object[num][3];
					int aux = 0;

					for (int i = 0; i < tokenTypeList.getSize(); i++) {

						for (int j = 0; j < tokenTypeList.getTokenType(i)
								.getTokenListSize(); j++) {

							data[aux][0] = tokenTypeList.getTokenType(i)
									.getToken(j);
							String color = "";

							int colorAux = tokenTypeList.getTokenType(i)
									.getColor().getRed();

							if (Integer.toHexString(colorAux).length() == 1) {
								color += "0" + Integer.toHexString(colorAux);
							} else {
								color += Integer.toHexString(colorAux);
							}

							colorAux = tokenTypeList.getTokenType(i).getColor()
									.getGreen();
							if (Integer.toHexString(colorAux).length() == 1) {
								color += "0" + Integer.toHexString(colorAux);
							} else {
								color += Integer.toHexString(colorAux);
							}
							colorAux = tokenTypeList.getTokenType(i).getColor()
									.getBlue();
							if (Integer.toHexString(colorAux).length() == 1) {
								color += "0" + Integer.toHexString(colorAux);
							} else {
								color += Integer.toHexString(colorAux);
							}

							String s = "";
							boolean isItalic = tokenTypeList.getTokenType(i)
									.isItalic();
							boolean isBold = tokenTypeList.getTokenType(i)
									.isBold();

							if (!tokenTypeList.getTokenType(i).getToken(j)
									.equalsIgnoreCase("<")) {
								if (!isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</CENTER></div></body></html>";
								else if (isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><I>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</I></CENTER></div></body></html>";
								else if (!isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</B></CENTER></div></body></html>";
								else if (isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B><I>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</I></B></CENTER></div></body></html>";
							} else {
								if (!isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER>"
											+ "&lt;"
											+ "</CENTER></div></body></html>";
								else if (isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><I>"
											+ "&lt;"
											+ "</I></CENTER></div></body></html>";
								else if (!isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B>"
											+ "&lt;"
											+ "</B></CENTER></div></body></html>";
								else if (isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B><I>"
											+ "&lt;"
											+ "</I></B></CENTER></div></body></html>";
							}
							data[aux][1] = s;
							data[aux][2] = tokenTypeList.getTokenType(i)
									.getColor();
							aux++;
						}
					}

					LexiconTableModel myModel = new LexiconTableModel();
					myModel.setValues(_reservedWordTableColumns, data);
					_reservedWordTableSorter.setModel(myModel);
					int c = _reservedWordTableSorter.getColumn();

					if (c != -1)
						_reservedWordTableSorter.sortByColumn(c);

					_reservedWordTableSorter.fireTableDataChanged();
					_reservedWordTextField.setText("");
					_previewTextField.setText("");

					// Updates the log
					AcideLog.getLog().info(
							_labels.getString("s417")
									+ _reservedWordTextField.getText());
				}
				else
					// Error message
					JOptionPane.showMessageDialog(null, labels.getString("s1002"),"Error", JOptionPane.ERROR_MESSAGE);
			}
		});

		// QUIT BUTTON
		_quitButton = new JButton(_labels.getString("s387"));
		_quitButton.setVerticalTextPosition(AbstractButton.CENTER);
		_quitButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_quitButton.setMnemonic(KeyEvent.VK_Q);
		_quitButton.setToolTipText(_labels.getString("s388"));
		_quitButton.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				int row = _reservedWordTable.getSelectedRow();
				int col = 0;
				for (int i = 0; i < _reservedWordTable.getColumnCount(); i++) {
					if (_reservedWordTable.getColumnName(i) == _labels
							.getString("s374")) {
						col = i;
					}
				}
				if (row != -1) {
					String value = (String) _reservedWordTable.getValueAt(row, col);
					TokenTypeList tokenTypeList = TokenTypeList.getInstance();
					tokenTypeList.removeTokenAs(value);
					int num = 0;
					for (int i = 0; i < tokenTypeList.getSize(); i++) {
						num = num
								+ tokenTypeList.getTokenType(i)
										.getTokenListSize();
					}

					Object[][] data = new Object[num][3];
					int aux = 0;
					for (int i = 0; i < tokenTypeList.getSize(); i++) {
						for (int j = 0; j < tokenTypeList.getTokenType(i)
								.getTokenListSize(); j++) {

							data[aux][0] = tokenTypeList.getTokenType(i)
									.getToken(j);
							String color = "";

							int colorAux = tokenTypeList.getTokenType(i)
									.getColor().getRed();

							if (Integer.toHexString(colorAux).length() == 1) {
								color += "0" + Integer.toHexString(colorAux);
							} else {
								color += Integer.toHexString(colorAux);
							}

							colorAux = tokenTypeList.getTokenType(i).getColor()
									.getGreen();
							if (Integer.toHexString(colorAux).length() == 1) {
								color += "0" + Integer.toHexString(colorAux);
							} else {
								color += Integer.toHexString(colorAux);
							}
							colorAux = tokenTypeList.getTokenType(i).getColor()
									.getBlue();
							if (Integer.toHexString(colorAux).length() == 1) {
								color += "0" + Integer.toHexString(colorAux);
							} else {
								color += Integer.toHexString(colorAux);
							}

							String s = "";

							boolean isItalic = tokenTypeList.getTokenType(i)
									.isItalic();
							boolean isBold = tokenTypeList.getTokenType(i)
									.isBold();

							if (!tokenTypeList.getTokenType(i).getToken(j)
									.equalsIgnoreCase("<")) {
								if (!isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</CENTER></div></body></html>";
								else if (isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><I>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</I></CENTER></div></body></html>";
								else if (!isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</B></CENTER></div></body></html>";
								else if (isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B><I>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</I></B></CENTER></div></body></html>";
							} else {
								if (!isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER>"
											+ "&lt;"
											+ "</CENTER></div></body></html>";
								else if (isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><I>"
											+ "&lt;"
											+ "</I></CENTER></div></body></html>";
								else if (!isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B>"
											+ "&lt;"
											+ "</B></CENTER></div></body></html>";
								else if (isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B><I>"
											+ "&lt;"
											+ "</I></B></CENTER></div></body></html>";
							}
							data[aux][1] = s;
							data[aux][2] = tokenTypeList.getTokenType(i)
									.getColor();
							aux++;
						}
					}

					LexiconTableModel myModel = new LexiconTableModel();
					myModel.setValues(_reservedWordTableColumns, data);
					_reservedWordTableSorter.setModel(myModel);

					int column = _reservedWordTableSorter.getColumn();
					if (column != -1)
						_reservedWordTableSorter.sortByColumn(column);
					_reservedWordTableSorter.fireTableDataChanged();
					_reservedWordTextField.setText("");
					_previewTextField.setText("");

					// Updates the log
					AcideLog.getLog().info(_labels.getString("s419") + value);
				}
			}
		});

		// MODIFY BUTTON
		_modifyButton = new JButton(_labels.getString("s436"));
		_modifyButton.setVerticalTextPosition(AbstractButton.CENTER);
		_modifyButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_modifyButton.setMnemonic(KeyEvent.VK_M);
		_modifyButton.setToolTipText(_labels.getString("s437"));
		_modifyButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				int row = _reservedWordTable.getSelectedRow();
				int col = 0;
				for (int i = 0; i < _reservedWordTable.getColumnCount(); i++) {
					if (_reservedWordTable.getColumnName(i) == _labels
							.getString("s374")) {
						col = i;
					}
				}

				if (row != -1) {
					String val = (String) _reservedWordTable.getValueAt(row, col);
					TokenTypeList tokenTypeList = TokenTypeList.getInstance();
					tokenTypeList.removeTokenAs(val);
					TokenType tokenType = new TokenType();
					tokenType.setColor(_previewTextField.getForeground());
					tokenType.setItalic(_previewTextField.getFont().isItalic());
					tokenType.setBold(_previewTextField.getFont().isBold());
					tokenType.setCaseSensitive(_caseSensitiveCheckBox
							.getState());
					tokenType.setToken(val);
					tokenType.setName();
					tokenTypeList.insertTokenType(tokenType,
							_reservedWordTextField.getText());

					int num = 0;
					for (int i = 0; i < tokenTypeList.getSize(); i++) {
						num = num
								+ tokenTypeList.getTokenType(i)
										.getTokenListSize();
					}

					Object[][] data = new Object[num][3];
					int aux = 0;
					for (int i = 0; i < tokenTypeList.getSize(); i++) {
						for (int j = 0; j < tokenTypeList.getTokenType(i)
								.getTokenListSize(); j++) {

							data[aux][0] = tokenTypeList.getTokenType(i)
									.getToken(j);

							String color = "";

							int colorAux = tokenTypeList.getTokenType(i)
									.getColor().getRed();

							if (Integer.toHexString(colorAux).length() == 1) {
								color += "0" + Integer.toHexString(colorAux);
							} else {
								color += Integer.toHexString(colorAux);
							}

							colorAux = tokenTypeList.getTokenType(i).getColor()
									.getGreen();
							if (Integer.toHexString(colorAux).length() == 1) {
								color += "0" + Integer.toHexString(colorAux);
							} else {
								color += Integer.toHexString(colorAux);
							}
							colorAux = tokenTypeList.getTokenType(i).getColor()
									.getBlue();
							if (Integer.toHexString(colorAux).length() == 1) {
								color += "0" + Integer.toHexString(colorAux);
							} else {
								color += Integer.toHexString(colorAux);
							}
							String s = "";

							boolean isItalic = tokenTypeList.getTokenType(i)
									.isItalic();
							boolean isBold = tokenTypeList.getTokenType(i)
									.isBold();

							if (!tokenTypeList.getTokenType(i).getToken(j)
									.equalsIgnoreCase("<")) {
								if (!isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</CENTER></div></body></html>";
								else if (isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><I>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</I></CENTER></div></body></html>";
								else if (!isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</B></CENTER></div></body></html>";
								else if (isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B><I>"
											+ tokenTypeList.getTokenType(i)
													.getToken(j)
											+ "</I></B></CENTER></div></body></html>";
							} else {
								if (!isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER>"
											+ "&lt;"
											+ "</CENTER></div></body></html>";
								else if (isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><I>"
											+ "&lt;"
											+ "</I></CENTER></div></body></html>";
								else if (!isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B>"
											+ "&lt;"
											+ "</B></CENTER></div></body></html>";
								else if (isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B><I>"
											+ "&lt;"
											+ "</I></B></CENTER></div></body></html>";
							}
							data[aux][1] = s;

							data[aux][2] = tokenTypeList.getTokenType(i)
									.getColor();
							aux++;
						}
					}
					LexiconTableModel myModel = new LexiconTableModel();
					myModel.setValues(_reservedWordTableColumns, data);
					_reservedWordTableSorter.setModel(myModel);
					int column = _reservedWordTableSorter.getColumn();
					if (column != -1)
						_reservedWordTableSorter.sortByColumn(column);
					_reservedWordTableSorter.fireTableDataChanged();
					_reservedWordTextField.setText("");
					_previewTextField.setText("");
				}
			}
		});

		// SET DELIMITERS BUTTON
		_setDelimitersButton = new JButton(_labels.getString("s438"));
		_setDelimitersButton.setVerticalTextPosition(AbstractButton.CENTER);
		_setDelimitersButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_setDelimitersButton.setMnemonic(KeyEvent.VK_S);
		_setDelimitersButton.setToolTipText(_labels.getString("s439"));
		_setDelimitersButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				TokenTypeList tokenTypeList = TokenTypeList.getInstance();
				DelimiterList dividerList = DelimiterList.getInstance();
				for (int i = 0; i < dividerList.getSize(); i++) {
					String val = dividerList.getDelimiterAt(i);
					tokenTypeList.removeTokenAs(val);
				}
				for (int i = 0; i < dividerList.getSize(); i++) {
					String value = dividerList.getDelimiterAt(i);
					TokenType tokenType = new TokenType();
					tokenType.setColor(_previewTextField.getForeground());
					tokenType.setItalic(_previewTextField.getFont().isItalic());
					tokenType.setBold(_previewTextField.getFont().isBold());
					tokenType.setToken(value);
					tokenType.setName();
					tokenType.setCaseSensitive(_caseSensitiveCheckBox
							.getState());
					tokenTypeList.insertTokenType(tokenType, value);
				}
				int num = 0;
				for (int i = 0; i < tokenTypeList.getSize(); i++) {
					num = num
							+ tokenTypeList.getTokenType(i).getTokenListSize();
				}

				Object[][] data = new Object[num][3];
				int aux = 0;
				for (int i = 0; i < tokenTypeList.getSize(); i++) {
					for (int j = 0; j < tokenTypeList.getTokenType(i)
							.getTokenListSize(); j++) {
						data[aux][0] = tokenTypeList.getTokenType(i)
								.getToken(j);
						String color = "";
						int auxC = tokenTypeList.getTokenType(i).getColor()
								.getRed();
						if (Integer.toHexString(auxC).length() == 1) {
							color += "0" + Integer.toHexString(auxC);
						} else {
							color += Integer.toHexString(auxC);
						}

						auxC = tokenTypeList.getTokenType(i).getColor()
								.getGreen();
						if (Integer.toHexString(auxC).length() == 1) {
							color += "0" + Integer.toHexString(auxC);
						} else {
							color += Integer.toHexString(auxC);
						}
						auxC = tokenTypeList.getTokenType(i).getColor()
								.getBlue();
						if (Integer.toHexString(auxC).length() == 1) {
							color += "0" + Integer.toHexString(auxC);
						} else {
							color += Integer.toHexString(auxC);
						}

						String s = "";

						boolean isItalic = tokenTypeList.getTokenType(i)
								.isItalic();
						boolean isBold = tokenTypeList.getTokenType(i).isBold();

						if (!tokenTypeList.getTokenType(i).getToken(j)
								.equalsIgnoreCase("<")) {
							if (!isItalic && !isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER>"
										+ tokenTypeList.getTokenType(i)
												.getToken(j)
										+ "</CENTER></div></body></html>";
							else if (isItalic && !isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><I>"
										+ tokenTypeList.getTokenType(i)
												.getToken(j)
										+ "</I></CENTER></div></body></html>";
							else if (!isItalic && isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><B>"
										+ tokenTypeList.getTokenType(i)
												.getToken(j)
										+ "</B></CENTER></div></body></html>";
							else if (isItalic && isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><B><I>"
										+ tokenTypeList.getTokenType(i)
												.getToken(j)
										+ "</I></B></CENTER></div></body></html>";
						} else {
							if (!isItalic && !isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER>"
										+ "&lt;"
										+ "</CENTER></div></body></html>";
							else if (isItalic && !isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><I>"
										+ "&lt;"
										+ "</I></CENTER></div></body></html>";
							else if (!isItalic && isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><B>"
										+ "&lt;"
										+ "</B></CENTER></div></body></html>";
							else if (isItalic && isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><B><I>"
										+ "&lt;"
										+ "</I></B></CENTER></div></body></html>";
						}
						data[aux][1] = s;

						data[aux][2] = tokenTypeList.getTokenType(i).getColor();
						aux++;
					}
				}
				LexiconTableModel myModel = new LexiconTableModel();
				myModel.setValues(_reservedWordTableColumns, data);
				_reservedWordTableSorter.setModel(myModel);
				int column = _reservedWordTableSorter.getColumn();
				if (column != -1)
					_reservedWordTableSorter.sortByColumn(column);
				_reservedWordTableSorter.fireTableDataChanged();

				_reservedWordTextField.setText("");
				_previewTextField.setText("");
			}
		});
		
		// Adds the components to the frame with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		// RESERVED WORD
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 0;
		add(_reservedWordLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		add(_reservedWordTextField, constraints);

		// CASE SENSITIVE
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_caseSensitiveLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		add(_caseSensitiveCheckBox, constraints);

		// COLOR
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 2;
		add(_colorLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		add(_colorPaletteButton, constraints);

		// FONT TYPE
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 3;
		add(_fontTypeLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		add(_fontTypeComboBox, constraints);

		// PREVIEW
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 4;
		add(_previewLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		add(_previewTextField, constraints);

		// ADD BUTTON
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 5;
		add(_addButton, constraints);

		// QUIT BUTTON
		constraints.gridx = 1;
		add(_quitButton, constraints);

		// SET DELIMITERS BUTTON
		constraints.gridy = 6;
		constraints.gridx = 0;
		add(_setDelimitersButton, constraints);

		// MODIFY BUTTON
		constraints.gridx = 1;
		add(_modifyButton, constraints);
	}
	
	/**
	 * Returns the word list table
	 * 
	 * @return the word list table
	 */
	public JScrollPane getWordListTable() {

		TokenTypeList tokenTypeList = TokenTypeList.getInstance();

		int num = 0;
		for (int i = 0; i < tokenTypeList.getSize(); i++) {
			num = num + tokenTypeList.getTokenType(i).getTokenListSize();
		}

		Object[][] data = new Object[num][3];
		int aux = 0;
		for (int i = 0; i < tokenTypeList.getSize(); i++) {
			for (int j = 0; j < tokenTypeList.getTokenType(i)
					.getTokenListSize(); j++) {

				data[aux][0] = tokenTypeList.getTokenType(i).getToken(j);
				String color = "";

				int colorAux = tokenTypeList.getTokenType(i).getColor()
						.getRed();

				if (Integer.toHexString(colorAux).length() == 1) {
					color += "0" + Integer.toHexString(colorAux);
				} else {
					color += Integer.toHexString(colorAux);
				}

				colorAux = tokenTypeList.getTokenType(i).getColor().getGreen();
				if (Integer.toHexString(colorAux).length() == 1) {
					color += "0" + Integer.toHexString(colorAux);
				} else {
					color += Integer.toHexString(colorAux);
				}
				colorAux = tokenTypeList.getTokenType(i).getColor().getBlue();
				if (Integer.toHexString(colorAux).length() == 1) {
					color += "0" + Integer.toHexString(colorAux);
				} else {
					color += Integer.toHexString(colorAux);
				}
				String s = "";

				boolean isItalic = tokenTypeList.getTokenType(i).isItalic();
				boolean isBold = tokenTypeList.getTokenType(i).isBold();

				if (!tokenTypeList.getTokenType(i).getToken(j)
						.equalsIgnoreCase("<")) {
					if (!isItalic && !isBold)
						s = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER>"
								+ tokenTypeList.getTokenType(i).getToken(j)
								+ "</CENTER></div></body></html>";
					else if (isItalic && !isBold)
						s = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><I>"
								+ tokenTypeList.getTokenType(i).getToken(j)
								+ "</I></CENTER></div></body></html>";
					else if (!isItalic && isBold)
						s = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><B>"
								+ tokenTypeList.getTokenType(i).getToken(j)
								+ "</B></CENTER></div></body></html>";
					else if (isItalic && isBold)
						s = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><B><I>"
								+ tokenTypeList.getTokenType(i).getToken(j)
								+ "</I></B></CENTER></div></body></html>";
				} else {
					if (!isItalic && !isBold)
						s = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER>" + "&lt;"
								+ "</CENTER></div></body></html>";
					else if (isItalic && !isBold)
						s = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><I>" + "&lt;"
								+ "</I></CENTER></div></body></html>";
					else if (!isItalic && isBold)
						s = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><B>" + "&lt;"
								+ "</B></CENTER></div></body></html>";
					else if (isItalic && isBold)
						s = "<html><head></head><body><div style=\"color:"
								+ color + ";\"><CENTER><B><I>" + "&lt;"
								+ "</I></B></CENTER></div></body></html>";
				}
				data[aux][1] = s;

				data[aux][2] = tokenTypeList.getTokenType(i).getColor();
				aux++;
			}
		}

		LexiconTableModel myModel = new LexiconTableModel();
		myModel.setValues(_reservedWordTableColumns, data);
		_reservedWordTableSorter = new LexiconTableSorter(myModel);
		_reservedWordTable = new JTable(_reservedWordTableSorter);
		_reservedWordTable.addKeyListener(new KeyListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent keyEvent) {
				checkRows();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyTyped(KeyEvent keyEvent) {
				checkRows();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyPressed(KeyEvent keyEvent) {
				checkRows();
			}

		});

		_reservedWordTable.addMouseListener(new MouseListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent
			 * )
			 */
			@Override
			public void mouseClicked(MouseEvent mouseEvent) {
				checkRows();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent
			 * )
			 */
			@Override
			public void mousePressed(MouseEvent mouseEvent) {
				checkRows();
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent
			 * )
			 */
			@Override
			public void mouseReleased(MouseEvent mouseEvent) {
			} // checkRows();}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent
			 * )
			 */
			@Override
			public void mouseEntered(MouseEvent mouseEvent) {
			} // checkRows();}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent
			 * )
			 */
			@Override
			public void mouseExited(MouseEvent mouseEvent) {
			} // checkRows();}
		});

		_reservedWordTableSorter.addTableHeaderMouseListeners(_reservedWordTable);
		_reservedWordTable.setDefaultRenderer(Color.class, new ColorCellRenderer(true));
		_reservedWordTable.setDefaultEditor(Color.class, new CellColorEditor());
		_reservedWordTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		_reservedWordTable.setPreferredScrollableViewportSize(new Dimension(310,
				200));

		JScrollPane scrollPane = new JScrollPane(_reservedWordTable);
		return scrollPane;
	}
	
	/**
	 * Checks the rows for the type of font to display
	 */
	public void checkRows() {

		// Gets the labels
		ResourceBundle labels = AcideLanguageManager.getInstance().getLabels();
		
		int row = _reservedWordTable.getSelectedRow();
		int col = 0;
		for (int i = 0; i < _reservedWordTable.getColumnCount(); i++) {
			if (_reservedWordTable.getColumnName(i) == labels.getString("s374")) {
				col = i;
			}
		}
		
		if (row != -1) {

			String value = (String) _reservedWordTable.getValueAt(row, col);
			TokenTypeList tokenTypeList = TokenTypeList.getInstance();
			TokenType tokenType = tokenTypeList.getTokenAt(value);

			_reservedWordTextField.setText(value);
			_previewTextField.setText(value);
			_previewTextField.setForeground(tokenType.getColor());
			_caseSensitiveCheckBox.setState(tokenType.isCaseSensitive());

			if (!tokenType.isItalic() && !tokenType.isBold()) {
				_previewTextField.setFont(new Font(_previewTextField.getFont()
						.getFontName(), Font.PLAIN, _previewTextField.getFont()
						.getSize()));
				_fontTypeComboBox.setSelectedIndex(0);
			} else if (tokenType.isItalic() && !tokenType.isBold()) {
				_previewTextField.setFont(new Font(_previewTextField.getFont()
						.getFontName(), Font.ITALIC, _previewTextField
						.getFont().getSize()));
				_fontTypeComboBox.setSelectedIndex(1);
			} else if (!tokenType.isItalic() && tokenType.isBold()) {
				_previewTextField.setFont(new Font(_previewTextField.getFont()
						.getFontName(), Font.BOLD, _previewTextField.getFont()
						.getSize()));
				_fontTypeComboBox.setSelectedIndex(2);
			} else if (tokenType.isItalic() && tokenType.isBold()) {
				_previewTextField.setFont(new Font(_previewTextField.getFont()
						.getFontName(), Font.BOLD + Font.ITALIC,
						_previewTextField.getFont().getSize()));
				_fontTypeComboBox.setSelectedIndex(3);
			}
		}
	}

	/**
	 * Returns the reserved word label.
	 * 
	 * @return the reserverd word label.
	 */
	public JLabel getReservedWordLabel() {
		return _reservedWordLabel;
	}
}
