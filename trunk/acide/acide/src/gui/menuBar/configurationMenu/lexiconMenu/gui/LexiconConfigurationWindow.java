package gui.menuBar.configurationMenu.lexiconMenu.gui;

import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.lexiconMenu.utils.LexiconTableModel;
import gui.menuBar.configurationMenu.lexiconMenu.utils.LexiconTableSorter;

import java.awt.BorderLayout;
import java.awt.Checkbox;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
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
import java.awt.event.WindowEvent;
import java.util.ResourceBundle;
import javax.swing.AbstractButton;
import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import language.AcideLanguage;
import operations.lexicon.Comments;
import operations.lexicon.DelimiterList;
import operations.lexicon.TokenType;
import operations.lexicon.TokenTypeList;
import operations.listeners.AcideWindowListener;
import operations.log.AcideLog;
import resources.ResourceManager;

import es.configuration.lexicon.LexiconConfiguration;

/************************************************************************
 * Lexicon configuration window of ACIDE - A Configurable IDE.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
 * 
 ************************************************************************ 
 * @author <ul>
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan José Ortiz Sánchez
 *         </ul>
 *         <ul>
 *         Delfín Rupérez Cañas
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Martín Lázaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo Gómez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 * @see JFrame
 ***********************************************************************/
public class LexiconConfigurationWindow extends JFrame {

	/**
	 * Lexicon configuration window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Lexicon configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * Lexicon configuration window color frame dialog.
	 */
	private JFrame _colorFrame;
	/**
	 * Lexicon configuration window reserved words panel.
	 */
	private JPanel _reservedWordsPanel;
	/**
	 * Lexicon configuration window delimiters panel.
	 */
	private JPanel _delimitersPanel;
	/**
	 * Lexicon configuration window comment panel.
	 */
	private JPanel _commentPanel;
	/**
	 * Lexicon configuration window right panel.
	 */
	private JPanel _rightPanel;
	/**
	 * Lexicon configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Lexicon configuration window case sensitive check box.
	 */
	private Checkbox _caseSensitiveCheckBox;
	/**
	 * Lexicon configuration window color combo box.
	 */
	private JComboBox _colorComboBox;
	/**
	 * Lexicon configuration window font type combo box.
	 */
	private JComboBox _fontTypeComboBox;
	/**
	 * Lexicon configuration window color comment combo box.
	 */
	private JComboBox _colorCommentComboBox;
	/**
	 * Lexicon configuration window case sensitive label.
	 */
	private final JLabel _caseSensitiveLabel;
	/**
	 * Lexicon configuration window word label.
	 */
	private final JLabel _reservedWordLabel;
	/**
	 * Lexicon configuration window color label.
	 */
	private JLabel _colorLabel;
	/**
	 * Lexicon configuration window color comment label.
	 */
	private JLabel _colorCommentLabel;
	/**
	 * Lexicon configuration window preview label.
	 */
	private JLabel _previewLabel;
	/**
	 * Lexicon configuration window preview comment label.
	 */
	private JLabel _previewCommentLabel;
	/**
	 * Lexicon configuration window font type label.
	 */
	private JLabel _fontTypeLabel;
	/**
	 * Lexicon configuration window delimiter label.
	 */
	private final JLabel _delimiterLabel;
	/**
	 * Lexicon configuration window comment label.
	 */
	private final JLabel _commentLabel;
	/**
	 * Lexicon configuration window preview text field.
	 */
	private JTextField _previewTextField;
	/**
	 * Lexicon configuration window preview comment text field.
	 */
	private JTextField _previewCommentTextField;
	/**
	 * Lexicon configuration window word text field.
	 */
	private JTextField _reservedWordTextField;
	/**
	 * Lexicon configuration window delimiter text field.
	 */
	private final JTextField _delimiterTextField;
	/**
	 * Lexicon configuration window comment text field.
	 */
	private final JTextField _commentTextField;
	/**
	 * Lexicon configuration window modify button.
	 */
	private JButton _modifyButton;
	/**
	 * Lexicon configuration window quit button.
	 */
	private JButton _quitButton;
	/**
	 * Lexicon configuration window add button.
	 */
	private JButton _addButton;
	/**
	 * Lexicon configuration window set delimiters button.
	 */
	private JButton _setDelimitersButton;
	/**
	 * Lexicon configuration window add delimiter button.
	 */
	private JButton _addDelimiterButton;
	/**
	 * Lexicon configuration window delete delimiter button.
	 */
	private JButton _deleteDelimiterButton;
	/**
	 * Lexicon configuration window apply button.
	 */
	private JButton _applyButton;
	/**
	 * Lexicon configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Labels to display in the selected language.
	 */
	private ResourceBundle _labels = AcideLanguage.getInstance().getLabels();
	/**
	 * Word list table.
	 */
	private JTable _wordListTable;
	/**
	 * Delimiter list table.
	 */
	private JTable _delimiterListTable;
	/**
	 * Word list columns.
	 */
	private String[] _wordListColumns = { _labels.getString("s374"),
			_labels.getString("s443"), _labels.getString("s375") };
	/**
	 * Delimiter list table columns.
	 */
	private String[] _delimiterListTableColumns = { _labels.getString("s440") };
	/**
	 * Sorter table for the word list table.
	 */
	private LexiconTableSorter _wordListTableSorter;
	/**
	 * Sorter table for the delimiter list table.
	 */
	private LexiconTableSorter _delimiterListTableSorter;

	/**
	 * Creates a new lexicon configuration window.
	 */
	public LexiconConfigurationWindow() {

		// Updates the log
		AcideLog.getLog().info(_labels.getString("s376"));

		final String tempPath = LexiconConfiguration.getInstance().saveTemp(
				LexiconConfiguration.getInstance().getName(), false);

		// FRAME
		addWindowListener(new java.awt.event.WindowAdapter() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent
			 * )
			 */
			@Override
			public void windowClosing(WindowEvent windowEvent) {
				try {
					LexiconConfiguration.getInstance().load(
							ResourceManager.getInstance().getProperty(
									"languagePath"));
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
			}
		});

		// Sets the layout
		setLayout(new GridBagLayout());

		// RESERVED WORDS PANEL
		_reservedWordsPanel = new JPanel(new GridBagLayout());
		_reservedWordsPanel.setBorder(BorderFactory.createTitledBorder(null,
				_labels.getString("s428"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// DIVIDERS PANEL
		_delimitersPanel = new JPanel(new GridBagLayout());
		_delimitersPanel.setBorder(BorderFactory.createTitledBorder(null,
				_labels.getString("s429"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// COMMENT PANEL
		_commentPanel = new JPanel(new GridBagLayout());
		_commentPanel.setBorder(BorderFactory.createTitledBorder(null,
				_labels.getString("s430"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// RIGHT PANEL
		_rightPanel = new JPanel(new GridBagLayout());

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// COLOR COMBOBOX
		_colorComboBox = new JComboBox();
		_colorComboBox.addItem("");
		_colorComboBox.addItem(_labels.getString("s378"));
		_colorComboBox.addItem(_labels.getString("s379"));
		_colorComboBox.addItem(_labels.getString("s380"));
		_colorComboBox.addItem(_labels.getString("s381"));
		_colorComboBox.addItem(_labels.getString("s382"));
		_colorComboBox.addItem(_labels.getString("s383"));
		_colorComboBox.setEnabled(true);
		_colorComboBox.setToolTipText(_labels.getString("s384"));

		// COLOR COMBOBOX COMMENT
		_colorCommentComboBox = new JComboBox();
		_colorCommentComboBox.addItem("");
		_colorCommentComboBox.addItem(_labels.getString("s378"));
		_colorCommentComboBox.addItem(_labels.getString("s379"));
		_colorCommentComboBox.addItem(_labels.getString("s380"));
		_colorCommentComboBox.addItem(_labels.getString("s381"));
		_colorCommentComboBox.addItem(_labels.getString("s382"));
		_colorCommentComboBox.addItem(_labels.getString("s383"));
		_colorCommentComboBox.setEnabled(true);
		_colorCommentComboBox.setToolTipText(_labels.getString("s384"));

		// ADD BUTTON
		_addButton = new JButton(_labels.getString("s385"));
		_addButton.setVerticalTextPosition(AbstractButton.CENTER);
		_addButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_addButton.setMnemonic(KeyEvent.VK_A);
		_addButton.setToolTipText(_labels.getString("s386"));

		// ADD MODIFY
		_modifyButton = new JButton(_labels.getString("s436"));
		_modifyButton.setVerticalTextPosition(AbstractButton.CENTER);
		_modifyButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_modifyButton.setMnemonic(KeyEvent.VK_M);
		_modifyButton.setToolTipText(_labels.getString("s437"));
		// _btnModify.setEnabled(false);

		// SET DELIMITERS BUTTON
		_setDelimitersButton = new JButton(_labels.getString("s438"));
		_setDelimitersButton.setVerticalTextPosition(AbstractButton.CENTER);
		_setDelimitersButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_setDelimitersButton.setMnemonic(KeyEvent.VK_S);
		_setDelimitersButton.setToolTipText(_labels.getString("s439"));

		// QUIT BUTTON
		_quitButton = new JButton(_labels.getString("s387"));
		_quitButton.setVerticalTextPosition(AbstractButton.CENTER);
		_quitButton.setHorizontalTextPosition(AbstractButton.LEADING);
		_quitButton.setMnemonic(KeyEvent.VK_Q);
		_quitButton.setToolTipText(_labels.getString("s388"));
		// _btnQuit.setEnabled(false);

		// ADD DIVIDER BUTTON
		_addDelimiterButton = new JButton(_labels.getString("s385"));
		_addDelimiterButton.setVerticalTextPosition(AbstractButton.CENTER);
		_addDelimiterButton.setHorizontalTextPosition(AbstractButton.LEADING);
		// addDivider.setToolTipText(labels.getString("s386"));

		// DELETE DIVIDER BUTTON
		_deleteDelimiterButton = new JButton(_labels.getString("s387"));
		_deleteDelimiterButton.setVerticalTextPosition(AbstractButton.CENTER);
		_deleteDelimiterButton
				.setHorizontalTextPosition(AbstractButton.LEADING);
		// deleteDivider.setToolTipText(labels.getString("s388"));

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
		_colorCommentLabel = new JLabel(_labels.getString("s391"),
				JLabel.CENTER);

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

		// PREVIEW COMMENT
		_previewCommentLabel = new JLabel(_labels.getString("s392"),
				JLabel.CENTER);
		_previewCommentTextField = new JTextField();
		_previewCommentTextField.setToolTipText(_labels.getString("s393"));
		_previewCommentTextField.setEditable(false);
		_previewCommentTextField.setHorizontalAlignment(JTextField.CENTER);
		_previewCommentTextField.setFont(new Font(_reservedWordLabel.getFont()
				.getFontName(), Font.PLAIN, _reservedWordLabel.getFont()
				.getSize()));
		_previewCommentTextField.setForeground(Color.BLACK);
		_previewCommentTextField.setText(_labels.getString("s394"));
		_previewCommentTextField.setText(Comments.getInstance()
				.getLineComment() + " " + _labels.getString("s444"));
		_previewCommentTextField.setForeground(Comments.getInstance()
				.getLineCommentColor());
		_previewCommentTextField.setFont(new Font(_reservedWordLabel.getFont()
				.getFontName(), Font.ITALIC, _reservedWordLabel.getFont()
				.getSize()));

		// TYPE FONT
		_fontTypeLabel = new JLabel(_labels.getString("s395"), JLabel.CENTER);
		_fontTypeComboBox = new JComboBox();
		_fontTypeComboBox.addItem(_labels.getString("s396"));
		_fontTypeComboBox.addItem(_labels.getString("s397"));
		_fontTypeComboBox.addItem(_labels.getString("s398"));
		_fontTypeComboBox.addItem(_labels.getString("s399"));
		_fontTypeComboBox.setEnabled(true);
		_fontTypeComboBox.setToolTipText(_labels.getString("s400"));

		// DIVIDER
		_delimiterLabel = new JLabel(_labels.getString("s432"), JLabel.CENTER);
		_delimiterTextField = new JTextField();

		// COMMENT
		_commentLabel = new JLabel(_labels.getString("s433"), JLabel.CENTER);
		_commentTextField = new JTextField();
		_commentTextField.setText(Comments.getInstance().getLineComment());

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

		_commentTextField.addKeyListener(new KeyListener() {
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
				_previewCommentTextField.setText(_commentTextField.getText());
			}

		});
		_colorComboBox.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				Color color = Color.BLACK;
				String selectedItem = (String) _colorComboBox.getSelectedItem();

				// PREDEFINED COLORS
				if (selectedItem.equals(_labels.getString("s401")))
					color = Color.BLUE;
				else if (selectedItem.equals(_labels.getString("s402")))
					color = Color.BLUE.darker().darker();
				else if (selectedItem.equals(_labels.getString("s403")))
					color = Color.RED;
				else if (selectedItem.equals(_labels.getString("s404")))
					color = Color.GREEN.darker().darker();
				else if (selectedItem.equals(_labels.getString("s405")))
					color = Color.BLACK;
				else if (selectedItem.equals(_labels.getString("s406"))) {

					// SHOWS THE COLOR PALETTE WINDOW
					buildColorPaletteWindow();
				}

				_previewTextField.setText(_reservedWordTextField.getText());
				_previewTextField.setForeground(color);
			}

		});

		_colorCommentComboBox.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				Color color = Color.BLACK;
				String selectedItem = (String) _colorCommentComboBox
						.getSelectedItem();

				// PREDEFINED COLORS
				if (selectedItem.equals(_labels.getString("s401")))
					color = Color.BLUE;
				else if (selectedItem.equals(_labels.getString("s402")))
					color = Color.BLUE.darker().darker();
				else if (selectedItem.equals(_labels.getString("s403")))
					color = Color.RED;
				else if (selectedItem.equals(_labels.getString("s404")))
					color = Color.GREEN.darker().darker();
				else if (selectedItem.equals(_labels.getString("s405")))
					color = Color.BLACK;
				else if (selectedItem.equals(_labels.getString("s406"))) {

					// SHOWS THE COLOR PALETTE WINDOW
					buildColorPaletteWindow();
				}
				_previewCommentTextField.setText(_commentTextField.getText());
				_previewCommentTextField.setForeground(color);
			}
		});

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
				AcideLanguage language = AcideLanguage.getInstance();

				try {
					language.getLanguage(ResourceManager.getInstance().getProperty(
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
					myModel.setValues(_wordListColumns, data);
					_wordListTableSorter.setModel(myModel);
					int c = _wordListTableSorter.getColumn();

					if (c != -1)
						_wordListTableSorter.sortByColumn(c);

					_wordListTableSorter.fireTableDataChanged();
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

				int row = _wordListTable.getSelectedRow();
				int col = 0;
				for (int i = 0; i < _wordListTable.getColumnCount(); i++) {
					if (_wordListTable.getColumnName(i) == _labels
							.getString("s374")) {
						col = i;
					}
				}
				if (row != -1) {
					String value = (String) _wordListTable.getValueAt(row, col);
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
					myModel.setValues(_wordListColumns, data);
					_wordListTableSorter.setModel(myModel);

					int column = _wordListTableSorter.getColumn();
					if (column != -1)
						_wordListTableSorter.sortByColumn(column);
					_wordListTableSorter.fireTableDataChanged();
					_reservedWordTextField.setText("");
					_previewTextField.setText("");

					// Updates the log
					AcideLog.getLog().info(_labels.getString("s419") + value);
				}
			}
		});

		_addDelimiterButton.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				DelimiterList dividerList = DelimiterList.getInstance();
				dividerList.insertDelimiter(_delimiterTextField.getText());
				int num = dividerList.getSize();

				Object[][] data = new Object[num][1];
				int aux = 0;

				for (int i = 0; i < dividerList.getSize(); i++) {
					data[aux][0] = dividerList.getDelimiterAt(i);
					aux++;
				}

				LexiconTableModel myModel = new LexiconTableModel();
				myModel.setValues(_delimiterListTableColumns, data);
				_delimiterListTableSorter.setModel(myModel);
				int column = _delimiterListTableSorter.getColumn();
				if (column != -1)
					_delimiterListTableSorter.sortByColumn(column);
				_delimiterListTableSorter.fireTableDataChanged();
				_delimiterTextField.setText("");
			}
		});

		_deleteDelimiterButton.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				int row = _delimiterListTable.getSelectedRow();
				int col = 0;
				for (int i = 0; i < _delimiterListTable.getColumnCount(); i++) {
					col = i;
				}
				if (row != -1) {

					String value = (String) _delimiterListTable.getValueAt(row,
							col);
					DelimiterList dl = DelimiterList.getInstance();
					dl.deleteDelimiter(value);
					int num = dl.getSize();

					Object[][] data = new Object[num][1];
					int aux = 0;
					for (int i = 0; i < dl.getSize(); i++) {
						data[aux][0] = dl.getDelimiterAt(i);
						aux++;
					}
					LexiconTableModel myModel = new LexiconTableModel();
					myModel.setValues(_delimiterListTableColumns, data);
					_delimiterListTableSorter.setModel(myModel);
					int column = _delimiterListTableSorter.getColumn();
					if (column != -1)
						_delimiterListTableSorter.sortByColumn(column);
					_delimiterListTableSorter.fireTableDataChanged();
				}
			}
		});

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

				int row = _wordListTable.getSelectedRow();
				int col = 0;
				for (int i = 0; i < _wordListTable.getColumnCount(); i++) {
					if (_wordListTable.getColumnName(i) == _labels
							.getString("s374")) {
						col = i;
					}
				}

				if (row != -1) {
					String val = (String) _wordListTable.getValueAt(row, col);
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
					myModel.setValues(_wordListColumns, data);
					_wordListTableSorter.setModel(myModel);
					int column = _wordListTableSorter.getColumn();
					if (column != -1)
						_wordListTableSorter.sortByColumn(column);
					_wordListTableSorter.fireTableDataChanged();
					_reservedWordTextField.setText("");
					_previewTextField.setText("");
				}
			}
		});

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
				myModel.setValues(_wordListColumns, data);
				_wordListTableSorter.setModel(myModel);
				int column = _wordListTableSorter.getColumn();
				if (column != -1)
					_wordListTableSorter.sortByColumn(column);
				_wordListTableSorter.fireTableDataChanged();

				_reservedWordTextField.setText("");
				_previewTextField.setText("");
			}
		});

		// APPLY BUTTON
		_applyButton = new JButton();
		_applyButton.setText(_labels.getString("s434"));
		_applyButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				Comments.getInstance().setLineComment(
						_commentTextField.getText());
				Comments.getInstance().setLineCommentColor(
						_previewCommentTextField.getForeground());

				int editor = MainWindow.getInstance().getFileEditorManager()
						.getNumFileEditorPanels();

				for (int i = 0; i < editor; i++) {
					MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(i)
							.resetDocument();
				}

				dispose();
				MainWindow
						.getInstance()
						.getStatusBar()
						.getLexiconMessage()
						.setText(
								_labels.getString("s449")
										+ " "
										+ LexiconConfiguration.getInstance()
												.getName());
			}
		});

		// CANCEL BUTTON
		_cancelButton = new JButton();
		_cancelButton.setText(_labels.getString("s435"));
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent actionEvent) {

				try {
					LexiconConfiguration.getInstance().loadTemp(tempPath);
				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					exception.printStackTrace();
				}
				dispose();
			}
		});

		// Adds the escape key listener to the cancel button
		actionListener = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				dispose();
			}
		};

		_cancelButton.registerKeyboardAction(actionListener, "EscapeKey",
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0,
						true), JComponent.WHEN_IN_FOCUSED_WINDOW);

		// ADD THE COMPONENTS TO THE FRAME WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();

		// RESERVED WORD
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_reservedWordsPanel.add(_reservedWordLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		_reservedWordsPanel.add(_reservedWordTextField, constraints);

		// CASE SENSITIVE
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 1;
		_reservedWordsPanel.add(_caseSensitiveLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		_reservedWordsPanel.add(_caseSensitiveCheckBox, constraints);

		// COLOR
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 2;
		_reservedWordsPanel.add(_colorLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		_reservedWordsPanel.add(_colorComboBox, constraints);

		// FONT TYPE
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 3;
		_reservedWordsPanel.add(_fontTypeLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		_reservedWordsPanel.add(_fontTypeComboBox, constraints);

		// PREVIEW
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 4;
		_reservedWordsPanel.add(_previewLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		_reservedWordsPanel.add(_previewTextField, constraints);

		// ADD BUTTON
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 5;
		_reservedWordsPanel.add(_addButton, constraints);

		// QUIT BUTTON
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 1;
		_reservedWordsPanel.add(_quitButton, constraints);

		// SET DELIMITERS BUTTON
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridy = 6;
		constraints.gridx = 0;
		_reservedWordsPanel.add(_setDelimitersButton, constraints);

		// MODIFY BUTTON
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 1;
		_reservedWordsPanel.add(_modifyButton, constraints);
		setListTables();

		// DELIMITER PANEL
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_delimitersPanel.add(_delimiterLabel, constraints);
		constraints.gridx = 1;
		_delimitersPanel.add(_delimiterTextField, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;
		_delimitersPanel.add(_addDelimiterButton, constraints);
		constraints.gridx = 1;
		_delimitersPanel.add(_deleteDelimiterButton, constraints);

		// COMMENT
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		_commentPanel.add(_commentLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 0;
		_commentPanel.add(_commentTextField, constraints);

		// COLOR COMMENT
		constraints.gridx = 0;
		constraints.gridy = 1;
		_commentPanel.add(_colorCommentLabel, constraints);
		constraints.gridx = 1;
		_commentPanel.add(_colorCommentComboBox, constraints);

		// PREVIEW COMMENT
		constraints.gridx = 0;
		constraints.gridy = 2;
		_commentPanel.add(_previewCommentLabel, constraints);
		constraints.gridx = 1;
		_commentPanel.add(_previewCommentTextField, constraints);

		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		add(_reservedWordsPanel, constraints);

		// RIGHT PANEL
		constraints.ipady = 22;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		_rightPanel.add(_delimitersPanel, constraints);
		constraints.ipady = 22;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		_rightPanel.add(_commentPanel, constraints);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.ipady = 0;
		constraints.gridx = 1;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		add(_rightPanel, constraints);

		// APPLY BUTTON
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.gridwidth = 2;
		_buttonPanel.add(_applyButton);
		_buttonPanel.add(_cancelButton);
		add(_buttonPanel, constraints);

		// FRAME
		setTitle(_labels.getString("s377") + " - "
				+ LexiconConfiguration.getInstance().getName());
		setIconImage(ICON.getImage());
		setVisible(true);
		setResizable(true);
		setLocationRelativeTo(null);
		setResizable(false);
		setLocationRelativeTo(null);

		// Updates the log
		AcideLog.getLog().info(_labels.getString("s420"));

		addWindowListener(new AcideWindowListener());
		pack();
		setLocationRelativeTo(null);
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
		myModel.setValues(_wordListColumns, data);
		_wordListTableSorter = new LexiconTableSorter(myModel);
		_wordListTable = new JTable(_wordListTableSorter);
		_wordListTable.addKeyListener(new KeyListener() {
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

		_wordListTable.addMouseListener(new MouseListener() {
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

		_wordListTableSorter.addTableHeaderMouseListeners(_wordListTable);

		_wordListTable.setDefaultRenderer(Color.class, new ColorRenderer(true));
		_wordListTable.setDefaultEditor(Color.class, new ColorEditor());
		_wordListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		_wordListTable.setPreferredScrollableViewportSize(new Dimension(310,
				200));

		JScrollPane scrollPane = new JScrollPane(_wordListTable);
		return scrollPane;
	}

	/**
	 * Returns the delimiter list table
	 * 
	 * @return the delimiter list table
	 */
	public JScrollPane getDelimiterListTable() {

		DelimiterList delimiterList = DelimiterList.getInstance();

		int num = delimiterList.getSize();

		Object[][] data = new Object[num][1];
		int aux = 0;
		for (int i = 0; i < delimiterList.getSize(); i++) {
			data[aux][0] = delimiterList.getDelimiterAt(i);
			aux++;
		}

		LexiconTableModel myModel = new LexiconTableModel();
		myModel.setValues(_delimiterListTableColumns, data);
		_delimiterListTableSorter = new LexiconTableSorter(myModel);
		_delimiterListTable = new JTable(_delimiterListTableSorter);
		_delimiterListTableSorter
				.addTableHeaderMouseListeners(_delimiterListTable);

		_delimiterListTable.setDefaultRenderer(Color.class, new ColorRenderer(
				true));
		_delimiterListTable.setDefaultEditor(Color.class, new ColorEditor());
		_delimiterListTable.setPreferredScrollableViewportSize(new Dimension(
				280, 200));
		JScrollPane scrollPane = new JScrollPane(_delimiterListTable);
		return scrollPane;
	}

	/**
	 * Sets the list tables into the window with the layout
	 */
	public void setListTables() {

		GridBagConstraints constraints = new GridBagConstraints();

		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 7;
		constraints.gridwidth = 2;
		_reservedWordsPanel.add(getWordListTable(), constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 0;
		constraints.gridy = 2;
		_delimitersPanel.add(getDelimiterListTable(), constraints);
	}

	/**
	 * Checks the rows for the type of font to display
	 */
	public void checkRows() {

		int row = _wordListTable.getSelectedRow();
		int col = 0;
		for (int i = 0; i < _wordListTable.getColumnCount(); i++) {
			if (_wordListTable.getColumnName(i) == _labels.getString("s374")) {
				col = i;
			}
		}
		if (row != -1) {

			String value = (String) _wordListTable.getValueAt(row, col);
			TokenTypeList tokenTypeList = TokenTypeList.getInstance();
			TokenType tokenType = tokenTypeList.getTokenAt(value);

			_colorComboBox.setSelectedIndex(0);
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
	 * Builds the color palette window
	 */
	public void buildColorPaletteWindow() {

		// Updates the log
		AcideLog.getLog().info(_labels.getString("s407"));

		// FRAME
		_colorFrame = new JFrame();
		_colorFrame.setTitle(_labels.getString("s408"));
		_colorFrame.setIconImage(ICON.getImage());
		_colorFrame.setLayout(new BorderLayout());

		// MAIN PANEL
		JPanel _mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());

		final JColorChooser _colorChooser = new JColorChooser(Color.BLUE);
		_colorChooser.getSelectionModel().addChangeListener(
				new ChangeListener() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see
					 * javax.swing.event.ChangeListener#stateChanged(javax.swing
					 * .event.ChangeEvent)
					 */
					@Override
					public void stateChanged(ChangeEvent changeEvent) {
						Color newColor = _colorChooser.getColor();
						_previewTextField.setText(_reservedWordTextField
								.getText());
						_previewTextField.setForeground(newColor);
					}
				});
		_colorChooser.setBorder(BorderFactory.createTitledBorder(null,
				_labels.getString("s409"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		JButton _acceptButton = new JButton(_labels.getString("s410"));
		_acceptButton.setToolTipText(_labels.getString("s411"));
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
				_colorFrame.dispose();
			}
		});

		// ADD THE COMPONENTS TO THE FRAME WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();

		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 5;
		constraints.insets = new Insets(5, 5, 5, 5);
		_mainPanel.add(_colorChooser, constraints);
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 3;
		constraints.gridy = 1;
		constraints.insets = new Insets(10, 5, 5, 5);
		_mainPanel.add(_acceptButton, constraints);
		_colorFrame.add(_mainPanel, BorderLayout.CENTER);
		_colorFrame.setResizable(false);
		_colorFrame.setVisible(true);
		_colorFrame.setLocation(250, 100);
		_colorFrame.pack();

		// Updates the log
		AcideLog.getLog().info(_labels.getString("s412"));
	}
}

/************************************************************************
 * Color renderer of the class.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
 * 
 ************************************************************************ 
 * @author <ul>
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan José Ortiz Sánchez
 *         </ul>
 *         <ul>
 *         Delfín Rupérez Cañas
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Martín Lázaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo Gómez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 * @see JLabel
 * @see TableCellRenderer
 ***********************************************************************/
class ColorRenderer extends JLabel implements TableCellRenderer {
	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Unselected border
	 */
	private Border _unselectedBorder = null;
	/**
	 * Selected border
	 */
	private Border _selectedBorder = null;
	/**
	 * Flag that indicates if the color renderer is bordered or not
	 */
	private boolean _isBordered = true;

	/**
	 * Class constructor
	 * 
	 * @param isBordered
	 *            is bordered flag
	 */
	public ColorRenderer(boolean isBordered) {

		_isBordered = isBordered;

		// MUST do this for background to show up.
		setOpaque(true);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax
	 * .swing.JTable, java.lang.Object, boolean, boolean, int, int)
	 */
	@Override
	public Component getTableCellRendererComponent(JTable table, Object color,
			boolean isSelected, boolean hasFocus, int row, int column) {

		Color newColor = (Color) color;
		setBackground(newColor);
		if (_isBordered) {

			if (isSelected) {
				if (_selectedBorder == null) {
					_selectedBorder = BorderFactory.createMatteBorder(2, 5, 2,
							5, table.getSelectionBackground());
				}
				setBorder(_selectedBorder);
			} else {
				if (_unselectedBorder == null) {
					_unselectedBorder = BorderFactory.createMatteBorder(2, 5,
							2, 5, table.getBackground());
				}
				setBorder(_unselectedBorder);
			}
		}
		return this;
	}
}

/************************************************************************
 * Color editor of the class
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
 * 
 ************************************************************************ 
 * @author <ul>
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan José Ortiz Sánchez
 *         </ul>
 *         <ul>
 *         Delfín Rupérez Cañas
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Martín Lázaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo Gómez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 * @see AbstractCellEditor
 * @see ActionListener
 * @see TableCellEditor
 ***********************************************************************/
class ColorEditor extends AbstractCellEditor implements TableCellEditor,
		ActionListener {
	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Current color
	 */
	private Color _currentColor;
	/**
	 * Edit button
	 */
	private JButton _editButton;
	/**
	 * Color chooser
	 */
	private JColorChooser _colorChooser;
	/**
	 * Dialog
	 */
	private JDialog _dialog;
	/**
	 * String for the button
	 */
	protected static final String EDIT = "edit";

	/**
	 * Class constructor
	 */
	public ColorEditor() {

		_editButton = new JButton();
		_editButton.setActionCommand(EDIT);
		_editButton.addActionListener(this);
		_editButton.setBorderPainted(false);
		_colorChooser = new JColorChooser();
		_dialog = JColorChooser.createDialog(_editButton, "Pick a Color", true,
				_colorChooser, this, null);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		if (EDIT.equals(actionEvent.getActionCommand())) {

			_editButton.setBackground(_currentColor);
			_colorChooser.setColor(_currentColor);
			_dialog.setVisible(true);

			fireEditingStopped();

		} else {
			_currentColor = _colorChooser.getColor();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.CellEditor#getCellEditorValue()
	 */
	@Override
	public Object getCellEditorValue() {
		return _currentColor;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * javax.swing.table.TableCellEditor#getTableCellEditorComponent(javax.swing
	 * .JTable, java.lang.Object, boolean, int, int)
	 */
	@Override
	public Component getTableCellEditorComponent(JTable table, Object value,
			boolean isSelected, int row, int column) {
		_currentColor = (Color) value;
		return _editButton;
	}
}