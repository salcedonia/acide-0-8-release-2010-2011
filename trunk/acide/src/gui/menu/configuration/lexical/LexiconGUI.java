package gui.menu.configuration.lexical;

import gui.MainWindow;
import java.awt.BorderLayout;
import java.awt.Checkbox;
import java.awt.Color;
import java.awt.Component;
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
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import language.Language;
import operations.lexicon.Comments;
import operations.lexicon.DividerList;
import operations.lexicon.TokenType;
import operations.lexicon.TokenTypeList;
import operations.listeners.AcideWindowListener;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

import es.configuration.lexica.MyTableModel;
import es.configuration.lexica.TableSorter;
import es.configuration.programmingLanguage.ProgrammingLanguage;

/**
 * 
 */
public class LexiconGUI extends JFrame {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private ResourceBundle _labels = Language.getInstance().getLabels();
	/**
	 * 
	 */
	private JFrame _frame;
	/**
	 * 
	 */
	private JTable _wordListTable;
	/**
	 * 
	 */
	private JTable _delimiterListTable;
	/**
	 * 
	 */
	private String[] _wordListColumns = { _labels.getString("s374"),
			_labels.getString("s443"), _labels.getString("s375") };
	/**
	 * 
	 */
	private String[] _delimiterListTableColumns = { _labels.getString("s440") };
	/**
	 * 
	 */
	private JPanel _tokenPanel;
	/**
	 * 
	 */
	private JPanel _dividersPanel;
	/**
	 * 
	 */
	private JPanel _commentPanel;
	/**
	 * 
	 */
	private TableSorter _wordListTableSorter;
	/**
	 * 
	 */
	private TableSorter _delimiterListTableSorter;
	/**
	 * 
	 */
	private JTextField _tfPreview;
	/**
	 * 
	 */
	private JTextField _tfPreviewComment;
	/**
	 * 
	 */
	private JTextField _tfWord;
	/**
	 * 
	 */
	private final JLabel _lblCaseSensitive;
	/**
	 * 
	 */
	private Checkbox _chbCaseSensitive;
	/**
	 * 
	 */
	private JComboBox _cmbColor;
	/**
	 * 
	 */
	private JFrame _colorFrame;
	/**
	 * 
	 */
	private JComboBox _cmbFontType;
	/**
	 * 
	 */
	private JComboBox _cmbColorComment;
	/**
	 * 
	 */
	private JButton _btnModify;
	/**
	 * 
	 */
	private JButton _btnQuit;
	/**
	 * 
	 */
	private JButton _btnAdd;
	/**
	 * 
	 */
	private JButton _btnSetDelimiters;
	/**
	 * 
	 */
	private JButton _btnAddDelimiter;
	/**
	 * 
	 */
	private JButton _btnDeleteDelimiter;
	/**
	 * 
	 */
	private final JLabel _lblWord;
	/**
	 * 
	 */
	private JLabel _lblColor;
	/**
	 * 
	 */
	private JLabel _lblColorComment;
	/**
	 * 
	 */
	private JLabel _lblPreview;
	/**
	 * 
	 */
	private JLabel _lblPreviewComment;
	/**
	 * 
	 */
	private JLabel _lblFontType;
	/**
	 * 
	 */
	private final JLabel _lblDelimiter;
	/**
	 * 
	 */
	private final JTextField _tfDelimiter;
	/**
	 * 
	 */
	private final JLabel _lblComment;
	/**
	 * 
	 */
	private final JTextField _tfComment;
	/**
	 * 
	 */
	private JButton _bntApply;
	/**
	 * 
	 */
	private JButton _btnCancel;
	/**
	 *
	 */
	private JPanel _rightPanel;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 */
	public LexiconGUI() {

		_logger.info(_labels.getString("s376"));

		final String tempPath = ProgrammingLanguage.getInstance().saveTemp(
				ProgrammingLanguage.getInstance().getName(), false);

		// FRAME
		_frame = new JFrame();
		_frame.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				try {
					ProgrammingLanguage.getInstance().load(
							PropertiesManager.getProperty("languagePath"));
				} catch (Exception e1) {
					e1.printStackTrace();
				}
			}
		});
		_frame.setLayout(new GridBagLayout());
		_frame.setTitle(_labels.getString("s377") + " - "
				+ ProgrammingLanguage.getInstance().getName());
		_frame.setIconImage(new ImageIcon(ICON).getImage());

		// TOKEN PANEL
		_tokenPanel = new JPanel();
		_tokenPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s428")));
		_tokenPanel.setLayout(new GridBagLayout());

		// DIVIDERS PANEL
		_dividersPanel = new JPanel();
		_dividersPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s429")));
		_dividersPanel.setLayout(new GridBagLayout());

		// COMMENT PANEL
		_commentPanel = new JPanel();
		_commentPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s430")));
		_commentPanel.setLayout(new GridBagLayout());

		// RIGHT PANEL
		_rightPanel = new JPanel();
		_rightPanel.setLayout(new GridBagLayout());

		// COLOR COMBOBOX
		_cmbColor = new JComboBox();
		_cmbColor.addItem("");
		_cmbColor.addItem(_labels.getString("s378"));
		_cmbColor.addItem(_labels.getString("s379"));
		_cmbColor.addItem(_labels.getString("s380"));
		_cmbColor.addItem(_labels.getString("s381"));
		_cmbColor.addItem(_labels.getString("s382"));
		_cmbColor.addItem(_labels.getString("s383"));
		_cmbColor.setEnabled(true);
		_cmbColor.setToolTipText(_labels.getString("s384"));

		// COLOR COMBOBOX COMMENT
		_cmbColorComment = new JComboBox();
		_cmbColorComment.addItem("");
		_cmbColorComment.addItem(_labels.getString("s378"));
		_cmbColorComment.addItem(_labels.getString("s379"));
		_cmbColorComment.addItem(_labels.getString("s380"));
		_cmbColorComment.addItem(_labels.getString("s381"));
		_cmbColorComment.addItem(_labels.getString("s382"));
		_cmbColorComment.addItem(_labels.getString("s383"));
		_cmbColorComment.setEnabled(true);
		_cmbColorComment.setToolTipText(_labels.getString("s384"));

		// ADD BUTTON
		_btnAdd = new JButton(_labels.getString("s385"));
		_btnAdd.setVerticalTextPosition(AbstractButton.CENTER);
		_btnAdd.setHorizontalTextPosition(AbstractButton.LEADING);
		_btnAdd.setMnemonic(KeyEvent.VK_A);
		_btnAdd.setToolTipText(_labels.getString("s386"));

		// ADD MODIFY
		_btnModify = new JButton(_labels.getString("s436"));
		_btnModify.setVerticalTextPosition(AbstractButton.CENTER);
		_btnModify.setHorizontalTextPosition(AbstractButton.LEADING);
		_btnModify.setMnemonic(KeyEvent.VK_M);
		_btnModify.setToolTipText(_labels.getString("s437"));
		// _btnModify.setEnabled(false);

		// SET DELIMITERS BUTTON
		_btnSetDelimiters = new JButton(_labels.getString("s438"));
		_btnSetDelimiters.setVerticalTextPosition(AbstractButton.CENTER);
		_btnSetDelimiters.setHorizontalTextPosition(AbstractButton.LEADING);
		_btnSetDelimiters.setMnemonic(KeyEvent.VK_S);
		_btnSetDelimiters.setToolTipText(_labels.getString("s439"));

		// QUIT BUTTON
		_btnQuit = new JButton(_labels.getString("s387"));
		_btnQuit.setVerticalTextPosition(AbstractButton.CENTER);
		_btnQuit.setHorizontalTextPosition(AbstractButton.LEADING);
		_btnQuit.setMnemonic(KeyEvent.VK_Q);
		_btnQuit.setToolTipText(_labels.getString("s388"));
		// _btnQuit.setEnabled(false);

		// ADD DIVIDER
		_btnAddDelimiter = new JButton(_labels.getString("s385"));
		_btnAddDelimiter.setVerticalTextPosition(AbstractButton.CENTER);
		_btnAddDelimiter.setHorizontalTextPosition(AbstractButton.LEADING);
		// addDivider.setToolTipText(labels.getString("s386"));

		// DELETE DIVIDER
		_btnDeleteDelimiter = new JButton(_labels.getString("s387"));
		_btnDeleteDelimiter.setVerticalTextPosition(AbstractButton.CENTER);
		_btnDeleteDelimiter.setHorizontalTextPosition(AbstractButton.LEADING);
		// deleteDivider.setToolTipText(labels.getString("s388"));

		// CASE SENSITIVE
		_lblCaseSensitive = new JLabel(_labels.getString("s431"), JLabel.CENTER);
		_chbCaseSensitive = new Checkbox();

		// WORD
		_lblWord = new JLabel(_labels.getString("s389"), JLabel.CENTER);
		_tfWord = new JTextField();
		_tfWord.setToolTipText(_labels.getString("s390"));

		// COLOR
		_lblColor = new JLabel(_labels.getString("s391"), JLabel.CENTER);
		_lblColorComment = new JLabel(_labels.getString("s391"), JLabel.CENTER);

		// PREVIEW
		_lblPreview = new JLabel(_labels.getString("s392"), JLabel.CENTER);
		_tfPreview = new JTextField();
		_tfPreview.setToolTipText(_labels.getString("s393"));
		_tfPreview.setEditable(false);
		_tfPreview.setHorizontalAlignment(JTextField.CENTER);
		_tfPreview.setFont(new Font(_lblWord.getFont().getFontName(),
				Font.PLAIN, _lblWord.getFont().getSize()));
		_tfPreview.setForeground(Color.BLACK);
		_tfPreview.setText(_labels.getString("s394"));

		// PREVIEW COMMENT
		_lblPreviewComment = new JLabel(_labels.getString("s392"),
				JLabel.CENTER);
		_tfPreviewComment = new JTextField();
		_tfPreviewComment.setToolTipText(_labels.getString("s393"));
		_tfPreviewComment.setEditable(false);
		_tfPreviewComment.setHorizontalAlignment(JTextField.CENTER);
		_tfPreviewComment.setFont(new Font(_lblWord.getFont().getFontName(),
				Font.PLAIN, _lblWord.getFont().getSize()));
		_tfPreviewComment.setForeground(Color.BLACK);
		_tfPreviewComment.setText(_labels.getString("s394"));
		_tfPreviewComment.setText(Comments.getInstance().getLineComment() + " "
				+ _labels.getString("s444"));
		_tfPreviewComment.setForeground(Comments.getInstance()
				.getLineCommentColor());
		_tfPreviewComment.setFont(new Font(_lblWord.getFont().getFontName(),
				Font.ITALIC, _lblWord.getFont().getSize()));

		// TYPE FONT
		_lblFontType = new JLabel(_labels.getString("s395"), JLabel.CENTER);
		_cmbFontType = new JComboBox();
		_cmbFontType.addItem(_labels.getString("s396"));
		_cmbFontType.addItem(_labels.getString("s397"));
		_cmbFontType.addItem(_labels.getString("s398"));
		_cmbFontType.addItem(_labels.getString("s399"));
		_cmbFontType.setEnabled(true);
		_cmbFontType.setToolTipText(_labels.getString("s400"));

		// DIVIDER
		_lblDelimiter = new JLabel(_labels.getString("s432"), JLabel.CENTER);
		_tfDelimiter = new JTextField();

		// COMMENT
		_lblComment = new JLabel(_labels.getString("s433"), JLabel.CENTER);
		_tfComment = new JTextField();
		_tfComment.setText(Comments.getInstance().getLineComment());

		// LISTENERS
		_tfWord.addKeyListener(new KeyListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
			 */
			public void keyTyped(KeyEvent arg0) {
				// previewTextField.setText(palabraField.getText());
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
			 */
			public void keyPressed(KeyEvent arg0) {
				// previewTextField.setText(palabraField.getText());
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
			 */
			public void keyReleased(KeyEvent arg0) {
				_tfPreview.setText(_tfWord.getText());
			}

		});
		_tfComment.addKeyListener(new KeyListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
			 */
			public void keyTyped(KeyEvent arg0) {
				// previewTextField.setText(palabraField.getText());
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
			 */
			public void keyPressed(KeyEvent arg0) {
				// previewTextField.setText(palabraField.getText());
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
			 */
			public void keyReleased(KeyEvent arg0) {
				_tfPreviewComment.setText(_tfComment.getText());
			}

		});
		_cmbColor.addActionListener(new ActionListener() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				Color color = Color.BLACK;
				String selectedItem = (String) _cmbColor.getSelectedItem();

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

				_tfPreview.setText(_tfWord.getText());
				_tfPreview.setForeground(color);
			}

		});

		_cmbColorComment.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				Color color = Color.BLACK;
				String selectedItem = (String) _cmbColorComment
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
				_tfPreviewComment.setText(_tfComment.getText());
				_tfPreviewComment.setForeground(color);
			}
		});

		_cmbFontType.addActionListener(new ActionListener() {
			
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
				
				Font font = _lblWord.getFont();
				String selectedItem = (String) _cmbFontType.getSelectedItem();
				
				if (selectedItem.equals(_labels.getString("s413")))
					_tfPreview.setFont(new Font(font.getFontName(), Font.PLAIN, font
							.getSize()));
				else if (selectedItem.equals(_labels.getString("s414")))
					_tfPreview.setFont(new Font(font.getFontName(), Font.ITALIC, font
							.getSize()));
				else if (selectedItem.equals(_labels.getString("s415")))
					_tfPreview.setFont(new Font(font.getFontName(), Font.BOLD, font
							.getSize()));
				else if (selectedItem.equals(_labels.getString("s416")))
					_tfPreview.setFont(new Font(font.getFontName(), Font.BOLD
							+ Font.ITALIC, font.getSize()));
				_tfPreview.setText(_tfWord.getText());
			}
		});

		_btnAdd.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				TokenType tokenType = new TokenType();
				tokenType.setColor(_tfPreview.getForeground());
				tokenType.setItalic(_tfPreview.getFont().isItalic());
				tokenType.setBold(_tfPreview.getFont().isBold());
				tokenType.setToken(_tfWord.getText());
				tokenType.setName();
				tokenType.setCaseSensitive(_chbCaseSensitive.getState());
				
				TokenTypeList tokenTypeList = TokenTypeList.getInstance();
				tokenTypeList.insertTokenType(tokenType, _tfWord.getText());

				int num = 0;
				for (int i = 0; i < tokenTypeList.getSize(); i++) {
					num = num + tokenTypeList.getTokenType(i).getTokenListSize();
				}

				Object[][] data = new Object[num][3];
				int aux = 0;
				
				for (int i = 0; i < tokenTypeList.getSize(); i++) {
					
					for (int j = 0; j < tokenTypeList.getTokenType(i).getTokenListSize(); j++) {
						
						data[aux][0] = tokenTypeList.getTokenType(i).getToken(j);
						String color = "";
						
						int colorAux = tokenTypeList.getTokenType(i).getColor().getRed();
						
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
										+ color
										+ ";\"><CENTER>"
										+ tokenTypeList.getTokenType(i).getToken(j)
										+ "</CENTER></div></body></html>";
							else if (isItalic && !isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><I>"
										+ tokenTypeList.getTokenType(i).getToken(j)
										+ "</I></CENTER></div></body></html>";
							else if (!isItalic && isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><B>"
										+ tokenTypeList.getTokenType(i).getToken(j)
										+ "</B></CENTER></div></body></html>";
							else if (isItalic && isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><B><I>"
										+ tokenTypeList.getTokenType(i).getToken(j)
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
				
				MyTableModel myModel = new MyTableModel();
				myModel.setValues(_wordListColumns, data);
				_wordListTableSorter.setModel(myModel);
				int c = _wordListTableSorter.getCol();
				
				if (c != -1)
					_wordListTableSorter.sortByColumn(c);
				_wordListTableSorter.fireTableDataChanged();
				_tfWord.setText("");
				_tfPreview.setText("");
				
				_logger.info(_labels.getString("s417") + _tfWord.getText());
			}
		});
		
		_btnQuit.addActionListener(new ActionListener() {
			
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
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
					tokenTypeList.removeToken(value);
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
							
							int colorAux = tokenTypeList.getTokenType(i).getColor().getRed();
							
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
											+ color
											+ ";\"><CENTER>"
											+ tokenTypeList.getTokenType(i).getToken(j)
											+ "</CENTER></div></body></html>";
								else if (isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><I>"
											+ tokenTypeList.getTokenType(i).getToken(j)
											+ "</I></CENTER></div></body></html>";
								else if (!isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B>"
											+ tokenTypeList.getTokenType(i).getToken(j)
											+ "</B></CENTER></div></body></html>";
								else if (isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B><I>"
											+ tokenTypeList.getTokenType(i).getToken(j)
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
					
					MyTableModel myModel = new MyTableModel();
					myModel.setValues(_wordListColumns, data);
					_wordListTableSorter.setModel(myModel);
					
					int column = _wordListTableSorter.getCol();
					if (column != -1)
						_wordListTableSorter.sortByColumn(column);
					_wordListTableSorter.fireTableDataChanged();
					_tfWord.setText("");
					_tfPreview.setText("");
					_logger.info(_labels.getString("s419") + value);
				}
			}
		});

		_btnAddDelimiter.addActionListener(new ActionListener() {
			
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				DividerList dividerList = DividerList.getInstance();
				dividerList.insertDivider(_tfDelimiter.getText());
				int num = dividerList.getSize();
				
				Object[][] data = new Object[num][1];
				int aux = 0;
				
				for (int i = 0; i < dividerList.getSize(); i++) {
					data[aux][0] = dividerList.getDivider(i);
					aux++;
				}
				
				MyTableModel myModel = new MyTableModel();
				myModel.setValues(_delimiterListTableColumns, data);
				_delimiterListTableSorter.setModel(myModel);
				int column = _delimiterListTableSorter.getCol();
				if (column != -1)
					_delimiterListTableSorter.sortByColumn(column);
				_delimiterListTableSorter.fireTableDataChanged();
				_tfDelimiter.setText("");
			}
		});

		_btnDeleteDelimiter.addActionListener(new ActionListener() {
			
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				int row = _delimiterListTable.getSelectedRow();
				int col = 0;
				for (int i = 0; i < _delimiterListTable.getColumnCount(); i++) {
					col = i;
				}
				if (row != -1) {
					
					String value = (String) _delimiterListTable.getValueAt(row,
							col);
					DividerList dl = DividerList.getInstance();
					dl.deleteDelimiter(value);
					int num = dl.getSize();
					
					Object[][] data = new Object[num][1];
					int aux = 0;
					for (int i = 0; i < dl.getSize(); i++) {
						data[aux][0] = dl.getDivider(i);
						aux++;
					}
					MyTableModel myModel = new MyTableModel();
					myModel.setValues(_delimiterListTableColumns, data);
					_delimiterListTableSorter.setModel(myModel);
					int column = _delimiterListTableSorter.getCol();
					if (column != -1)
						_delimiterListTableSorter.sortByColumn(column);
					_delimiterListTableSorter.fireTableDataChanged();
				}
			}
		});

		_btnModify.addActionListener(new ActionListener() {
			
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
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
					tokenTypeList.removeToken(val);
					TokenType tokenType = new TokenType();
					tokenType.setColor(_tfPreview.getForeground());
					tokenType.setItalic(_tfPreview.getFont().isItalic());
					tokenType.setBold(_tfPreview.getFont().isBold());
					tokenType.setCaseSensitive(_chbCaseSensitive.getState());
					tokenType.setToken(val);
					tokenType.setName();
					tokenTypeList.insertTokenType(tokenType, _tfWord.getText());
					
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
							
							int colorAux = tokenTypeList.getTokenType(i).getColor().getRed();
							
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
											+ color
											+ ";\"><CENTER>"
											+ tokenTypeList.getTokenType(i).getToken(j)
											+ "</CENTER></div></body></html>";
								else if (isItalic && !isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><I>"
											+ tokenTypeList.getTokenType(i).getToken(j)
											+ "</I></CENTER></div></body></html>";
								else if (!isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B>"
											+ tokenTypeList.getTokenType(i).getToken(j)
											+ "</B></CENTER></div></body></html>";
								else if (isItalic && isBold)
									s = "<html><head></head><body><div style=\"color:"
											+ color
											+ ";\"><CENTER><B><I>"
											+ tokenTypeList.getTokenType(i).getToken(j)
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
					MyTableModel myModel = new MyTableModel();
					myModel.setValues(_wordListColumns, data);
					_wordListTableSorter.setModel(myModel);
					int column = _wordListTableSorter.getCol();
					if (column != -1)
						_wordListTableSorter.sortByColumn(column);
					_wordListTableSorter.fireTableDataChanged();
					_tfWord.setText("");
					_tfPreview.setText("");
				}
			}
		});

		_btnSetDelimiters.addActionListener(new ActionListener() {
			
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				TokenTypeList tokenTypeList = TokenTypeList.getInstance();
				DividerList dividerList = DividerList.getInstance();
				for (int i = 0; i < dividerList.getSize(); i++) {
					String val = dividerList.getDivider(i);
					tokenTypeList.removeToken(val);
				}
				for (int i = 0; i < dividerList.getSize(); i++) {
					String value = dividerList.getDivider(i);
					TokenType tokenType = new TokenType();
					tokenType.setColor(_tfPreview.getForeground());
					tokenType.setItalic(_tfPreview.getFont().isItalic());
					tokenType.setBold(_tfPreview.getFont().isBold());
					tokenType.setToken(value);
					tokenType.setName();
					tokenType.setCaseSensitive(_chbCaseSensitive.getState());
					tokenTypeList.insertTokenType(tokenType, value);
				}
				int num = 0;
				for (int i = 0; i < tokenTypeList.getSize(); i++) {
					num = num + tokenTypeList.getTokenType(i).getTokenListSize();
				}

				Object[][] data = new Object[num][3];
				int aux = 0;
				for (int i = 0; i < tokenTypeList.getSize(); i++) {
					for (int j = 0; j < tokenTypeList.getTokenType(i).getTokenListSize(); j++) {
						data[aux][0] = tokenTypeList.getTokenType(i).getToken(j);
						String color = "";
						int auxC = tokenTypeList.getTokenType(i).getColor().getRed();
						if (Integer.toHexString(auxC).length() == 1) {
							color += "0" + Integer.toHexString(auxC);
						} else {
							color += Integer.toHexString(auxC);
						}

						auxC = tokenTypeList.getTokenType(i).getColor().getGreen();
						if (Integer.toHexString(auxC).length() == 1) {
							color += "0" + Integer.toHexString(auxC);
						} else {
							color += Integer.toHexString(auxC);
						}
						auxC = tokenTypeList.getTokenType(i).getColor().getBlue();
						if (Integer.toHexString(auxC).length() == 1) {
							color += "0" + Integer.toHexString(auxC);
						} else {
							color += Integer.toHexString(auxC);
						}
						
						String s = "";
						
						boolean isItalic = tokenTypeList.getTokenType(i).isItalic();
						boolean isBold = tokenTypeList.getTokenType(i).isBold();
						
						if (!tokenTypeList.getTokenType(i).getToken(j)
								.equalsIgnoreCase("<")) {
							if (!isItalic && !isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER>"
										+ tokenTypeList.getTokenType(i).getToken(j)
										+ "</CENTER></div></body></html>";
							else if (isItalic && !isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><I>"
										+ tokenTypeList.getTokenType(i).getToken(j)
										+ "</I></CENTER></div></body></html>";
							else if (!isItalic && isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><B>"
										+ tokenTypeList.getTokenType(i).getToken(j)
										+ "</B></CENTER></div></body></html>";
							else if (isItalic && isBold)
								s = "<html><head></head><body><div style=\"color:"
										+ color
										+ ";\"><CENTER><B><I>"
										+ tokenTypeList.getTokenType(i).getToken(j)
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
				MyTableModel myModel = new MyTableModel();
				myModel.setValues(_wordListColumns, data);
				_wordListTableSorter.setModel(myModel);
				int column = _wordListTableSorter.getCol();
				if (column != -1)
					_wordListTableSorter.sortByColumn(column);
				_wordListTableSorter.fireTableDataChanged();

				_tfWord.setText("");
				_tfPreview.setText("");
			}
		});

		// APPLY BUTTON
		_bntApply = new JButton();
		_bntApply.setText(_labels.getString("s434"));
		_bntApply.addActionListener(new ActionListener() {
			
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				Comments.getInstance().setLineComment(_tfComment.getText());
				Comments.getInstance().setLineCommentColor(
						_tfPreviewComment.getForeground());
				
				int editor = MainWindow.getInstance().getEditorBuilder()
						.getNumEditors();
				
				for (int i = 0; i < editor; i++) {
					MainWindow.getInstance().getEditorBuilder().getEditorAt(i)
							.resetDoc();
				}
				// Lenguaje l = Lenguaje.getInstance();
				// l.guardar(l.getNombre(), false);
				_frame.dispose();
				MainWindow
						.getInstance()
						.getStatusBar()
						.getMessagelexical()
						.setText(
								_labels.getString("s449")
										+ " "
										+ ProgrammingLanguage.getInstance()
												.getName());
			}
		});
		
		// CANCEL BUTTON
		_btnCancel = new JButton();
		_btnCancel.setText(_labels.getString("s435"));
		_btnCancel.addActionListener(new ActionListener() {
			
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				try {
					ProgrammingLanguage.getInstance().loadTemp(tempPath);
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				_frame.dispose();
			}
		});

		ActionListener l = new ActionListener() {
			
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_frame.dispose();
			}
		};

		_btnCancel.registerKeyboardAction(l, "EscapeKey", KeyStroke.getKeyStroke(
				java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);
		
		// ADD THE COMPONENTS TO THE FRAME WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		
		// WORD
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_tokenPanel.add(_lblWord, constraints);
		constraints.gridx = 1;
		_tokenPanel.add(_tfWord, constraints);

		// CASE SENSITIVE
		constraints.ipadx = 0;
		constraints.insets = new Insets(10, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_tokenPanel.add(_lblCaseSensitive, constraints);
		constraints.gridx = 1;
		_tokenPanel.add(_chbCaseSensitive, constraints);

		// COLOR
		constraints.gridx = 0;
		constraints.gridy = 2;
		_tokenPanel.add(_lblColor, constraints);
		constraints.gridx = 1;
		_tokenPanel.add(_cmbColor, constraints);

		// FONT TYPE
		constraints.gridx = 0;
		constraints.gridy = 3;
		_tokenPanel.add(_lblFontType, constraints);
		constraints.gridx = 1;
		_tokenPanel.add(_cmbFontType, constraints);

		// PREVIEW
		constraints.gridx = 0;
		constraints.gridy = 4;
		_tokenPanel.add(_lblPreview, constraints);
		constraints.gridx = 1;
		_tokenPanel.add(_tfPreview, constraints);

		// ADD BUTTON
		constraints.ipadx = 70;
		constraints.gridx = 0;
		constraints.gridy = 5;
		_tokenPanel.add(_btnAdd, constraints);
		
		// QUIT BUTTON
		constraints.gridx = 1;
		_tokenPanel.add(_btnQuit, constraints);

		// SET DELIMITERS BUTTON
		constraints.gridy = 6;
		constraints.gridx = 0;
		_tokenPanel.add(_btnSetDelimiters, constraints);
		
		// MODIFY BUTTON
		constraints.gridx = 1;
		_tokenPanel.add(_btnModify, constraints);
		setListTables();

		// DELIMITER PANEL
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_dividersPanel.add(_lblDelimiter, constraints);
		constraints.gridx = 1;
		_dividersPanel.add(_tfDelimiter, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;
		_dividersPanel.add(_btnAddDelimiter, constraints);
		constraints.gridx = 1;
		_dividersPanel.add(_btnDeleteDelimiter, constraints);

		// COMMENT
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		_commentPanel.add(_lblComment, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 0;
		_commentPanel.add(_tfComment, constraints);

		// COLOR COMMENT
		constraints.gridx = 0;
		constraints.gridy = 1;
		_commentPanel.add(_lblColorComment, constraints);
		constraints.gridx = 1;
		_commentPanel.add(_cmbColorComment, constraints);

		// PREVIEW COMMENT
		constraints.gridx = 0;
		constraints.gridy = 2;
		_commentPanel.add(_lblPreviewComment, constraints);
		constraints.gridx = 1;
		_commentPanel.add(_tfPreviewComment, constraints);

		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		_tokenPanel.setSize(new Dimension(100, 100));
		_frame.add(_tokenPanel, constraints);

		// RIGHT PANEL
		constraints.ipady = 22;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		_rightPanel.add(_dividersPanel, constraints);
		constraints.ipady = 22;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		_rightPanel.add(_commentPanel, constraints);
		constraints.ipady = 0;
		constraints.gridx = 1;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		_frame.add(_rightPanel, constraints);

		// APPLY BUTTON
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		_frame.add(_bntApply, constraints);
		
		// CANCEL BUTTON
		constraints.ipady = 0;
		constraints.gridx = 1;
		constraints.gridy = 1;
		constraints.ipadx = 0;
		_frame.add(_btnCancel, constraints);

		_frame.setVisible(true);
		_frame.setResizable(true);
		_frame.setLocationRelativeTo(null);
		_frame.setResizable(false);
		_frame.setLocationRelativeTo(null);
		_logger.info(_labels.getString("s420"));
		_frame.addWindowListener(new AcideWindowListener());
		_frame.pack();
		_frame.setLocationRelativeTo(null);
	}

	/**
	 * 
	 * @return
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
			for (int j = 0; j < tokenTypeList.getTokenType(i).getTokenListSize(); j++) {
				
				data[aux][0] = tokenTypeList.getTokenType(i).getToken(j);
				String color = "";
				
				int colorAux = tokenTypeList.getTokenType(i).getColor().getRed();
				
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
				
				if (!tokenTypeList.getTokenType(i).getToken(j).equalsIgnoreCase("<")) {
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

		MyTableModel myModel = new MyTableModel();
		myModel.setValues(_wordListColumns, data);
		_wordListTableSorter = new TableSorter(myModel);
		_wordListTable = new JTable(_wordListTableSorter);
		_wordListTable.addKeyListener(new KeyListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
			 */
			public void keyReleased(KeyEvent arg0) {
				checkRows();
			}
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
			 */
			public void keyTyped(KeyEvent arg0) {
				checkRows();
			}
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
			 */
			public void keyPressed(KeyEvent arg0) {
				checkRows();
			}

		});

		_wordListTable.addMouseListener(new MouseListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
			 */
			public void mouseClicked(MouseEvent arg0) {
				checkRows();
			}
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
			 */
			public void mousePressed(MouseEvent arg0) {
				checkRows();
			}
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
			 */
			public void mouseReleased(MouseEvent arg0) {
			} // checkRows();}
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent)
			 */
			public void mouseEntered(MouseEvent arg0) {
			} // checkRows();}
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent)
			 */
			public void mouseExited(MouseEvent arg0) {
			} // checkRows();}
		});

		_wordListTableSorter.addMouseListenerToHeaderInTable(_wordListTable);

		_wordListTable.setDefaultRenderer(Color.class, new ColorRenderer(true));
		_wordListTable.setDefaultEditor(Color.class, new ColorEditor());
		_wordListTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		_wordListTable
				.setPreferredScrollableViewportSize(new Dimension(310, 200));
		
		JScrollPane scrollPane = new JScrollPane(_wordListTable);
		return scrollPane;
	}

	/*
	 * 
	 */
	public JScrollPane getDelimiterListTable() {
		
		DividerList delimiterList = DividerList.getInstance();

		int num = delimiterList.getSize();

		Object[][] data = new Object[num][1];
		int aux = 0;
		for (int i = 0; i < delimiterList.getSize(); i++) {
			data[aux][0] = delimiterList.getDivider(i);
			aux++;
		}

		MyTableModel myModel = new MyTableModel();
		myModel.setValues(_delimiterListTableColumns, data);
		_delimiterListTableSorter = new TableSorter(myModel);
		_delimiterListTable = new JTable(_delimiterListTableSorter);
		_delimiterListTableSorter.addMouseListenerToHeaderInTable(_delimiterListTable);

		_delimiterListTable.setDefaultRenderer(Color.class, new ColorRenderer(
				true));
		_delimiterListTable.setDefaultEditor(Color.class, new ColorEditor());
		_delimiterListTable.setPreferredScrollableViewportSize(new Dimension(
				280, 200));
		JScrollPane scrollPane = new JScrollPane(_delimiterListTable);
		return scrollPane;
	}

	/**
	 * 
	 */
	public void setListTables() {
		
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 0;
		constraints.gridy = 7;
		constraints.gridwidth = 2;
		_tokenPanel.add(getWordListTable(), constraints);
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.insets = new Insets(5, 5, 5, 5);
		_dividersPanel.add(getDelimiterListTable(), constraints);
	}

	/**
	 * 
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
			TokenType tokenType = tokenTypeList.getToken(value);
			
			_cmbColor.setSelectedIndex(0);
			_tfWord.setText(value);
			_tfPreview.setText(value);
			_tfPreview.setForeground(tokenType.getColor());
			_chbCaseSensitive.setState(tokenType.isCaseSensitive());
			
			if (!tokenType.isItalic() && !tokenType.isBold()) {
				_tfPreview.setFont(new Font(_tfPreview.getFont().getFontName(), 
						Font.PLAIN, _tfPreview.getFont().getSize()));
				_cmbFontType.setSelectedIndex(0);
			} else if (tokenType.isItalic() && !tokenType.isBold()) {
				_tfPreview.setFont(new Font(_tfPreview.getFont().getFontName(), 
						Font.ITALIC, _tfPreview.getFont().getSize()));
				_cmbFontType.setSelectedIndex(1);
			} else if (!tokenType.isItalic() && tokenType.isBold()) {
				_tfPreview.setFont(new Font(_tfPreview.getFont().getFontName(), 
						Font.BOLD, _tfPreview.getFont().getSize()));
				_cmbFontType.setSelectedIndex(2);
			} else if (tokenType.isItalic() && tokenType.isBold()) {
				_tfPreview.setFont(new Font(_tfPreview.getFont().getFontName(), 
						Font.BOLD + Font.ITALIC, _tfPreview.getFont().getSize()));
				_cmbFontType.setSelectedIndex(3);
			}
		}
	}
	
	/**
	 * 
	 */
	public void buildColorPaletteWindow() {
		
		_logger.info(_labels.getString("s407"));
		
		// FRAME
		_colorFrame = new JFrame();
		_colorFrame.setTitle(_labels.getString("s408"));
		_colorFrame.setIconImage(new ImageIcon(ICON).getImage());
		_colorFrame.setLayout(new BorderLayout());
		
		// MAIN PANEL
		JPanel _mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());
		
		final JColorChooser _colorChooser = new JColorChooser(Color.BLUE);
		_colorChooser.getSelectionModel().addChangeListener(
				new ChangeListener() {
					/*
					 * (non-Javadoc)
					 * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent)
					 */
					public void stateChanged(ChangeEvent e) {
						Color newColor = _colorChooser.getColor();
						_tfPreview.setText(_tfWord
								.getText());
						_tfPreview.setForeground(newColor);
					}
				});
		_colorChooser.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s409")));
		
		JButton _btnAccept = new JButton(_labels
				.getString("s410"));
		_btnAccept.setToolTipText(_labels.getString("s411"));
		_btnAccept.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent arg0) {
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
		_mainPanel.add(_btnAccept, constraints);
		_colorFrame.add(_mainPanel, BorderLayout.CENTER);
		_colorFrame.setResizable(false);
		_colorFrame.setVisible(true);
		_colorFrame.setLocation(250, 100);
		_colorFrame.pack();
		_logger.info(_labels.getString("s412"));
	}

}

class ColorRenderer extends JLabel implements TableCellRenderer {
	/**
	 * serialVersionUID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private Border _unselectedBorder = null;
	/**
	 * 
	 */
	private Border _selectedBorder = null;
	/**
	 * 
	 */
	private boolean _isBordered = true;

	/**
	 * Constructor of the class.
	 * 
	 * @param isBordered
	 */
	public ColorRenderer(boolean isBordered) {
		
		_isBordered = isBordered;
		
		// MUST do this for background to show up.
		setOpaque(true); 
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.table.TableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
	 */
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
					_unselectedBorder = BorderFactory.createMatteBorder(2, 5, 2,
							5, table.getBackground());
				}
				setBorder(_unselectedBorder);
			}
		}
		return this;
	}
}

/**
 * 
 */
class ColorEditor extends AbstractCellEditor implements TableCellEditor,
		ActionListener {
	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private Color currentColor;
	/**
	 * 
	 */
	private JButton _btnEdit;
	/**
	 * 
	 */
	private JColorChooser _colorChooser;
	/**
	 * 
	 */
	private JDialog _dialog;
	/**
	 * 
	 */
	protected static final String EDIT = "edit";

	/**
	 * Constructor of the class.
	 */
	public ColorEditor() {
		
		_btnEdit = new JButton();
		_btnEdit.setActionCommand(EDIT);
		_btnEdit.addActionListener(this);
		_btnEdit.setBorderPainted(false);
		_colorChooser = new JColorChooser();
		_dialog = JColorChooser.createDialog(_btnEdit, "Pick a Color", true,
				_colorChooser, this, null);
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		
		if (EDIT.equals(e.getActionCommand())) {

			_btnEdit.setBackground(currentColor);
			_colorChooser.setColor(currentColor);
			_dialog.setVisible(true);

			fireEditingStopped();

		} else {
			currentColor = _colorChooser.getColor();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.CellEditor#getCellEditorValue()
	 */
	public Object getCellEditorValue() {
		return currentColor;
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.table.TableCellEditor#getTableCellEditorComponent(javax.swing.JTable, java.lang.Object, boolean, int, int)
	 */
	public Component getTableCellEditorComponent(JTable table, Object value,
			boolean isSelected, int row, int column) {
		currentColor = (Color) value;
		return _btnEdit;
	}
}
