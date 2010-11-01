package gui.menu.edit;

import gui.MainWindow;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import language.Language;

import org.apache.log4j.Logger;

import operations.factory.OperationsFactory;
import operations.listeners.AcideKeyboardListener;
import operations.log.Log;
import properties.PropertiesManager;

/**
 * Search GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class SearchGUI extends JFrame {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the icon of the window.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Instance of the class.
	 */
	private static SearchGUI _instance;
	/**
	 * Main panel of the window.
	 */
	private JPanel _mainPanel;
	/**
	 * Direction panel.
	 */
	private JPanel _directionPanel;
	/**
	 * Option panel.
	 */
	private JPanel _optionPanel;
	/**
	 * Scope panel.
	 */
	private JPanel _scopePanel;
	/**
	 * Search text field.
	 */
	private JTextField _searchTextField;
	/**
	 * Case sensitive check box.
	 */
	private JCheckBox _caseSensitiveCheckBox;
	/**
	 * Regular expressions check box.
	 */
	private JCheckBox _regularExpressionsCheckBox;
	/**
	 * Complete words check box.
	 */
	private JCheckBox _completeWordsCheckBox;
	/**
	 * Forward radio button.
	 */
	private JRadioButton _forwardRadioButton;
	/**
	 * Backward radio button.
	 */
	private JRadioButton _backwardRadioButton;
	/**
	 * All radio button.
	 */
	private JRadioButton _allRadioButton;
	/**
	 * Current document radio button.
	 */
	private JRadioButton _currentDocumentRadioButton;
	/**
	 * All documents radio button.
	 */
	private JRadioButton _allDocumentsRadioButton;
	/**
	 * Selected radio button.
	 */
	private JRadioButton _selectedRadioButton;
	/**
	 * Button group.
	 */
	private ButtonGroup _buttonGroup;
	/**
	 * Search button.
	 */
	private JButton _searchButton;
	/**
	 * Cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Result of the operation.
	 */
	private int _result;
	/**
	 * Selected text.
	 */
	private String _selectedText;
	/**
	 * Initial position.
	 */
	private int _initialPosition;
	/**
	 * Final position.
	 */
	private int _finalPosition;
	/**
	 * Selected editor Index.
	 */
	private static int _selectedEditorIndex;
	/**
	 * Flag that indicates if the search is over or not.
	 */
	private static boolean _isEnd = false;
	/**
	 * Current position of the search.
	 */
	private int _currentPosition;
	/**
	 * Current document.
	 */
	private int _currentDocument;
	/**
	 * Counter.
	 */
	private int _counter;
	/**
	 * Flag that indicates if the search has completed a cycle or not.
	 */
	private static boolean _isCycle;
	/**
	 * Flag that indicates  if it is the first searching.
	 */
	private static boolean _isFirst;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();
	/**
	 * Instance of the main window.
	 */
	private MainWindow _mainWindow = MainWindow.getInstance();
	/**
	 * Search class.
	 */
	private Search _search = OperationsFactory.getInstance().buildSearch();
	/**
	 * Labels to display.
	 */
	private ResourceBundle _labels;

	/**
	 * Constructor of the class.
	 */
	public SearchGUI() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		_labels = language.getLabels();
						
		// FRAME
		setTitle(_labels.getString("s556"));
		setIconImage(new ImageIcon(ICON).getImage());
		
		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());
		
		// DIRECTION PANEL
		_directionPanel = new JPanel();
		_directionPanel.setBorder(BorderFactory.createTitledBorder(null, _labels
				.getString("s567"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_directionPanel.setLayout(new GridLayout(0, 1));
		_buttonGroup = new ButtonGroup();
		_forwardRadioButton = new JRadioButton(_labels.getString("s568"), true);
		_backwardRadioButton = new JRadioButton(_labels.getString("s569"), false);
		_allRadioButton = new JRadioButton(_labels.getString("s570"), false);
		_buttonGroup.add(_forwardRadioButton);
		_buttonGroup.add(_backwardRadioButton);
		_buttonGroup.add(_allRadioButton);
		_directionPanel.add(_forwardRadioButton);
		_directionPanel.add(_backwardRadioButton);
		_directionPanel.add(_allRadioButton);
		
		// OPTION PANEL
		_optionPanel = new JPanel();
		_optionPanel.setBorder(BorderFactory.createTitledBorder(null, _labels
				.getString("s559"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_caseSensitiveCheckBox = new JCheckBox(_labels.getString("s560"), false);
		_regularExpressionsCheckBox =new JCheckBox(_labels.getString("s561"), false);
		_completeWordsCheckBox = new JCheckBox(_labels.getString("s562"), false);
		_optionPanel.setLayout(new GridLayout(0, 1));
		_optionPanel.add(_caseSensitiveCheckBox);
		_optionPanel.add(_regularExpressionsCheckBox);
		_optionPanel.add(_completeWordsCheckBox);
		
		// SCOPE PANEL
		_scopePanel = new JPanel();
		_scopePanel.setBorder(BorderFactory.createTitledBorder(null, _labels
				.getString("s563"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_scopePanel.setLayout(new GridLayout(0, 1));
		_buttonGroup = new ButtonGroup();
		_currentDocumentRadioButton = new JRadioButton(_labels.getString("s565"),
				true);
		_allDocumentsRadioButton = new JRadioButton(_labels.getString("s566"), false);
		_selectedRadioButton = new JRadioButton(_labels.getString("s564"), false);
		_buttonGroup.add(_selectedRadioButton);
		_buttonGroup.add(_currentDocumentRadioButton);
		_buttonGroup.add(_allDocumentsRadioButton);
		_scopePanel.add(_selectedRadioButton);
		_scopePanel.add(_currentDocumentRadioButton);
		_scopePanel.add(_allDocumentsRadioButton);
		
		// SEARCH BUTTON
		_searchButton = new JButton();
		_searchButton.setText(_labels.getString("s556"));
		_searchButton.setMnemonic(java.awt.event.KeyEvent.VK_F3);
		
		// CANCEL BUTTON
		_cancelButton = new JButton();
		_cancelButton.setText(_labels.getString("s42"));
		
		JLabel etiq1 = new JLabel(_labels.getString("s557"), JLabel.CENTER);
		_searchTextField = new JTextField();
		_searchTextField.setText("");
	
		// ADD THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();

		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 2;
		constraints.insets = new Insets(5, 5, 5, 5);
		_mainPanel.add(etiq1, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 300;
		_mainPanel.add(_searchTextField, constraints);
		constraints.fill = GridBagConstraints.CENTER;
		constraints.gridy = 2;
		constraints.gridx = 0;
		_mainPanel.add(_optionPanel, constraints);
		constraints.fill = GridBagConstraints.CENTER;
		constraints.gridy = 3;
		constraints.gridx = 0;
		constraints.ipadx = 100;
		constraints.gridwidth = 1;
		_mainPanel.add(_scopePanel, constraints);
		constraints.gridy = 3;
		constraints.gridx = 1;
		_mainPanel.add(_directionPanel, constraints);
		constraints.fill = GridBagConstraints.CENTER;
		constraints.ipadx = 60;
		constraints.weightx = 0.5;
		constraints.gridwidth = 2;
		constraints.gridy = 4;
		constraints.gridx = 0;
		_mainPanel.add(_searchButton, constraints);
		constraints.fill = GridBagConstraints.CENTER;
		constraints.ipadx = 60;
		constraints.weightx = 0.5;
		constraints.gridwidth = 2;
		constraints.gridy = 4;
		constraints.gridx = 1;
		_mainPanel.add(_cancelButton, constraints);
		
		add(_mainPanel);
		setResizable(false);
		setAlwaysOnTop(true);
		pack();
		setLocationRelativeTo(null);

		// LISTENERS
		_searchButton.addActionListener(new SearchButtonListener());
		_cancelButton.addActionListener(new CancelButtonListener());
		
		AcideKeyboardListener key = new AcideKeyboardListener();
		_searchTextField.addKeyListener(key);
		_caseSensitiveCheckBox.addKeyListener(key);
		_regularExpressionsCheckBox.addKeyListener(key);
		_completeWordsCheckBox.addKeyListener(key);
		_forwardRadioButton.addKeyListener(key);
		_backwardRadioButton.addKeyListener(key);
		_allRadioButton.addKeyListener(key);
		_selectedRadioButton.addKeyListener(key);
		_allDocumentsRadioButton.addKeyListener(key);
		_currentDocumentRadioButton.addKeyListener(key);
	}

	/**
	 * Listener for the search button.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class SearchButtonListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			
			// INITIALIZE THE VARIABLES
			_isCycle = false;
			_currentPosition = -2;
			_currentDocument = -1;
			_initialPosition = -1;
			_selectedText = null;
			_result = -1;
			_isFirst = true;
			
			SearchGUI searchGUI = SearchGUI.getInstance();
			
			Direction direction = Direction.FORWARD;
			
			// SELECTS THE DIRECTION OF THE SEARCH
			if (_forwardRadioButton.isSelected())
				direction = Direction.FORWARD;
		
			if (_backwardRadioButton.isSelected())
				direction = Direction.BACKWARD;
			
			if (_allRadioButton.isSelected())
				direction = Direction.BOTH;
			
			if (_completeWordsCheckBox.isSelected())
				_regularExpressionsCheckBox.setSelected(false);
			
			// IF THE SEARCH TEXT IS EMPTY
			if (_searchTextField.getText().equals("")) {
				
				searchGUI.setOnTop(false);
				
				// INFORMS TO THE USER
				JOptionPane.showMessageDialog(null, _labels.getString("s585"));
				searchGUI.setOnTop(true);
				_mainWindow.getStatusBar()
						.setMessage(_labels.getString("s585"));
			}
			
			int selectedEditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();

			// SEARCH IN THE CURRENT DOCUMENT
			if (_currentDocumentRadioButton.isSelected()) {
				
				_selectedEditorIndex = -1;
				_counter = 0;
				_result = -1;
				_selectedText = null;
				selectedEditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
				_result = _mainWindow.getEditorBuilder().getEditorAt(selectedEditor)
						.getEditor().getCaretPosition();
				
				// BACKWARD DIRECTION
				if (direction == Direction.BACKWARD) {
					_result = _mainWindow.getEditorBuilder()
							.getEditorAt(selectedEditor).getEditor()
							.getSelectionStart();
				}
				selectedEditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();

				_result = _search.search(_result, _searchTextField.getText(), _mainWindow
						.getEditorBuilder().getEditorAt(selectedEditor).getText(),
						_caseSensitiveCheckBox.isSelected(), _regularExpressionsCheckBox.isSelected(),
						_completeWordsCheckBox.isSelected(), direction.ordinal());

				if (_result != -1) {

					_mainWindow
							.getEditorBuilder()
							.getEditorAt(
									_mainWindow.getEditorBuilder()
											.getSelectedEditorIndex())
							.selectText(_result, _searchTextField.getText().length());

					_logger.info(_labels.getString("s583") + " " + _searchTextField.getText()
							+ " " + _labels.getString("s574"));

					_mainWindow.getStatusBar().setMessage(
							_labels.getString("s583") + " " + _searchTextField.getText()
									+ " " + _labels.getString("s574"));
					
					// IF REGULAR EXPRESSIONS
					if (_regularExpressionsCheckBox.isSelected()) {
						
						// SHOWS THE SEARCH IN THE TEXT EDITOR
						_mainWindow
								.getEditorBuilder()
								.getEditorAt(
										_mainWindow.getEditorBuilder()
												.getSelectedEditorIndex())
								.selectText(_result,
										_search.getRegularExpresion().length());
						
						// UPDATES THE STATUS BAR
						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s577") + " "
										+ _search.getRegularExpresion() + " "
										+ _labels.getString("s574"));
					
						_logger.info(_labels.getString("s577") + " "
								+ _search.getRegularExpresion() + " "
								+ _labels.getString("s574"));
					}
				}
				else {
					_logger.info(_labels.getString("s573"));
					searchGUI.setOnTop(false);
					JOptionPane.showMessageDialog(null,
							_labels.getString("s573"));
					searchGUI.setOnTop(true);
					_mainWindow.getStatusBar().setMessage(
							_labels.getString("s573"));
				}
			}

			// IF SELECT TEXT
			if (_selectedRadioButton.isSelected()) {
				
				_counter = 0;
				_selectedEditorIndex = -1;
				selectedEditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
				
				if (_selectedText == null) {
					
					_selectedText = _mainWindow.getEditorBuilder()
							.getEditorAt(selectedEditor).getEditor()
							.getSelectedText();
					_initialPosition = _mainWindow.getEditorBuilder()
							.getEditorAt(selectedEditor).getEditor()
							.getSelectionStart();
					_finalPosition = _mainWindow.getEditorBuilder()
							.getEditorAt(selectedEditor).getEditor()
							.getSelectionEnd();
					_result = 0;
					
					if (direction == Direction.BACKWARD)
						_result = _finalPosition;
					if ((_regularExpressionsCheckBox.isSelected()) && (direction != Direction.BACKWARD))
						_result = _initialPosition;
				} else {
					_result = _mainWindow.getEditorBuilder()
							.getEditorAt(selectedEditor).getEditor()
							.getCaretPosition()
							- _initialPosition;

					if (direction == Direction.BACKWARD) {
						_result = _mainWindow.getEditorBuilder()
								.getEditorAt(selectedEditor).getEditor()
								.getSelectionStart()
								- _initialPosition;
					}
				}

				if (_selectedText == null) {
					searchGUI.setOnTop(false);
					JOptionPane.showMessageDialog(null,
							_labels.getString("s616"));
					searchGUI.setOnTop(true);
					_mainWindow.getStatusBar().setMessage(
							_labels.getString("s616"));
				} else {
					_result = _search.search(_result, _searchTextField.getText(),
							_selectedText, _caseSensitiveCheckBox.isSelected(),
							_regularExpressionsCheckBox.isSelected(), _completeWordsCheckBox.isSelected(),
							direction.ordinal());
					
					if (_result != -1) {
						_mainWindow
								.getEditorBuilder()
								.getEditorAt(
										_mainWindow.getEditorBuilder()
												.getSelectedEditorIndex())
								.selectText(_result + _initialPosition,
										_searchTextField.getText().length());

						_logger.info(_labels.getString("s583") + " "
								+ _searchTextField.getText() + " "
								+ _labels.getString("s574"));

						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s583") + " " + _searchTextField.getText()
										+ " " + _labels.getString("s574"));

						// REGULAR EXPRESSIONS
						if (_regularExpressionsCheckBox.isSelected()) {

							// SHOWS THE SEARCH IN THE TEXT EDITOR
							_mainWindow
									.getEditorBuilder()
									.getEditorAt(
											_mainWindow.getEditorBuilder()
													.getSelectedEditorIndex())
									.selectText(
											_result + _initialPosition,
											_search.getRegularExpresion()
													.length());

							// UPDATES THE STATUS BAR
							_mainWindow.getStatusBar().setMessage(
									_labels.getString("s329") + " "
											+ _searchTextField.getText() + " "
											+ _labels.getString("s574"));
							
							_logger.info(_labels.getString("s329") + " "
									+ _search.getRegularExpresion() + " "
									+ _labels.getString("s574"));							
						}
					}else {
						
						_selectedText = null;
						_logger.info(_labels.getString("s573"));
						searchGUI.setOnTop(false);
						JOptionPane.showMessageDialog(null,
								_labels.getString("s573"));
						searchGUI.setOnTop(true);
						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s573"));
						searchGUI.setOnTop(false);
						int op = JOptionPane.showConfirmDialog(null,
								_labels.getString("s575"));
						searchGUI.setOnTop(true);
						
						if (op == JOptionPane.OK_OPTION) {
							_currentDocumentRadioButton.setSelected(true);
							
							if (direction != Direction.BACKWARD)
								_result = _finalPosition;
							else
								_result = _initialPosition;
							_searchButton.doClick();
						}
					}
				}
			}
			
			if (_allDocumentsRadioButton.isSelected() == true) {
				_selectedText = null;
				selectedEditor = _mainWindow.getEditorBuilder().getNumEditors();
				if ((_isCycle == false) && (_currentPosition == -2)) {
					_selectedEditorIndex = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
					_currentDocument = _selectedEditorIndex;
					_currentPosition = _mainWindow.getEditorBuilder()
							.getEditorAt(_selectedEditorIndex).getEditor().getCaretPosition();
				}

				if (_isEnd == false) {
					
					if (direction == Direction.FORWARD)
						_result = _mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex)
								.getEditor().getCaretPosition();
					if (direction == Direction.BACKWARD)
						_result = _mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex)
								.getEditor().getSelectionStart();
					if (direction == Direction.BOTH) {
						_result = _mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex)
								.getEditor().getCaretPosition();
					}
					
					Direction auxDirection = direction;
					
					if (direction == Direction.BOTH)
						auxDirection = Direction.FORWARD;
					
					_result = _search.search(_result, _searchTextField.getText(), _mainWindow
							.getEditorBuilder().getEditorAt(_selectedEditorIndex).getText(),
							_caseSensitiveCheckBox.isSelected(), _regularExpressionsCheckBox.isSelected(),
							_completeWordsCheckBox.isSelected(), auxDirection.ordinal());
					
					if ((_isCycle) && (_selectedEditorIndex == _currentDocument)
							&& (_result >= _currentPosition))
						_isEnd = true;
					else 
						if ((_isCycle) && (_selectedEditorIndex == _currentDocument)
							&& (_result == -1))
							_isEnd = true;
					
					if (_result != -1) {
						
						_counter++;
						
						if (!_regularExpressionsCheckBox.isSelected()) {

							_mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex)
									.selectText(_result, _searchTextField.getText().length());

							_logger.info(_labels.getString("s583") + " "
									+ _searchTextField.getText() + " "
									+ _labels.getString("s574"));
							_mainWindow.getStatusBar().setMessage(
									_labels.getString("s583") + " "
											+ _searchTextField.getText() + " "
											+ _labels.getString("s574"));
						} else {
							_mainWindow
									.getEditorBuilder()
									.getEditorAt(_selectedEditorIndex)
									.selectText(
											_result,
											_search.getRegularExpresion()
													.length());
							_logger.info(_labels.getString("s577") + " "
									+ _search.getRegularExpresion()
									+ _labels.getString("s577"));
							_mainWindow.getStatusBar().setMessage(
									_labels.getString("s577") + " "
											+ _search.getRegularExpresion()
											+ _labels.getString("s577"));
						}
					} else {
						_logger.info(_labels.getString("s573"));
						_mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex)
								.getEditor().setCaretPosition(0);
						if (_forwardRadioButton.isSelected() == true)
							_selectedEditorIndex++;
						else if (_backwardRadioButton.isSelected() == true)
							_selectedEditorIndex--;
						else
							_selectedEditorIndex++;
						
						if (direction == Direction.FORWARD) {
							if (_selectedEditorIndex >= selectedEditor) {
								_isEnd = true;
							} else {
								_mainWindow.getEditorBuilder()
										.setSelectedEditorAt(_selectedEditorIndex);
								_mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex)
										.getEditor().setCaretPosition(0);
								_searchButton.doClick();
							}
						}
						if (direction == Direction.BACKWARD) {
							if (_selectedEditorIndex < 0) {
								_isEnd = true;
							} else {
								_mainWindow.getEditorBuilder()
										.setSelectedEditorAt(_selectedEditorIndex);
								_mainWindow
										.getEditorBuilder()
										.getEditorAt(_selectedEditorIndex)
										.getEditor()
										.setCaretPosition(
												_mainWindow.getEditorBuilder()
														.getEditorAt(_selectedEditorIndex)
														.getEditor().getText()
														.length() - 1);
								_searchButton.doClick();
							}
						}
						if (direction == Direction.BOTH) {
							if (_selectedEditorIndex >= selectedEditor) {
								_selectedEditorIndex = 0;
								_isCycle = true;
								_mainWindow.getEditorBuilder()
										.setSelectedEditorAt(_selectedEditorIndex);
								_mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex)
										.getEditor().setCaretPosition(0);
								_searchButton.doClick();

							} else {
								_mainWindow.getEditorBuilder()
										.setSelectedEditorAt(_selectedEditorIndex);
								_mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex)
										.getEditor().setCaretPosition(0);
								_searchButton.doClick();
							}
						}
					}
				}

				if (_isEnd && _isFirst) {
					if (_counter == 0) {
						searchGUI.setOnTop(false);
						JOptionPane.showMessageDialog(null,
								_labels.getString("s576"));
						searchGUI.setOnTop(true);
						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s576"));
					} else {
						searchGUI.setOnTop(false);
						JOptionPane.showMessageDialog(null,
								_labels.getString("s586"));
						searchGUI.setOnTop(true);
						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s586"));
					}
					_isFirst = false;
				}
			}
		}
	}

	/**
	 * Listener for the search button.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class CancelButtonListener implements ActionListener {
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			_instance.dispose();
		}
	}
	
	/**
	 * Returns the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public static SearchGUI getInstance() {
		if (_instance == null)
			_instance = new SearchGUI();
		return _instance;
	}

	/**
	 * Initialize the instance.
	 */
	public void inicialize() {
		_instance = null;
	}

	/**
	 * Returns the search button.
	 * 
	 * @return The search button.
	 */
	public JButton getSearchButton() {
		return _searchButton;
	}

	/**
	 * Returns if is cycle or not.
	 * 
	 * @return True if it is cycle.
	 */
	public boolean isCycle() {
		return _isCycle;
	}

	/**
	 * Set a new value to is cycle flag.
	 * 
	 * @param isCycle New value.
	 */
	public void setCycle(boolean isCycle) {
		_isCycle = isCycle;
	}

	/**
	 * Returns is end flag.
	 * 
	 * @return True if it is end.
	 */
	public boolean isEnd() {
		return _isEnd;
	}

	/**
	 * Set a new value to is end flag.
	 * 
	 * @param isEnd New value.
	 */
	public void setEnd(boolean isEnd) {
		_isEnd = isEnd;
	}

	/**
	 * Set a new value to current position.
	 * 
	 * @param currentPosition New value.
	 */
	public void setCurrentPosition(int currentPosition) {
		_currentPosition = currentPosition;
	}

	/**
	 * Returns the search.
	 * 
	 * @return The search.
	 */
	public Search getSearch() {
		return _search;
	}

	/**
	 * Returns the current document radio button.
	 * 
	 * @return The current document radio button.
	 */
	public JRadioButton getCurrentDocumentRadioButton() {
		return _currentDocumentRadioButton;
	}

	/**
	 * Set a new value to current document radio button.
	 * 
	 * @param currentDocumentRadioButton New value to set.
	 */
	public void setCurrentDocumentRadioButton(boolean currentDocumentRadioButton) {
		_currentDocumentRadioButton.setSelected(currentDocumentRadioButton);
	}

	/**
	 * Returns the all radio button.
	 *  
	 * @return The all radio button.
	 */
	public JRadioButton getAllRadioButton() {
		return _allRadioButton;
	}

	/**
	 * Returns the selected radio button.
	 * 
	 * @return The selected radio button.
	 */
	public JRadioButton getSelectedRadioButton() {
		return _selectedRadioButton;
	}

	/**
	 * Set a new value to the search text field text.
	 * 
	 * @param text New value to set.
	 */
	public void setSearchTextFieldText(String text) {
		_searchTextField.setText(text);
	}

	/**
	 * Set a new value to forward radio button.
	 * 
	 * @param forwardRadioButton New value to set.
	 */
	public void setForwardRadioButton(boolean forwardRadioButton) {
		_forwardRadioButton.setSelected(forwardRadioButton);
	}

	/**
	 * Set a new value to is first flag.
	 * 
	 * @param isFirst New value to set.
	 */
	public static void setFirst(boolean isFirst) {
		_isFirst = isFirst;
	}

	/**
	 * Set a new value to selectedText.
	 *  
	 * @param selectedText New value to set.
	 */
	public void setSelectedText(String selectedText) {
		_selectedText = selectedText;
	}

	/**
	 * Set the window on top.
	 * 
	 * @param b If it is true set the window on the top and don't do it in other case.
	 */
	public void setOnTop(boolean b) {
		setAlwaysOnTop(b);
	}
}

