package gui.menu.edit;

import gui.MainWindow;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import javax.swing.JTextField;

import language.Language;
import operations.factory.OperationsFactory;
import operations.listeners.AcideKeyboardListener;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/**
 * Replace GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ReplaceGUI extends JFrame {

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
	private static ReplaceGUI _instance;
	/**
	 * Main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * Button panel.
	 */
	private JPanel _buttonPanel;
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
	 * Button group.
	 */
	private ButtonGroup _buttonGroup;
	/**
	 * Replace label.
	 */
	private JLabel _replaceLabel;
	/**
	 * Replace text field.
	 */
	private JTextField _replaceTextField;
	/**
	 * Replaced label.
	 */
	private JLabel _replacedLabel;
	/**
	 * Replaced text field.
	 */
	private JTextField _replacedTextField;
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
	 * Search button.
	 */
	private JButton _searchButton;
	/**
	 * Replace button.
	 */
	private JButton _replaceButton;
	/**
	 * Replace all button.
	 */
	private JButton _replaceAllButton;
	/**
	 * Cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Result of the operations.
	 */
	private int _result;
	/**
	 * Initial position.
	 */
	private int _initialPosition;
	/**
	 * Final position.
	 */
	private int _finalPosition;
	/**
	 * Selected text.
	 */
	private String _selectedText;
	/**
	 * Counter for the matches.
	 */
	private int _counter;
	/**
	 * Current position.
	 */
	private int _currentPosition;
	/**
	 * Selected editor index.
	 */
	private static int _selectedEditorIndex;
	/**
	 * Flag that indicates if it is the end of a search.
	 */
	private static boolean _isEnd = false;
	/**
	 * Flag that indicates if there is a cycle in the search.
	 */
	private static boolean _isCycle = false;
	/**
	 * Flag that indicates if it is the first search.
	 */
	private static boolean _isFirstSearch;
	/**
	 * Current document.
	 */
	private int _currentDocument;
	/**
	 * Flag that indicates if it is the first replacement.
	 */
	private static boolean _isFirstReplacement = true;
	/**
	 * Labels to display.
	 */
	private ResourceBundle _labels;
	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();
	/**
	 * Search class.
	 */
	private Search _search = OperationsFactory.getInstance().buildSearch();
	/**
	 * Main window of the application.
	 */
	private MainWindow _mainWindow = MainWindow.getInstance();

	/**
	 * Constructor of the class.
	 */
	public ReplaceGUI() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		_labels = language.getLabels();

		// INITIAL VARIABLES
		_initialPosition = 0;
		_selectedText = null;
		_result = -1;

		// FRAME
		setLayout(new GridBagLayout());
		setSize(new Dimension(550, 390));
		setTitle(_labels.getString("s572"));
		setIconImage(new ImageIcon(ICON).getImage());

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout());
		
		// MAIN PANEL
		_mainPanel = new JPanel(new GridBagLayout());
		
		// DIRECTION PANEL
		_directionPanel = new JPanel();
		_directionPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s567")));
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
		_optionPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s559")));
		_optionPanel.setLayout(new GridLayout(0, 1));
		_caseSensitiveCheckBox = new JCheckBox(_labels.getString("s560"), false);
		_regularExpressionsCheckBox = new JCheckBox(_labels.getString("s561"), false);
		_completeWordsCheckBox = new JCheckBox(_labels.getString("s562"), false);
		_optionPanel.add(_caseSensitiveCheckBox);
		_optionPanel.add(_regularExpressionsCheckBox);
		_optionPanel.add(_completeWordsCheckBox);
		
		// SCORE PANEL
		_scopePanel = new JPanel();
		_scopePanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s563")));
		_scopePanel.setLayout(new GridLayout(0, 1));
		_buttonGroup = new ButtonGroup();
		_currentDocumentRadioButton = new JRadioButton(_labels.getString("s565"), true);
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
		
		// REPLACE BUTTON
		_replaceButton = new JButton();
		_replaceButton.setText(_labels.getString("s572"));
		_replaceButton.setEnabled(true);
		
		// REPLACE ALL BUTTON
		_replaceAllButton = new JButton();
		_replaceAllButton.setText(_labels.getString("s571"));
		_replaceAllButton.setEnabled(true);
		
		// CANCEL BUTTON
		_cancelButton = new JButton();
		_cancelButton.setText(_labels.getString("s42"));
		
		// REPLACE 
		_replaceLabel = new JLabel(_labels.getString("s557"), JLabel.CENTER);
		_replaceTextField = new JTextField();
		_replaceTextField.setText("");
		
		// REPLACED
		_replacedLabel = new JLabel(_labels.getString("s558"), JLabel.CENTER);
		_replacedTextField = new JTextField();
		_replacedTextField.setText("");

		// ADD THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 2;
		constraints.insets = new Insets(5, 5, 5, 5);
		_mainPanel.add(_replaceLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 300;
		_mainPanel.add(_replaceTextField, constraints);

		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.gridwidth = 2;
		constraints.insets = new Insets(5, 5, 5, 5);
		_mainPanel.add(_replacedLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 3;
		constraints.ipadx = 300;
		_mainPanel.add(_replacedTextField, constraints);

		constraints.fill = GridBagConstraints.CENTER;
		constraints.gridy = 4;
		constraints.gridx = 0;
		_mainPanel.add(_optionPanel, constraints);
		constraints.fill = GridBagConstraints.CENTER;
		constraints.gridy = 5;
		constraints.gridx = 0;
		constraints.ipadx = 100;
		constraints.gridwidth = 1;
		_mainPanel.add(_scopePanel, constraints);
		constraints.gridy = 5;
		constraints.gridx = 1;
		_mainPanel.add(_directionPanel, constraints);

		_buttonPanel.add(_searchButton);
		_buttonPanel.add(_replaceButton);
		_buttonPanel.add(_replaceAllButton);
		_buttonPanel.add(_cancelButton);
		constraints.fill = GridBagConstraints.CENTER;
		constraints.gridy = 6;
		constraints.gridx = 0;
		constraints.gridwidth = 2;
		constraints.insets = new Insets(0, 0, 0, 0);
		_mainPanel.add(_buttonPanel, constraints);
		
		add(_mainPanel);
		setResizable(false);
		setAlwaysOnTop(true);
		setLocationRelativeTo(null);
		
		// LISTENERS
		_selectedRadioButton.addKeyListener(new AcideKeyboardListener());
		_replaceTextField.addKeyListener(new AcideKeyboardListener());
		_replacedTextField.addKeyListener(new AcideKeyboardListener());
		_caseSensitiveCheckBox.addKeyListener(new AcideKeyboardListener());
		_regularExpressionsCheckBox.addKeyListener(new AcideKeyboardListener());
		_completeWordsCheckBox.addKeyListener(new AcideKeyboardListener());
		_forwardRadioButton.addKeyListener(new AcideKeyboardListener());
		_backwardRadioButton.addKeyListener(new AcideKeyboardListener());
		_allRadioButton.addKeyListener(new AcideKeyboardListener());
		_replaceButton.addKeyListener(new AcideKeyboardListener());
		_replaceAllButton.addKeyListener(new AcideKeyboardListener());
		_allDocumentsRadioButton.addKeyListener(new AcideKeyboardListener());
		_currentDocumentRadioButton.addKeyListener(new AcideKeyboardListener());
		_searchButton.addKeyListener(new AcideKeyboardListener());
		_searchButton.addActionListener(new SearchButtonListener());
		_replaceButton.addActionListener(new ReplaceButtonListener());
		_replaceAllButton.addActionListener(new ReplaceAllButtonListener());
		_cancelButton.addActionListener(new CancelButtonListener());
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
	 * Set a new value to is cycle flag.
	 * 
	 * @param isCycle New value to set.
	 */
	public void setCycle(boolean isCycle) {
		_isCycle = isCycle;
	}

	/**
	 * Set a new value to is end flag.
	 * 
	 * @param isEnd New value to set.
	 */
	public void setEnd(boolean isEnd) {
		_isEnd = isEnd;
	}

	/**
	 * Set a new value to current position.
	 * 
	 * @param currentPosition New value to set.
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
	 * Return the all radio button.
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
	 * Set a new value to is first search flag.
	 * 
	 * @param isFirstSearch New value to set.
	 */
	public static void setIsFirstSearch(boolean isFirstSearch) {
		_isFirstSearch = isFirstSearch;
	}

	/**
	 * Returns the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public static ReplaceGUI getInstance() {
		if (_instance == null)
			_instance = new ReplaceGUI();
		return _instance;
	}

	/**
	 * Initialize the instance.
	 */
	public void inicialize() {
		_instance = null;
	}
	
	/**
	 * Set a new value to replace text field.
	 * 
	 * @param text New value to set.
	 */
	public void setReplaceTextField(String text) {
		_replaceTextField.setText(text);
	}

	/**
	 * Set a new value to selected text.
	 * 
	 * @param selectedText New value to set.
	 */
	public void setSelectedText(String selectedText) {
		_selectedText = selectedText;
	}
	
	/**
	 * Set a new value to is first replacement flag.
	 * 
	 * @param isFirstReplacement New value to set.
	 */
	public static void setIsFirstReplacement(boolean isFirstReplacement) {
		_isFirstReplacement = isFirstReplacement;
	}

	/**
	 * Set the window on the top.
	 * 
	 * @param b True if it set on the top and false in other case.
	 */
	public void setTop(boolean b) {
		setAlwaysOnTop(b);
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
			
			ReplaceGUI replaceGUI = ReplaceGUI.getInstance();
			
			_isFirstReplacement = false;
			
			// GET THE SEARCH DIRECTION
			Direction direccion = Direction.FORWARD;
	
			if (_forwardRadioButton.isSelected())
				direccion = Direction.FORWARD;
			if (_backwardRadioButton.isSelected()) 
				direccion = Direction.BACKWARD;
			if (_allRadioButton.isSelected())
				direccion = Direction.BOTH;
			
			if (_completeWordsCheckBox.isSelected())
				_regularExpressionsCheckBox.setSelected(false);
			
			if (_replaceTextField.getText().equals("")) {
				
				replaceGUI.setTop(false);
				JOptionPane.showMessageDialog(null, _labels.getString("s585"));
				replaceGUI.setTop(true);
				_mainWindow.getStatusBar().setMessage(_labels.getString("s585"));
			}
			
			int selectedEditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
			
			if (_currentDocumentRadioButton.isSelected()) {
				_selectedEditorIndex = -1;
				_counter = 0;
				_result = -1;
				_selectedText = null;
				
				selectedEditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
				_result = _mainWindow.getEditorBuilder().getEditorAt(selectedEditor).getEditor()
						.getCaretPosition();
				
				if (direccion == Direction.BACKWARD)
					_result = _mainWindow.getEditorBuilder().getEditorAt(selectedEditor)
							.getEditor().getSelectionStart();
				
				selectedEditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();

				_result = _search.search(_result, _replaceTextField.getText(), _mainWindow.getEditorBuilder()
						.getEditorAt(selectedEditor).getText(),
						_caseSensitiveCheckBox.isSelected(), _regularExpressionsCheckBox.isSelected(),
						_completeWordsCheckBox.isSelected(), direccion.ordinal());

				if (_result != -1) {
					
					// SHOWS THE SEARCH IN THE TEXT EDITOR
					_mainWindow.getEditorBuilder()
							.getEditorAt(
									_mainWindow.getEditorBuilder().getSelectedEditorIndex())
							.selectText(_result, _replaceTextField.getText().length());

					// UPDATES THE LOG
					_logger.info(_labels.getString("s583") + _replaceTextField.getText()
							+ _labels.getString("s574"));
					
					// UPDATES THE STATUS BAR
					_mainWindow.getStatusBar().setMessage(
							_labels.getString("s583") + " " + _replaceTextField.getText() + " "
									+ _labels.getString("s574"));

					if (_regularExpressionsCheckBox.isSelected() == true) {

						// SHOW THE SEARCH IN THE TEXT EDITOR
						_mainWindow.getEditorBuilder()
								.getEditorAt(
										_mainWindow.getEditorBuilder()
												.getSelectedEditorIndex())
								.selectText(_result,
										_search.getRegularExpresion().length());

						// UPDATES THE LOG
						_logger.info(_labels.getString("s577") + " "
								+ _search.getRegularExpresion() + " "
								+ _labels.getString("s574"));
						
						// UPDATES THE STATUS BAR
						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s577") + " "
										+ _search.getRegularExpresion() + " "
										+ _labels.getString("s574"));
					}
				}
				else {
					_logger.info(_labels.getString("s573"));
					replaceGUI.setTop(false);
					JOptionPane.showMessageDialog(null,
							_labels.getString("s573"));
					replaceGUI.setTop(true);
					_mainWindow.getStatusBar().setMessage(_labels.getString("s573"));
				}
			}

			// SELECTED TEXT
			if (_selectedRadioButton.isSelected()) {
				
				_counter = 0;
				_selectedEditorIndex = -1;
				selectedEditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
				
				if (_selectedText == null) {
					
					_selectedText = _mainWindow.getEditorBuilder().getEditorAt(selectedEditor)
							.getEditor().getSelectedText();
					_initialPosition = _mainWindow.getEditorBuilder().getEditorAt(selectedEditor)
							.getEditor().getSelectionStart();
					_finalPosition = _mainWindow.getEditorBuilder().getEditorAt(selectedEditor)
							.getEditor().getSelectionEnd();
					_result = 0;
					
					if (direccion == Direction.BACKWARD)
						_result = _finalPosition;
					
					if ((_regularExpressionsCheckBox.isSelected()) && (direccion != Direction.BACKWARD))
						_result = _initialPosition;
					
				} else {

					_result = _mainWindow.getEditorBuilder().getEditorAt(selectedEditor)
							.getEditor().getCaretPosition()
							- _initialPosition;

					if (direccion == Direction.BACKWARD) {
						_result = _mainWindow.getEditorBuilder().getEditorAt(selectedEditor)
								.getEditor().getSelectionStart()
								- _initialPosition;
					}
				}

				if (_selectedText == null) {
					replaceGUI.setTop(false);
					JOptionPane.showMessageDialog(null,
							_labels.getString("s616"));
					replaceGUI.setTop(true);
					_mainWindow.getStatusBar().setMessage(_labels.getString("s616"));
				}

				else {
					
					_result = _search.search(_result, _replaceTextField.getText(), _selectedText,
							_caseSensitiveCheckBox.isSelected(), _regularExpressionsCheckBox.isSelected(),
							_completeWordsCheckBox.isSelected(), direccion.ordinal());

					if (_result != -1) {
						
						// SHOWS THE SEARCH IN THE TEXT EDITOR
						_mainWindow.getEditorBuilder()
								.getEditorAt(
										_mainWindow.getEditorBuilder()
												.getSelectedEditorIndex())
								.selectText(_result + _initialPosition, _replaceTextField.getText().length());

						// UPDATES THE LOG
						_logger.info(_labels.getString("s583") + _replaceTextField.getText()
										+ _labels.getString("s574"));

						// UPDATES THE STATUS BAR
						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s583") + " " + _replaceTextField.getText()
										+ " " + _labels.getString("s574"));

						if (_regularExpressionsCheckBox.isSelected()) {

							// SHOWS THE SEARCH IN THE TEXT EDITOR
							_mainWindow.getEditorBuilder()
									.getEditorAt(
											_mainWindow.getEditorBuilder()
													.getSelectedEditorIndex())
									.selectText(_result + _initialPosition,
											_search.getRegularExpresion().length());
							
							// UPDATES THE LOG
							_logger.info(_labels.getString("s329") + " "
									+ _search.getRegularExpresion() + " "
									+ _labels.getString("s574"));
							
							// UPDATES THE STATUS BAR
							_mainWindow.getStatusBar().setMessage(
									_labels.getString("s329") + " "
											+ _replaceTextField.getText() + " "
											+ _labels.getString("s574"));
						}
					}
					else {
						
						_selectedText = null;
						_logger.info(_labels.getString("s573"));
						replaceGUI.setTop(false);
						JOptionPane.showMessageDialog(null,
								_labels.getString("s573"));
						_mainWindow.getStatusBar().setMessage(_labels.getString("s573"));
						
						int option = JOptionPane.showConfirmDialog(null,
								_labels.getString("s575"));
						replaceGUI.setTop(true);
						
						if (option == JOptionPane.OK_OPTION) {
							_currentDocumentRadioButton.setSelected(true);
							
							if (direccion != Direction.BACKWARD)
								_result = _finalPosition;
							else
								_result = _initialPosition;
							
							_searchButton.doClick();
						}
					}
				}
			}
			
			// ALL DOCUMENTS SEARCH
			if (_allDocumentsRadioButton.isSelected()) {
				
				_selectedText = null;
				selectedEditor = _mainWindow.getEditorBuilder().getNumEditors();
				
				if (!_isCycle && _currentPosition == -2) {
					_selectedEditorIndex = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
					_currentDocument = _selectedEditorIndex;
					_currentPosition = _mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex).getEditor()
							.getCaretPosition();
				}
				if (!_isEnd) {
					if (direccion == Direction.FORWARD)
						_result = _mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex).getEditor()
								.getCaretPosition();
					if (direccion == Direction.BACKWARD)
						_result = _mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex).getEditor()
								.getSelectionStart();
					if (direccion == Direction.BOTH) {
						_result = _mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex).getEditor()
								.getCaretPosition();
					}
					
					Direction auxDirection = direccion;
					
					if (direccion == Direction.BOTH)
						auxDirection = Direction.FORWARD;
					
					_result = _search.search(_result, _replaceTextField.getText(), _mainWindow.getEditorBuilder()
							.getEditorAt(_selectedEditorIndex).getText(), _caseSensitiveCheckBox.isSelected(),
							_regularExpressionsCheckBox.isSelected(), _completeWordsCheckBox.isSelected(), auxDirection.ordinal());
					
					if (_isCycle && (_selectedEditorIndex == _currentDocument)
							&& (_result >= _currentPosition))
						_isEnd = true;
					else if (_isCycle && (_selectedEditorIndex == _currentDocument) && (_result == -1))
						_isEnd = true;
					if (_result != -1) {
						
						_counter++;
						
						if (!_regularExpressionsCheckBox.isSelected()) {
							
							// SHOWS THE SEARCH IN THE TEXT EDITOR
							_mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex)
									.selectText(_result, _replaceTextField.getText().length());
							
							// UPDATES THE LOG
							_logger.info(_labels.getString("s583") + " "
									+ _replaceTextField.getText() + " "
									+ _labels.getString("s574"));
							
							// UPDATES THE STATUS BAR
							_mainWindow.getStatusBar().setMessage(
									_labels.getString("s583") + " "
											+ _replaceTextField.getText() + " "
											+ _labels.getString("s574"));
						} else {
							
							// SHOWS THE SEARCH IN THE TEXT EDITOR
							_mainWindow.getEditorBuilder()
									.getEditorAt(_selectedEditorIndex)
									.selectText(_result,
											_search.getRegularExpresion().length());
							
							// UPDATES THE LOG
							_logger.info(_labels.getString("s577") + " "
									+ _search.getRegularExpresion() + " "
									+ _labels.getString("s577"));
							
							// UPDATES THE STATUS BAR
							_mainWindow.getStatusBar().setMessage(
									_labels.getString("s577") + " "
											+ _search.getRegularExpresion() + " "
											+ _labels.getString("s577"));
						}

					} else {
						
						_logger.info(_labels.getString("s573"));
						_mainWindow.getStatusBar().setMessage(_labels.getString("s573"));
						_mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex).getEditor()
								.setCaretPosition(0);
						
						if (_forwardRadioButton.isSelected())
							_selectedEditorIndex++;
						else if (_backwardRadioButton.isSelected())
							_selectedEditorIndex--;
						else
							_selectedEditorIndex++;
						
						if (direccion == Direction.FORWARD) {
							if (_selectedEditorIndex >= selectedEditor) {
								_isEnd = true;
							} else {
								_mainWindow.getEditorBuilder().setSelectedEditorAt(_selectedEditorIndex);
								_mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex).getEditor()
										.setCaretPosition(0);

								_searchButton.doClick();
							}
						}
						
						if (direccion == Direction.BACKWARD) {
							if (_selectedEditorIndex < 0) {
								_isEnd = true;
							} else {
								_mainWindow.getEditorBuilder().setSelectedEditorAt(_selectedEditorIndex);
								_mainWindow.getEditorBuilder()
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
						
						if (direccion == Direction.BOTH) {
							if (_selectedEditorIndex >= selectedEditor) {
								_selectedEditorIndex = 0;
								_isCycle = true;
								_mainWindow.getEditorBuilder().setSelectedEditorAt(_selectedEditorIndex);
								_mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex).getEditor()
										.setCaretPosition(0);
								_searchButton.doClick();

							} else {
								_mainWindow.getEditorBuilder().setSelectedEditorAt(_selectedEditorIndex);
								_mainWindow.getEditorBuilder().getEditorAt(_selectedEditorIndex).getEditor()
										.setCaretPosition(0);
								_searchButton.doClick();
							}
						}
					}
				}

				if (_isEnd && _isFirstSearch) {
					
					if (_counter == 0) {
						replaceGUI.setTop(false);
						JOptionPane.showMessageDialog(null,
								_labels.getString("s576"));
						replaceGUI.setTop(true);
						_mainWindow.getStatusBar().setMessage(_labels.getString("s576"));
					} else {
						replaceGUI.setTop(false);
						JOptionPane.showMessageDialog(null,
								_labels.getString("s586"));
						replaceGUI.setTop(true);
						_mainWindow.getStatusBar().setMessage(_labels.getString("s586"));
					}
					_isFirstSearch = false;
				}
			}
		}
	}

	/**
	 * Listener for the cancel button.
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
	 * Listener for the replace button.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class ReplaceButtonListener implements ActionListener {
		
		/**
		 * Original position.
		 */
		int _originalPosition = -1;
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			if (_selectedRadioButton.isSelected()) {
				
				int selectedEditorIndex = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
				String text = null;
				
				if (_isFirstReplacement == true)
					_searchButton.doClick();
				
				text = _mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex).getEditor()
						.getSelectedText();
				
				if (_result != -1) {
					if (text != null) {
						
						int position;
						ReplaceGUI replaceGUI = ReplaceGUI.getInstance();

						if (replaceGUI._forwardRadioButton.isSelected())
							position = _mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex)
									.getEditor().getSelectionEnd();
						else
							position = _mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex)
									.getEditor().getSelectionStart();

						_mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex).getEditor()
								.replaceSelection(_replacedTextField.getText());
						
						if (replaceGUI._forwardRadioButton.isSelected())
							_selectedText = replaceGUI._selectedText.replaceFirst(_replaceTextField.getText(),
									_replacedTextField.getText());
						_mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex).getEditor()
								.setCaretPosition(position);
						
						// UPDATES THE LOG
						_logger.info(_labels.getString("s583") + " "
								+ _replaceTextField.getText() + " " + _labels.getString("s580")
								+ " " + _replacedTextField.getText());
						
						// UPDATES THE STATUS BAR
						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s583") + " " + _replaceTextField.getText()
										+ " " + _labels.getString("s580") + " "
										+ _replacedTextField.getText());
						_isFirstReplacement = false;
						_searchButton.doClick();
					}
				}
			}
			if ((_currentDocumentRadioButton.isSelected())
					|| (_allDocumentsRadioButton.isSelected())) {

				int numeditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();

				if (_isFirstReplacement) {
					_originalPosition = _mainWindow.getEditorBuilder().getEditorAt(numeditor)
							.getEditor().getCaretPosition();
					_searchButton.doClick();
				}
				if (_mainWindow.getEditorBuilder().getEditorAt(numeditor).getEditor()
						.getSelectedText() != null) {
					_mainWindow.getEditorBuilder().getEditorAt(numeditor).getEditor()
							.replaceSelection(_replacedTextField.getText());
					
					// UPDATES THE LOG
					_logger.info(_labels.getString("s579") + " " + _replaceTextField.getText()
							+ " " + _labels.getString("s580") + " "
							+ _replacedTextField.getText());

					// UPDATES THE STATUS BAR
					_mainWindow.getStatusBar().setMessage(
							_labels.getString("s579") + " " + _replaceTextField.getText() + " "
									+ _labels.getString("s580") + " "
									+ _replacedTextField.getText());
					_isFirstReplacement = false;
					_searchButton.doClick();
				}
				if (_mainWindow.getEditorBuilder().getEditorAt(numeditor).getEditor()
						.getSelectedText() == null)
					_mainWindow.getEditorBuilder().getEditorAt(numeditor).getEditor()
							.setCaretPosition(_originalPosition);
			}
		}
	}

	/**
	 * Listener for the replace all button.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class ReplaceAllButtonListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			ReplaceGUI replaceGUI = ReplaceGUI.getInstance();
			String selectedText = null;
			String result = null;
			String textPre = null;
			String textPos = null;
			
			int selectedEditorIndex = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
			int size = _mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex)
					.getEditor().getText().length();
			
			if (_selectedRadioButton.isSelected() == true) {
				
				replaceGUI.setTop(false);
				
				int option = JOptionPane.showConfirmDialog(null,
						_labels.getString("s581"));
				
				replaceGUI.setTop(true);
				
				if (option == JOptionPane.OK_OPTION) {
					
					int selectedStart = _mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex)
							.getEditor().getSelectionStart();
					
					selectedText = _mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex)
							.getEditor().getSelectedText();
					
					int f = selectedStart + selectedText.length();
					String text = _mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex)
							.getEditor().getText();
					
					textPre = text.substring(0, selectedStart);
					textPos = text.substring(f, size);
					result = selectedText.replaceAll(_replaceTextField.getText(), _replacedTextField.getText());
					String temp = textPre.concat(result);
					result = null;
					result = temp.concat(textPos);
					_mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex).getEditor()
							.setText(result);

				}
			}

			if (_currentDocumentRadioButton.isSelected()) {
				
				selectedEditorIndex = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
				int position = _mainWindow.getEditorBuilder().getSelectedEditor()
						.getEditor().getCaretPosition();
				selectedText = _mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex).getEditor()
						.getText();
				replaceGUI.setTop(false);
				
				int option = JOptionPane.showConfirmDialog(null,
						_labels.getString("s581"));
				replaceGUI.setTop(true);
				if (option == JOptionPane.OK_OPTION) {
					result = selectedText.replaceAll(_replaceTextField.getText(), _replacedTextField.getText());
					_mainWindow.getEditorBuilder().getEditorAt(selectedEditorIndex).getEditor()
							.setText(result);
					_mainWindow.getEditorBuilder().getSelectedEditor().getEditor()
							.setCaretPosition(position);
					
					// UPDATES THE LOG
					_logger.info(_labels.getString("s582") + _replaceTextField.getText()
									+ _labels.getString("s580") + _replacedTextField.getText());
				}
			} else if (_allDocumentsRadioButton.isSelected() == true) {
				
				selectedEditorIndex = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
				
				int position = _mainWindow.getEditorBuilder().getSelectedEditor()
						.getEditor().getCaretPosition();
				
				int numEditors = _mainWindow.getEditorBuilder().getNumEditors();
				replaceGUI.setTop(false);
				
				int option = JOptionPane.showConfirmDialog(null,
						_labels.getString("s581"));
				replaceGUI.setTop(true);
				if (option == JOptionPane.OK_OPTION) {
					
					for (int j = 0; j < numEditors; j++) {
						
						selectedText = _mainWindow.getEditorBuilder().getEditorAt(j).getText();
						result = selectedText.replaceAll(_replaceTextField.getText(), _replacedTextField.getText());
						_mainWindow.getEditorBuilder().setSelectedEditorAt(j);
						_mainWindow.getEditorBuilder().getEditorAt(j).getEditor()
								.setText(result);
					}
				}
				_mainWindow.getEditorBuilder().setSelectedEditorAt(selectedEditorIndex);
				_mainWindow.getEditorBuilder().getSelectedEditor().getEditor()
						.setCaretPosition(position);
			}
		}
	}
}
