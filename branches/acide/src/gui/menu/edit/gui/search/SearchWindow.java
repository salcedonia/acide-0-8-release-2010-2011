package gui.menu.edit.gui.search;

import gui.mainWindow.MainWindow;
import gui.menu.edit.utils.Direction;
import gui.menu.edit.utils.Search;

import java.awt.Color;
import java.awt.Cursor;
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

import operations.factory.OperationsFactory;
import operations.listeners.AcideKeyboardListener;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************
 * Search window of ACIDE - A Configurable IDE
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
 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan Jos� Ortiz S�nchez
 *         </ul>
 *         <ul>
 *         Delf�n Rup�rez Ca�as
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Mart�n L�zaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo G�mez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 * @see JFrame
 ***********************************************************************/
public class SearchWindow extends JFrame {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the window icon
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Class instance
	 */
	private static SearchWindow _instance;
	/**
	 * Window main panel
	 */
	private JPanel _mainPanel;
	/**
	 * Window direction panel
	 */
	private JPanel _directionPanel;
	/**
	 * Window option panel
	 */
	private JPanel _optionPanel;
	/**
	 * Window scope panel
	 */
	private JPanel _scopePanel;
	/**
	 * Search label
	 */
	private JLabel _searchLabel;
	/**
	 * Search text field
	 */
	private JTextField _searchTextField;
	/**
	 * Case sensitive check box
	 */
	private JCheckBox _caseSensitiveCheckBox;
	/**
	 * Regular expressions check box
	 */
	private JCheckBox _regularExpressionsCheckBox;
	/**
	 * Complete words check box
	 */
	private JCheckBox _completeWordsCheckBox;
	/**
	 * Forward radio button
	 */
	private JRadioButton _forwardRadioButton;
	/**
	 * Backward radio button
	 */
	private JRadioButton _backwardRadioButton;
	/**
	 * All radio button
	 */
	private JRadioButton _allRadioButton;
	/**
	 * Current document radio button
	 */
	private JRadioButton _currentDocumentRadioButton;
	/**
	 * All documents radio button
	 */
	private JRadioButton _allDocumentsRadioButton;
	/**
	 * Selected text radio button
	 */
	private JRadioButton _selectedTextRadioButton;
	/**
	 * Button group
	 */
	private ButtonGroup _buttonGroup;
	/**
	 * Search button
	 */
	private JButton _searchButton;
	/**
	 * Cancel button
	 */
	private JButton _cancelButton;
	/**
	 * Result of the operation
	 */
	private int _result;
	/**
	 * Selected text
	 */
	private String _selectedText;
	/**
	 * Initial position
	 */
	private int _initialPosition;
	/**
	 * Final position
	 */
	private int _finalPosition;
	/**
	 * Selected editor Index
	 */
	private static int _selectedEditorIndex;
	/**
	 * Flag that indicates if the search is over or not
	 */
	private static boolean _isEnd = false;
	/**
	 * Current position of the search
	 */
	private int _currentPosition;
	/**
	 * Current document
	 */
	private int _currentDocument;
	/**
	 * Counter
	 */
	private int _counter;
	/**
	 * Flag that indicates if the search uses a cycle or not
	 */
	private static boolean _isCycle;
	/**
	 * Flag that indicates if it is the first searching
	 */
	private static boolean _isFirst;
	/**
	 * Search class
	 */
	private Search _search = OperationsFactory.getInstance().buildSearch();

	/**
	 * Class constructor
	 */
	public SearchWindow() {

		// Gets the language
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {

			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();

		// FRAME
		setTitle(labels.getString("s556"));
		setIconImage(new ImageIcon(ICON).getImage());

		// MAIN PANEL
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());

		// DIRECTION PANEL
		_directionPanel = new JPanel();
		_directionPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s567"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_directionPanel.setLayout(new GridLayout(0, 1));
		_buttonGroup = new ButtonGroup();
		_forwardRadioButton = new JRadioButton(labels.getString("s568"), true);
		_backwardRadioButton = new JRadioButton(labels.getString("s569"), false);
		_allRadioButton = new JRadioButton(labels.getString("s570"), false);
		_buttonGroup.add(_forwardRadioButton);
		_buttonGroup.add(_backwardRadioButton);
		_buttonGroup.add(_allRadioButton);
		_directionPanel.add(_forwardRadioButton);
		_directionPanel.add(_backwardRadioButton);
		_directionPanel.add(_allRadioButton);

		// OPTION PANEL
		_optionPanel = new JPanel();
		_optionPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s559"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_caseSensitiveCheckBox = new JCheckBox(labels.getString("s560"), false);
		_regularExpressionsCheckBox = new JCheckBox(labels.getString("s561"),
				false);
		_completeWordsCheckBox = new JCheckBox(labels.getString("s562"), false);
		_optionPanel.setLayout(new GridLayout(0, 1));
		_optionPanel.add(_caseSensitiveCheckBox);
		_optionPanel.add(_regularExpressionsCheckBox);
		_optionPanel.add(_completeWordsCheckBox);

		// SCOPE PANEL
		_scopePanel = new JPanel();
		_scopePanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s563"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		_scopePanel.setLayout(new GridLayout(0, 1));
		_buttonGroup = new ButtonGroup();
		_currentDocumentRadioButton = new JRadioButton(
				labels.getString("s565"), true);
		_allDocumentsRadioButton = new JRadioButton(labels.getString("s566"),
				false);
		_selectedTextRadioButton = new JRadioButton(labels.getString("s564"),
				false);
		_buttonGroup.add(_selectedTextRadioButton);
		_buttonGroup.add(_currentDocumentRadioButton);
		_buttonGroup.add(_allDocumentsRadioButton);
		_scopePanel.add(_selectedTextRadioButton);
		_scopePanel.add(_currentDocumentRadioButton);
		_scopePanel.add(_allDocumentsRadioButton);

		// SEARCH BUTTON
		_searchButton = new JButton();
		_searchButton.setText(labels.getString("s556"));
		_searchButton.setMnemonic(java.awt.event.KeyEvent.VK_F3);

		// CANCEL BUTTON
		_cancelButton = new JButton();
		_cancelButton.setText(labels.getString("s42"));

		// SEARCH
		_searchLabel = new JLabel(labels.getString("s557"), JLabel.CENTER);
		_searchTextField = new JTextField();
		_searchTextField.setText("");

		// ADD THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();

		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 2;
		constraints.insets = new Insets(5, 5, 5, 5);
		_mainPanel.add(_searchLabel, constraints);
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

		// Listeners
		_searchButton.addActionListener(new SearchButtonListener());
		_cancelButton.addActionListener(new CancelButtonListener());
		_searchTextField.addKeyListener(new AcideKeyboardListener());
		_caseSensitiveCheckBox.addKeyListener(new AcideKeyboardListener());
		_regularExpressionsCheckBox.addKeyListener(new AcideKeyboardListener());
		_completeWordsCheckBox.addKeyListener(new AcideKeyboardListener());
		_forwardRadioButton.addKeyListener(new AcideKeyboardListener());
		_backwardRadioButton.addKeyListener(new AcideKeyboardListener());
		_allRadioButton.addKeyListener(new AcideKeyboardListener());
		_selectedTextRadioButton.addKeyListener(new AcideKeyboardListener());
		_allDocumentsRadioButton.addKeyListener(new AcideKeyboardListener());
		_currentDocumentRadioButton.addKeyListener(new AcideKeyboardListener());
	}

	/**
	 * Returns the unique class instance
	 * 
	 * @return the unique class instance
	 */
	public static SearchWindow getInstance() {
		if (_instance == null)
			_instance = new SearchWindow();
		return _instance;
	}

	/**
	 * Initializes the class instance
	 */
	public void inicialize() {
		_instance = null;
	}

	/**
	 * Returns the search button
	 * 
	 * @return the search button
	 */
	public JButton getSearchButton() {
		return _searchButton;
	}

	/**
	 * Returns if is cycle or not
	 * 
	 * @return true if it is cycle
	 */
	public boolean getIsCycle() {
		return _isCycle;
	}

	/**
	 * Sets a new value to is cycle flag
	 * 
	 * @param isCycle
	 *            new value to set
	 */
	public void setIsCycle(boolean isCycle) {
		_isCycle = isCycle;
	}

	/**
	 * Returns is end flag
	 * 
	 * @return true if it is end
	 */
	public boolean getIsEnd() {
		return _isEnd;
	}

	/**
	 * Sets a new value to is end flag
	 * 
	 * @param isEnd
	 *            new value to set
	 */
	public void setIsEnd(boolean isEnd) {
		_isEnd = isEnd;
	}

	/**
	 * Sets a new value to current position
	 * 
	 * @param currentPosition
	 *            new value to set
	 */
	public void setCurrentPosition(int currentPosition) {
		_currentPosition = currentPosition;
	}

	/**
	 * Returns the search
	 * 
	 * @return the search
	 */
	public Search getSearch() {
		return _search;
	}

	/**
	 * Returns the current document radio button
	 * 
	 * @return the current document radio button
	 */
	public JRadioButton getCurrentDocumentRadioButton() {
		return _currentDocumentRadioButton;
	}

	/**
	 * Sets a new value to current document radio button
	 * 
	 * @param currentDocumentRadioButton
	 *            new value to set
	 */
	public void setCurrentDocumentRadioButton(boolean currentDocumentRadioButton) {
		_currentDocumentRadioButton.setSelected(currentDocumentRadioButton);
	}

	/**
	 * Returns the all radio button
	 * 
	 * @return the all radio button
	 */
	public JRadioButton getAllRadioButton() {
		return _allRadioButton;
	}

	/**
	 * Returns the selected text radio button
	 * 
	 * @return the selected text radio button
	 */
	public JRadioButton getSelectedTextRadioButton() {
		return _selectedTextRadioButton;
	}

	/**
	 * Sets a new value to the search text field text
	 * 
	 * @param text
	 *            new value to set
	 */
	public void setSearchTextFieldText(String text) {
		_searchTextField.setText(text);
	}

	/**
	 * Sets a new value to forward radio button
	 * 
	 * @param forwardRadioButton
	 *            new value to set
	 */
	public void setForwardRadioButton(boolean forwardRadioButton) {
		_forwardRadioButton.setSelected(forwardRadioButton);
	}

	/**
	 * Sets a new value to selectedText
	 * 
	 * @param selectedText
	 *            new value to set
	 */
	public void setSelectedText(String selectedText) {
		_selectedText = selectedText;
	}

	/**
	 * Sets the window on top
	 * 
	 * @param b
	 *            if it is true set the window on the top and don't do it in
	 *            other case
	 */
	public void setOnTop(boolean b) {
		setAlwaysOnTop(b);
	}

	/**
	 * Returns the result
	 * 
	 * @return the result
	 */
	public int getResult() {
		return _result;
	}

	/**
	 * Sets a new value to the result
	 * 
	 * @param result
	 *            new value to set
	 */
	public void setResult(int result) {
		_result = result;
	}

	/**
	 * Returns the selected text
	 * 
	 * @return the selected text
	 */
	public String getSelectedText() {
		return _selectedText;
	}

	/**
	 * Returns the initial position
	 * 
	 * @return the initial position
	 */
	public int getInitialPosition() {
		return _initialPosition;
	}

	/**
	 * Sets a new value to the initial position
	 * 
	 * @param initialPosition
	 *            new value to set
	 */
	public void setInitialPosition(int initialPosition) {
		_initialPosition = initialPosition;
	}

	/**
	 * Returns the final position
	 * 
	 * @return the final position
	 */
	public int getFinalPosition() {
		return _finalPosition;
	}

	/**
	 * Sets a new value to the final position
	 * 
	 * @param finalPosition
	 *            new value to set
	 */
	public void setFinalPosition(int finalPosition) {
		_finalPosition = finalPosition;
	}

	/**
	 * Returns the selected editor index
	 * 
	 * @return the selected editor index
	 */
	public static int getSelectedEditorIndex() {
		return _selectedEditorIndex;
	}

	/**
	 * Sets a new value to the selected editor index
	 * 
	 * @param selectedEditorIndex
	 *            new value to set
	 */
	public static void setSelectedEditorIndex(int selectedEditorIndex) {
		_selectedEditorIndex = selectedEditorIndex;
	}

	/**
	 * Returns the current position
	 * 
	 * @return the current position
	 */
	public int getCurrentPosition() {
		return _currentPosition;
	}

	/**
	 * Sets a new value to the current document
	 * 
	 * @param currentDocument
	 *            new value to set
	 */
	public void setCurrentDocument(int currentDocument) {
		_currentDocument = currentDocument;
	}

	/**
	 * Returns the current document
	 * 
	 * @return the current document
	 */
	public int getCurrentDocument() {
		return _currentDocument;
	}

	/**
	 * Sets a new value to the counter
	 * 
	 * @param counter
	 *            new value to set
	 */
	public void setCounter(int counter) {
		_counter = counter;
	}

	/**
	 * Returns the counter
	 * 
	 * @return the counter
	 */
	public int getCounter() {
		return _counter;
	}

	/**
	 * Sets a new value to the is first flag
	 * 
	 * @param _isFirst
	 *            new value to set
	 */
	public static void setIsFirst(boolean isFirst) {
		_isFirst = isFirst;
	}

	/**
	 * Returns the is first flag
	 * 
	 * @return the is first flag
	 */
	public static boolean getIsFirst() {
		return _isFirst;
	}

	/**
	 * Returns the forward radio button
	 * 
	 * @return the forward radio button
	 */
	public JRadioButton getForwardRadioButton() {
		return _forwardRadioButton;
	}

	/**
	 * Returns the backward radio button
	 * 
	 * @return the backward radio button
	 */
	public JRadioButton getBackwardRadioButton() {
		return _backwardRadioButton;
	}

	/**
	 * Returns the complete words check box
	 * 
	 * @return the complete words check box
	 */
	public JCheckBox getCompleteWordsCheckBox() {
		return _completeWordsCheckBox;
	}

	/**
	 * Returns the regular expressions check box
	 * 
	 * @return the regular expressions check box
	 */
	public JCheckBox getRegularExpressionsCheckBox() {
		return _regularExpressionsCheckBox;
	}

	/**
	 * Returns the search text field
	 * 
	 * @return the search text field
	 */
	public JTextField getSearchTextField() {
		return _searchTextField;
	}

	/**
	 * Returns the case sensitive check box
	 * 
	 * @return the case sensitive check box
	 */
	public JCheckBox getCaseSensitiveCheckBox() {
		return _caseSensitiveCheckBox;
	}

	/**
	 * Returns the all documents radio button
	 * 
	 * @return the all documents radio button
	 */
	public JRadioButton getAllDocumentsRadioButton() {
		return _allDocumentsRadioButton;
	}

	/************************************************************************
	 * Search button listener
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
	 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>
	 *         <li><b>Version 0.1-0.6:</b>
	 *         <ul>
	 *         Diego Cardiel Freire
	 *         </ul>
	 *         <ul>
	 *         Juan Jos� Ortiz S�nchez
	 *         </ul>
	 *         <ul>
	 *         Delf�n Rup�rez Ca�as
	 *         </ul>
	 *         </li>
	 *         <li><b>Version 0.7:</b>
	 *         <ul>
	 *         Miguel Mart�n L�zaro
	 *         </ul>
	 *         </li>
	 *         <li><b>Version 0.8:</b>
	 *         <ul>
	 *         Javier Salcedo G�mez
	 *         </ul>
	 *         </li>
	 *         </ul>
	 ************************************************************************ 
	 * @version 0.8
	 * @see ActionListener
	 ***********************************************************************/
	class SearchButtonListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Gets the language
			Language language = Language.getInstance();
			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception exception) {

				// Updates the log
				Log.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			// Gets the labels
			ResourceBundle labels = language.getLabels();

			// INITIALIZE THE VARIABLES
			_isCycle = false;
			_currentPosition = -2;
			_currentDocument = -1;
			_initialPosition = -1;
			_selectedText = null;
			_result = -1;
			_isFirst = true;

			SearchWindow searchWindow = SearchWindow.getInstance();
			MainWindow _mainWindow = MainWindow.getInstance();
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

				searchWindow.setOnTop(false);

				// INFORMS TO THE USER
				JOptionPane.showMessageDialog(null, labels.getString("s585"));
				searchWindow.setOnTop(true);
				_mainWindow.getStatusBar()
						.setMessage(labels.getString("s585"));
			} else {
				
				// Puts the wait cursor
				MainWindow.getInstance().setCursor(Cursor
						.getPredefinedCursor(Cursor.WAIT_CURSOR));
				
				int selectedEditor = _mainWindow.getEditorManager()
						.getSelectedEditorIndex();

				// SEARCH IN THE CURRENT DOCUMENT
				if (_currentDocumentRadioButton.isSelected()) {

					_selectedEditorIndex = -1;
					_counter = 0;
					_result = -1;
					_selectedText = null;
					selectedEditor = _mainWindow.getEditorManager()
							.getSelectedEditorIndex();
					_result = _mainWindow.getEditorManager()
							.getEditorAt(selectedEditor).getEditor()
							.getCaretPosition();

					// BACKWARD DIRECTION
					if (direction == Direction.BACKWARD) {
						_result = _mainWindow.getEditorManager()
								.getEditorAt(selectedEditor).getEditor()
								.getSelectionStart();
					}
					selectedEditor = _mainWindow.getEditorManager()
							.getSelectedEditorIndex();

					_result = _search.search(
							_result,
							_searchTextField.getText(),
							_mainWindow.getEditorManager()
									.getEditorAt(selectedEditor).getText(),
							_caseSensitiveCheckBox.isSelected(),
							_regularExpressionsCheckBox.isSelected(),
							_completeWordsCheckBox.isSelected(), direction);

					if (_result != -1) {

						_mainWindow
								.getEditorManager()
								.getEditorAt(
										_mainWindow.getEditorManager()
												.getSelectedEditorIndex())
								.selectText(_result,
										_searchTextField.getText().length());

						// Updates the log
						Log.getLog().info(
								labels.getString("s583") + " "
										+ _searchTextField.getText() + " "
										+ labels.getString("s574"));

						// Updates the status bar
						_mainWindow.getStatusBar().setMessage(
								labels.getString("s583") + " "
										+ _searchTextField.getText() + " "
										+ labels.getString("s574"));

						// IF REGULAR EXPRESSIONS
						if (_regularExpressionsCheckBox.isSelected()) {

							// SHOWS THE SEARCH IN THE TEXT EDITOR
							_mainWindow
									.getEditorManager()
									.getEditorAt(
											_mainWindow.getEditorManager()
													.getSelectedEditorIndex())
									.selectText(
											_result,
											_search.getRegularExpresion()
													.length());

							// Updates the status bar
							_mainWindow.getStatusBar().setMessage(
									labels.getString("s577") + " "
											+ _search.getRegularExpresion()
											+ " " + labels.getString("s574"));

							// Updates the log
							Log.getLog().info(
									labels.getString("s577") + " "
											+ _search.getRegularExpresion()
											+ " " + labels.getString("s574"));
						}
					} else {
						// Updates the log
						Log.getLog().info(labels.getString("s573"));

						searchWindow.setOnTop(false);
						JOptionPane.showMessageDialog(null,
								labels.getString("s573"));
						searchWindow.setOnTop(true);

						// Updates the status bar
						_mainWindow.getStatusBar().setMessage(
								labels.getString("s573"));
					}
				}

				// IF SELECT TEXT
				if (_selectedTextRadioButton.isSelected()) {

					_counter = 0;
					_selectedEditorIndex = -1;
					selectedEditor = _mainWindow.getEditorManager()
							.getSelectedEditorIndex();

					if (_selectedText == null) {

						_selectedText = _mainWindow.getEditorManager()
								.getEditorAt(selectedEditor).getEditor()
								.getSelectedText();
						_initialPosition = _mainWindow.getEditorManager()
								.getEditorAt(selectedEditor).getEditor()
								.getSelectionStart();
						_finalPosition = _mainWindow.getEditorManager()
								.getEditorAt(selectedEditor).getEditor()
								.getSelectionEnd();
						_result = 0;

						if (direction == Direction.BACKWARD)
							_result = _finalPosition;
						if ((_regularExpressionsCheckBox.isSelected())
								&& (direction != Direction.BACKWARD))
							_result = _initialPosition;
					} else {
						_result = _mainWindow.getEditorManager()
								.getEditorAt(selectedEditor).getEditor()
								.getCaretPosition()
								- _initialPosition;

						if (direction == Direction.BACKWARD) {
							_result = _mainWindow.getEditorManager()
									.getEditorAt(selectedEditor).getEditor()
									.getSelectionStart()
									- _initialPosition;
						}
					}

					if (_selectedText == null) {
						searchWindow.setOnTop(false);
						JOptionPane.showMessageDialog(null,
								labels.getString("s616"));
						searchWindow.setOnTop(true);
						_mainWindow.getStatusBar().setMessage(
								labels.getString("s616"));
					} else {
						_result = _search.search(_result,
								_searchTextField.getText(), _selectedText,
								_caseSensitiveCheckBox.isSelected(),
								_regularExpressionsCheckBox.isSelected(),
								_completeWordsCheckBox.isSelected(), direction);

						if (_result != -1) {
							_mainWindow
									.getEditorManager()
									.getEditorAt(
											_mainWindow.getEditorManager()
													.getSelectedEditorIndex())
									.selectText(_result + _initialPosition,
											_searchTextField.getText().length());

							// Updates the log
							Log.getLog().info(
									labels.getString("s583") + " "
											+ _searchTextField.getText() + " "
											+ labels.getString("s574"));

							// Updates the status bar
							_mainWindow.getStatusBar().setMessage(
									labels.getString("s583") + " "
											+ _searchTextField.getText() + " "
											+ labels.getString("s574"));

							// REGULAR EXPRESSIONS
							if (_regularExpressionsCheckBox.isSelected()) {

								// SHOWS THE SEARCH IN THE TEXT EDITOR
								_mainWindow
										.getEditorManager()
										.getEditorAt(
												_mainWindow
														.getEditorManager()
														.getSelectedEditorIndex())
										.selectText(
												_result + _initialPosition,
												_search.getRegularExpresion()
														.length());

								// Updates the status bar
								_mainWindow.getStatusBar().setMessage(
										labels.getString("s329") + " "
												+ _searchTextField.getText()
												+ " "
												+ labels.getString("s574"));

								// Updates the log
								Log.getLog().info(
										labels.getString("s329") + " "
												+ _search.getRegularExpresion()
												+ " "
												+ labels.getString("s574"));
							}
						} else {

							_selectedText = null;

							// Updates the log
							Log.getLog().info(labels.getString("s573"));

							searchWindow.setOnTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s573"));
							searchWindow.setOnTop(true);
							_mainWindow.getStatusBar().setMessage(
									labels.getString("s573"));
							searchWindow.setOnTop(false);
							int op = JOptionPane.showConfirmDialog(null,
									labels.getString("s575"));
							searchWindow.setOnTop(true);

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

				// ALL DOCUMENTS SEARCH
				if (_allDocumentsRadioButton.isSelected()) {

					_selectedText = null;
					selectedEditor = _mainWindow.getEditorManager()
							.getNumEditors();

					if ((_isCycle == false) && (_currentPosition == -2)) {
						_selectedEditorIndex = _mainWindow.getEditorManager()
								.getSelectedEditorIndex();
						_currentDocument = _selectedEditorIndex;
						_currentPosition = _mainWindow.getEditorManager()
								.getEditorAt(_selectedEditorIndex).getEditor()
								.getCaretPosition();
					}

					if (_isEnd == false) {

						if (direction == Direction.FORWARD)
							_result = _mainWindow.getEditorManager()
									.getEditorAt(_selectedEditorIndex)
									.getEditor().getCaretPosition();
						if (direction == Direction.BACKWARD)
							_result = _mainWindow.getEditorManager()
									.getEditorAt(_selectedEditorIndex)
									.getEditor().getSelectionStart();
						if (direction == Direction.BOTH) {
							_result = _mainWindow.getEditorManager()
									.getEditorAt(_selectedEditorIndex)
									.getEditor().getCaretPosition();
						}

						Direction auxDirection = direction;

						if (direction == Direction.BOTH)
							auxDirection = Direction.FORWARD;

						_result = _search.search(
								_result,
								_searchTextField.getText(),
								_mainWindow.getEditorManager()
										.getEditorAt(_selectedEditorIndex)
										.getText(),
								_caseSensitiveCheckBox.isSelected(),
								_regularExpressionsCheckBox.isSelected(),
								_completeWordsCheckBox.isSelected(),
								auxDirection);

						if ((_isCycle)
								&& (_selectedEditorIndex == _currentDocument)
								&& (_result >= _currentPosition))
							_isEnd = true;
						else if ((_isCycle)
								&& (_selectedEditorIndex == _currentDocument)
								&& (_result == -1))
							_isEnd = true;

						if (_result != -1) {

							_counter++;

							if (!_regularExpressionsCheckBox.isSelected()) {

								_mainWindow
										.getEditorManager()
										.getEditorAt(_selectedEditorIndex)
										.selectText(
												_result,
												_searchTextField.getText()
														.length());

								// Updates the log
								Log.getLog().info(
										labels.getString("s583") + " "
												+ _searchTextField.getText()
												+ " "
												+ labels.getString("s574"));

								// Updates the status bar
								_mainWindow.getStatusBar().setMessage(
										labels.getString("s583") + " "
												+ _searchTextField.getText()
												+ " "
												+ labels.getString("s574"));
							} else {

								_mainWindow
										.getEditorManager()
										.getEditorAt(_selectedEditorIndex)
										.selectText(
												_result,
												_search.getRegularExpresion()
														.length());
								// Updates the log
								Log.getLog().info(
										labels.getString("s577") + " "
												+ _search.getRegularExpresion()
												+ labels.getString("s577"));

								// Updates the status bar
								_mainWindow.getStatusBar().setMessage(
										labels.getString("s577") + " "
												+ _search.getRegularExpresion()
												+ labels.getString("s577"));
							}
						} else {

							// Updates the log
							Log.getLog().info(labels.getString("s573"));

							_mainWindow.getEditorManager()
									.getEditorAt(_selectedEditorIndex)
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
									_mainWindow.getEditorManager()
											.setSelectedEditorAt(
													_selectedEditorIndex);
									_mainWindow.getEditorManager()
											.getEditorAt(_selectedEditorIndex)
											.getEditor().setCaretPosition(0);
									_searchButton.doClick();
								}
							}
							if (direction == Direction.BACKWARD) {
								if (_selectedEditorIndex < 0) {
									_isEnd = true;
								} else {
									_mainWindow.getEditorManager()
											.setSelectedEditorAt(
													_selectedEditorIndex);
									_mainWindow
											.getEditorManager()
											.getEditorAt(_selectedEditorIndex)
											.getEditor()
											.setCaretPosition(
													_mainWindow
															.getEditorManager()
															.getEditorAt(
																	_selectedEditorIndex)
															.getEditor()
															.getText().length() - 1);
									_searchButton.doClick();
								}
							}
							if (direction == Direction.BOTH) {
								if (_selectedEditorIndex >= selectedEditor) {
									_selectedEditorIndex = 0;
									_isCycle = true;
									_mainWindow.getEditorManager()
											.setSelectedEditorAt(
													_selectedEditorIndex);
									_mainWindow.getEditorManager()
											.getEditorAt(_selectedEditorIndex)
											.getEditor().setCaretPosition(0);
									_searchButton.doClick();

								} else {
									_mainWindow.getEditorManager()
											.setSelectedEditorAt(
													_selectedEditorIndex);
									_mainWindow.getEditorManager()
											.getEditorAt(_selectedEditorIndex)
											.getEditor().setCaretPosition(0);
									_searchButton.doClick();
								}
							}
						}
					}

					if (_isEnd && _isFirst) {
						if (_counter == 0) {
							
							searchWindow.setOnTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s576"));
							searchWindow.setOnTop(true);
							_mainWindow.getStatusBar().setMessage(
									labels.getString("s576"));
						} else {
							
							searchWindow.setOnTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s586"));
							searchWindow.setOnTop(true);
							_mainWindow.getStatusBar().setMessage(
									labels.getString("s586"));
						}
						_isFirst = false;
					}
				}
			}
			
			// Puts the default cursor
			MainWindow.getInstance().setCursor(Cursor
					.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}

	/************************************************************************
	 * Cancel button listener
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
	 *         <li><b>Fernando S�enz P�rez (Team Director)</b></li>
	 *         <li><b>Version 0.1-0.6:</b>
	 *         <ul>
	 *         Diego Cardiel Freire
	 *         </ul>
	 *         <ul>
	 *         Juan Jos� Ortiz S�nchez
	 *         </ul>
	 *         <ul>
	 *         Delf�n Rup�rez Ca�as
	 *         </ul>
	 *         </li>
	 *         <li><b>Version 0.7:</b>
	 *         <ul>
	 *         Miguel Mart�n L�zaro
	 *         </ul>
	 *         </li>
	 *         <li><b>Version 0.8:</b>
	 *         <ul>
	 *         Javier Salcedo G�mez
	 *         </ul>
	 *         </li>
	 *         </ul>
	 ************************************************************************ 
	 * @version 0.8
	 * @see ActionListener
	 ***********************************************************************/
	class CancelButtonListener implements ActionListener {
		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Closes the window
			SearchWindow.getInstance().dispose();
		}
	}
}