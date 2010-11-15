package gui.menu.edit.gui.replace;

import gui.mainWindow.MainWindow;
import gui.menu.edit.utils.Direction;
import gui.menu.edit.utils.Search;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
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
 * Replace window of ACIDE - A Configurable IDE
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
public class ReplaceWindow extends JFrame {

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
	private static ReplaceWindow _instance;
	/**
	 * Main panel
	 */
	private JPanel _mainPanel;
	/**
	 * Button panel
	 */
	private JPanel _buttonPanel;
	/**
	 * Direction panel
	 */
	private JPanel _directionPanel;
	/**
	 * Option panel
	 */
	private JPanel _optionPanel;
	/**
	 * Scope panel
	 */
	private JPanel _scopePanel;
	/**
	 * Button group
	 */
	private ButtonGroup _buttonGroup;
	/**
	 * Replace label
	 */
	private JLabel _replaceLabel;
	/**
	 * Replace text field
	 */
	private JTextField _replaceTextField;
	/**
	 * Replaced label
	 */
	private JLabel _replacedLabel;
	/**
	 * Replaced text field
	 */
	private JTextField _replacedTextField;
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
	 * Current document radio button
	 */
	private JRadioButton _currentDocumentRadioButton;
	/**
	 * All documents radio button
	 */
	private JRadioButton _allDocumentsRadioButton;
	/**
	 * Selected radio button
	 */
	private JRadioButton _selectedTextRadioButton;
	/**
	 * Search button
	 */
	private JButton _searchButton;
	/**
	 * Replace button
	 */
	private JButton _replaceButton;
	/**
	 * Replace all button
	 */
	private JButton _replaceAllButton;
	/**
	 * Cancel button
	 */
	private JButton _cancelButton;
	/**
	 * Result of the operations
	 */
	private int _result;
	/**
	 * Initial position
	 */
	private int _initialPosition;
	/**
	 * Final position
	 */
	private int _finalPosition;
	/**
	 * Selected text
	 */
	private String _selectedText;
	/**
	 * Counter for the matches
	 */
	private int _counter;
	/**
	 * Current position
	 */
	private int _currentPosition;
	/**
	 * Selected editor index
	 */
	private static int _selectedEditorIndex;
	/**
	 * Flag that indicates if it is the end of a search
	 */
	private static boolean _isEnd = false;
	/**
	 * Flag that indicates if there is a cycle in the search
	 */
	private static boolean _isCycle = false;
	/**
	 * Flag that indicates if it is the first search
	 */
	private static boolean _isFirstSearch;
	/**
	 * Current document
	 */
	private int _currentDocument;
	/**
	 * Flag that indicates if it is the first replacement
	 */
	private static boolean _isFirstReplacement = true;
	/**
	 * Search class
	 */
	private Search _search = OperationsFactory.getInstance().buildSearch();

	/**
	 * Class constructor
	 */
	public ReplaceWindow() {

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

		// INITIAL VARIABLES
		_initialPosition = 0;
		_selectedText = null;
		_result = -1;

		// FRAME
		setLayout(new GridBagLayout());
		setSize(new Dimension(550, 390));
		setTitle(labels.getString("s572"));
		setIconImage(new ImageIcon(ICON).getImage());

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout());

		// MAIN PANEL
		_mainPanel = new JPanel(new GridBagLayout());

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
		_optionPanel.setLayout(new GridLayout(0, 1));
		_caseSensitiveCheckBox = new JCheckBox(labels.getString("s560"), false);
		_regularExpressionsCheckBox = new JCheckBox(labels.getString("s561"),
				false);
		_completeWordsCheckBox = new JCheckBox(labels.getString("s562"), false);
		_optionPanel.add(_caseSensitiveCheckBox);
		_optionPanel.add(_regularExpressionsCheckBox);
		_optionPanel.add(_completeWordsCheckBox);

		// SCORE PANEL
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

		// REPLACE BUTTON
		_replaceButton = new JButton();
		_replaceButton.setText(labels.getString("s572"));
		_replaceButton.setEnabled(true);

		// REPLACE ALL BUTTON
		_replaceAllButton = new JButton();
		_replaceAllButton.setText(labels.getString("s571"));
		_replaceAllButton.setEnabled(true);

		// CANCEL BUTTON
		_cancelButton = new JButton();
		_cancelButton.setText(labels.getString("s42"));

		// REPLACE
		_replaceLabel = new JLabel(labels.getString("s557"), JLabel.CENTER);
		_replaceTextField = new JTextField();
		_replaceTextField.setText("");

		// REPLACED
		_replacedLabel = new JLabel(labels.getString("s558"), JLabel.CENTER);
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

		// Listeners
		_selectedTextRadioButton.addKeyListener(new AcideKeyboardListener());
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
	 * Returns the search button
	 * 
	 * @return the search button
	 */
	public JButton getSearchButton() {
		return _searchButton;
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
	 * Sets a new value to is first search flag
	 * 
	 * @param isFirstSearch
	 *            new value to set
	 */
	public static void setIsFirstSearch(boolean isFirstSearch) {
		_isFirstSearch = isFirstSearch;
	}

	/**
	 * Returns the unique class instance
	 * 
	 * @return The unique class instance
	 */
	public static ReplaceWindow getInstance() {
		if (_instance == null)
			_instance = new ReplaceWindow();
		return _instance;
	}

	/**
	 * Initializes the instance
	 */
	public void inicialize() {
		_instance = null;
	}

	/**
	 * Sets a new value to replace text field
	 * 
	 * @param text
	 *            new value to set
	 */
	public void setReplaceTextField(String text) {
		_replaceTextField.setText(text);
	}

	/**
	 * Sets a new value to selected text
	 * 
	 * @param selectedText
	 *            new value to set
	 */
	public void setSelectedText(String selectedText) {
		_selectedText = selectedText;
	}

	/**
	 * Sets a new value to is first replacement flag
	 * 
	 * @param isFirstReplacement
	 *            new value to set
	 */
	public static void setIsFirstReplacement(boolean isFirstReplacement) {
		_isFirstReplacement = isFirstReplacement;
	}

	/**
	 * Sets the window on the top
	 * 
	 * @param b
	 *            true if it set on the top and false in other case
	 */
	public void setTop(boolean b) {
		setAlwaysOnTop(b);
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
	 * Returns the replace text field
	 * 
	 * @return the replace text field
	 */
	public JTextField getReplaceTextField() {
		return _replaceTextField;
	}

	/**
	 * Sets a new value to the selected editor index
	 * 
	 * @param selectedEditorIndex
	 *            new value to set
	 */
	public void setSelectedEditorIndex(int selectedEditorIndex) {
		_selectedEditorIndex = selectedEditorIndex;
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
	 * Sets a new value to the result
	 * 
	 * @param result
	 *            new value to set
	 */
	public void setResult(int result) {
		_result = result;
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
	 * Returns the case sensitive check box
	 * 
	 * @return the case sensitive check box
	 */
	public JCheckBox getCaseSensitiveCheckBox() {
		return _caseSensitiveCheckBox;
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
	 * Sets a new value to the initial position
	 * 
	 * @param initialPosition
	 *            new value to set
	 */
	public void setInitialPosition(int initialPosition) {
		_initialPosition = initialPosition;
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
	 * Returns the final position
	 * 
	 * @return the final position
	 */
	public int getFinalPosition() {
		return _finalPosition;
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
	 * Returns the counter
	 * 
	 * @return the counter
	 */
	public int getCounter() {
		return _counter;
	}

	/**
	 * Returns the is end flag
	 * 
	 * @return the is end flag
	 */
	public boolean getIsEnd() {
		return _isEnd;
	}

	/**
	 * Returns the is first flag
	 * 
	 * @return the is first flag
	 */
	public boolean getIsFirstSearch() {
		return _isFirstSearch;
	}

	/**
	 * Returns the all documents radio button
	 * 
	 * @return the all documents radio button
	 */
	public JRadioButton getAllDocumentsRadioButton() {
		return _allDocumentsRadioButton;
	}

	/**
	 * Returns the is cycle flag
	 * 
	 * @return the is cycle flag
	 */
	public boolean getIsCycle() {
		return _isCycle;
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
	 * Returns the selected editor index
	 * 
	 * @return the selected editor index
	 */
	public int getSelectedEditorIndex() {
		return _selectedEditorIndex;
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
	 * Returns the is first replacement flag
	 * 
	 * @return the is first replacement flag
	 */
	public boolean getIsFirstReplacement() {
		return _isFirstReplacement;
	}

	/**
	 * Returns the replaced text field
	 * 
	 * @return the replaced text field
	 */
	public JTextField getReplacedTextField() {
		return _replacedTextField;
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

			ReplaceWindow replaceGUI = ReplaceWindow.getInstance();
			MainWindow mainWindow = MainWindow.getInstance();

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
				JOptionPane.showMessageDialog(null, labels.getString("s585"));
				replaceGUI.setTop(true);
				mainWindow.getStatusBar().setMessage(labels.getString("s585"));
			} else {

				// Puts the default cursor
				MainWindow.getInstance().setCursor(
						Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				int selectedEditor = mainWindow.getEditorManager()
						.getSelectedEditorIndex();

				if (_currentDocumentRadioButton.isSelected()) {
					_selectedEditorIndex = -1;
					_counter = 0;
					_result = -1;
					_selectedText = null;

					selectedEditor = mainWindow.getEditorManager()
							.getSelectedEditorIndex();
					_result = mainWindow.getEditorManager()
							.getEditorAt(selectedEditor).getEditor()
							.getCaretPosition();

					if (direccion == Direction.BACKWARD)
						_result = mainWindow.getEditorManager()
								.getEditorAt(selectedEditor).getEditor()
								.getSelectionStart();

					selectedEditor = mainWindow.getEditorManager()
							.getSelectedEditorIndex();

					_result = _search.search(
							_result,
							_replaceTextField.getText(),
							mainWindow.getEditorManager()
									.getEditorAt(selectedEditor).getText(),
							_caseSensitiveCheckBox.isSelected(),
							_regularExpressionsCheckBox.isSelected(),
							_completeWordsCheckBox.isSelected(), direccion);

					if (_result != -1) {

						// SHOWS THE SEARCH IN THE TEXT EDITOR
						mainWindow
								.getEditorManager()
								.getEditorAt(
										mainWindow.getEditorManager()
												.getSelectedEditorIndex())
								.selectText(_result,
										_replaceTextField.getText().length());

						// UPDATES THE LOG
						Log.getLog().info(
								labels.getString("s583")
										+ _replaceTextField.getText()
										+ labels.getString("s574"));

						// UPDATES THE STATUS BAR
						mainWindow.getStatusBar().setMessage(
								labels.getString("s583") + " "
										+ _replaceTextField.getText() + " "
										+ labels.getString("s574"));

						if (_regularExpressionsCheckBox.isSelected() == true) {

							// SHOW THE SEARCH IN THE TEXT EDITOR
							mainWindow
									.getEditorManager()
									.getEditorAt(
											mainWindow.getEditorManager()
													.getSelectedEditorIndex())
									.selectText(
											_result,
											_search.getRegularExpresion()
													.length());

							// UPDATES THE LOG
							Log.getLog().info(
									labels.getString("s577") + " "
											+ _search.getRegularExpresion()
											+ " " + labels.getString("s574"));

							// UPDATES THE STATUS BAR
							mainWindow.getStatusBar().setMessage(
									labels.getString("s577") + " "
											+ _search.getRegularExpresion()
											+ " " + labels.getString("s574"));
						}
					} else {

						// UPDATES THE LOG
						Log.getLog().info(labels.getString("s573"));
						replaceGUI.setTop(false);
						JOptionPane.showMessageDialog(null,
								labels.getString("s573"));
						replaceGUI.setTop(true);
						mainWindow.getStatusBar().setMessage(
								labels.getString("s573"));
					}
				}

				// SELECTED TEXT SEARCH
				if (_selectedTextRadioButton.isSelected()) {

					_counter = 0;
					_selectedEditorIndex = -1;
					selectedEditor = mainWindow.getEditorManager()
							.getSelectedEditorIndex();

					if (_selectedText == null) {

						_selectedText = mainWindow.getEditorManager()
								.getEditorAt(selectedEditor).getEditor()
								.getSelectedText();
						_initialPosition = mainWindow.getEditorManager()
								.getEditorAt(selectedEditor).getEditor()
								.getSelectionStart();
						_finalPosition = mainWindow.getEditorManager()
								.getEditorAt(selectedEditor).getEditor()
								.getSelectionEnd();
						_result = 0;

						if (direccion == Direction.BACKWARD)
							_result = _finalPosition;

						if ((_regularExpressionsCheckBox.isSelected())
								&& (direccion != Direction.BACKWARD))
							_result = _initialPosition;

					} else {

						_result = mainWindow.getEditorManager()
								.getEditorAt(selectedEditor).getEditor()
								.getCaretPosition()
								- _initialPosition;

						if (direccion == Direction.BACKWARD) {
							_result = mainWindow.getEditorManager()
									.getEditorAt(selectedEditor).getEditor()
									.getSelectionStart()
									- _initialPosition;
						}
					}

					if (_selectedText == null) {
						replaceGUI.setTop(false);
						JOptionPane.showMessageDialog(null,
								labels.getString("s616"));
						replaceGUI.setTop(true);
						mainWindow.getStatusBar().setMessage(
								labels.getString("s616"));
					}

					else {

						_result = _search.search(_result,
								_replaceTextField.getText(), _selectedText,
								_caseSensitiveCheckBox.isSelected(),
								_regularExpressionsCheckBox.isSelected(),
								_completeWordsCheckBox.isSelected(), direccion);

						if (_result != -1) {

							// SHOWS THE SEARCH IN THE TEXT EDITOR
							mainWindow
									.getEditorManager()
									.getEditorAt(
											mainWindow.getEditorManager()
													.getSelectedEditorIndex())
									.selectText(
											_result + _initialPosition,
											_replaceTextField.getText()
													.length());

							// UPDATES THE LOG
							Log.getLog().info(
									labels.getString("s583")
											+ _replaceTextField.getText()
											+ labels.getString("s574"));

							// UPDATES THE STATUS BAR
							mainWindow.getStatusBar().setMessage(
									labels.getString("s583") + " "
											+ _replaceTextField.getText() + " "
											+ labels.getString("s574"));

							if (_regularExpressionsCheckBox.isSelected()) {

								// SHOWS THE SEARCH IN THE TEXT EDITOR
								mainWindow
										.getEditorManager()
										.getEditorAt(
												mainWindow
														.getEditorManager()
														.getSelectedEditorIndex())
										.selectText(
												_result + _initialPosition,
												_search.getRegularExpresion()
														.length());

								// UPDATES THE LOG
								Log.getLog().info(
										labels.getString("s329") + " "
												+ _search.getRegularExpresion()
												+ " "
												+ labels.getString("s574"));

								// UPDATES THE STATUS BAR
								mainWindow.getStatusBar().setMessage(
										labels.getString("s329") + " "
												+ _replaceTextField.getText()
												+ " "
												+ labels.getString("s574"));
							}
						} else {

							_selectedText = null;

							// UDPATES THE LOG
							Log.getLog().info(labels.getString("s573"));
							replaceGUI.setTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s573"));
							mainWindow.getStatusBar().setMessage(
									labels.getString("s573"));

							int option = JOptionPane.showConfirmDialog(null,
									labels.getString("s575"));
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
					selectedEditor = mainWindow.getEditorManager()
							.getNumEditors();

					if (!_isCycle && _currentPosition == -2) {
						_selectedEditorIndex = mainWindow.getEditorManager()
								.getSelectedEditorIndex();
						_currentDocument = _selectedEditorIndex;
						_currentPosition = mainWindow.getEditorManager()
								.getEditorAt(_selectedEditorIndex).getEditor()
								.getCaretPosition();
					}
					if (!_isEnd) {
						if (direccion == Direction.FORWARD)
							_result = mainWindow.getEditorManager()
									.getEditorAt(_selectedEditorIndex)
									.getEditor().getCaretPosition();
						if (direccion == Direction.BACKWARD)
							_result = mainWindow.getEditorManager()
									.getEditorAt(_selectedEditorIndex)
									.getEditor().getSelectionStart();
						if (direccion == Direction.BOTH) {
							_result = mainWindow.getEditorManager()
									.getEditorAt(_selectedEditorIndex)
									.getEditor().getCaretPosition();
						}

						Direction auxDirection = direccion;

						if (direccion == Direction.BOTH)
							auxDirection = Direction.FORWARD;

						_result = _search.search(
								_result,
								_replaceTextField.getText(),
								mainWindow.getEditorManager()
										.getEditorAt(_selectedEditorIndex)
										.getText(),
								_caseSensitiveCheckBox.isSelected(),
								_regularExpressionsCheckBox.isSelected(),
								_completeWordsCheckBox.isSelected(),
								auxDirection);

						if (_isCycle
								&& (_selectedEditorIndex == _currentDocument)
								&& (_result >= _currentPosition))
							_isEnd = true;
						else if (_isCycle
								&& (_selectedEditorIndex == _currentDocument)
								&& (_result == -1))
							_isEnd = true;
						if (_result != -1) {

							_counter++;

							if (!_regularExpressionsCheckBox.isSelected()) {

								// SHOWS THE SEARCH IN THE TEXT EDITOR
								mainWindow
										.getEditorManager()
										.getEditorAt(_selectedEditorIndex)
										.selectText(
												_result,
												_replaceTextField.getText()
														.length());

								// UPDATES THE LOG
								Log.getLog().info(
										labels.getString("s583") + " "
												+ _replaceTextField.getText()
												+ " "
												+ labels.getString("s574"));

								// UPDATES THE STATUS BAR
								mainWindow.getStatusBar().setMessage(
										labels.getString("s583") + " "
												+ _replaceTextField.getText()
												+ " "
												+ labels.getString("s574"));
							} else {

								// SHOWS THE SEARCH IN THE TEXT EDITOR
								mainWindow
										.getEditorManager()
										.getEditorAt(_selectedEditorIndex)
										.selectText(
												_result,
												_search.getRegularExpresion()
														.length());

								// UPDATES THE LOG
								Log.getLog().info(
										labels.getString("s577") + " "
												+ _search.getRegularExpresion()
												+ " "
												+ labels.getString("s577"));

								// UPDATES THE STATUS BAR
								mainWindow.getStatusBar().setMessage(
										labels.getString("s577") + " "
												+ _search.getRegularExpresion()
												+ " "
												+ labels.getString("s577"));
							}

						} else {

							// UPDATES THE LOG
							Log.getLog().info(labels.getString("s573"));

							// UPDATES THE STATUS BAR
							mainWindow.getStatusBar().setMessage(
									labels.getString("s573"));
							mainWindow.getEditorManager()
									.getEditorAt(_selectedEditorIndex)
									.getEditor().setCaretPosition(0);

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
									mainWindow.getEditorManager()
											.setSelectedEditorAt(
													_selectedEditorIndex);
									mainWindow.getEditorManager()
											.getEditorAt(_selectedEditorIndex)
											.getEditor().setCaretPosition(0);

									_searchButton.doClick();
								}
							}

							if (direccion == Direction.BACKWARD) {
								if (_selectedEditorIndex < 0) {
									_isEnd = true;
								} else {
									mainWindow.getEditorManager()
											.setSelectedEditorAt(
													_selectedEditorIndex);
									mainWindow
											.getEditorManager()
											.getEditorAt(_selectedEditorIndex)
											.getEditor()
											.setCaretPosition(
													mainWindow
															.getEditorManager()
															.getEditorAt(
																	_selectedEditorIndex)
															.getEditor()
															.getText().length() - 1);

									_searchButton.doClick();
								}
							}

							if (direccion == Direction.BOTH) {
								if (_selectedEditorIndex >= selectedEditor) {
									_selectedEditorIndex = 0;
									_isCycle = true;
									mainWindow.getEditorManager()
											.setSelectedEditorAt(
													_selectedEditorIndex);
									mainWindow.getEditorManager()
											.getEditorAt(_selectedEditorIndex)
											.getEditor().setCaretPosition(0);
									_searchButton.doClick();

								} else {
									mainWindow.getEditorManager()
											.setSelectedEditorAt(
													_selectedEditorIndex);
									mainWindow.getEditorManager()
											.getEditorAt(_selectedEditorIndex)
											.getEditor().setCaretPosition(0);
									_searchButton.doClick();
								}
							}
						}
					}

					if (_isEnd && _isFirstSearch) {

						if (_counter == 0) {
							replaceGUI.setTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s576"));
							replaceGUI.setTop(true);
							mainWindow.getStatusBar().setMessage(
									labels.getString("s576"));
						} else {
							replaceGUI.setTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s586"));
							replaceGUI.setTop(true);
							mainWindow.getStatusBar().setMessage(
									labels.getString("s586"));
						}
						_isFirstSearch = false;
					}
				}
			}

			// Puts the default cursor
			MainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
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

			// Closes the replace window
			ReplaceWindow.getInstance().dispose();
		}
	}

	/************************************************************************
	 * Replace button listener
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
	 * @see ActionListener
	 ***********************************************************************/
	class ReplaceButtonListener implements ActionListener {

		/**
		 * Original position.
		 */
		int _originalPosition = -1;

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
			ResourceBundle _labels = language.getLabels();

			// Puts the default cursor
			MainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			// SELECTED TEXT SEARCH
			if (_selectedTextRadioButton.isSelected()) {

				int selectedEditorIndex = MainWindow.getInstance()
						.getEditorManager().getSelectedEditorIndex();
				String text = null;

				if (_isFirstReplacement == true)
					_searchButton.doClick();

				text = MainWindow.getInstance().getEditorManager()
						.getEditorAt(selectedEditorIndex).getEditor()
						.getSelectedText();

				if (_result != -1) {
					if (text != null) {

						int position;

						// FORWARD
						if (ReplaceWindow.getInstance()._forwardRadioButton
								.isSelected())
							position = MainWindow.getInstance()
									.getEditorManager()
									.getEditorAt(selectedEditorIndex)
									.getEditor().getSelectionEnd();
						else
							// BACKWARD OR BOTH
							position = MainWindow.getInstance()
									.getEditorManager()
									.getEditorAt(selectedEditorIndex)
									.getEditor().getSelectionStart();

						// GETS THE REPLACE SELECTION
						MainWindow.getInstance().getEditorManager()
								.getEditorAt(selectedEditorIndex).getEditor()
								.replaceSelection(_replacedTextField.getText());

						// FORWARD
						if (ReplaceWindow.getInstance()._forwardRadioButton
								.isSelected())
							_selectedText = ReplaceWindow.getInstance()._selectedText
									.replaceFirst(_replaceTextField.getText(),
											_replacedTextField.getText());

						// SET THE CARET POSITION
						MainWindow.getInstance().getEditorManager()
								.getEditorAt(selectedEditorIndex).getEditor()
								.setCaretPosition(position);

						// UPDATES THE LOG
						Log.getLog().info(
								_labels.getString("s583") + " "
										+ _replaceTextField.getText() + " "
										+ _labels.getString("s580") + " "
										+ _replacedTextField.getText());

						// UPDATES THE STATUS BAR
						MainWindow
								.getInstance()
								.getStatusBar()
								.setMessage(
										_labels.getString("s583") + " "
												+ _replaceTextField.getText()
												+ " "
												+ _labels.getString("s580")
												+ " "
												+ _replacedTextField.getText());

						_isFirstReplacement = false;
						_searchButton.doClick();
					}
				}
			}

			// CURRENT DOCUMENT SEARCH
			if ((_currentDocumentRadioButton.isSelected())
					|| (_allDocumentsRadioButton.isSelected())) {

				int numeditor = MainWindow.getInstance().getEditorManager()
						.getSelectedEditorIndex();

				if (_isFirstReplacement) {
					_originalPosition = MainWindow.getInstance()
							.getEditorManager().getEditorAt(numeditor)
							.getEditor().getCaretPosition();
					_searchButton.doClick();
				}

				if (MainWindow.getInstance().getEditorManager()
						.getEditorAt(numeditor).getEditor().getSelectedText() != null) {
					MainWindow.getInstance().getEditorManager()
							.getEditorAt(numeditor).getEditor()
							.replaceSelection(_replacedTextField.getText());

					// UPDATES THE LOG
					Log.getLog().info(
							_labels.getString("s579") + " "
									+ _replaceTextField.getText() + " "
									+ _labels.getString("s580") + " "
									+ _replacedTextField.getText());

					// UPDATES THE STATUS BAR
					MainWindow
							.getInstance()
							.getStatusBar()
							.setMessage(
									_labels.getString("s579") + " "
											+ _replaceTextField.getText() + " "
											+ _labels.getString("s580") + " "
											+ _replacedTextField.getText());
					_isFirstReplacement = false;
					_searchButton.doClick();
				}

				if (MainWindow.getInstance().getEditorManager()
						.getEditorAt(numeditor).getEditor().getSelectedText() == null)

					// AVOIDS EXCEPTION
					if (_originalPosition >= 0)
						MainWindow.getInstance().getEditorManager()
								.getEditorAt(numeditor).getEditor()
								.setCaretPosition(_originalPosition);
			}

			// Puts the default cursor
			MainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}

	/************************************************************************
	 * Replace all button listener
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
	 * @see ActionListener
	 ***********************************************************************/
	class ReplaceAllButtonListener implements ActionListener {

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

			// Puts the default cursor
			MainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			String selectedText = null;
			String result = null;
			String textPre = null;
			String textPos = null;

			int selectedEditorIndex = MainWindow.getInstance()
					.getEditorManager().getSelectedEditorIndex();
			int size = MainWindow.getInstance().getEditorManager()
					.getEditorAt(selectedEditorIndex).getEditor().getText()
					.length();

			// SELECTED TEXT SEARCH
			if (_selectedTextRadioButton.isSelected()) {

				// GET THE SELECTION START
				int selectedStart = MainWindow.getInstance().getEditorManager()
						.getEditorAt(selectedEditorIndex).getEditor()
						.getSelectionStart();

				// GET THE SELECTED TEXT
				selectedText = MainWindow.getInstance().getEditorManager()
						.getEditorAt(selectedEditorIndex).getEditor()
						.getSelectedText();

				int selectionSize = selectedStart + selectedText.length();
				String text = MainWindow.getInstance().getEditorManager()
						.getEditorAt(selectedEditorIndex).getEditor().getText();

				textPre = text.substring(0, selectedStart);
				textPos = text.substring(selectionSize, size);

				// REPLACE ALL
				result = selectedText.replaceAll(_replaceTextField.getText(),
						_replacedTextField.getText());

				String temp = textPre.concat(result);
				result = null;
				result = temp.concat(textPos);

				// UPDATES THE EDITOR TEXT
				MainWindow.getInstance().getEditorManager()
						.getEditorAt(selectedEditorIndex).getEditor()
						.setText(result);

			}

			// CURRENT DOCUMENT SEARCH
			if (_currentDocumentRadioButton.isSelected()) {

				// GETS THE SELECTED EDITOR INDEX
				selectedEditorIndex = MainWindow.getInstance()
						.getEditorManager().getSelectedEditorIndex();

				// CARET POSITION
				int caretPosition = MainWindow.getInstance().getEditorManager()
						.getSelectedEditor().getEditor().getCaretPosition();

				// GETS THE SELECTED TEXT
				selectedText = MainWindow.getInstance().getEditorManager()
						.getEditorAt(selectedEditorIndex).getEditor().getText();

				// REPLACE ALL
				result = selectedText.replaceAll(_replaceTextField.getText(),
						_replacedTextField.getText());

				// UPDATES THE EDITOR TEXT
				MainWindow.getInstance().getEditorManager()
						.getEditorAt(selectedEditorIndex).getEditor()
						.setText(result);

				// SETS THE ORIGINAL CARET POSITION
				MainWindow.getInstance().getEditorManager().getSelectedEditor()
						.getEditor().setCaretPosition(caretPosition);

				// UPDATES THE LOG
				Log.getLog().info(
						labels.getString("s582") + _replaceTextField.getText()
								+ labels.getString("s580")
								+ _replacedTextField.getText());

			} else

			// ALL DOCUMENTS SEARCH
			if (_allDocumentsRadioButton.isSelected()) {

				// SELECTED EDITOR INDEX
				selectedEditorIndex = MainWindow.getInstance()
						.getEditorManager().getSelectedEditorIndex();

				// CARET POSITION
				int position = MainWindow.getInstance().getEditorManager()
						.getSelectedEditor().getEditor().getCaretPosition();

				// NUM EDITORS
				int numEditors = MainWindow.getInstance().getEditorManager()
						.getNumEditors();

				for (int editor = 0; editor < numEditors; editor++) {

					// GETS THE SELECTED TEXT
					selectedText = MainWindow.getInstance().getEditorManager()
							.getEditorAt(editor).getText();
					
					// REPLACES ALL
					result = selectedText.replaceAll(
							_replaceTextField.getText(),
							_replacedTextField.getText());
					
					// SET THE SELECTED EDITOR AT THE CURRENT EDITOR
					MainWindow.getInstance().getEditorManager()
							.setSelectedEditorAt(editor);
					
					// UPDATES THE TEXT IN THE CURRENT EDITOR
					MainWindow.getInstance().getEditorManager().getEditorAt(editor)
							.getEditor().setText(result);
				}

				// UPDATES THE SELECTED EDITOR INDEX
				MainWindow.getInstance().getEditorManager()
						.setSelectedEditorAt(selectedEditorIndex);
				
				// UPDATES THE CARET POSITON 
				MainWindow.getInstance().getEditorManager().getSelectedEditor()
						.getEditor().setCaretPosition(position);
			}

			// Puts the default cursor
			MainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}
}