package gui.menuBar.editMenu.gui.replace;

import gui.listeners.AcideKeyboardListener;
import gui.mainWindow.MainWindow;
import gui.menuBar.editMenu.utils.SearchDirection;
import gui.menuBar.editMenu.utils.SearchEngine;

import java.awt.Cursor;
import java.awt.FlowLayout;
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

import language.AcideLanguage;
import operations.factory.AcideOperationsFactory;
import operations.log.AcideLog;

import org.apache.commons.lang.StringUtils;

import resources.ResourceManager;

/************************************************************************
 * ACIDE - A Configurable IDE replace window.
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
	 * Replace window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Replace window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * Replace window unique class instance.
	 */
	private static ReplaceWindow _instance;
	/**
	 * Replace window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Replace window direction panel.
	 */
	private JPanel _directionPanel;
	/**
	 * Replace window option panel.
	 */
	private JPanel _optionPanel;
	/**
	 * Replace window scope panel.
	 */
	private JPanel _scopePanel;
	/**
	 * Replace window button group.
	 */
	private ButtonGroup _buttonGroup;
	/**
	 * Replace window replace label.
	 */
	private JLabel _replaceLabel;
	/**
	 * Replace window replace text field.
	 */
	private JTextField _replaceTextField;
	/**
	 * Replace window replaced label.
	 */
	private JLabel _replacedLabel;
	/**
	 * Replace window replaced text field.
	 */
	private JTextField _replacedTextField;
	/**
	 * Replace window case sensitive check box.
	 */
	private JCheckBox _caseSensitiveCheckBox;
	/**
	 * Replace window regular expressions check box.
	 */
	private JCheckBox _regularExpressionsCheckBox;
	/**
	 * Replace window complete words check box.
	 */
	private JCheckBox _completeWordsCheckBox;
	/**
	 * Replace window forward radio button.
	 */
	private JRadioButton _forwardRadioButton;
	/**
	 * Replace window backward radio button.
	 */
	private JRadioButton _backwardRadioButton;
	/**
	 * Replace window all radio button.
	 */
	private JRadioButton _allRadioButton;
	/**
	 * Replace window current document radio button.
	 */
	private JRadioButton _currentDocumentRadioButton;
	/**
	 * Replace window all documents radio button.
	 */
	private JRadioButton _allDocumentsRadioButton;
	/**
	 * Replace window selected radio button.
	 */
	private JRadioButton _selectedTextRadioButton;
	/**
	 * Replace window search button.
	 */
	private JButton _searchButton;
	/**
	 * Replace window replace button.
	 */
	private JButton _replaceButton;
	/**
	 * Replace window replace all button.
	 */
	private JButton _replaceAllButton;
	/**
	 * Replace window cancel button.
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
	 * Search class.
	 */
	private SearchEngine _search = AcideOperationsFactory.getInstance()
			.buildSearch();

	/**
	 * Creates a new replace window.
	 */
	public ReplaceWindow() {

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

		// INITIAL VARIABLES
		_initialPosition = 0;
		_selectedText = null;
		_result = -1;

		// Sets the layout
		setLayout(new GridBagLayout());

		// DIRECTION PANEL
		_directionPanel = new JPanel(new GridLayout(0, 1));
		_directionPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s567"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// BUTTON GROUP
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
				TitledBorder.DEFAULT_POSITION));
		_optionPanel.setLayout(new GridLayout(0, 1));
		_caseSensitiveCheckBox = new JCheckBox(labels.getString("s560"), false);
		_regularExpressionsCheckBox = new JCheckBox(labels.getString("s561"),
				false);
		_completeWordsCheckBox = new JCheckBox(labels.getString("s562"), false);
		_optionPanel.add(_caseSensitiveCheckBox);
		_optionPanel.add(_regularExpressionsCheckBox);
		_optionPanel.add(_completeWordsCheckBox);

		// SCOPE PANEL
		_scopePanel = new JPanel(new GridLayout(0, 1));
		_scopePanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s563"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// BUTTON GROUP
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

		// REPLACE TEXT
		_replaceLabel = new JLabel(labels.getString("s557"), JLabel.CENTER);
		_replaceTextField = new JTextField();
		_replaceTextField.setText("");

		// REPLACED TEXT
		_replacedLabel = new JLabel(labels.getString("s558"), JLabel.CENTER);
		_replacedTextField = new JTextField();
		_replacedTextField.setText("");

		// ADD THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();

		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 2;
		add(_replaceLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridy = 1;
		add(_replaceTextField, constraints);
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridy = 2;
		add(_replacedLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridy = 3;
		add(_replacedTextField, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridy = 4;
		constraints.gridwidth = 2;
		add(_optionPanel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridy = 5;
		constraints.gridwidth = 1;
		add(_scopePanel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 1;
		constraints.gridy = 5;
		add(_directionPanel, constraints);

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout());
		_buttonPanel.add(_searchButton);
		_buttonPanel.add(_replaceButton);
		_buttonPanel.add(_replaceAllButton);
		_buttonPanel.add(_cancelButton);
		constraints.anchor = GridBagConstraints.LINE_END;
		constraints.gridx = 0;
		constraints.gridy = 6;
		constraints.gridwidth = 2;
		add(_buttonPanel, constraints);

		// FRAME
		setTitle(labels.getString("s572"));
		setIconImage(ICON.getImage());
		setResizable(false);
		pack();
		setAlwaysOnTop(true);
		setLocationRelativeTo(null);

		// LISTENERS
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
	 * Returns the search button.
	 * 
	 * @return the search button.
	 */
	public JButton getSearchButton() {
		return _searchButton;
	}

	/**
	 * Sets a new value to is cycle flag.
	 * 
	 * @param isCycle
	 *            new value to set.
	 */
	public void setIsCycle(boolean isCycle) {
		_isCycle = isCycle;
	}

	/**
	 * Sets a new value to is end flag.
	 * 
	 * @param isEnd
	 *            new value to set.
	 */
	public void setIsEnd(boolean isEnd) {
		_isEnd = isEnd;
	}

	/**
	 * Sets a new value to current position.
	 * 
	 * @param currentPosition
	 *            new value to set.
	 */
	public void setCurrentPosition(int currentPosition) {
		_currentPosition = currentPosition;
	}

	/**
	 * Returns the search.
	 * 
	 * @return the search.
	 */
	public SearchEngine getSearch() {
		return _search;
	}

	/**
	 * Returns the current document radio button.
	 * 
	 * @return the current document radio button.
	 */
	public JRadioButton getCurrentDocumentRadioButton() {
		return _currentDocumentRadioButton;
	}

	/**
	 * Returns the all radio button.
	 * 
	 * @return the all radio button.
	 */
	public JRadioButton getAllRadioButton() {
		return _allRadioButton;
	}

	/**
	 * Returns the selected text radio button.
	 * 
	 * @return the selected text radio button.
	 */
	public JRadioButton getSelectedTextRadioButton() {
		return _selectedTextRadioButton;
	}

	/**
	 * Sets a new value to is first search flag.
	 * 
	 * @param isFirstSearch
	 *            new value to set.
	 */
	public static void setIsFirstSearch(boolean isFirstSearch) {
		_isFirstSearch = isFirstSearch;
	}

	/**
	 * Returns the replace window unique class instance.
	 * 
	 * @return the replace window unique class instance.
	 */
	public static ReplaceWindow getInstance() {
		if (_instance == null)
			_instance = new ReplaceWindow();
		return _instance;
	}

	/**
	 * Initializes the instance.
	 */
	public void inicialize() {
		_instance = null;
	}

	/**
	 * Sets a new value to replace text field.
	 * 
	 * @param text
	 *            new value to set.
	 */
	public void setReplaceTextField(String text) {
		_replaceTextField.setText(text);
	}

	/**
	 * Sets a new value to selected text.
	 * 
	 * @param selectedText
	 *            new value to set.
	 */
	public void setSelectedText(String selectedText) {
		_selectedText = selectedText;
	}

	/**
	 * Sets a new value to is first replacement flag.
	 * 
	 * @param isFirstReplacement
	 *            new value to set.
	 */
	public static void setIsFirstReplacement(boolean isFirstReplacement) {
		_isFirstReplacement = isFirstReplacement;
	}

	/**
	 * Sets the window on the top.
	 * 
	 * @param b
	 *            true if it set on the top and false in other case.
	 */
	public void setTop(boolean b) {
		setAlwaysOnTop(b);
	}

	/**
	 * Returns the forward radio button.
	 * 
	 * @return the forward radio button.
	 */
	public JRadioButton getForwardRadioButton() {
		return _forwardRadioButton;
	}

	/**
	 * Returns the backward radio button.
	 * 
	 * @return the backward radio button.
	 */
	public JRadioButton getBackwardRadioButton() {
		return _backwardRadioButton;
	}

	/**
	 * Returns the complete words check box.
	 * 
	 * @return the complete words check box.
	 */
	public JCheckBox getCompleteWordsCheckBox() {
		return _completeWordsCheckBox;
	}

	/**
	 * Returns the regular expressions check box.
	 * 
	 * @return the regular expressions check box.
	 */
	public JCheckBox getRegularExpressionsCheckBox() {
		return _regularExpressionsCheckBox;
	}

	/**
	 * Returns the replace text field.
	 * 
	 * @return the replace text field.
	 */
	public JTextField getReplaceTextField() {
		return _replaceTextField;
	}

	/**
	 * Sets a new value to the selected editor index.
	 * 
	 * @param selectedEditorIndex
	 *            new value to set.
	 */
	public void setSelectedEditorIndex(int selectedEditorIndex) {
		_selectedEditorIndex = selectedEditorIndex;
	}

	/**
	 * Sets a new value to the counter.
	 * 
	 * @param counter
	 *            new value to set.
	 */
	public void setCounter(int counter) {
		_counter = counter;
	}

	/**
	 * Sets a new value to the result.
	 * 
	 * @param result
	 *            new value to set.
	 */
	public void setResult(int result) {
		_result = result;
	}

	/**
	 * Returns the result.
	 * 
	 * @return the result.
	 */
	public int getResult() {
		return _result;
	}

	/**
	 * Returns the case sensitive check box.
	 * 
	 * @return the case sensitive check box.
	 */
	public JCheckBox getCaseSensitiveCheckBox() {
		return _caseSensitiveCheckBox;
	}

	/**
	 * Returns the selected text.
	 * 
	 * @return the selected text.
	 */
	public String getSelectedText() {
		return _selectedText;
	}

	/**
	 * Sets a new value to the initial position.
	 * 
	 * @param initialPosition
	 *            new value to set.
	 */
	public void setInitialPosition(int initialPosition) {
		_initialPosition = initialPosition;
	}

	/**
	 * Sets a new value to the final position.
	 * 
	 * @param finalPosition
	 *            new value to set.
	 */
	public void setFinalPosition(int finalPosition) {
		_finalPosition = finalPosition;
	}

	/**
	 * Returns the final position.
	 * 
	 * @return the final position.
	 */
	public int getFinalPosition() {
		return _finalPosition;
	}

	/**
	 * Returns the initial position.
	 * 
	 * @return the initial position.
	 */
	public int getInitialPosition() {
		return _initialPosition;
	}

	/**
	 * Returns the replacement counter.
	 * 
	 * @return the replacement counter.
	 */
	public int getCounter() {
		return _counter;
	}

	/**
	 * Returns the is end flag.
	 * 
	 * @return the is end flag.
	 */
	public boolean getIsEnd() {
		return _isEnd;
	}

	/**
	 * Returns the is first flag.
	 * 
	 * @return the is first flag.
	 */
	public boolean getIsFirstSearch() {
		return _isFirstSearch;
	}

	/**
	 * Returns the all documents radio button.
	 * 
	 * @return the all documents radio button.
	 */
	public JRadioButton getAllDocumentsRadioButton() {
		return _allDocumentsRadioButton;
	}

	/**
	 * Returns the is cycle flag.
	 * 
	 * @return the is cycle flag.
	 */
	public boolean getIsCycle() {
		return _isCycle;
	}

	/**
	 * Returns the current position.
	 * 
	 * @return the current position.
	 */
	public int getCurrentPosition() {
		return _currentPosition;
	}

	/**
	 * Returns the selected editor index.
	 * 
	 * @return the selected editor index.
	 */
	public int getSelectedEditorIndex() {
		return _selectedEditorIndex;
	}

	/**
	 * Sets a new value to the current document.
	 * 
	 * @param currentDocument
	 *            new value to set.
	 */
	public void setCurrentDocument(int currentDocument) {
		_currentDocument = currentDocument;
	}

	/**
	 * Returns the current document index.
	 * 
	 * @return the current document index.
	 */
	public int getCurrentDocument() {
		return _currentDocument;
	}

	/**
	 * Returns the is first replacement flag.
	 * 
	 * @return the is first replacement flag.
	 */
	public boolean getIsFirstReplacement() {
		return _isFirstReplacement;
	}

	/**
	 * Returns the replaced text field.
	 * 
	 * @return the replaced text field.
	 */
	public JTextField getReplacedTextField() {
		return _replacedTextField;
	}

	/************************************************************************
	 * ACIDE - A Configurable IDE replace window search button listener.
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

			ReplaceWindow replaceGUI = ReplaceWindow.getInstance();
			MainWindow mainWindow = MainWindow.getInstance();

			_isFirstReplacement = false;

			// Gets the search direction
			SearchDirection direccion = SearchDirection.FORWARD;

			if (_forwardRadioButton.isSelected())
				direccion = SearchDirection.FORWARD;
			if (_backwardRadioButton.isSelected())
				direccion = SearchDirection.BACKWARD;
			if (_allRadioButton.isSelected())
				direccion = SearchDirection.BOTH;

			// If the complete words is selected
			if (_completeWordsCheckBox.isSelected())

				// Regular expressions are disabled
				_regularExpressionsCheckBox.setSelected(false);

			// If the replace text field is empty
			if (_replaceTextField.getText().equals("")) {

				// Error message
				replaceGUI.setTop(false);
				JOptionPane.showMessageDialog(null, labels.getString("s585"),
						"Error", JOptionPane.ERROR_MESSAGE);
				replaceGUI.setTop(true);

				// Updates the status bar
				mainWindow.getStatusBar().setMessage(labels.getString("s585"));
			} else {

				// Puts the wait cursor
				MainWindow.getInstance().setCursor(
						Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				// Gets the selected editor
				int selectedEditor = mainWindow.getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				// CURRENT DOCUMENT
				if (_currentDocumentRadioButton.isSelected()) {

					_selectedEditorIndex = -1;
					_counter = 0;
					_result = -1;
					_selectedText = null;

					selectedEditor = mainWindow.getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					// Gets the caret position of the selected editor
					_result = mainWindow.getFileEditorManager()
							.getFileEditorPanelAt(selectedEditor)
							.getActiveTextEditionArea().getCaretPosition();

					// BACKWARD
					if (direccion == SearchDirection.BACKWARD)
						_result = mainWindow.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditor)
								.getActiveTextEditionArea().getSelectionStart();

					selectedEditor = mainWindow.getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					_result = _search.search(_result,
							_replaceTextField.getText(), mainWindow
									.getFileEditorManager()
									.getFileEditorPanelAt(selectedEditor)
									.getTextEditionAreaContent(),
							_caseSensitiveCheckBox.isSelected(),
							_regularExpressionsCheckBox.isSelected(),
							_completeWordsCheckBox.isSelected(), direccion);

					if (_result != -1) {

						// Selects the text in the editor
						mainWindow
								.getFileEditorManager()
								.getFileEditorPanelAt(
										mainWindow
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex())
								.selectText(_result,
										_replaceTextField.getText().length());

						// Updates the log
						AcideLog.getLog().info(
								labels.getString("s583")
										+ _replaceTextField.getText()
										+ labels.getString("s574"));

						// Updates the status bar
						mainWindow.getStatusBar().setMessage(
								labels.getString("s583") + " "
										+ _replaceTextField.getText() + " "
										+ labels.getString("s574"));

						// REGULAR EXPRESSIONS
						if (_regularExpressionsCheckBox.isSelected()) {

							// Selects the text in the editor
							mainWindow
									.getFileEditorManager()
									.getFileEditorPanelAt(
											mainWindow
													.getFileEditorManager()
													.getSelectedFileEditorPanelIndex())
									.selectText(
											_result,
											_search.getRegularExpresion()
													.length());

							// Updates the log
							AcideLog.getLog().info(
									labels.getString("s577") + " "
											+ _search.getRegularExpresion()
											+ " " + labels.getString("s574"));

							// Updates the status bar
							mainWindow.getStatusBar().setMessage(
									labels.getString("s577") + " "
											+ _search.getRegularExpresion()
											+ " " + labels.getString("s574"));
						}
					} else {

						// Updates the log
						AcideLog.getLog().info(labels.getString("s573"));
						
						// Warning message
						replaceGUI.setTop(false);
						JOptionPane.showMessageDialog(null,
								labels.getString("s573"));
						replaceGUI.setTop(true);
						
						// Updates the status bar
						mainWindow.getStatusBar().setMessage(
								labels.getString("s573"));
					}
				}

				// SELECTED TEXT SEARCH
				if (_selectedTextRadioButton.isSelected()) {

					_counter = 0;
					_selectedEditorIndex = -1;
					selectedEditor = mainWindow.getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					if (_selectedText == null) {

						_selectedText = mainWindow.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditor)
								.getActiveTextEditionArea().getSelectedText();
						_initialPosition = mainWindow.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditor)
								.getActiveTextEditionArea().getSelectionStart();
						_finalPosition = mainWindow.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditor)
								.getActiveTextEditionArea().getSelectionEnd();
						_result = 0;

						if (direccion == SearchDirection.BACKWARD)
							_result = _finalPosition;

						if ((_regularExpressionsCheckBox.isSelected())
								&& (direccion != SearchDirection.BACKWARD))
							_result = _initialPosition;

					} else {

						_result = mainWindow.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditor)
								.getActiveTextEditionArea().getCaretPosition()
								- _initialPosition;

						if (direccion == SearchDirection.BACKWARD) {
							_result = mainWindow.getFileEditorManager()
									.getFileEditorPanelAt(selectedEditor)
									.getActiveTextEditionArea()
									.getSelectionStart()
									- _initialPosition;
						}
					}

					if (_selectedText == null) {
						
						// Warning message
						replaceGUI.setTop(false);
						JOptionPane.showMessageDialog(null,
								labels.getString("s616"));
						replaceGUI.setTop(true);
						
						// Updates the status bar
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

							// Selects the text in the editor
							mainWindow
									.getFileEditorManager()
									.getFileEditorPanelAt(
											mainWindow
													.getFileEditorManager()
													.getSelectedFileEditorPanelIndex())
									.selectText(
											_result + _initialPosition,
											_replaceTextField.getText()
													.length());

							// Updates the log
							AcideLog.getLog().info(
									labels.getString("s583")
											+ _replaceTextField.getText()
											+ labels.getString("s574"));

							// Updates the status bar
							mainWindow.getStatusBar().setMessage(
									labels.getString("s583") + " "
											+ _replaceTextField.getText() + " "
											+ labels.getString("s574"));

							// REGULAR EXPRESSIONS
							if (_regularExpressionsCheckBox.isSelected()) {

								// Selects the text in the editor
								mainWindow
										.getFileEditorManager()
										.getFileEditorPanelAt(
												mainWindow
														.getFileEditorManager()
														.getSelectedFileEditorPanelIndex())
										.selectText(
												_result + _initialPosition,
												_search.getRegularExpresion()
														.length());

								// Updates the log
								AcideLog.getLog().info(
										labels.getString("s329") + " "
												+ _search.getRegularExpresion()
												+ " "
												+ labels.getString("s574"));

								// Updates the status bar
								mainWindow.getStatusBar().setMessage(
										labels.getString("s329") + " "
												+ _replaceTextField.getText()
												+ " "
												+ labels.getString("s574"));
							}
						} else {

							_selectedText = null;

							// Updates the log
							AcideLog.getLog().info(labels.getString("s573"));
							
							// Warning message
							replaceGUI.setTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s573"));
							
							// Updates the status bar
							mainWindow.getStatusBar().setMessage(
									labels.getString("s573"));

							int chosenOption = JOptionPane.showConfirmDialog(null,
									labels.getString("s575"));
							replaceGUI.setTop(true);

							if (chosenOption == JOptionPane.OK_OPTION) {
								_currentDocumentRadioButton.setSelected(true);

								if (direccion != SearchDirection.BACKWARD)
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
					selectedEditor = mainWindow.getFileEditorManager()
							.getNumFileEditorPanels();

					if (!_isCycle && _currentPosition == -2) {
						_selectedEditorIndex = mainWindow
								.getFileEditorManager()
								.getSelectedFileEditorPanelIndex();
						_currentDocument = _selectedEditorIndex;
						_currentPosition = mainWindow.getFileEditorManager()
								.getFileEditorPanelAt(_selectedEditorIndex)
								.getActiveTextEditionArea().getCaretPosition();
					}
					if (!_isEnd) {
						if (direccion == SearchDirection.FORWARD)
							_result = mainWindow.getFileEditorManager()
									.getFileEditorPanelAt(_selectedEditorIndex)
									.getActiveTextEditionArea()
									.getCaretPosition();
						if (direccion == SearchDirection.BACKWARD)
							_result = mainWindow.getFileEditorManager()
									.getFileEditorPanelAt(_selectedEditorIndex)
									.getActiveTextEditionArea()
									.getSelectionStart();
						if (direccion == SearchDirection.BOTH) {
							_result = mainWindow.getFileEditorManager()
									.getFileEditorPanelAt(_selectedEditorIndex)
									.getActiveTextEditionArea()
									.getCaretPosition();
						}

						SearchDirection auxDirection = direccion;

						if (direccion == SearchDirection.BOTH)
							auxDirection = SearchDirection.FORWARD;

						_result = _search.search(
								_result,
								_replaceTextField.getText(),
								mainWindow
										.getFileEditorManager()
										.getFileEditorPanelAt(
												_selectedEditorIndex)
										.getTextEditionAreaContent(),
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

								// Selects the text in the editor
								mainWindow
										.getFileEditorManager()
										.getFileEditorPanelAt(
												_selectedEditorIndex)
										.selectText(
												_result,
												_replaceTextField.getText()
														.length());

								// Updates the log
								AcideLog.getLog().info(
										labels.getString("s583") + " "
												+ _replaceTextField.getText()
												+ " "
												+ labels.getString("s574"));

								// Updates the status bar
								mainWindow.getStatusBar().setMessage(
										labels.getString("s583") + " "
												+ _replaceTextField.getText()
												+ " "
												+ labels.getString("s574"));
							} else {

								// Selects the text in the editor
								mainWindow
										.getFileEditorManager()
										.getFileEditorPanelAt(
												_selectedEditorIndex)
										.selectText(
												_result,
												_search.getRegularExpresion()
														.length());

								// Updates the log
								AcideLog.getLog().info(
										labels.getString("s577") + " "
												+ _search.getRegularExpresion()
												+ " "
												+ labels.getString("s577"));

								// Updates the status bar
								mainWindow.getStatusBar().setMessage(
										labels.getString("s577") + " "
												+ _search.getRegularExpresion()
												+ " "
												+ labels.getString("s577"));
							}

						} else {

							// Updates the log
							AcideLog.getLog().info(labels.getString("s573"));

							// Updates the status bar
							mainWindow.getStatusBar().setMessage(
									labels.getString("s573"));
							
							// Sets the caret position in the first position
							mainWindow.getFileEditorManager()
									.getFileEditorPanelAt(_selectedEditorIndex)
									.getActiveTextEditionArea()
									.setCaretPosition(0);

							if (_forwardRadioButton.isSelected())
								_selectedEditorIndex++;
							else if (_backwardRadioButton.isSelected())
								_selectedEditorIndex--;
							else
								_selectedEditorIndex++;

							if (direccion == SearchDirection.FORWARD) {
								if (_selectedEditorIndex >= selectedEditor) {
									_isEnd = true;
								} else {
									mainWindow.getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													_selectedEditorIndex);
									mainWindow
											.getFileEditorManager()
											.getFileEditorPanelAt(
													_selectedEditorIndex)
											.getActiveTextEditionArea()
											.setCaretPosition(0);

									_searchButton.doClick();
								}
							}

							if (direccion == SearchDirection.BACKWARD) {
								if (_selectedEditorIndex < 0) {
									_isEnd = true;
								} else {
									mainWindow.getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													_selectedEditorIndex);
									mainWindow
											.getFileEditorManager()
											.getFileEditorPanelAt(
													_selectedEditorIndex)
											.getActiveTextEditionArea()
											.setCaretPosition(
													mainWindow
															.getFileEditorManager()
															.getFileEditorPanelAt(
																	_selectedEditorIndex)
															.getActiveTextEditionArea()
															.getText().length() - 1);

									_searchButton.doClick();
								}
							}

							if (direccion == SearchDirection.BOTH) {
								if (_selectedEditorIndex >= selectedEditor) {
									_selectedEditorIndex = 0;
									_isCycle = true;
									mainWindow.getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													_selectedEditorIndex);
									mainWindow
											.getFileEditorManager()
											.getFileEditorPanelAt(
													_selectedEditorIndex)
											.getActiveTextEditionArea()
											.setCaretPosition(0);
									_searchButton.doClick();

								} else {
									mainWindow.getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													_selectedEditorIndex);
									mainWindow
											.getFileEditorManager()
											.getFileEditorPanelAt(
													_selectedEditorIndex)
											.getActiveTextEditionArea()
											.setCaretPosition(0);
									_searchButton.doClick();
								}
							}
						}
					}

					if (_isEnd && _isFirstSearch) {

						if (_counter == 0) {
							
							// Warning message
							replaceGUI.setTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s576"));
							replaceGUI.setTop(true);
							
							// Updates the status bar
							mainWindow.getStatusBar().setMessage(
									labels.getString("s576"));
						} else {
							
							// Shows message
							replaceGUI.setTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s586"));
							replaceGUI.setTop(true);
							
							// Updates the status bar
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
	 * ACIDE - A Configurable IDE replace window cancel button listener.
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
	 * ACIDE - A Configurable IDE replace window replace button listener.
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
		int _originalCaretPosition = -1;

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
			ResourceBundle _labels = language.getLabels();

			// Puts the wait cursor
			MainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			// SELECTED TEXT SEARCH
			if (_selectedTextRadioButton.isSelected()) {

				int selectedEditorIndex = MainWindow.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				String selectedText = null;

				// If it is the first replacement
				if (_isFirstReplacement)
					_searchButton.doClick();

				// Gets the selected text
				selectedText = MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(selectedEditorIndex)
						.getActiveTextEditionArea().getSelectedText();

				if (_result != -1) {

					if (selectedText != null) {

						int caretPosition;

						// FORWARD
						if (ReplaceWindow.getInstance()._forwardRadioButton
								.isSelected())
							caretPosition = MainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(selectedEditorIndex)
									.getActiveTextEditionArea()
									.getSelectionEnd();
						else
							// BACKWARD OR BOTH
							caretPosition = MainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(selectedEditorIndex)
									.getActiveTextEditionArea()
									.getSelectionStart();

						// Gets the replace selection
						MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(selectedEditorIndex)
								.getActiveTextEditionArea()
								.replaceSelection(_replacedTextField.getText());

						// FORWARD
						if (ReplaceWindow.getInstance()._forwardRadioButton
								.isSelected())
							_selectedText = ReplaceWindow.getInstance()._selectedText
									.replaceFirst(_replaceTextField.getText(),
											_replacedTextField.getText());

						// Sets the caret position
						MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(selectedEditorIndex)
								.getActiveTextEditionArea()
								.setCaretPosition(caretPosition);

						// Updates the log
						AcideLog.getLog().info(
								_labels.getString("s583") + " "
										+ _replaceTextField.getText() + " "
										+ _labels.getString("s580") + " "
										+ _replacedTextField.getText());

						// Updates the status bar
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

			// CURRENT DOCUMENT OR ALL DOCUMENTS SEARCH
			if ((_currentDocumentRadioButton.isSelected())
					|| (_allDocumentsRadioButton.isSelected())) {

				// Gets the selected editor index
				int selectedEditorIndex = MainWindow.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				// If it is the first replacement
				if (_isFirstReplacement) {

					// Stores the selected editor caret position
					_originalCaretPosition = MainWindow.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getCaretPosition();

					// Searches for the string
					_searchButton.doClick();
				}

				// If there is anything selected
				if (MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(selectedEditorIndex)
						.getActiveTextEditionArea().getSelectedText() != null) {

					// Gets the replacement selection
					MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea()
							.replaceSelection(_replacedTextField.getText());

					// Updates the log
					AcideLog.getLog().info(
							_labels.getString("s579") + " "
									+ _replaceTextField.getText() + " "
									+ _labels.getString("s580") + " "
									+ _replacedTextField.getText());

					// Updates the status bar
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
				} else
				// Nothing is selected

				// If the original caret position has a valid value
				if (_originalCaretPosition >= 0)

					// Sets the original caret position
					MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea()
							.setCaretPosition(_originalCaretPosition);
			}

			// Puts the default cursor
			MainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}

	/************************************************************************
	 * Replace all button listener.
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

			// Puts the wait cursor
			MainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			// Initializes the variables
			String selectedEditorText = null;
			String textReplaced = null;
			String textBeforeSelectedText = null;
			String textAfterSelectedText = null;

			// Gets the selected editor index
			int selectedEditorIndex = MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// Gets the selected editor text size
			int selectedEditorTextSize = MainWindow.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(selectedEditorIndex)
					.getActiveTextEditionArea().getText().length();

			int numberOfReplacements = 0;

			// SELECTED TEXT SEARCH
			if (_selectedTextRadioButton.isSelected()) {

				// If there is anything selected
				if (MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(selectedEditorIndex)
						.getActiveTextEditionArea().getSelectedText() != null) {

					// Gets the number of replacements
					numberOfReplacements = StringUtils.countMatches(MainWindow
							.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getText(),
							_replaceTextField.getText());

					// Gets the selection start
					int selectionStart = MainWindow.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getSelectionStart();

					// Gets the selected editor text
					selectedEditorText = MainWindow.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getSelectedText();

					// Gets the selection size
					int selectionSize = selectionStart
							+ selectedEditorText.length();
					String text = MainWindow.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getText();

					// Gets the text before the selected text
					textBeforeSelectedText = text.substring(0, selectionStart);

					// Gets the text after the selected text
					textAfterSelectedText = text.substring(selectionSize,
							selectedEditorTextSize);

					// If there are replacements
					if (numberOfReplacements > 0)
						// REPLACE ALL
						textReplaced = selectedEditorText.replaceAll(
								_replaceTextField.getText(),
								_replacedTextField.getText());

					// Builds the final text replaced
					String temporalText = textBeforeSelectedText
							.concat(textReplaced);
					textReplaced = null;
					textReplaced = temporalText.concat(textAfterSelectedText);

					// Updates the selected editor text with the text
					MainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().setText(textReplaced);
				}
			}

			// CURRENT DOCUMENT SEARCH
			if (_currentDocumentRadioButton.isSelected()) {

				// Gets the number of replacements
				numberOfReplacements = StringUtils.countMatches(MainWindow
						.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(selectedEditorIndex)
						.getActiveTextEditionArea().getText(),
						_replaceTextField.getText());

				// Gets the selected editor index
				selectedEditorIndex = MainWindow.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				// Gets the selected editor text
				selectedEditorText = MainWindow.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(selectedEditorIndex)
						.getActiveTextEditionArea().getText();

				// If there are replacements
				if (numberOfReplacements > 0)
					// REPLACE ALL
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea()
							.setText(
									selectedEditorText.replaceAll(
											_replaceTextField.getText(),
											_replacedTextField.getText()));

				// Updates the log
				AcideLog.getLog().info(
						labels.getString("s582") + _replaceTextField.getText()
								+ labels.getString("s580")
								+ _replacedTextField.getText());

			} else

			// ALL DOCUMENTS SEARCH
			if (_allDocumentsRadioButton.isSelected()) {

				// Gets the selected editor index
				selectedEditorIndex = MainWindow.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				// Gets the original caret position
				int originalCaretPosition = MainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.getActiveTextEditionArea().getCaretPosition();

				// Gets the number of editors
				int numEditors = MainWindow.getInstance()
						.getFileEditorManager().getNumFileEditorPanels();

				// Initializes the number of replacements
				numberOfReplacements = 0;

				// For each one of the opened editors
				for (int editorIndex = 0; editorIndex < numEditors; editorIndex++) {

					// Gets the selected editor
					selectedEditorText = MainWindow.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(editorIndex)
							.getTextEditionAreaContent();

					// Sets the selected editor at the current editor
					MainWindow.getInstance().getFileEditorManager()
							.setSelectedFileEditorPanelAt(editorIndex);

					// Updates the number of replacements
					numberOfReplacements += StringUtils.countMatches(MainWindow
							.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(editorIndex)
							.getActiveTextEditionArea().getText(),
							_replaceTextField.getText());

					// If there are replacements
					if (numberOfReplacements > 0)
						// REPLACES ALL
						MainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(editorIndex)
								.getActiveTextEditionArea()
								.setText(
										selectedEditorText.replaceAll(
												_replaceTextField.getText(),
												_replacedTextField.getText()));
				}

				// Sets the original selected editor index
				MainWindow.getInstance().getFileEditorManager()
						.setSelectedFileEditorPanelAt(selectedEditorIndex);

				// Sets the original caret position
				MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getActiveTextEditionArea()
						.setCaretPosition(originalCaretPosition);
			}

			// Informs of the number of replacements
			JOptionPane.showMessageDialog(ReplaceWindow.getInstance(),
					labels.getString("s1000") + numberOfReplacements,
					labels.getString("s572"), JOptionPane.INFORMATION_MESSAGE);

			// Updates the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setMessage(
							labels.getString("s1000") + numberOfReplacements);

			// Puts the default cursor
			MainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));

			MainWindow.getInstance().validate();
			MainWindow.getInstance().repaint();
		}
	}
}