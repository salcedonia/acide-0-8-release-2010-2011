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
package acide.gui.menuBar.editMenu.gui.replace;

import acide.factory.operations.AcideOperationsFactory;
import acide.gui.listeners.AcideKeyboardListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.editMenu.utils.AcideSearchDirection;
import acide.gui.menuBar.editMenu.utils.AcideSearchEngine;

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

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE replace window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideReplaceWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE replace window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE replace window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE replace window unique class instance.
	 */
	private static AcideReplaceWindow _instance;
	/**
	 * ACIDE - A Configurable IDE replace window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE replace window direction panel.
	 */
	private JPanel _directionPanel;
	/**
	 * ACIDE - A Configurable IDE replace window option panel.
	 */
	private JPanel _optionPanel;
	/**
	 * ACIDE - A Configurable IDE replace window scope panel.
	 */
	private JPanel _scopePanel;
	/**
	 * ACIDE - A Configurable IDE replace window button group.
	 */
	private ButtonGroup _buttonGroup;
	/**
	 * ACIDE - A Configurable IDE replace window replace label.
	 */
	private JLabel _replaceLabel;
	/**
	 * ACIDE - A Configurable IDE replace window replace text field.
	 */
	private JTextField _replaceTextField;
	/**
	 * ACIDE - A Configurable IDE replace window replaced label.
	 */
	private JLabel _replacedLabel;
	/**
	 * ACIDE - A Configurable IDE replace window replaced text field.
	 */
	private JTextField _replacedTextField;
	/**
	 * ACIDE - A Configurable IDE replace window case sensitive check box.
	 */
	private JCheckBox _caseSensitiveCheckBox;
	/**
	 * ACIDE - A Configurable IDE replace window regular expressions check box.
	 */
	private JCheckBox _regularExpressionsCheckBox;
	/**
	 * ACIDE - A Configurable IDE replace window complete words check box.
	 */
	private JCheckBox _completeWordsCheckBox;
	/**
	 * ACIDE - A Configurable IDE replace window forward radio button.
	 */
	private JRadioButton _forwardRadioButton;
	/**
	 * ACIDE - A Configurable IDE replace window backward radio button.
	 */
	private JRadioButton _backwardRadioButton;
	/**
	 * ACIDE - A Configurable IDE replace window all radio button.
	 */
	private JRadioButton _allRadioButton;
	/**
	 * ACIDE - A Configurable IDE replace window current document radio button.
	 */
	private JRadioButton _currentDocumentRadioButton;
	/**
	 * ACIDE - A Configurable IDE replace window all documents radio button.
	 */
	private JRadioButton _allDocumentsRadioButton;
	/**
	 * ACIDE - A Configurable IDE replace window selected radio button.
	 */
	private JRadioButton _selectedTextRadioButton;
	/**
	 * ACIDE - A Configurable IDE replace window search button.
	 */
	private JButton _searchButton;
	/**
	 * ACIDE - A Configurable IDE replace window replace button.
	 */
	private JButton _replaceButton;
	/**
	 * ACIDE - A Configurable IDE replace window replace all button.
	 */
	private JButton _replaceAllButton;
	/**
	 * ACIDE - A Configurable IDE replace window cancel button.
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
	private AcideSearchEngine _search = AcideOperationsFactory.getInstance()
			.buildSearch();

	/**
	 * Creates a new ACIDE - A Configurable IDE replace window.
	 */
	public AcideReplaceWindow() {

		// Initializes the variables
		_initialPosition = 0;
		_selectedText = null;
		_result = -1;

		// Sets the layout
		setLayout(new GridBagLayout());

		// DIRECTION PANEL
		_directionPanel = new JPanel(new GridLayout(0, 1));
		_directionPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s567"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// BUTTON GROUP
		_buttonGroup = new ButtonGroup();
		_forwardRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s568"), true);
		_backwardRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s569"), false);
		_allRadioButton = new JRadioButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s570"), false);
		_buttonGroup.add(_forwardRadioButton);
		_buttonGroup.add(_backwardRadioButton);
		_buttonGroup.add(_allRadioButton);
		_directionPanel.add(_forwardRadioButton);
		_directionPanel.add(_backwardRadioButton);
		_directionPanel.add(_allRadioButton);

		// OPTION PANEL
		_optionPanel = new JPanel();
		_optionPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s559"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_optionPanel.setLayout(new GridLayout(0, 1));
		_caseSensitiveCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s560"), false);
		_regularExpressionsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s561"), false);
		_completeWordsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s562"), false);
		_optionPanel.add(_caseSensitiveCheckBox);
		_optionPanel.add(_regularExpressionsCheckBox);
		_optionPanel.add(_completeWordsCheckBox);

		// SCOPE PANEL
		_scopePanel = new JPanel(new GridLayout(0, 1));
		_scopePanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s563"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// BUTTON GROUP
		_buttonGroup = new ButtonGroup();
		_currentDocumentRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s565"), true);
		_allDocumentsRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s566"), false);
		_selectedTextRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s564"), false);
		_buttonGroup.add(_selectedTextRadioButton);
		_buttonGroup.add(_currentDocumentRadioButton);
		_buttonGroup.add(_allDocumentsRadioButton);
		_scopePanel.add(_selectedTextRadioButton);
		_scopePanel.add(_currentDocumentRadioButton);
		_scopePanel.add(_allDocumentsRadioButton);

		// SEARCH BUTTON
		_searchButton = new JButton();
		_searchButton.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s556"));

		// REPLACE BUTTON
		_replaceButton = new JButton();
		_replaceButton.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s572"));
		_replaceButton.setEnabled(true);

		// REPLACE ALL BUTTON
		_replaceAllButton = new JButton();
		_replaceAllButton.setText(AcideLanguageManager.getInstance()
				.getLabels().getString("s571"));
		_replaceAllButton.setEnabled(true);

		// CANCEL BUTTON
		_cancelButton = new JButton();
		_cancelButton.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s42"));

		// REPLACE TEXT
		_replaceLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s557"), JLabel.CENTER);
		_replaceTextField = new JTextField();
		_replaceTextField.setText("");

		// REPLACED TEXT
		_replacedLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s558"), JLabel.CENTER);
		_replacedTextField = new JTextField();
		_replacedTextField.setText("");

		// Sets the listeners of the window components
		setListeners();
		
		// Adds the components to the window with the layout
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
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s572"));
		setIconImage(ICON.getImage());
		setResizable(false);
		pack();
		setAlwaysOnTop(true);
		setLocationRelativeTo(null);
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners(){
		
		// SEARCH BUTTON
		_searchButton.addActionListener(new SearchButtonListener());
		_searchButton.addKeyListener(new AcideKeyboardListener());

		// REPLACE BUTTON
		_replaceButton.addActionListener(new ReplaceButtonListener());
		_replaceButton.addKeyListener(new AcideKeyboardListener());
		
		// REPLACE ALL BUTTON
		_replaceAllButton.addActionListener(new ReplaceAllButtonListener());
		_replaceAllButton.addKeyListener(new AcideKeyboardListener());
		
		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonListener());
		
		// WINDOW
		_selectedTextRadioButton.addKeyListener(new AcideKeyboardListener());
		_replaceTextField.addKeyListener(new AcideKeyboardListener());
		_replacedTextField.addKeyListener(new AcideKeyboardListener());
		_caseSensitiveCheckBox.addKeyListener(new AcideKeyboardListener());
		_regularExpressionsCheckBox.addKeyListener(new AcideKeyboardListener());
		_completeWordsCheckBox.addKeyListener(new AcideKeyboardListener());
		_forwardRadioButton.addKeyListener(new AcideKeyboardListener());
		_backwardRadioButton.addKeyListener(new AcideKeyboardListener());
		_allRadioButton.addKeyListener(new AcideKeyboardListener());
		_allDocumentsRadioButton.addKeyListener(new AcideKeyboardListener());
		_currentDocumentRadioButton.addKeyListener(new AcideKeyboardListener());
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
	public AcideSearchEngine getSearch() {
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
	public static AcideReplaceWindow getInstance() {
		if (_instance == null)
			_instance = new AcideReplaceWindow();
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

	/**
	 * Counts the number of times a substring occures in a provided string.
	 * 
	 * @param source
	 *            The <code>String</code> object that will be searched in.
	 * @param substring
	 *            The string whose occurances will we counted.
	 * @param matchCase
	 *            A <code>boolean</code> indicating if the match is going to be
	 *            performed in a case-sensitive manner or not.
	 * @return An <code>int</code> value containing the number of occurances of
	 *         the substring.
	 * @since 1.0
	 */
	public static int countMatches(String source, String substring,
			boolean matchCase) {
		if (null == source) {
			return 0;
		}

		if (null == substring) {
			return 0;
		}

		int current_index = 0;
		int substring_index = 0;
		int count = 0;

		if (!matchCase) {
			source = source.toLowerCase();
			substring = substring.toLowerCase();
		}

		while (current_index < source.length() - 1) {
			substring_index = source.indexOf(substring, current_index);

			if (-1 == substring_index) {
				break;
			} else {
				current_index = substring_index + substring.length();
				count++;
			}
		}

		return count;
	}

	/**
	 * ACIDE - A Configurable IDE replace window search button listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
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
			AcideLanguageManager language = AcideLanguageManager.getInstance();
			try {
				language.getLanguage(AcideResourceManager.getInstance()
						.getProperty("language"));
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			// Gets the labels
			ResourceBundle labels = language.getLabels();

			// Gets the replace window
			AcideReplaceWindow replaceWindow = AcideReplaceWindow.getInstance();

			// Gets the main window
			AcideMainWindow mainWindow = AcideMainWindow.getInstance();

			_isFirstReplacement = false;

			// Gets the search direction
			AcideSearchDirection direccion = AcideSearchDirection.FORWARD;

			if (_forwardRadioButton.isSelected())
				direccion = AcideSearchDirection.FORWARD;
			if (_backwardRadioButton.isSelected())
				direccion = AcideSearchDirection.BACKWARD;
			if (_allRadioButton.isSelected())
				direccion = AcideSearchDirection.BOTH;

			// If the complete words is selected
			if (_completeWordsCheckBox.isSelected())

				// Regular expressions are disabled
				_regularExpressionsCheckBox.setSelected(false);

			// If the replace text field is empty
			if (_replaceTextField.getText().equals("")) {

				// Error message
				replaceWindow.setTop(false);
				JOptionPane.showMessageDialog(null, labels.getString("s585"),
						"Error", JOptionPane.ERROR_MESSAGE);
				replaceWindow.setTop(true);

				// Updates the status message in the status bar
				mainWindow.getStatusBar().setStatusMessage(
						labels.getString("s585"));
			} else {

				// Puts the wait cursor
				AcideMainWindow.getInstance().setCursor(
						Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				// Gets the selected editor
				int selectedFileEditorPanelIndex = mainWindow
						.getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				// CURRENT DOCUMENT
				if (_currentDocumentRadioButton.isSelected()) {

					_selectedEditorIndex = -1;
					_counter = 0;
					_result = -1;
					_selectedText = null;

					// Gets the selected editor panel index
					selectedFileEditorPanelIndex = mainWindow
							.getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					// Gets the caret position of the selected editor
					_result = mainWindow.getFileEditorManager()
							.getFileEditorPanelAt(selectedFileEditorPanelIndex)
							.getActiveTextEditionArea().getCaretPosition();

					// BACKWARD
					if (direccion == AcideSearchDirection.BACKWARD)
						_result = mainWindow
								.getFileEditorManager()
								.getFileEditorPanelAt(
										selectedFileEditorPanelIndex)
								.getActiveTextEditionArea().getSelectionStart();

					// Gets the selected file editor panel index
					selectedFileEditorPanelIndex = mainWindow
							.getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					// Does the search
					_result = _search.search(
							_result,
							_replaceTextField.getText(),
							mainWindow
									.getFileEditorManager()
									.getFileEditorPanelAt(
											selectedFileEditorPanelIndex)
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

						// Updates the status message in the status bar
						mainWindow.getStatusBar().setStatusMessage(
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

							// Updates the status message in the status bar
							mainWindow.getStatusBar().setStatusMessage(
									labels.getString("s577") + " "
											+ _search.getRegularExpresion()
											+ " " + labels.getString("s574"));
						}
					} else {

						// Updates the log
						AcideLog.getLog().info(labels.getString("s573"));

						// Warning message
						replaceWindow.setTop(false);
						JOptionPane.showMessageDialog(null,
								labels.getString("s573"));
						replaceWindow.setTop(true);

						// Updates the status message in the status bar
						mainWindow.getStatusBar().setStatusMessage(
								labels.getString("s573"));
					}
				}

				// SELECTED TEXT SEARCH
				if (_selectedTextRadioButton.isSelected()) {

					_counter = 0;
					_selectedEditorIndex = -1;
					selectedFileEditorPanelIndex = mainWindow
							.getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					if (_selectedText == null) {

						_selectedText = mainWindow
								.getFileEditorManager()
								.getFileEditorPanelAt(
										selectedFileEditorPanelIndex)
								.getActiveTextEditionArea().getSelectedText();
						_initialPosition = mainWindow
								.getFileEditorManager()
								.getFileEditorPanelAt(
										selectedFileEditorPanelIndex)
								.getActiveTextEditionArea().getSelectionStart();
						_finalPosition = mainWindow
								.getFileEditorManager()
								.getFileEditorPanelAt(
										selectedFileEditorPanelIndex)
								.getActiveTextEditionArea().getSelectionEnd();
						_result = 0;

						if (direccion == AcideSearchDirection.BACKWARD)
							_result = _finalPosition;

						if ((_regularExpressionsCheckBox.isSelected())
								&& (direccion != AcideSearchDirection.BACKWARD))
							_result = _initialPosition;

					} else {

						_result = mainWindow
								.getFileEditorManager()
								.getFileEditorPanelAt(
										selectedFileEditorPanelIndex)
								.getActiveTextEditionArea().getCaretPosition()
								- _initialPosition;

						if (direccion == AcideSearchDirection.BACKWARD) {
							_result = mainWindow
									.getFileEditorManager()
									.getFileEditorPanelAt(
											selectedFileEditorPanelIndex)
									.getActiveTextEditionArea()
									.getSelectionStart()
									- _initialPosition;
						}
					}

					if (_selectedText == null) {

						// Warning message
						replaceWindow.setTop(false);
						JOptionPane.showMessageDialog(null,
								labels.getString("s616"));
						replaceWindow.setTop(true);

						// Updates the status message in the status bar
						mainWindow.getStatusBar().setStatusMessage(
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

							// Updates the status message in the status bar
							mainWindow.getStatusBar().setStatusMessage(
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

								// Updates the status message in the status bar
								mainWindow.getStatusBar().setStatusMessage(
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
							replaceWindow.setTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s573"));

							// Updates the status message in the status bar
							mainWindow.getStatusBar().setStatusMessage(
									labels.getString("s573"));

							int chosenOption = JOptionPane.showConfirmDialog(
									null, labels.getString("s575"));
							replaceWindow.setTop(true);

							if (chosenOption == JOptionPane.OK_OPTION) {
								_currentDocumentRadioButton.setSelected(true);

								if (direccion != AcideSearchDirection.BACKWARD)
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
					selectedFileEditorPanelIndex = mainWindow
							.getFileEditorManager()
							.getNumberOfFileEditorPanels();

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
						if (direccion == AcideSearchDirection.FORWARD)
							_result = mainWindow.getFileEditorManager()
									.getFileEditorPanelAt(_selectedEditorIndex)
									.getActiveTextEditionArea()
									.getCaretPosition();
						if (direccion == AcideSearchDirection.BACKWARD)
							_result = mainWindow.getFileEditorManager()
									.getFileEditorPanelAt(_selectedEditorIndex)
									.getActiveTextEditionArea()
									.getSelectionStart();
						if (direccion == AcideSearchDirection.BOTH) {
							_result = mainWindow.getFileEditorManager()
									.getFileEditorPanelAt(_selectedEditorIndex)
									.getActiveTextEditionArea()
									.getCaretPosition();
						}

						AcideSearchDirection auxDirection = direccion;

						if (direccion == AcideSearchDirection.BOTH)
							auxDirection = AcideSearchDirection.FORWARD;

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

								// Updates the status message in the status bar
								mainWindow.getStatusBar().setStatusMessage(
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

								// Updates the status message in the status bar
								mainWindow.getStatusBar().setStatusMessage(
										labels.getString("s577") + " "
												+ _search.getRegularExpresion()
												+ " "
												+ labels.getString("s577"));
							}

						} else {

							// Updates the log
							AcideLog.getLog().info(labels.getString("s573"));

							// Updates the status message in the status bar
							mainWindow.getStatusBar().setStatusMessage(
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

							if (direccion == AcideSearchDirection.FORWARD) {
								if (_selectedEditorIndex >= selectedFileEditorPanelIndex) {
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

							if (direccion == AcideSearchDirection.BACKWARD) {
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

							if (direccion == AcideSearchDirection.BOTH) {
								if (_selectedEditorIndex >= selectedFileEditorPanelIndex) {
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
							replaceWindow.setTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s576"));
							replaceWindow.setTop(true);

							// Updates the status bar
							mainWindow.getStatusBar().setStatusMessage(
									labels.getString("s576"));
						} else {

							// Shows message
							replaceWindow.setTop(false);
							JOptionPane.showMessageDialog(null,
									labels.getString("s586"));
							replaceWindow.setTop(true);

							// Updates the status message in the status bar
							mainWindow.getStatusBar().setStatusMessage(
									labels.getString("s586"));
						}
						_isFirstSearch = false;
					}
				}
			}

			// Puts the default cursor
			AcideMainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}

	/**
	 * ACIDE - A Configurable IDE replace window cancel button listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
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
			AcideReplaceWindow.getInstance().dispose();
		}
	}

	/**
	 * ACIDE - A Configurable IDE replace window replace button listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
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
			AcideLanguageManager language = AcideLanguageManager.getInstance();
			try {
				language.getLanguage(AcideResourceManager.getInstance()
						.getProperty("language"));
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			// Gets the labels
			ResourceBundle _labels = language.getLabels();

			// Puts the wait cursor
			AcideMainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			// SELECTED TEXT SEARCH
			if (_selectedTextRadioButton.isSelected()) {

				int selectedEditorIndex = AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				String selectedText = null;

				// If it is the first replacement
				if (_isFirstReplacement)
					_searchButton.doClick();

				// Gets the selected text
				selectedText = AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(selectedEditorIndex)
						.getActiveTextEditionArea().getSelectedText();

				if (_result != -1) {

					if (selectedText != null) {

						int caretPosition;

						// FORWARD
						if (AcideReplaceWindow.getInstance()._forwardRadioButton
								.isSelected())
							caretPosition = AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(selectedEditorIndex)
									.getActiveTextEditionArea()
									.getSelectionEnd();
						else
							// BACKWARD OR BOTH
							caretPosition = AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(selectedEditorIndex)
									.getActiveTextEditionArea()
									.getSelectionStart();

						// Gets the replace selection
						AcideMainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(selectedEditorIndex)
								.getActiveTextEditionArea()
								.replaceSelection(_replacedTextField.getText());

						// FORWARD
						if (AcideReplaceWindow.getInstance()._forwardRadioButton
								.isSelected())
							_selectedText = AcideReplaceWindow.getInstance()._selectedText
									.replaceFirst(_replaceTextField.getText(),
											_replacedTextField.getText());

						// Sets the caret position
						AcideMainWindow.getInstance().getFileEditorManager()
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
						AcideMainWindow
								.getInstance()
								.getStatusBar()
								.setStatusMessage(
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
				int selectedEditorIndex = AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				// If it is the first replacement
				if (_isFirstReplacement) {

					// Stores the selected editor caret position
					_originalCaretPosition = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getCaretPosition();

					// Searches for the string
					_searchButton.doClick();
				}

				// If there is anything selected
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(selectedEditorIndex)
						.getActiveTextEditionArea().getSelectedText() != null) {

					// Gets the replacement selection
					AcideMainWindow.getInstance().getFileEditorManager()
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
					AcideMainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
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
					AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea()
							.setCaretPosition(_originalCaretPosition);
			}

			// Puts the default cursor
			AcideMainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}

	/**
	 * ACIDE - A Configurable IDE replace window replace all button listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ReplaceAllButtonListener implements ActionListener {

		/**
		 * Local number of replacements.
		 */
		private int _localNumberOfReplacements = 0;
		/**
		 * Global number of replacements.
		 */
		private int _globalNumberOfReplacements = 0;

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
			AcideLanguageManager language = AcideLanguageManager.getInstance();
			try {
				language.getLanguage(AcideResourceManager.getInstance()
						.getProperty("language"));
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			// Gets the labels
			ResourceBundle labels = language.getLabels();

			// Puts the wait cursor
			AcideMainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			// Initializes the variables
			String selectedEditorText = null;
			String textReplaced = null;
			String textBeforeSelectedText = null;
			String textAfterSelectedText = null;

			// Gets the selected editor index
			int selectedEditorIndex = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// Gets the selected editor text size
			int selectedEditorTextSize = AcideMainWindow.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(selectedEditorIndex)
					.getActiveTextEditionArea().getText().length();

			_globalNumberOfReplacements = 0;

			// SELECTED TEXT SEARCH
			if (_selectedTextRadioButton.isSelected()) {

				// If there is anything selected
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(selectedEditorIndex)
						.getActiveTextEditionArea().getSelectedText() != null) {

					// Gets the number of replacements
					_globalNumberOfReplacements = countMatches(AcideMainWindow
							.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getText(),
							_replaceTextField.getText(),
							_caseSensitiveCheckBox.isSelected());

					// If there are replacements
					if (_globalNumberOfReplacements > 0) {

						// Gets the selection start
						int selectionStart = AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditorIndex)
								.getActiveTextEditionArea().getSelectionStart();

						// Gets the selected editor text
						selectedEditorText = AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditorIndex)
								.getActiveTextEditionArea().getSelectedText();

						// Gets the selection size
						int selectionSize = selectionStart
								+ selectedEditorText.length();
						String text = AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditorIndex)
								.getActiveTextEditionArea().getText();

						// Gets the text before the selected text
						textBeforeSelectedText = text.substring(0,
								selectionStart);

						// Gets the text after the selected text
						textAfterSelectedText = text.substring(selectionSize,
								selectedEditorTextSize);

						// REPLACE ALL
						if (_caseSensitiveCheckBox.isSelected())
							textReplaced = selectedEditorText.replaceAll(
									_replaceTextField.getText(),
									_replacedTextField.getText());
						else

							// Prepends the Case-insensitve pattern modifier
							// (?i)
							// before our regex to indicate that we don’t care
							// about the case sensitivity of the regex.
							textReplaced = selectedEditorText.replaceAll("(?i)"
									+ _replaceTextField.getText(),
									_replacedTextField.getText());

						// Builds the final text replaced
						String temporalText = textBeforeSelectedText
								.concat(textReplaced);
						textReplaced = null;
						textReplaced = temporalText
								.concat(textAfterSelectedText);

						// Updates the selected editor text with the text
						AcideMainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(selectedEditorIndex)
								.getActiveTextEditionArea()
								.setText(textReplaced);
					}
				}
			}

			// CURRENT DOCUMENT SEARCH
			if (_currentDocumentRadioButton.isSelected()) {

				// Gets the number of replacements
				_globalNumberOfReplacements = countMatches(AcideMainWindow
						.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(selectedEditorIndex)
						.getActiveTextEditionArea().getText(),
						_replaceTextField.getText(),
						_caseSensitiveCheckBox.isSelected());

				// If there are replacements
				if (_globalNumberOfReplacements > 0) {

					// Gets the selected editor index
					selectedEditorIndex = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					// Gets the selected editor text
					selectedEditorText = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getText();

					// REPLACE ALL
					if (_caseSensitiveCheckBox.isSelected())
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditorIndex)
								.getActiveTextEditionArea()
								.setText(
										selectedEditorText.replaceAll(
												_replaceTextField.getText(),
												_replacedTextField.getText()));
					else
						// Prepends the Case-insensitve pattern modifier (?i)
						// before our regex to indicate that we don’t care
						// about the case sensitivity of the regex.
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditorIndex)
								.getActiveTextEditionArea()
								.setText(
										selectedEditorText.replaceAll("(?i)"
												+ _replaceTextField.getText(),
												_replacedTextField.getText()));
					// Updates the log
					AcideLog.getLog().info(
							labels.getString("s582")
									+ _replaceTextField.getText()
									+ labels.getString("s580")
									+ _replacedTextField.getText());
				}

			} else

			// ALL DOCUMENTS SEARCH
			if (_allDocumentsRadioButton.isSelected()) {

				// Gets the selected editor index
				selectedEditorIndex = AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				// Gets the original caret position
				int originalCaretPosition = AcideMainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.getActiveTextEditionArea().getCaretPosition();

				// Gets the number of editors
				int numEditors = AcideMainWindow.getInstance()
						.getFileEditorManager().getNumberOfFileEditorPanels();

				// Initializes the number of replacements
				_globalNumberOfReplacements = 0;

				// For each one of the opened editors
				for (int editorIndex = 0; editorIndex < numEditors; editorIndex++) {

					// Gets the selected editor
					selectedEditorText = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(editorIndex)
							.getTextEditionAreaContent();

					// Sets the selected editor at the current editor
					AcideMainWindow.getInstance().getFileEditorManager()
							.setSelectedFileEditorPanelAt(editorIndex);

					// Gets the number of replacements
					_localNumberOfReplacements = countMatches(AcideMainWindow
							.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(editorIndex)
							.getActiveTextEditionArea().getText(),
							_replaceTextField.getText(),
							_caseSensitiveCheckBox.isSelected());

					// If there are replacements
					if (_localNumberOfReplacements > 0) {

						// Updates the number of replacements
						_globalNumberOfReplacements += _localNumberOfReplacements;

						// REPLACES ALL
						if (_caseSensitiveCheckBox.isSelected())
							AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(editorIndex)
									.getActiveTextEditionArea()
									.setText(
											selectedEditorText.replaceAll(
													_replaceTextField.getText(),
													_replacedTextField
															.getText()));
						else
							// Prepends the Case-insensitve pattern modifier
							// (?i)
							// before our regex to indicate that we don’t care
							// about the case sensitivity of the regex.
							AcideMainWindow
									.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(editorIndex)
									.getActiveTextEditionArea()
									.setText(
											selectedEditorText.replaceAll(
													"(?i)"
															+ _replaceTextField
																	.getText(),
													_replacedTextField
															.getText()));
					}
				}

				// Sets the original selected editor index
				AcideMainWindow.getInstance().getFileEditorManager()
						.setSelectedFileEditorPanelAt(selectedEditorIndex);

				// Sets the original caret position
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getActiveTextEditionArea()
						.setCaretPosition(originalCaretPosition);
			}

			// Informs of the number of replacements
			JOptionPane.showMessageDialog(AcideReplaceWindow.getInstance(),
					labels.getString("s1000") + _globalNumberOfReplacements,
					labels.getString("s572"), JOptionPane.INFORMATION_MESSAGE);

			// Updates the status message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setStatusMessage(
							labels.getString("s1000")
									+ _globalNumberOfReplacements);

			// Puts the default cursor
			AcideMainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));

			// Validates the main window
			AcideMainWindow.getInstance().validate();

			// Repaints the main window
			AcideMainWindow.getInstance().repaint();
		}
	}
}