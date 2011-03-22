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
package acide.gui.menuBar.editMenu.gui.search;

import acide.factory.operations.AcideOperationsFactory;
import acide.gui.listeners.AcideSearchAndReplaceWindowKeyboardListener;
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

/**
 * ACIDE - A Configurable IDE search window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideSearchWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE search window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE search window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE search window class unique instance.
	 */
	private static AcideSearchWindow _instance;
	/**
	 * ACIDE - A Configurable IDE search window direction panel.
	 */
	private JPanel _directionPanel;
	/**
	 * ACIDE - A Configurable IDE search window option panel.
	 */
	private JPanel _optionPanel;
	/**
	 * ACIDE - A Configurable IDE search window scope panel.
	 */
	private JPanel _scopePanel;
	/**
	 * ACIDE - A Configurable IDE search window search label.
	 */
	private JLabel _searchLabel;
	/**
	 * ACIDE - A Configurable IDE search window search text field.
	 */
	private JTextField _searchTextField;
	/**
	 * ACIDE - A Configurable IDE search window case sensitive check box.
	 */
	private JCheckBox _caseSensitiveCheckBox;
	/**
	 * ACIDE - A Configurable IDE search window regular expressions check box.
	 */
	private JCheckBox _regularExpressionsCheckBox;
	/**
	 * ACIDE - A Configurable IDE search window complete words check box.
	 */
	private JCheckBox _completeWordsCheckBox;
	/**
	 * ACIDE - A Configurable IDE search window forward radio button.
	 */
	private JRadioButton _forwardRadioButton;
	/**
	 * ACIDE - A Configurable IDE search window backward radio button.
	 */
	private JRadioButton _backwardRadioButton;
	/**
	 * ACIDE - A Configurable IDE search window all radio button.
	 */
	private JRadioButton _allRadioButton;
	/**
	 * ACIDE - A Configurable IDE search window current document radio button.
	 */
	private JRadioButton _currentDocumentRadioButton;
	/**
	 * ACIDE - A Configurable IDE search window all documents radio button.
	 */
	private JRadioButton _allDocumentsRadioButton;
	/**
	 * ACIDE - A Configurable IDE search window selected text radio button.
	 */
	private JRadioButton _selectedTextRadioButton;
	/**
	 * ACIDE - A Configurable IDE search window button group.
	 */
	private ButtonGroup _buttonGroup;
	/**
	 * ACIDE - A Configurable IDE search window search button.
	 */
	private JButton _searchButton;
	/**
	 * ACIDE - A Configurable IDE search window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Result of the operation.
	 */
	private int _matchStartPosition;
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
	 * Selected editor index.
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
	 * Flag that indicates if the search uses a cycle or not.
	 */
	private static boolean _isCycle;
	/**
	 * Flag that indicates if it is the first searching.
	 */
	private static boolean _isFirst;
	/**
	 * Search class.
	 */
	private AcideSearchEngine _search = AcideOperationsFactory.getInstance()
			.buildSearch();
	/**
	 * ACIDE - A Configurable IDE search window button panel.
	 */
	private JPanel _buttonPanel;

	/**
	 * Creates a new ACIDE - A Configurable IDE search window.
	 */
	public AcideSearchWindow() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// DIRECTION PANEL
		_directionPanel = new JPanel();
		_directionPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s567"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_directionPanel.setLayout(new GridLayout(0, 1));
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
		_caseSensitiveCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s560"), false);
		_regularExpressionsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s561"), false);
		_completeWordsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s562"), false);
		_optionPanel.setLayout(new GridLayout(0, 1));
		_optionPanel.add(_caseSensitiveCheckBox);
		_optionPanel.add(_regularExpressionsCheckBox);
		_optionPanel.add(_completeWordsCheckBox);

		// SCOPE PANEL
		_scopePanel = new JPanel();
		_scopePanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s563"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_scopePanel.setLayout(new GridLayout(0, 1));
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
		_searchButton.setMnemonic(java.awt.event.KeyEvent.VK_F3);

		// CANCEL BUTTON
		_cancelButton = new JButton();
		_cancelButton.setText(AcideLanguageManager.getInstance().getLabels()
				.getString("s42"));

		// SEARCH
		_searchLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s557"), JLabel.CENTER);
		_searchTextField = new JTextField();
		_searchTextField.setText("");

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
		add(_searchLabel, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridy = 1;
		add(_searchTextField, constraints);
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridy = 2;
		add(_optionPanel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridwidth = 1;
		constraints.gridy = 3;
		add(_scopePanel, constraints);
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 1;
		constraints.gridy = 3;
		add(_directionPanel, constraints);

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		_buttonPanel.add(_searchButton);
		_buttonPanel.add(_cancelButton);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.LINE_END;
		constraints.gridx = 0;
		constraints.gridy = 4;
		constraints.gridwidth = 2;
		add(_buttonPanel, constraints);

		// FRAME
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s556"));
		setIconImage(ICON.getImage());
		setResizable(false);
		setAlwaysOnTop(true);
		pack();
		setLocationRelativeTo(null);
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// SEARCH BUTTON
		_searchButton.addActionListener(new SearchButtonListener());

		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonListener());

		// WINDOW
		_searchTextField.addKeyListener(new AcideSearchAndReplaceWindowKeyboardListener());
		_caseSensitiveCheckBox.addKeyListener(new AcideSearchAndReplaceWindowKeyboardListener());
		_regularExpressionsCheckBox.addKeyListener(new AcideSearchAndReplaceWindowKeyboardListener());
		_completeWordsCheckBox.addKeyListener(new AcideSearchAndReplaceWindowKeyboardListener());
		_forwardRadioButton.addKeyListener(new AcideSearchAndReplaceWindowKeyboardListener());
		_backwardRadioButton.addKeyListener(new AcideSearchAndReplaceWindowKeyboardListener());
		_allRadioButton.addKeyListener(new AcideSearchAndReplaceWindowKeyboardListener());
		_selectedTextRadioButton.addKeyListener(new AcideSearchAndReplaceWindowKeyboardListener());
		_allDocumentsRadioButton.addKeyListener(new AcideSearchAndReplaceWindowKeyboardListener());
		_currentDocumentRadioButton.addKeyListener(new AcideSearchAndReplaceWindowKeyboardListener());
	}

	/**
	 * Returns the unique class instance.
	 * 
	 * @return the unique class instance.
	 */
	public static AcideSearchWindow getInstance() {
		if (_instance == null)
			_instance = new AcideSearchWindow();
		return _instance;
	}

	/**
	 * Initializes the class instance.
	 */
	public void initialize() {
		_instance = null;
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
	 * Returns if is cycle or not.
	 * 
	 * @return true if it is cycle.
	 */
	public boolean getIsCycle() {
		return _isCycle;
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
	 * Sets a new value to the backward radio button.
	 * 
	 * @param isSelected
	 *            new value to set.
	 */
	public void setBackwardRadioButton(boolean isSelected) {
		_backwardRadioButton.setSelected(isSelected);
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
	 * Sets a new value to current document radio button.
	 * 
	 * @param currentDocumentRadioButton
	 *            new value to set.
	 */
	public void setCurrentDocumentRadioButton(boolean currentDocumentRadioButton) {
		_currentDocumentRadioButton.setSelected(currentDocumentRadioButton);
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
	 * Sets a new value to the search text field text.
	 * 
	 * @param text
	 *            new value to set.
	 */
	public void setSearchTextFieldText(String text) {
		_searchTextField.setText(text);
	}

	/**
	 * Sets a new value to forward radio button.
	 * 
	 * @param forwardRadioButton
	 *            new value to set
	 */
	public void setForwardRadioButton(boolean forwardRadioButton) {
		_forwardRadioButton.setSelected(forwardRadioButton);
	}

	/**
	 * Sets a new value to selectedText.
	 * 
	 * @param selectedText
	 *            new value to set.
	 */
	public void setSelectedText(String selectedText) {
		_selectedText = selectedText;
	}

	/**
	 * Sets the window on top.
	 * 
	 * @param b
	 *            if it is true set the window on the top and don't do it in
	 *            other case.
	 */
	public void setOnTop(boolean b) {
		setAlwaysOnTop(b);
	}

	/**
	 * Returns the result.
	 * 
	 * @return the result.
	 */
	public int getResult() {
		return _matchStartPosition;
	}

	/**
	 * Sets a new value to the result.
	 * 
	 * @param result
	 *            new value to set.
	 */
	public void setResult(int result) {
		_matchStartPosition = result;
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
	 * Returns the initial position.
	 * 
	 * @return the initial position.
	 */
	public int getInitialPosition() {
		return _initialPosition;
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
	 * Returns the final position.
	 * 
	 * @return the final position.
	 */
	public int getFinalPosition() {
		return _finalPosition;
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
	 * Returns the selected editor index.
	 * 
	 * @return the selected editor index.
	 */
	public static int getSelectedEditorIndex() {
		return _selectedEditorIndex;
	}

	/**
	 * Sets a new value to the selected editor index.
	 * 
	 * @param selectedEditorIndex
	 *            new value to set.
	 */
	public static void setSelectedEditorIndex(int selectedEditorIndex) {
		_selectedEditorIndex = selectedEditorIndex;
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
	 * Sets a new value to the current document.
	 * 
	 * @param currentDocument
	 *            new value to set.
	 */
	public void setCurrentDocument(int currentDocument) {
		_currentDocument = currentDocument;
	}

	/**
	 * Returns the current document.
	 * 
	 * @return the current document.
	 */
	public int getCurrentDocument() {
		return _currentDocument;
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
	 * Returns the counter.
	 * 
	 * @return the counter.
	 */
	public int getCounter() {
		return _counter;
	}

	/**
	 * Sets a new value to the is first flag.
	 * 
	 * @param _isFirst
	 *            new value to set.
	 */
	public static void setIsFirst(boolean isFirst) {
		_isFirst = isFirst;
	}

	/**
	 * Returns the is first flag.
	 * 
	 * @return the is first flag.
	 */
	public static boolean getIsFirst() {
		return _isFirst;
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
	 * Returns the search text field.
	 * 
	 * @return the search text field.
	 */
	public JTextField getSearchTextField() {
		return _searchTextField;
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
	 * Returns the all documents radio button.
	 * 
	 * @return the all documents radio button.
	 */
	public JRadioButton getAllDocumentsRadioButton() {
		return _allDocumentsRadioButton;
	}

	/**
	 * ACIDE - A Configurable IDE search window search button listener.
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

			// INITIALIZE THE VARIABLES
			_isCycle = false;
			_currentPosition = -2;
			_currentDocument = -1;
			_initialPosition = -1;
			_selectedText = null;
			_matchStartPosition = -1;
			_isFirst = true;

			AcideSearchDirection direction = AcideSearchDirection.FORWARD;

			// SELECTS THE DIRECTION OF THE SEARCH
			if (_forwardRadioButton.isSelected())
				direction = AcideSearchDirection.FORWARD;

			if (_backwardRadioButton.isSelected())
				direction = AcideSearchDirection.BACKWARD;

			if (_allRadioButton.isSelected())
				direction = AcideSearchDirection.BOTH;

			if (_completeWordsCheckBox.isSelected())
				_regularExpressionsCheckBox.setSelected(false);

			// IF THE SEARCH TEXT IS EMPTY
			if (_searchTextField.getText().equals("")) {

				AcideSearchWindow.getInstance().setOnTop(false);

				// INFORMS TO THE USER
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s585"));
				AcideSearchWindow.getInstance().setOnTop(true);
				AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s585"));
			} else {

				// Puts the wait cursor
				AcideMainWindow.getInstance().setCursor(
						Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				int selectedEditor = AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanelIndex();

				// SEARCH IN THE CURRENT DOCUMENT
				if (_currentDocumentRadioButton.isSelected()) {

					_selectedEditorIndex = -1;
					_counter = 0;
					_matchStartPosition = -1;
					_selectedText = null;
					selectedEditor = AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanelIndex();
					_matchStartPosition = AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedEditor)
							.getActiveTextEditionArea().getCaretPosition();

					// BACKWARD DIRECTION
					if (direction == AcideSearchDirection.BACKWARD) {
						_matchStartPosition = AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditor)
								.getActiveTextEditionArea().getSelectionStart();
					}
					selectedEditor = AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					_matchStartPosition = _search.search(_matchStartPosition,
							_searchTextField.getText(), AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(selectedEditor)
									.getTextEditionAreaContent(),
							_caseSensitiveCheckBox.isSelected(),
							_regularExpressionsCheckBox.isSelected(),
							_completeWordsCheckBox.isSelected(), direction);

					if (_matchStartPosition != -1) {

						AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(
										AcideMainWindow.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex())
								.selectText(_matchStartPosition,
										_searchTextField.getText().length());

						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s583")
										+ " "
										+ _searchTextField.getText()
										+ " "
										+ AcideLanguageManager.getInstance()
												.getLabels().getString("s574"));

						// Updates the status bar
						AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s583")
										+ " "
										+ _searchTextField.getText()
										+ " "
										+ AcideLanguageManager.getInstance()
												.getLabels().getString("s574"));

						// IF REGULAR EXPRESSIONS
						if (_regularExpressionsCheckBox.isSelected()) {

							// SHOWS THE SEARCH IN THE TEXT EDITOR
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(
											AcideMainWindow.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanelIndex())
									.selectText(
											_matchStartPosition,
											_search.getRegularExpresion()
													.length());

							// Updates the status bar
							AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s577")
											+ " "
											+ _search.getRegularExpresion()
											+ " "
											+ AcideLanguageManager
													.getInstance().getLabels()
													.getString("s574"));

							// Updates the log
							AcideLog.getLog().info(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s577")
											+ " "
											+ _search.getRegularExpresion()
											+ " "
											+ AcideLanguageManager
													.getInstance().getLabels()
													.getString("s574"));
						}
					} else {
						// Updates the log
						AcideLog.getLog().info(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s573"));

						AcideSearchWindow.getInstance().setOnTop(false);
						JOptionPane.showMessageDialog(null,
								AcideLanguageManager.getInstance().getLabels()
										.getString("s573"));
						AcideSearchWindow.getInstance().setOnTop(true);

						// Updates the status bar
						AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s573"));
					}
				}

				// IF SELECT TEXT
				if (_selectedTextRadioButton.isSelected()) {

					_counter = 0;
					_selectedEditorIndex = -1;
					selectedEditor = AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanelIndex();

					if (_selectedText == null) {

						_selectedText = AcideMainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(selectedEditor)
								.getActiveTextEditionArea().getSelectedText();
						_initialPosition = AcideMainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(selectedEditor)
								.getActiveTextEditionArea().getSelectionStart();
						_finalPosition = AcideMainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(selectedEditor)
								.getActiveTextEditionArea().getSelectionEnd();
						_matchStartPosition = 0;

						if (direction == AcideSearchDirection.BACKWARD)
							_matchStartPosition = _finalPosition;
						if ((_regularExpressionsCheckBox.isSelected())
								&& (direction != AcideSearchDirection.BACKWARD))
							_matchStartPosition = _initialPosition;
					} else {
						_matchStartPosition = AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(selectedEditor)
								.getActiveTextEditionArea().getCaretPosition()
								- _initialPosition;

						if (direction == AcideSearchDirection.BACKWARD) {
							_matchStartPosition = AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(selectedEditor)
									.getActiveTextEditionArea()
									.getSelectionStart()
									- _initialPosition;
						}
					}

					if (_selectedText == null) {
						AcideSearchWindow.getInstance().setOnTop(false);
						JOptionPane.showMessageDialog(null,
								AcideLanguageManager.getInstance().getLabels()
										.getString("s616"));
						AcideSearchWindow.getInstance().setOnTop(true);
						AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s616"));
					} else {
						_matchStartPosition = _search.search(
								_matchStartPosition,
								_searchTextField.getText(), _selectedText,
								_caseSensitiveCheckBox.isSelected(),
								_regularExpressionsCheckBox.isSelected(),
								_completeWordsCheckBox.isSelected(), direction);

						if (_matchStartPosition != -1) {
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(
											AcideMainWindow.getInstance()
													.getFileEditorManager()
													.getSelectedFileEditorPanelIndex())
									.selectText(
											_matchStartPosition
													+ _initialPosition,
											_searchTextField.getText().length());

							// Updates the log
							AcideLog.getLog().info(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s583")
											+ " "
											+ _searchTextField.getText()
											+ " "
											+ AcideLanguageManager
													.getInstance().getLabels()
													.getString("s574"));

							// Updates the status bar
							AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s583")
											+ " "
											+ _searchTextField.getText()
											+ " "
											+ AcideLanguageManager
													.getInstance().getLabels()
													.getString("s574"));

							// REGULAR EXPRESSIONS
							if (_regularExpressionsCheckBox.isSelected()) {

								// SHOWS THE SEARCH IN THE TEXT EDITOR
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(
												AcideMainWindow.getInstance()
														.getFileEditorManager()
														.getSelectedFileEditorPanelIndex())
										.selectText(
												_matchStartPosition
														+ _initialPosition,
												_search.getRegularExpresion()
														.length());

								// Updates the status bar
								AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
										AcideLanguageManager.getInstance()
												.getLabels().getString("s329")
												+ " "
												+ _searchTextField.getText()
												+ " "
												+ AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s574"));

								// Updates the log
								AcideLog.getLog().info(
										AcideLanguageManager.getInstance()
												.getLabels().getString("s329")
												+ " "
												+ _search.getRegularExpresion()
												+ " "
												+ AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s574"));
							}
						} else {

							_selectedText = null;

							// Updates the log
							AcideLog.getLog().info(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s573"));

							AcideSearchWindow.getInstance().setOnTop(false);
							JOptionPane.showMessageDialog(null,
									AcideLanguageManager.getInstance()
											.getLabels().getString("s573"));
							AcideSearchWindow.getInstance().setOnTop(true);
							AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s573"));
							AcideSearchWindow.getInstance().setOnTop(false);
							int op = JOptionPane.showConfirmDialog(null,
									AcideLanguageManager.getInstance()
											.getLabels().getString("s575"));
							AcideSearchWindow.getInstance().setOnTop(true);

							if (op == JOptionPane.OK_OPTION) {
								_currentDocumentRadioButton.setSelected(true);

								if (direction != AcideSearchDirection.BACKWARD)
									_matchStartPosition = _finalPosition;
								else
									_matchStartPosition = _initialPosition;
								_searchButton.doClick();
							}
						}
					}
				}

				// ALL DOCUMENTS SEARCH
				if (_allDocumentsRadioButton.isSelected()) {

					_selectedText = null;
					selectedEditor = AcideMainWindow.getInstance().getFileEditorManager()
							.getNumberOfFileEditorPanels();

					if ((_isCycle == false) && (_currentPosition == -2)) {
						_selectedEditorIndex = AcideMainWindow.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanelIndex();
						_currentDocument = _selectedEditorIndex;
						_currentPosition = AcideMainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(_selectedEditorIndex)
								.getActiveTextEditionArea().getCaretPosition();
					}

					if (_isEnd == false) {

						if (direction == AcideSearchDirection.FORWARD)
							_matchStartPosition = AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(_selectedEditorIndex)
									.getActiveTextEditionArea()
									.getCaretPosition();
						if (direction == AcideSearchDirection.BACKWARD)
							_matchStartPosition = AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(_selectedEditorIndex)
									.getActiveTextEditionArea()
									.getSelectionStart();
						if (direction == AcideSearchDirection.BOTH) {
							_matchStartPosition = AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(_selectedEditorIndex)
									.getActiveTextEditionArea()
									.getCaretPosition();
						}

						AcideSearchDirection auxDirection = direction;

						if (direction == AcideSearchDirection.BOTH)
							auxDirection = AcideSearchDirection.FORWARD;

						_matchStartPosition = _search.search(
								_matchStartPosition,
								_searchTextField.getText(),
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(
												_selectedEditorIndex)
										.getTextEditionAreaContent(),
								_caseSensitiveCheckBox.isSelected(),
								_regularExpressionsCheckBox.isSelected(),
								_completeWordsCheckBox.isSelected(),
								auxDirection);

						if ((_isCycle)
								&& (_selectedEditorIndex == _currentDocument)
								&& (_matchStartPosition >= _currentPosition))
							_isEnd = true;
						else if ((_isCycle)
								&& (_selectedEditorIndex == _currentDocument)
								&& (_matchStartPosition == -1))
							_isEnd = true;

						if (_matchStartPosition != -1) {

							_counter++;

							if (!_regularExpressionsCheckBox.isSelected()) {

								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(
												_selectedEditorIndex)
										.selectText(
												_matchStartPosition,
												_searchTextField.getText()
														.length());

								// Updates the log
								AcideLog.getLog().info(
										AcideLanguageManager.getInstance()
												.getLabels().getString("s583")
												+ " "
												+ _searchTextField.getText()
												+ " "
												+ AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s574"));

								// Updates the status bar
								AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
										AcideLanguageManager.getInstance()
												.getLabels().getString("s583")
												+ " "
												+ _searchTextField.getText()
												+ " "
												+ AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s574"));
							} else {

								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(
												_selectedEditorIndex)
										.selectText(
												_matchStartPosition,
												_search.getRegularExpresion()
														.length());
								// Updates the log
								AcideLog.getLog().info(
										AcideLanguageManager.getInstance()
												.getLabels().getString("s577")
												+ " "
												+ _search.getRegularExpresion()
												+ AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s577"));

								// Updates the status bar
								AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
										AcideLanguageManager.getInstance()
												.getLabels().getString("s577")
												+ " "
												+ _search.getRegularExpresion()
												+ AcideLanguageManager
														.getInstance()
														.getLabels()
														.getString("s577"));
							}
						} else {

							// Updates the log
							AcideLog.getLog().info(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s573"));

							AcideMainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_selectedEditorIndex)
									.getActiveTextEditionArea()
									.setCaretPosition(0);
							if (_forwardRadioButton.isSelected() == true)
								_selectedEditorIndex++;
							else if (_backwardRadioButton.isSelected() == true)
								_selectedEditorIndex--;
							else
								_selectedEditorIndex++;

							if (direction == AcideSearchDirection.FORWARD) {
								if (_selectedEditorIndex >= selectedEditor) {
									_isEnd = true;
								} else {
									AcideMainWindow.getInstance().getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													_selectedEditorIndex);
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(
													_selectedEditorIndex)
											.getActiveTextEditionArea()
											.setCaretPosition(0);
									_searchButton.doClick();
								}
							}
							if (direction == AcideSearchDirection.BACKWARD) {
								if (_selectedEditorIndex < 0) {
									_isEnd = true;
								} else {
									AcideMainWindow.getInstance().getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													_selectedEditorIndex);
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(
													_selectedEditorIndex)
											.getActiveTextEditionArea()
											.setCaretPosition(
													AcideMainWindow.getInstance()
															.getFileEditorManager()
															.getFileEditorPanelAt(
																	_selectedEditorIndex)
															.getActiveTextEditionArea()
															.getText().length() - 1);
									_searchButton.doClick();
								}
							}
							if (direction == AcideSearchDirection.BOTH) {
								if (_selectedEditorIndex >= selectedEditor) {
									_selectedEditorIndex = 0;
									_isCycle = true;
									AcideMainWindow.getInstance().getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													_selectedEditorIndex);
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(
													_selectedEditorIndex)
											.getActiveTextEditionArea()
											.setCaretPosition(0);
									_searchButton.doClick();

								} else {
									AcideMainWindow.getInstance().getFileEditorManager()
											.setSelectedFileEditorPanelAt(
													_selectedEditorIndex);
									AcideMainWindow.getInstance()
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

					if (_isEnd && _isFirst) {
						if (_counter == 0) {

							AcideSearchWindow.getInstance().setOnTop(false);
							JOptionPane.showMessageDialog(null,
									AcideLanguageManager.getInstance()
											.getLabels().getString("s576"));
							AcideSearchWindow.getInstance().setOnTop(true);
							AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s576"));
						} else {

							AcideSearchWindow.getInstance().setOnTop(false);
							JOptionPane.showMessageDialog(null,
									AcideLanguageManager.getInstance()
											.getLabels().getString("s586"));
							AcideSearchWindow.getInstance().setOnTop(true);
							AcideMainWindow.getInstance().getStatusBar().setStatusMessage(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s586"));
						}
						_isFirst = false;
					}
				}
			}

			// Puts the default cursor
			AcideMainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}
	}

	/**
	 * ACIDE - A Configurable IDE search window cancel button listener.
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

			// Closes the window
			AcideSearchWindow.getInstance().dispose();
		}
	}
}