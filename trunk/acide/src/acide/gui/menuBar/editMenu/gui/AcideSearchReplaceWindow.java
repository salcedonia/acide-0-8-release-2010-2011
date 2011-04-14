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
package acide.gui.menuBar.editMenu.gui;

import acide.factory.operations.AcideOperationsFactory;
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
import java.awt.event.KeyEvent;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.border.TitledBorder;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE search/replace window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideSearchReplaceWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE search/replace window class serial version
	 * UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE search/replace window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon(
			"./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE search/replace window unique class instance.
	 */
	private static AcideSearchReplaceWindow _instance;
	/**
	 * ACIDE - A Configurable IDE search/replace window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE search/replace window direction panel.
	 */
	private JPanel _directionPanel;
	/**
	 * ACIDE - A Configurable IDE search/replace window option panel.
	 */
	private JPanel _optionPanel;
	/**
	 * ACIDE - A Configurable IDE search/replace window scope panel.
	 */
	private JPanel _scopePanel;
	/**
	 * ACIDE - A Configurable IDE search/replace window replace label.
	 */
	private JLabel _searchLabel;
	/**
	 * ACIDE - A Configurable IDE search/replace window replace text field.
	 */
	private JTextField _searchTextField;
	/**
	 * ACIDE - A Configurable IDE search/replace window replaced label.
	 */
	private JLabel _replaceLabel;
	/**
	 * ACIDE - A Configurable IDE search/replace window replaced text field.
	 */
	private JTextField _replaceTextField;
	/**
	 * ACIDE - A Configurable IDE search/replace window case sensitive check
	 * box.
	 */
	private JCheckBox _caseSensitiveCheckBox;
	/**
	 * ACIDE - A Configurable IDE search/replace window regular expressions
	 * check box.
	 */
	private JCheckBox _regularExpressionsCheckBox;
	/**
	 * ACIDE - A Configurable IDE search/replace window complete words check
	 * box.
	 */
	private JCheckBox _completeWordsCheckBox;
	/**
	 * ACIDE - A Configurable IDE search/replace window forward radio button.
	 */
	private JRadioButton _forwardRadioButton;
	/**
	 * ACIDE - A Configurable IDE search/replace window backward radio button.
	 */
	private JRadioButton _backwardRadioButton;
	/**
	 * ACIDE - A Configurable IDE search/replace window both directions radio
	 * button.
	 */
	private JRadioButton _bothDirectionsRadioButton;
	/**
	 * ACIDE - A Configurable IDE search/replace window current document radio
	 * button.
	 */
	private JRadioButton _currentDocumentRadioButton;
	/**
	 * ACIDE - A Configurable IDE search/replace window all documents radio
	 * button.
	 */
	private JRadioButton _allDocumentsRadioButton;
	/**
	 * ACIDE - A Configurable IDE search/replace window selected radio button.
	 */
	private JRadioButton _selectedTextRadioButton;
	/**
	 * ACIDE - A Configurable IDE search/replace window search button.
	 */
	private JButton _searchButton;
	/**
	 * ACIDE - A Configurable IDE search/replace window replace button.
	 */
	private JButton _replaceButton;
	/**
	 * ACIDE - A Configurable IDE search/replace window replace all button.
	 */
	private JButton _replaceAllButton;
	/**
	 * ACIDE - A Configurable IDE search/replace window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Result of the operations.
	 */
	private int _resultPosition;
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
	private int _allDocumentOccurrencesCounter;
	/**
	 * Current position.
	 */
	private int _currentCaretPosition;
	/**
	 * Selected editor index.
	 */
	private static int _currentCheckedEditorIndex;
	/**
	 * Flag that indicates if it is the end of a search.
	 */
	private static boolean _isSearchEnd = false;
	/**
	 * Flag that indicates if there is a cycle in the search.
	 */
	private static boolean _isCycle = false;
	/**
	 * Current document index.
	 */
	private int _currentCheckedDocumentIndex;
	/**
	 * Flag that indicates if it is the first replacement.
	 */
	private static boolean _isFirstReplacement = true;
	/**
	 * Search engine for performs the searches.
	 */
	private AcideSearchEngine _search = AcideOperationsFactory.getInstance()
			.buildSearch();

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window unique class
	 *         instance.
	 */
	public static AcideSearchReplaceWindow getInstance() {
		if (_instance == null)
			_instance = new AcideSearchReplaceWindow();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE search/replace window.
	 */
	public AcideSearchReplaceWindow() {

		// The initial position is 0
		_initialPosition = 0;

		// There is no selected text
		_selectedText = null;

		// The result is -1
		_resultPosition = -1;

		// Builds the window components
		buildComponents();

		// Adds the components to the window
		addComponents();

		// Sets the listeners of the window components
		setListeners();

		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE search/replace window components.
	 */
	private void buildComponents() {

		// Builds the search/replace text fields
		buildSearchReplaceTextFields();

		// Builds the direction panel
		buildDirectionPanel();

		// Builds the options panel
		buildOptionsPanel();

		// Builds the scope panel
		buildScopePanel();

		// Builds the button panel
		buildButtonPanel();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE search/replace window configuration.
	 */
	private void setWindowConfiguration() {

		// Sets the search window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s556"));

		// Sets the window icon image
		setIconImage(ICON.getImage());

		// The window is not resizable
		setResizable(false);

		// Packs the window components
		pack();

		// Always at the front
		setAlwaysOnTop(true);

		// Centers the window
		setLocationRelativeTo(null);
	}

	/**
	 * Adds the components to the window.
	 */
	public void addComponents() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 2;

		// Adds the replace label to the window
		add(_searchLabel, constraints);

		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridy = 1;

		// Adds the replace text field to the window
		add(_searchTextField, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.gridy = 2;

		// Adds the replaced label to the window
		add(_replaceLabel, constraints);

		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridy = 3;

		// Adds the replaced text field to the window
		add(_replaceTextField, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridy = 4;
		constraints.gridwidth = 2;

		// Adds the option panel to the window
		add(_optionPanel, constraints);

		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridy = 5;
		constraints.gridwidth = 1;

		// Adds the scope panel to the window
		add(_scopePanel, constraints);

		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 1;
		constraints.gridy = 5;

		// Adds the direction panel to the window
		add(_directionPanel, constraints);

		constraints.anchor = GridBagConstraints.LINE_END;
		constraints.gridx = 0;
		constraints.gridy = 6;
		constraints.gridwidth = 2;

		// Adds the button panel to the window
		add(_buttonPanel, constraints);

		// Validates the changes in the search/replace window
		validate();
	}

	/**
	 * Creates the search/replace labels and text fields.
	 */
	private void buildSearchReplaceTextFields() {

		// Creates the search label
		_searchLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s557"), JLabel.CENTER);

		// Creates the search text field
		_searchTextField = new JTextField();

		// Creates the replace label
		_replaceLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s558"), JLabel.CENTER);

		// Creates the replace text field
		_replaceTextField = new JTextField();
	}

	/**
	 * Creates the button panel and its components.
	 */
	private void buildButtonPanel() {

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the search button
		_searchButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s556"));

		// Creates the replace button
		_replaceButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s572"));

		// Enables it
		_replaceButton.setEnabled(true);

		// Creates the replace all button
		_replaceAllButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s571"));

		// Enables it
		_replaceAllButton.setEnabled(true);

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s42"));

		// Adds the search button to the button panel
		_buttonPanel.add(_searchButton);

		// Adds the replace button to the button panel
		_buttonPanel.add(_replaceButton);

		// Adds the replace all button to the button panel
		_buttonPanel.add(_replaceAllButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);
	}

	/**
	 * Creates the scope panel and its components.
	 */
	private void buildScopePanel() {

		// Creates the scope panel
		_scopePanel = new JPanel(new GridLayout(0, 1));

		// Sets the scope panel border
		_scopePanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s563"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// Creates the current document radio button
		_currentDocumentRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s565"), true);

		// Creates the all documents radio button
		_allDocumentsRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s566"), false);

		// Creates the selected text radio button
		_selectedTextRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s564"), false);

		// Adds the selected text radio button to the scope panel
		_scopePanel.add(_selectedTextRadioButton);

		// Adds the current document radio button to the scope panel
		_scopePanel.add(_currentDocumentRadioButton);

		// Adds the all documents radio button to the scope panel
		_scopePanel.add(_allDocumentsRadioButton);

		// Creates a button group
		ButtonGroup buttonGroup = new ButtonGroup();

		// Adds the selected text radio button to the button group
		buttonGroup.add(_selectedTextRadioButton);

		// Adds the current document radio button to the button group
		buttonGroup.add(_currentDocumentRadioButton);

		// Adds the all documents radio button to the button group
		buttonGroup.add(_allDocumentsRadioButton);
	}

	/**
	 * Creates the options panel and its components.
	 */
	private void buildOptionsPanel() {

		// Creates the option panel
		_optionPanel = new JPanel(new GridLayout(0, 1));

		// Sets the option panel border
		_optionPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s559"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// Creates the case sensitive check box
		_caseSensitiveCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s560"), false);

		// Creates the regular expressions check box
		_regularExpressionsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s561"), false);

		// Creates the complete words check box
		_completeWordsCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s562"), false);

		// Adds the case sensitive check box to the option panel
		_optionPanel.add(_caseSensitiveCheckBox);

		// Adds the regular expressions check box to the option panel
		_optionPanel.add(_regularExpressionsCheckBox);

		// Adds the complete words check box to the option panel
		_optionPanel.add(_completeWordsCheckBox);
	}

	/**
	 * Creates the direction panel and its components.
	 */
	private void buildDirectionPanel() {

		// Creates the direction panel
		_directionPanel = new JPanel(new GridLayout(0, 1));

		// Sets the direction panel border
		_directionPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s567"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// Creates the forward radio button
		_forwardRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s568"), true);

		// Creates the backward radio button
		_backwardRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s569"), false);

		// Creates the both directions radion button
		_bothDirectionsRadioButton = new JRadioButton(AcideLanguageManager
				.getInstance().getLabels().getString("s570"), false);

		// Adds the forward radio button to the direction panel
		_directionPanel.add(_forwardRadioButton);

		// Adds the backward radio button to the direction panel
		_directionPanel.add(_backwardRadioButton);

		// Adds the both directions radio button to the direction panel
		_directionPanel.add(_bothDirectionsRadioButton);

		// Creates a button group
		ButtonGroup buttonGroup = new ButtonGroup();

		// Adds the forward radio button to the button group
		buttonGroup.add(_forwardRadioButton);

		// Adds the backward radio button to the button group
		buttonGroup.add(_backwardRadioButton);

		// Adds the both directions radio button to the button group
		buttonGroup.add(_bothDirectionsRadioButton);
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// Sets the search button action listener
		_searchButton.addActionListener(new SearchButtonListener());

		// When the enter key is pressed the executes the search button action
		_searchButton.registerKeyboardAction(new SearchButtonListener(),
				"EnterKey", KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// Sets the replace button action listener
		_replaceButton.addActionListener(new ReplaceButtonListener());

		// Sets the replace all button action listener
		_replaceAllButton.addActionListener(new ReplaceAllButtonListener());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonListener());

		// When the escape key is pressed the executes the cancel button action
		_cancelButton.registerKeyboardAction(new CancelButtonListener(),
				"EscapeKey",
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);
	}

	/**
	 * Shows the window with the appropriated configuration depending on the
	 * parameter.
	 * 
	 * @param isReplaceConfiguration
	 *            flag that indicates if the window has the replace or the
	 *            search configuration. False -> search configuration and true
	 *            -> replace configuration.
	 */
	public void showWindow(boolean isReplaceConfiguration) {

		if (isReplaceConfiguration) {

			// Sets the replace window configuration
			setReplaceWindowConfiguration();

		} else {

			// Sets the search window configuration
			setSearchWindowConfiguration();
		}

		// Displays the window
		setVisible(true);
	}

	/**
	 * Sets the search window configuration disabling some components and
	 * changing the window title.
	 */
	private void setSearchWindowConfiguration() {

		// Sets the search window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s556"));

		// Disables the replace label
		_replaceLabel.setVisible(false);

		// Disables the replace text field
		_replaceTextField.setVisible(false);

		// Disables the replace button
		_replaceButton.setVisible(false);

		// Disables the replace all button
		_replaceAllButton.setVisible(false);

		// Sets the preferred size based on the components
		pack();

		// Centers the window
		setLocationRelativeTo(null);

		// Validates the changes in the search/replace window
		validate();

		// Repaints the search/replace window
		repaint();
	}

	/**
	 * Sets the replace window configuration enabling some components and
	 * changing the window title.
	 */
	private void setReplaceWindowConfiguration() {

		// Sets the replace window title
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s572"));

		// Enables the replace label
		_replaceLabel.setVisible(true);

		// Enables the replace text field
		_replaceTextField.setVisible(true);

		// Enables the replace button
		_replaceButton.setVisible(true);

		// Enables the replace all button
		_replaceAllButton.setVisible(true);

		// Sets the preferred size based on the components
		pack();

		// Centers the window
		setLocationRelativeTo(null);

		// Validates the changes in the search/replace window
		validate();

		// Repaints the search/replace window
		repaint();
	}

	/**
	 * Initializes the ACIDE - A Configurable IDE search/replace window
	 * instance.
	 * 
	 * Points it to null, so the next time the method getInstance() is invoked,
	 * it will generate the window.
	 */
	public void initialize() {
		_instance = null;
	}

	/**
	 * Initializes the ACIDE - A Configurable IDE search/replace window
	 * variables.
	 */
	public void initializeVariables() {

		// Sets the current position as -2
		_currentCaretPosition = -2;

		// Sets the is cycle flag as false
		_isCycle = false;

		// Sets the is end flag as false
		_isSearchEnd = false;

		// Sets the selected text as null
		_selectedText = null;

		// Sets the is first replacement as true
		_isFirstReplacement = true;

		// Sets the search engine temporal position as -2
		_search.setTemporalPosition(-2);

		// Sets the search engine is cycle flag as false
		_search.setIsCycle(false);
	}

	/**
	 * Sets the window in the front of all the opened windows.
	 * 
	 * @param alwaysOnTop
	 *            true if it set in the front and false in other case.
	 */
	public void bringToFront(boolean alwaysOnTop) {
		setAlwaysOnTop(alwaysOnTop);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window search
	 * engine.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window search
	 *         engine.
	 */
	public AcideSearchEngine getSearch() {
		return _search;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window search
	 * button.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window search
	 *         button.
	 */
	public JButton getSearchButton() {
		return _searchButton;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window current
	 * document radio button.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window current
	 *         document radio button.
	 */
	public JRadioButton getCurrentDocumentRadioButton() {
		return _currentDocumentRadioButton;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window both
	 * directions radio button.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window both
	 *         directions radio button.
	 */
	public JRadioButton getBothDirectionsRadioButton() {
		return _bothDirectionsRadioButton;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window selected
	 * text radio button.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window selected
	 *         text radio button.
	 */
	public JRadioButton getSelectedTextRadioButton() {
		return _selectedTextRadioButton;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window search text
	 * field.
	 * 
	 * @return the the ACIDE - A Configurable IDE search/replace window search
	 *         text field.
	 */
	public JTextField getSearchTextField() {
		return _searchTextField;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window replace text
	 * field.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window replace text
	 *         field.
	 */
	public JTextField getReplaceTextField() {
		return _replaceTextField;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window forward
	 * radio button.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window forward
	 *         radio button.
	 */
	public JRadioButton getForwardRadioButton() {
		return _forwardRadioButton;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window backward
	 * radio button.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window backward
	 *         radio button.
	 */
	public JRadioButton getBackwardRadioButton() {
		return _backwardRadioButton;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window complete
	 * words check box.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window complete
	 *         words check box.
	 */
	public JCheckBox getCompleteWordsCheckBox() {
		return _completeWordsCheckBox;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window regular
	 * expressions check box.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window regular
	 *         expressions check box.
	 */
	public JCheckBox getRegularExpressionsCheckBox() {
		return _regularExpressionsCheckBox;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window case
	 * sensitive check box.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window case
	 *         sensitive check box.
	 */
	public JCheckBox getCaseSensitiveCheckBox() {
		return _caseSensitiveCheckBox;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window all
	 * documents radio button.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window all
	 *         documents radio button.
	 */
	public JRadioButton getAllDocumentsRadioButton() {
		return _allDocumentsRadioButton;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE search/replace window
	 * result.
	 * 
	 * @param result
	 *            new value to set.
	 */
	public void setResult(int result) {
		_resultPosition = result;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window result.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window result.
	 */
	public int getResult() {
		return _resultPosition;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window selected
	 * text.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window selected
	 *         text.
	 */
	public String getSelectedText() {
		return _selectedText;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE search/replace window
	 * selected text.
	 * 
	 * @param selectedText
	 *            new value to set.
	 */
	public void setSelectedText(String selectedText) {
		_selectedText = selectedText;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE search/replace window
	 * initial position.
	 * 
	 * @param initialPosition
	 *            new value to set.
	 */
	public void setInitialPosition(int initialPosition) {
		_initialPosition = initialPosition;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE search/replace window
	 * final position.
	 * 
	 * @param finalPosition
	 *            new value to set.
	 */
	public void setFinalPosition(int finalPosition) {
		_finalPosition = finalPosition;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window final
	 * position.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window final
	 *         position.
	 */
	public int getFinalPosition() {
		return _finalPosition;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window initial
	 * position.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window initial
	 *         position.
	 */
	public int getInitialPosition() {
		return _initialPosition;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window replacement
	 * counter.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window replacement
	 *         counter.
	 */
	public int getCounter() {
		return _allDocumentOccurrencesCounter;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE search/replace window
	 * counter.
	 * 
	 * @param counter
	 *            new value to set.
	 */
	public void setCounter(int counter) {
		_allDocumentOccurrencesCounter = counter;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window is end flag.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window is end flag.
	 */
	public boolean getIsEnd() {
		return _isSearchEnd;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE search/replace window
	 * is end flag.
	 * 
	 * @param isEnd
	 *            new value to set.
	 */
	public void setIsEnd(boolean isEnd) {
		_isSearchEnd = isEnd;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE search/replace window
	 * is cycle flag.
	 * 
	 * @param isCycle
	 *            new value to set.
	 */
	public void setIsCycle(boolean isCycle) {
		_isCycle = isCycle;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window is cycle
	 * flag.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window is cycle
	 *         flag.
	 */
	public boolean getIsCycle() {
		return _isCycle;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window current
	 * position.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window current
	 *         position.
	 */
	public int getCurrentPosition() {
		return _currentCaretPosition;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE search/replace window
	 * current position.
	 * 
	 * @param currentPosition
	 *            new value to set.
	 */
	public void setCurrentPosition(int currentPosition) {
		_currentCaretPosition = currentPosition;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window selected
	 * editor index.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window selected
	 *         editor index.
	 */
	public int getSelectedEditorIndex() {
		return _currentCheckedEditorIndex;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE search/replace window
	 * selected editor index.
	 * 
	 * @param selectedEditorIndex
	 *            new value to set.
	 */
	public void setSelectedEditorIndex(int selectedEditorIndex) {
		_currentCheckedEditorIndex = selectedEditorIndex;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE search/replace window
	 * current document index.
	 * 
	 * @param currentDocumentIndex
	 *            new value to set.
	 */
	public void setCurrentDocument(int currentDocumentIndex) {
		_currentCheckedDocumentIndex = currentDocumentIndex;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window current
	 * document index.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window current
	 *         document index.
	 */
	public int getCurrentDocumentIndex() {
		return _currentCheckedDocumentIndex;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE search/replace window is first
	 * replacement flag.
	 * 
	 * @return the ACIDE - A Configurable IDE search/replace window is first
	 *         replacement flag.
	 */
	public boolean getIsFirstReplacement() {
		return _isFirstReplacement;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE search/replace window
	 * is first replacement flag.
	 * 
	 * @param isFirstReplacement
	 *            new value to set.
	 */
	public static void setIsFirstReplacement(boolean isFirstReplacement) {
		_isFirstReplacement = isFirstReplacement;
	}

	/**
	 * Counts the number of times a substring occurs in a provided string.
	 * 
	 * @param source
	 *            The <code>String</code> object that will be searched in.
	 * @param substring
	 *            The string whose occurrences will we counted.
	 * @param matchCase
	 *            A <code>boolean</code> indicating if the match is going to be
	 *            performed in a case-sensitive manner or not.
	 * @return An <code>integer</code> value containing the number of
	 *         occurrences of the substring.
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
	 * Performs the all documents search.
	 * 
	 * @param searchDirection
	 *            search direction.
	 * @param isReplace
	 *            is search or replace flag. True -> replace action and false ->
	 *            search action.
	 */
	public void allDocumentsSearchOrReplace(AcideSearchDirection searchDirection,
			boolean isReplace) {

		// Initializes the selected text
		_selectedText = null;

		// Gets the selected file editor panel index
		int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanelIndex();

		// If it is not a cycle and the current caret position is -2
		if (!_isCycle && _currentCaretPosition == -2) {

			// Starts with the current selected file editor panel
			_currentCheckedEditorIndex = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// The current checked document index is the current checked
			// editor index
			_currentCheckedDocumentIndex = _currentCheckedEditorIndex;

			// Gets the current caret position from the selected file
			// editor
			_currentCaretPosition = AcideMainWindow.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(_currentCheckedEditorIndex)
					.getActiveTextEditionArea().getCaretPosition();
		}

		// If it is not the end
		if (!_isSearchEnd) {

			// If the direction is forwards
			if (searchDirection == AcideSearchDirection.FORWARD)

				// The result position is the current caret position
				_resultPosition = AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(_currentCheckedEditorIndex)
						.getActiveTextEditionArea().getCaretPosition();

			// If the direction is backwards
			if (searchDirection == AcideSearchDirection.BACKWARD)

				// The result position is the selection start
				_resultPosition = AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(_currentCheckedEditorIndex)
						.getActiveTextEditionArea().getSelectionStart();

			// If the direction is both directions
			if (searchDirection == AcideSearchDirection.BOTH) {

				// The result position is the current caret position
				_resultPosition = AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(_currentCheckedEditorIndex)
						.getActiveTextEditionArea().getCaretPosition();
			}

			// Gets the search direction in a temporal variable
			AcideSearchDirection temporalDirection = searchDirection;

			// If the search direction is both directions
			if (searchDirection == AcideSearchDirection.BOTH)

				// The temporal direction will be forwards
				temporalDirection = AcideSearchDirection.FORWARD;

			// Performs the search
			_resultPosition = _search.search(_resultPosition,
					_searchTextField.getText(),
					AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(_currentCheckedEditorIndex)
							.getTextEditionAreaContent(),
					_caseSensitiveCheckBox.isSelected(),
					_regularExpressionsCheckBox.isSelected(),
					_completeWordsCheckBox.isSelected(), temporalDirection);

			// Updates the is search end flag
			if (_isCycle
					&& (_currentCheckedEditorIndex == _currentCheckedDocumentIndex)
					&& (_resultPosition >= _currentCaretPosition))
				_isSearchEnd = true;
			else if (_isCycle
					&& (_currentCheckedEditorIndex == _currentCheckedDocumentIndex)
					&& (_resultPosition == -1))
				_isSearchEnd = true;

			// If found anything
			if (_resultPosition != -1) {

				// Increments the all document occurrences counter
				_allDocumentOccurrencesCounter++;

				// If the regular expressions check box is not selected
				if (!_regularExpressionsCheckBox.isSelected()) {

					// Brings the main window to foreground
					AcideMainWindow.getInstance().setAlwaysOnTop(true);

					// Selects the text in the editor from the result
					// position
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(_currentCheckedEditorIndex)
							.selectText(_resultPosition,
									_searchTextField.getText().length());

					// Brings the main window to background
					AcideMainWindow.getInstance().setAlwaysOnTop(false);

					// If it is replace action
					if (isReplace)

						// Replaces the selected text in the checked file editor
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(
										_currentCheckedEditorIndex)
								.getActiveTextEditionArea()
								.replaceSelection(_replaceTextField.getText());

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s583")
									+ " "
									+ _searchTextField.getText()
									+ " "
									+ AcideLanguageManager.getInstance()
											.getLabels().getString("s574"));

					// Updates the status message in the status
					// bar
					AcideMainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s583")
											+ " "
											+ _searchTextField.getText()
											+ " "
											+ AcideLanguageManager
													.getInstance().getLabels()
													.getString("s574"));
				} else {

					// Brings the main window to foreground
					AcideMainWindow.getInstance().setAlwaysOnTop(true);

					// Selects the text in the editor from the regular
					// expression
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(_currentCheckedEditorIndex)
							.selectText(_resultPosition,
									_search.getRegularExpresion().length());

					// Brings the main window to background
					AcideMainWindow.getInstance().setAlwaysOnTop(false);

					// If it is replace action
					if (isReplace)

						// Replaces the selected text in the checked file editor
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(
										_currentCheckedEditorIndex)
								.getActiveTextEditionArea()
								.replaceSelection(_replaceTextField.getText());

					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s577")
									+ " "
									+ _search.getRegularExpresion()
									+ " "
									+ AcideLanguageManager.getInstance()
											.getLabels().getString("s577"));

					// Updates the status message in the status
					// bar
					AcideMainWindow
							.getInstance()
							.getStatusBar()
							.setStatusMessage(
									AcideLanguageManager.getInstance()
											.getLabels().getString("s577")
											+ " "
											+ _search.getRegularExpresion()
											+ " "
											+ AcideLanguageManager
													.getInstance().getLabels()
													.getString("s577"));
				}

			} else {

				// It did not find anything

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s573"));

				// Updates the status message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s573"));

				// Sets the caret position in the first position
				AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(_currentCheckedEditorIndex)
						.getActiveTextEditionArea().setCaretPosition(0);

				// Updates the rest of the components of the checked
				// editor
				AcideMainWindow.getInstance().getFileEditorManager()
						.updateRelatedComponentsAt(_currentCheckedEditorIndex);

				// If the forward radio button is selected
				if (_forwardRadioButton.isSelected())

					// It checks the following file editor at next place
					_currentCheckedEditorIndex++;

				// If the backward radio button is selected
				else if (_backwardRadioButton.isSelected())

					// It checks the previous file editor at next place
					_currentCheckedEditorIndex--;

				else
					// It checks the following file editor at next place
					_currentCheckedEditorIndex++;

				// If the search direction is forwards
				if (searchDirection == AcideSearchDirection.FORWARD) {

					// If the current checked editor index is < to the
					// number of file editor panels
					if (_currentCheckedEditorIndex >= AcideMainWindow
							.getInstance().getFileEditorManager()
							.getNumberOfFileEditorPanels()) {

						// It is the search end
						_isSearchEnd = true;
					} else {

						// Sets the selected file editor as the current
						// checked
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.setSelectedFileEditorPanelAt(
										_currentCheckedEditorIndex);

						// Sets its caret position
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getActiveTextEditionArea().setCaretPosition(0);

						// Updates the rest of the components of the checked
						// editor
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.updateRelatedComponentsAt(
										_currentCheckedEditorIndex);

						// Performs the search on it
						_searchButton.doClick();
					}
				}

				// If the search direction is backwards
				if (searchDirection == AcideSearchDirection.BACKWARD) {

					// If the current checked editor index is negative
					if (_currentCheckedEditorIndex < 0) {

						// The search is over
						_isSearchEnd = true;
					} else {

						// Sets the selected file editor as the current
						// checked
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.setSelectedFileEditorPanelAt(
										_currentCheckedEditorIndex);

						// Sets the caret position at the last position
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getActiveTextEditionArea()
								.setCaretPosition(
										AcideMainWindow.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getActiveTextEditionArea()
												.getText().length() - 1);

						// Updates the rest of the components of the checked
						// editor
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.updateRelatedComponentsAt(
										_currentCheckedEditorIndex);

						// Performs the search on it
						_searchButton.doClick();
					}
				}

				// If the search direction is both directions
				if (searchDirection == AcideSearchDirection.BOTH) {

					// If the current checked editor index is >= to the
					// original selected file editor panel index
					if (_currentCheckedEditorIndex >= selectedFileEditorPanelIndex) {

						// The current checked editor index is the first
						// editor
						_currentCheckedEditorIndex = 0;

						// It is a cycle
						_isCycle = true;

						// Sets the selected file editor as the current
						// checked
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.setSelectedFileEditorPanelAt(
										_currentCheckedEditorIndex);

						// Sets the caret position at the first position on
						// it
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getActiveTextEditionArea().setCaretPosition(0);

						// Updates the rest of the components of the checked
						// editor
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.updateRelatedComponentsAt(
										_currentCheckedEditorIndex);

						// Performs the search
						_searchButton.doClick();

					} else {

						// Sets the selected file editor as the current
						// checked
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.setSelectedFileEditorPanelAt(
										_currentCheckedEditorIndex);

						// Sets the caret position at the first position on
						// it
						AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getActiveTextEditionArea().setCaretPosition(0);

						// Updates the rest of the components of the checked
						// editor
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.updateRelatedComponentsAt(
										_currentCheckedEditorIndex);

						// Performs the search
						_searchButton.doClick();
					}
				}
			}
		}

		// If the search has finished
		if (_isSearchEnd) {

			// If there are no occurrences in all the documents
			if (_allDocumentOccurrencesCounter == 0) {

				// Brings the search/replace window background
				AcideSearchReplaceWindow.getInstance().bringToFront(false);

				// Displays the
				// "no more occurrences in all the documents" message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s576"));

				// Brings the search/replace window foreground
				AcideSearchReplaceWindow.getInstance().bringToFront(true);

				// Updates the status message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s576"));
			} else {

				// Brings the search/replace window background
				AcideSearchReplaceWindow.getInstance().bringToFront(false);

				// Shows "no more findings" message
				JOptionPane.showMessageDialog(null, AcideLanguageManager
						.getInstance().getLabels().getString("s586"));

				// Brings the search/replace window foreground
				AcideSearchReplaceWindow.getInstance().bringToFront(true);

				// Updates the status message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s586"));
			}
		}
	}

	/**
	 * <p>
	 * Performs the current document search/replace.
	 * </p>
	 * <p>
	 * As the current document and the selected text search/replace are based in
	 * the same principle, they are only one method and there is no need to
	 * split it in two.
	 * </p>
	 * 
	 * @param searchDirection
	 *            search direction.
	 * @param isReplace
	 *            is search or replace flag. True -> replace action and false ->
	 *            search action.
	 */
	public void currentDocumentSearchOrReplace(AcideSearchDirection searchDirection,
			boolean isReplace) {

		// The result position is -1
		_resultPosition = -1;

		// The selected text is null
		_selectedText = null;

		// Gets the result position from the caret position of the selected
		// editor
		_resultPosition = AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getActiveTextEditionArea()
				.getCaretPosition();

		// If the search direction is backwards
		if (searchDirection == AcideSearchDirection.BACKWARD)

			// The result position is the selection start
			_resultPosition = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getActiveTextEditionArea().getSelectionStart();

		// Performs the search storing the result position
		_resultPosition = _search.search(_resultPosition,
				_searchTextField.getText(), AcideMainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.getTextEditionAreaContent(),
				_caseSensitiveCheckBox.isSelected(),
				_regularExpressionsCheckBox.isSelected(),
				_completeWordsCheckBox.isSelected(), searchDirection);

		// If it found something
		if (_resultPosition != -1) {

			// Selects the text in the editor
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getSelectedFileEditorPanel()
					.selectText(_resultPosition,
							_searchTextField.getText().length());

			// If it is replace action
			if (isReplace)

				// Replaces the selected text in the checked file editor
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getActiveTextEditionArea()
						.replaceSelection(_replaceTextField.getText());

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s583")
							+ _searchTextField.getText()
							+ AcideLanguageManager.getInstance().getLabels()
									.getString("s574"));

			// Updates the status message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setStatusMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s583")
									+ " "
									+ _searchTextField.getText()
									+ " "
									+ AcideLanguageManager.getInstance()
											.getLabels().getString("s574"));

			// If the regular expressions check box is selected
			if (_regularExpressionsCheckBox.isSelected()) {

				// Selects the text in the editor from the regular
				// expression
				AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getSelectedFileEditorPanel()
						.selectText(_resultPosition,
								_search.getRegularExpresion().length());

				// If it is replace action
				if (isReplace)

					// Replaces the selected text in the checked file editor
					AcideMainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getActiveTextEditionArea()
							.replaceSelection(_replaceTextField.getText());

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s577")
								+ " "
								+ _search.getRegularExpresion()
								+ " "
								+ AcideLanguageManager.getInstance()
										.getLabels().getString("s574"));

				// Updates the status message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setStatusMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s577")
										+ " "
										+ _search.getRegularExpresion()
										+ " "
										+ AcideLanguageManager.getInstance()
												.getLabels().getString("s574"));
			}
		} else {

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s573"));

			// Brings the search/replace window background
			AcideSearchReplaceWindow.getInstance().bringToFront(false);

			// Displays a warning message
			JOptionPane.showMessageDialog(null, AcideLanguageManager
					.getInstance().getLabels().getString("s573"));

			// Brings the search/replace window foreground
			AcideSearchReplaceWindow.getInstance().bringToFront(true);

			// Updates the status message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setStatusMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s573"));
		}
	}

	/**
	 * Performs the search or the replacing action depending on a variable given
	 * as a parameter.
	 * 
	 * @param isSearch
	 *            is search or replace flag. True -> replace action and false ->
	 *            search action.
	 */
	private void searchOrReplace(boolean isSearch) {

		// Is not the first replacement
		_isFirstReplacement = false;

		// Gets the search direction
		AcideSearchDirection searchDirection = AcideSearchDirection.FORWARD;

		// If the forward radio button is selected
		if (_forwardRadioButton.isSelected())

			// The search direction is forwards
			searchDirection = AcideSearchDirection.FORWARD;

		// If the backward radio button is selected
		if (_backwardRadioButton.isSelected())

			// The search direction is backwards
			searchDirection = AcideSearchDirection.BACKWARD;

		// If the both directions radio button is selected
		if (_bothDirectionsRadioButton.isSelected())

			// The search direction is both directions
			searchDirection = AcideSearchDirection.BOTH;

		// If the complete words is selected
		if (_completeWordsCheckBox.isSelected())

			// Regular expressions are disabled
			_regularExpressionsCheckBox.setSelected(false);

		// If the replace text field is empty
		if (!_searchTextField.getText().equals("")) {

			// If there are opened file editors
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getNumberOfFileEditorPanels() > 0) {

				// Puts the wait cursor
				AcideMainWindow.getInstance().setCursor(
						Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				// If the current document radio button is selected or the
				// selected text radio button is selected
				if (_currentDocumentRadioButton.isSelected()
						|| _selectedTextRadioButton.isSelected())

					// Performs the current document search/replace
					currentDocumentSearchOrReplace(searchDirection, isSearch);

				// If the all documents radio button is selected
				if (_allDocumentsRadioButton.isSelected())

					// Performs the all documents search/replace
					allDocumentsSearchOrReplace(searchDirection, isSearch);

				// Puts the default cursor
				AcideMainWindow.getInstance().setCursor(
						Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
			}
		} else {

			// Brings the search/replace window background
			AcideSearchReplaceWindow.getInstance().bringToFront(false);

			// Displays an error message
			JOptionPane.showMessageDialog(null, AcideLanguageManager
					.getInstance().getLabels().getString("s585"), "Error",
					JOptionPane.ERROR_MESSAGE);

			// Brings the search/replace window foreground
			AcideSearchReplaceWindow.getInstance().bringToFront(true);

			// Updates the status message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setStatusMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s585"));
		}
	}

	/**
	 * ACIDE - A Configurable IDE search/replace window search button listener.
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

			// Performs the search action
			searchOrReplace(false);
		}
	}

	/**
	 * ACIDE - A Configurable IDE search/replace window cancel button listener.
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
			dispose();
		}
	}

	/**
	 * ACIDE - A Configurable IDE search/replace window replace button listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ReplaceButtonListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Performs the replace action
			searchOrReplace(true);
		}
	}

	/**
	 * ACIDE - A Configurable IDE search/replace window replace all button
	 * listener.
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

			// Puts the wait cursor
			AcideMainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			// Gets the original caret position
			int caretPosition = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getActiveTextEditionArea().getCaretPosition();

			// Gets the selected file editor panel index
			int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// Initializes the number of replacements
			_globalNumberOfReplacements = 0;

			// If the selected text radio button is selected
			if (_selectedTextRadioButton.isSelected())

				// Performs the selected text replace
				selectedTextReplace();

			// If the current document radio button is selected
			if (_currentDocumentRadioButton.isSelected())

				// Performs the current document replace
				currentDocumentReplace();

			// If the all documents radio button is selected
			if (_allDocumentsRadioButton.isSelected())

				// Performs the all document replace
				allDocumentsReplace();

			// Informs of the number of replacements
			JOptionPane.showMessageDialog(
					AcideSearchReplaceWindow.getInstance(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s1000")
							+ _globalNumberOfReplacements, AcideLanguageManager
							.getInstance().getLabels().getString("s572"),
					JOptionPane.INFORMATION_MESSAGE);

			// Updates the status message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setStatusMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s1000")
									+ _globalNumberOfReplacements);

			// Restores the original selected file editor panel
			AcideMainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(selectedFileEditorPanelIndex);

			// Restores the original caret position
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getActiveTextEditionArea()
					.setCaretPosition(caretPosition);

			// Puts the default cursor
			AcideMainWindow.getInstance().setCursor(
					Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
		}

		/**
		 * Performs the selected text replace.
		 */
		private void selectedTextReplace() {

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

			// If there is anything selected
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getFileEditorPanelAt(selectedEditorIndex)
					.getActiveTextEditionArea().getSelectedText() != null) {

				// Gets the number of replacements
				_globalNumberOfReplacements = countMatches(AcideMainWindow
						.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(selectedEditorIndex)
						.getActiveTextEditionArea().getText(),
						_searchTextField.getText(),
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

					// Gets the text from the selected file editor panel
					String text = AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getText();

					// Gets the text before the selected text
					textBeforeSelectedText = text.substring(0, selectionStart);

					// Gets the text after the selected text
					textAfterSelectedText = text.substring(selectionSize,
							selectedEditorTextSize);

					// If the case sensitive check box is selected
					if (_caseSensitiveCheckBox.isSelected()) {

						// Replaces all the occurrences without taking care of
						// the case sensitive
						textReplaced = textBeforeSelectedText.replaceAll(
								_searchTextField.getText(),
								_replaceTextField.getText())
								+ selectedEditorText.replaceAll(
										_searchTextField.getText(),
										_replaceTextField.getText())
								+ textAfterSelectedText.replaceAll(
										_searchTextField.getText(),
										_replaceTextField.getText());
					} else

						// Prepends the case insensitive pattern modifier
						// (?i)
						// before our regex to indicate that we don’t care
						// about the case sensitivity of the regex.
						textReplaced = textBeforeSelectedText.replaceAll("(?i)"
								+ _searchTextField.getText(),
								_replaceTextField.getText())
								+ selectedEditorText.replaceAll("(?i)"
										+ _searchTextField.getText(),
										_replaceTextField.getText())
								+ textAfterSelectedText.replaceAll("(?i)"
										+ _searchTextField.getText(),
										_replaceTextField.getText());

					// Updates the selected editor text with the text
					AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().setText(textReplaced);
				}
			}
		}

		/**
		 * Performs the current document replace.
		 */
		private void currentDocumentReplace() {

			String selectedEditorText = null;

			// Gets the selected editor index
			int selectedEditorIndex = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// Gets the number of replacements
			_globalNumberOfReplacements = countMatches(
					AcideMainWindow.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getText(),
					_searchTextField.getText(),
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

				// If the case sensitive check box
				if (_caseSensitiveCheckBox.isSelected())

					// Replaces all the occurrences without taking care of
					// the case sensitive
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea()
							.setText(
									selectedEditorText.replaceAll(
											_searchTextField.getText(),
											_replaceTextField.getText()));
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
											+ _searchTextField.getText(),
											_replaceTextField.getText()));
				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s582")
								+ _searchTextField.getText()
								+ AcideLanguageManager.getInstance()
										.getLabels().getString("s580")
								+ _replaceTextField.getText());
			}
		}

		/**
		 * Performs the all documents replace.
		 */
		private void allDocumentsReplace() {

			String selectedEditorText = null;

			// Gets the selected editor index
			int selectedEditorIndex = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// Gets the original caret position
			int originalCaretPosition = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getActiveTextEditionArea().getCaretPosition();

			// Initializes the number of replacements
			_globalNumberOfReplacements = 0;

			// For each one of the opened editors
			for (int editorIndex = 0; editorIndex < AcideMainWindow
					.getInstance().getFileEditorManager()
					.getNumberOfFileEditorPanels(); editorIndex++) {

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
						_searchTextField.getText(),
						_caseSensitiveCheckBox.isSelected());

				// If there are replacements
				if (_localNumberOfReplacements > 0) {

					// Updates the number of replacements
					_globalNumberOfReplacements += _localNumberOfReplacements;

					// If the case sensitive check box is selected
					if (_caseSensitiveCheckBox.isSelected())

						// Replaces all the occurrences without taking care of
						// the case sensitive
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getFileEditorPanelAt(editorIndex)
								.getActiveTextEditionArea()
								.setText(
										selectedEditorText.replaceAll(
												_searchTextField.getText(),
												_replaceTextField.getText()));
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
										selectedEditorText.replaceAll("(?i)"
												+ _searchTextField.getText(),
												_replaceTextField.getText()));
				}
			}

			// Sets the original selected editor index
			AcideMainWindow.getInstance().getFileEditorManager()
					.setSelectedFileEditorPanelAt(selectedEditorIndex);

			// Sets the original caret position
			AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getActiveTextEditionArea()
					.setCaretPosition(originalCaretPosition);
		}
	}
}
