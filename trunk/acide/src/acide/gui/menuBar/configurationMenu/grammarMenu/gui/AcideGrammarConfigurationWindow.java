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
package acide.gui.menuBar.configurationMenu.grammarMenu.gui;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.border.TitledBorder;

import acide.configuration.grammar.AcideGrammarConfiguration;
import acide.files.AcideFileManager;
import acide.files.bytes.AcideByteFileManager;
import acide.gui.listeners.AcideWindowClosingListener;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.process.parser.AcideGrammarFileCreationProcess;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE grammar configuration window.
 * 
 * @version 0.8
 * @see JFrame
 */
public class AcideGrammarConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE grammar configuration window class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window image icon.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * ACIDE - A Configurable IDE grammar configuration window categories panel.
	 */
	private JPanel _categoriesPanel;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window rules panel.
	 */
	private JPanel _rulesPanel;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window categories button
	 * panel.
	 */
	private JPanel _categoriesButtonPanel;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window rules button
	 * panel.
	 */
	private JPanel _rulesButtonPanel;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window categories text
	 * area.
	 */
	private JTextArea _categoriesTextArea;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window rules text area.
	 */
	private JTextArea _rulesTextArea;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window categories scroll
	 * pane.
	 */
	private JScrollPane _categoriesScrollPane;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window rules scroll
	 * pane.
	 */
	private JScrollPane _rulesScrollPane;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window load categories
	 * button.
	 */
	private JButton _loadCategoriesButton;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window save categories
	 * button.
	 */
	private JButton _saveCategoriesButton;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window load rules
	 * button.
	 */
	private JButton _loadRulesButton;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window save rules
	 * button.
	 */
	private JButton _saveRulesButton;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window verbose process
	 * check box.
	 */
	private JCheckBox _verboseProcessCheckBox;

	/**
	 * ACIDE - A Configurable IDE grammar configuration window is for modifying
	 * flag.
	 */
	private boolean _isForModifying;

	/**
	 * Creates a new ACIDE - A Configurable IDE grammar configuration window.
	 * 
	 * @param isForModifying
	 *            indicates if the window is used for modifying or creating a
	 *            grammar.
	 */
	public AcideGrammarConfigurationWindow(boolean isForModifying) {

		super();

		// Stores the is for modifying flag
		_isForModifying = isForModifying;

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s173"));

		// Builds the window components
		buildComponents();

		// Sets the listener of the window components
		setListeners();

		// Adds the components to the window
		addComponents();

		// Sets the window configuration
		setWindowConfiguration();
	}

	/**
	 * Sets the ACIDE - A Configurable IDE grammar configuration window
	 * configuration.
	 */
	private void setWindowConfiguration() {

		if (_isForModifying) {

			// Gets the ACIDE - A Configuration IDE current grammar
			// configuration
			String currentGrammarConfiguration = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getCurrentGrammarConfiguration().getPath();

			// Gets the name
			int lastIndexOfSlash = currentGrammarConfiguration
					.lastIndexOf("\\");
			if (lastIndexOfSlash == -1)
				lastIndexOfSlash = currentGrammarConfiguration.lastIndexOf("/");
			String name = currentGrammarConfiguration.substring(
					lastIndexOfSlash + 1,
					currentGrammarConfiguration.length() - 4);

			// Sets the title for an existent grammar
			setTitle(AcideLanguageManager.getInstance().getLabels()
					.getString("s230")
					+ " - " + name);

			String jarPath = null;

			try {

				// Gets the ACIDE - A Configurable IDE jar path
				jarPath = AcideResourceManager.getInstance().getProperty(
						"jarPath");
			} catch (Exception exception) {

				// Displays an error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s938"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}

			// Reallocates the grammar.g file
			AcideByteFileManager.getInstance().reallocateFile(
					"src/acide/process/parser/grammar/grammar.g", "grammar.g");

			// Reallocates the syntaxRules.txt file
			AcideByteFileManager.getInstance().reallocateFile(
					"src/acide/process/parser/grammar/syntaxRules.txt",
					"syntaxRules.txt");

			// Reallocates the lexicalCategories.txt file
			AcideByteFileManager.getInstance().reallocateFile(
					"src/acide/process/parser/grammar/lexicalCategories.txt",
					"lexicalCategories.txt");

			// Gets the syntaxRules.txt and lexical categories from the .jar
			// file
			Process process = null;
			try {
				process = Runtime.getRuntime().exec(
						"\"" + jarPath + "\" xvf " + name + ".jar "
								+ "syntaxRules.txt " + "lexicalCategories.txt");
				process.waitFor();
			} catch (Exception exception) {

				// Displays an error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s938"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}

			// Gets its content
			String fileContent = AcideFileManager.getInstance().load(
					"lexicalCategories.txt");

			if (fileContent != null)
				// Updates the categories text area
				_categoriesTextArea.setText(fileContent);

			// Gets its content
			fileContent = AcideFileManager.getInstance()
					.load("syntaxRules.txt");

			if (fileContent != null)
				// Updates the rules text area
				_rulesTextArea.setText(fileContent);

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s174"));

		} else
			// Sets the title for a new grammar
			setTitle(AcideLanguageManager.getInstance().getLabels()
					.getString("s184"));

		// Sets the window icon image
		setIconImage(new ImageIcon(ICON).getImage());

		// The window is not resizable
		setResizable(false);

		// Packs the window components
		pack();

		// Centers the window
		setLocationRelativeTo(null);

		// Sets the window visible
		setVisible(true);

		// Disables the main window
		AcideMainWindow.getInstance().setEnabled(false);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s174"));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE grammar configuration window
	 * components.
	 */
	private void buildComponents() {

		// Creates the categories panel
		_categoriesPanel = new JPanel(new GridBagLayout());

		// Sets the categories panel border
		_categoriesPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s175"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// Creates the rules panel
		_rulesPanel = new JPanel(new GridBagLayout());

		// Sets the rules panel border
		_rulesPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s176"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// Creates the button panel
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the categories button panel
		_categoriesButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the rules button panel
		_rulesButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// Creates the categories text area
		_categoriesTextArea = new JTextArea();

		// Sets the categories text area tool tip text
		_categoriesTextArea.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s179"));

		// Creates the categories scroll pane which contains the categories text
		// area
		_categoriesScrollPane = new JScrollPane(_categoriesTextArea);

		// Creates the rules text area
		_rulesTextArea = new JTextArea();

		// Sets the rules text area tool tip text
		_rulesTextArea.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s180"));

		// Creates the rules scroll pane which contains the rules text area
		_rulesScrollPane = new JScrollPane(_rulesTextArea);

		// Creates the accept button
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s177"));

		// Sets the accept button tool tip text
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s181"));

		// Creates the cancel button
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s178"));

		// Sets the cancel button tool tip text
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s182"));

		// Creates the load categories button
		_loadCategoriesButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s192"));

		// Sets the load categories tool tip text
		_loadCategoriesButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s193"));

		// Creates the save categories button
		_saveCategoriesButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s194"));

		// Sets the save categories button tool tip text
		_saveCategoriesButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s195"));

		// Creates the load rules button
		_loadRulesButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s196"));

		// Sets the load rules button tool tip text
		_loadRulesButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s197"));

		// Creates the save rules button
		_saveRulesButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s198"));

		// Sets the save rules button tool tip text
		_saveRulesButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s199"));

		// Creates the verbose process check box
		_verboseProcessCheckBox = new JCheckBox(AcideLanguageManager
				.getInstance().getLabels().getString("s1064"));
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE grammar
	 * configuration window with the layout.
	 */
	private void addComponents() {

		// Sets the layout
		setLayout(new GridBagLayout());

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 150;
		constraints.ipady = 150;
		constraints.insets = new Insets(5, 5, 5, 5);

		// Adds the categories panel to the categories scroll pane
		_categoriesPanel.add(_categoriesScrollPane, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridy = 1;
		constraints.anchor = GridBagConstraints.EAST;

		// Adds the load categories button to the categories button panel
		_categoriesButtonPanel.add(_loadCategoriesButton);

		// Adds the save categories button to the categories button panel
		_categoriesButtonPanel.add(_saveCategoriesButton);

		// Adds the categories button panel to the categories panel
		_categoriesPanel.add(_categoriesButtonPanel, constraints);

		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridy = 0;
		constraints.ipadx = 150;
		constraints.ipady = 205;

		// Adds the rules scroll panel to the rules panel
		_rulesPanel.add(_rulesScrollPane, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridy = 1;
		constraints.anchor = GridBagConstraints.EAST;

		// Adds the load rules button to the rules button panel
		_rulesButtonPanel.add(_loadRulesButton);

		// Adds the save rules button to the rules button panel
		_rulesButtonPanel.add(_saveRulesButton);

		// Adds the rules button panel to the rules panel
		_rulesPanel.add(_rulesButtonPanel, constraints);

		// Adds the accept button to the button panel
		_buttonPanel.add(_acceptButton);

		// Adds the cancel button to the button panel
		_buttonPanel.add(_cancelButton);

		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 0;

		// Adds the categories panel to the window
		add(_categoriesPanel, constraints);
		constraints.gridx = 1;

		// Adds the rules panel to the window
		add(_rulesPanel, constraints);
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 1;

		// Adds the verbose process check box to the window
		add(_verboseProcessCheckBox, constraints);
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridy = 2;

		// Adds the button panel to the window
		add(_buttonPanel, constraints);
	}

	/**
	 * Sets the listeners of the ACIDE - A Configurable IDE grammar
	 * configuration window components.
	 */
	public void setListeners() {

		// Sets the accept button action listener
		_acceptButton.addActionListener(new AcceptButtonAction());

		// Sets the cancel button action listener
		_cancelButton.addActionListener(new CancelButtonAction());

		// When the escape key is pressed the executes the escape key action
		_cancelButton.registerKeyboardAction(new EscapeKeyAction(),
				"EscapeKey", KeyStroke.getKeyStroke(
						java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// Sets the load rules button action listener
		_loadRulesButton.addActionListener(new LoadRulesButtonAction());

		// Sets the save rules button action listener
		_saveRulesButton.addActionListener(new SaveRulesButtonAction());

		// Sets the load categories button action listener
		_loadCategoriesButton
				.addActionListener(new LoadCategoriesButtonAction());

		// Sets the save categories button action listener
		_saveCategoriesButton
				.addActionListener(new SaveCategoriesButtonAction());

		// Sets the window closing listener
		addWindowListener(new AcideWindowClosingListener());
	}

	/**
	 * ACIDE - A Configurable IDE grammar configuration window accept button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class AcceptButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Creates the file content
			String textContent = "header{\npackage acide.process.parser.grammar;\n}\n";
			textContent += "class GrammarLexer extends Lexer;\n";
			textContent += _categoriesTextArea.getText();
			textContent += "\nclass GrammarParser extends Parser;\n";
			textContent += "options{k=2;}\n";
			textContent += _rulesTextArea.getText();

			// Saves the grammar.g file
			boolean isSaved = AcideFileManager.getInstance().write("grammar.g",
					textContent);

			// Saves the lexical categories file
			isSaved = isSaved
					&& AcideFileManager.getInstance().write(
							"lexicalCategories.txt",
							_categoriesTextArea.getText());

			// Saves the syntax rules file
			isSaved = isSaved
					&& AcideFileManager.getInstance().write("syntaxRules.txt",
							_rulesTextArea.getText());

			if (isSaved)

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s185"));
			else
				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s186"));

			// Set the main window enabled again
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();

			// Brings the main window to the front
			AcideMainWindow.getInstance().setAlwaysOnTop(true);

			// But not permanently
			AcideMainWindow.getInstance().setAlwaysOnTop(false);

			// Selects the new grammar name
			String newGrammarName = "";
			if (_isForModifying)
				newGrammarName = "lastModified.jar";
			else
				newGrammarName = "newGrammar.jar";

			// Selects the new grammar path
			String newGrammarPath = AcideGrammarConfiguration.DEFAULT_PATH
					+ newGrammarName;

			try {

				// Creates the process for the grammar file creation
				AcideGrammarFileCreationProcess process = new AcideGrammarFileCreationProcess(
						newGrammarName, _verboseProcessCheckBox.isSelected());

				// Starts the process
				process.start();

				// If the previous grammar configuration does not contain newGrammar or lastModified
				if (!AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getPreviousGrammarConfiguration().getPath()
						.contains("newGrammar")
						|| !AcideMainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getPreviousGrammarConfiguration().getPath()
								.contains("lastModified"))
					
					// Sets the previous grammar path
					AcideMainWindow
							.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getPreviousGrammarConfiguration()
							.setPath(
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.getCurrentGrammarConfiguration()
											.getPath());

				// Sets the current grammar path
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getCurrentGrammarConfiguration()
						.setPath(newGrammarPath);

				// Enables the save grammar menu item
				AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
						.getGrammarMenu().getSaveGrammarMenuItem()
						.setEnabled(true);

				// Updates the grammar message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setGrammarMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s248")
										+ " "
										+ AcideMainWindow
												.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanel()
												.getCurrentGrammarConfiguration()
												.getName());

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s935"));
			} catch (Exception exception) {

				// Displays an error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s930"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE grammar configuration window cancel button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class CancelButtonAction implements ActionListener {
		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Set the main window enabled again
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();

			// Brings the main window to the front
			AcideMainWindow.getInstance().setAlwaysOnTop(true);

			// But not permanently
			AcideMainWindow.getInstance().setAlwaysOnTop(false);

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s183"));
		}
	}

	/**
	 * ACIDE - A Configurable IDE grammar configuration window load rules button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class LoadRulesButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Removes the filter
			AcideFileManager
					.getInstance()
					.getFileChooser()
					.removeChoosableFileFilter(
							AcideFileManager.getInstance().getFileChooser()
									.getFileFilter());
			
			// Asks the the file to the user
			String absolutePath = AcideFileManager.getInstance()
					.askForOpenFile(true);

			if (absolutePath != null) {

				// Loads the file content
				String fileContent = AcideFileManager.getInstance().load(
						absolutePath);

				// Updates the rules text area with the file content
				_rulesTextArea.setText(fileContent);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s200"));
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE grammar configuration window save rules button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SaveRulesButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Removes the filter
			AcideFileManager
					.getInstance()
					.getFileChooser()
					.removeChoosableFileFilter(
							AcideFileManager.getInstance().getFileChooser()
									.getFileFilter());
			
			// Asks to the user for the file path
			String absolutePath = AcideFileManager.getInstance()
					.askForSaving(false);

			if (absolutePath != null) {

				// Tries to save the file content
				boolean isSaved = AcideFileManager.getInstance().write(
						absolutePath, _rulesTextArea.getText());

				// If it was saved successfully
				if (isSaved)
					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s202")
									+ absolutePath);
				else
					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s203"));
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE grammar configuration window load categories
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class LoadCategoriesButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Removes the filter
			AcideFileManager
					.getInstance()
					.getFileChooser()
					.removeChoosableFileFilter(
							AcideFileManager.getInstance().getFileChooser()
									.getFileFilter());
			
			// Asks the absolute path to the user
			String absolutePath = AcideFileManager.getInstance()
					.askForOpenFile(true);

			if (absolutePath != null) {

				// Loads the file content
				String fileContent = AcideFileManager.getInstance().load(
						absolutePath);

				// Updates the categories text area
				_categoriesTextArea.setText(fileContent);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s201"));
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE grammar configuration window save categories
	 * button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SaveCategoriesButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Removes the filter
			AcideFileManager
					.getInstance()
					.getFileChooser()
					.removeChoosableFileFilter(
							AcideFileManager.getInstance().getFileChooser()
									.getFileFilter());
			
			// Asks the path to the user
			String absolutePath = AcideFileManager.getInstance()
					.askForSaving(false);

			if (absolutePath != null) {

				// Tries to save the file content
				boolean isSaved = AcideFileManager.getInstance().write(
						absolutePath, _categoriesTextArea.getText());

				// If it was saved
				if (isSaved)
					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s204")
									+ absolutePath);
				else
					// Updates the log
					AcideLog.getLog().info(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s205"));
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE grammar configuration window escape key action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class EscapeKeyAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Set the main window enabled again
			AcideMainWindow.getInstance().setEnabled(true);

			// Closes the window
			dispose();

			// Brings the main window to the front
			AcideMainWindow.getInstance().setAlwaysOnTop(true);

			// But not permanently
			AcideMainWindow.getInstance().setAlwaysOnTop(false);
		}
	}
}
