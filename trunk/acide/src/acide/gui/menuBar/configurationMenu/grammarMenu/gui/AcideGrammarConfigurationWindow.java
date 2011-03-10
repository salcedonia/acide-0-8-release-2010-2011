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

import acide.gui.consolePanel.AcideConsolePanel;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.border.TitledBorder;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

import acide.files.AcideFileManager;
import acide.files.bytes.AcideByteFile;
import acide.configuration.project.AcideProjectConfiguration;
import acide.files.text.AcideTextFileExtensionFilterManager;

import acide.process.console.AcideConsoleProcess;
import acide.process.parser.AcideGrammarGenerator;
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
	private final JTextArea _categoriesTextArea;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window rules text area.
	 */
	private final JTextArea _rulesTextArea;
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
	 * ACIDE - A Configurable IDE grammar configuration window grammar name
	 * string.
	 */
	private String _grammarName;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window flag that
	 * indicates if the changes are saved or not.
	 */
	private static boolean _changesAreSaved;

	/**
	 * Creates a new ACIDE - A Configurable IDE grammar configuration window.
	 * 
	 * @param isForModifying
	 *            indicates if the window is used for modifying or creating a
	 *            grammar.
	 */
	public AcideGrammarConfigurationWindow(boolean isForModifying) {

		// The changes are saved
		_changesAreSaved = true;

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s173"));

		// Sets the layout
		setLayout(new GridBagLayout());

		// CATEGORIES PANEL
		_categoriesPanel = new JPanel(new GridBagLayout());
		_categoriesPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s175"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// RULES PANEL
		_rulesPanel = new JPanel(new GridBagLayout());
		_rulesPanel.setBorder(BorderFactory.createTitledBorder(null,
				AcideLanguageManager.getInstance().getLabels()
						.getString("s176"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// CATEGORIES BUTTON PANEL
		_categoriesButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// RULES BUTTON PANEL
		_rulesButtonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// CATEGORIES TEXT AREA
		_categoriesTextArea = new JTextArea();
		_categoriesTextArea.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s179"));
		_categoriesScrollPane = new JScrollPane(_categoriesTextArea);

		// RULES TEXT AREA
		_rulesTextArea = new JTextArea();
		_rulesTextArea.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s180"));
		_rulesScrollPane = new JScrollPane(_rulesTextArea);

		// ACCEPT BUTTON
		_acceptButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s177"));
		_acceptButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s181"));

		// CANCEL BUTTON
		_cancelButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s178"));
		_cancelButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s182"));

		// LOAD CATEGORIES BUTTON
		_loadCategoriesButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s192"));
		_loadCategoriesButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s193"));

		// SAVE CATEGORIES
		_saveCategoriesButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s194"));
		_saveCategoriesButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s195"));

		// LOAD RULES BUTTON
		_loadRulesButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s196"));
		_loadRulesButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s197"));

		// SAVE RULES BUTTON
		_saveRulesButton = new JButton(AcideLanguageManager.getInstance()
				.getLabels().getString("s198"));
		_saveRulesButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s199"));

		// Sets the listener of the window components
		setListeners();

		// Adds the components to the window with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		// CATEGORIES PANEL
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 150;
		constraints.ipady = 150;
		constraints.insets = new Insets(5, 5, 5, 5);
		_categoriesPanel.add(_categoriesScrollPane, constraints);
		constraints.fill = GridBagConstraints.NONE;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridy = 1;
		constraints.anchor = GridBagConstraints.EAST;
		_categoriesButtonPanel.add(_loadCategoriesButton);
		_categoriesButtonPanel.add(_saveCategoriesButton);
		_categoriesPanel.add(_categoriesButtonPanel, constraints);

		// RULES PANEL
		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridy = 0;
		constraints.ipadx = 150;
		constraints.ipady = 205;
		_rulesPanel.add(_rulesScrollPane, constraints);
		constraints.fill = GridBagConstraints.NONE;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridy = 1;
		constraints.anchor = GridBagConstraints.EAST;
		_rulesButtonPanel.add(_loadRulesButton);
		_rulesButtonPanel.add(_saveRulesButton);
		_rulesPanel.add(_rulesButtonPanel, constraints);

		// BUTTON PANEL
		_buttonPanel.add(_acceptButton);
		_buttonPanel.add(_cancelButton);

		constraints.fill = GridBagConstraints.BOTH;
		constraints.anchor = GridBagConstraints.CENTER;
		constraints.gridx = 0;
		constraints.gridy = 0;
		add(_categoriesPanel, constraints);
		constraints.gridx = 1;
		add(_rulesPanel, constraints);
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 2;
		add(_buttonPanel, constraints);

		if (isForModifying) {

			// Sets the window title
			setTitle();

			String jarPath = null;
			String currentGrammarConfiguration = null;

			try {

				// Gets the ACIDE - A Configurable IDE jar path
				jarPath = AcideResourceManager.getInstance().getProperty(
						"jarPath");

				// Gets the ACIDE - A Configuration IDE current grammar
				currentGrammarConfiguration = AcideResourceManager
						.getInstance().getProperty(
								"currentGrammarConfiguration");

				// Gets the grammar path
				int lastIndexOfSlash = currentGrammarConfiguration
						.lastIndexOf("\\");
				if (lastIndexOfSlash == -1)
					lastIndexOfSlash = currentGrammarConfiguration
							.lastIndexOf("/");
				currentGrammarConfiguration = currentGrammarConfiguration
						.substring(0, lastIndexOfSlash + 1);

				AcideConsolePanel s = new AcideConsolePanel(false);
				AcideConsoleProcess p = new AcideConsoleProcess();
				p.executeCommand(
						"cmd",
						currentGrammarConfiguration,
						"\""
								+ jarPath
								+ "\" xvf "
								+ _grammarName
								+ ".jar syntaxRules.txt lexicalCategories.txt",
						"exit", s);

				// Runtime.getRuntime().exec("\"" + jarPath + "\" xvf " +
				// grammarName + ".jar syntaxRules.txt lexicalCats.txt");

				Thread.sleep(200);

				// Gets its content
				String fileContent = AcideFileManager
						.getInstance()
						.load("lexicalCategories.txt");

				if (fileContent != null)
					// Updates the categories text area
					_categoriesTextArea.setText(fileContent);

				// Gets its content
				fileContent = AcideFileManager.getInstance().load(
						"syntaxRules.txt");

				if (fileContent != null)
					// Updates the rules text area
					_rulesTextArea.setText(fileContent);

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s174"));
			} catch (Exception exception) {

				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s938"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}
		} else
			// Sets the title for a new grammar
			setTitle(AcideLanguageManager.getInstance().getLabels()
					.getString("s184"));

		// FRAME
		setIconImage(new ImageIcon(ICON).getImage());
		setResizable(false);
		pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = getSize();
		setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);

		// Sets the window visible
		setVisible(true);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s174"));
	}

	/**
	 * Sets the window title when it is used for modifying an existing grammar.
	 */
	public void setTitle() {

		String currentGrammarConfiguration = null;

		try {

			// Gets the ACIDE - A Configuration IDE current grammar
			// configuration
			currentGrammarConfiguration = AcideResourceManager.getInstance()
					.getProperty("currentGrammarConfiguration");
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s936"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}

		// Gets the name
		int lastIndexOfSlash = currentGrammarConfiguration.lastIndexOf("\\");
		if (lastIndexOfSlash == -1)
			lastIndexOfSlash = currentGrammarConfiguration.lastIndexOf("/");
		_grammarName = currentGrammarConfiguration.substring(
				lastIndexOfSlash + 1, currentGrammarConfiguration.length() - 4);

		// Sets the title for an existent grammar
		setTitle(AcideLanguageManager.getInstance().getLabels()
				.getString("s230")
				+ " - " + _grammarName);
	}

	/**
	 * Sets the listeners of the window components.
	 */
	public void setListeners() {

		// ACCEPT BUTTON
		_acceptButton.addActionListener(new AcceptButtonAction());

		// CANCEL BUTTON
		_cancelButton.addActionListener(new CancelButtonAction());
		_cancelButton.registerKeyboardAction(new EscapeKeyAction(),
				"EscapeKey", KeyStroke.getKeyStroke(
						java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// LOAD RULES BUTTON
		_loadRulesButton.addActionListener(new LoadRulesButtonAction());

		// SAVE RULES BUTTON
		_saveRulesButton.addActionListener(new SaveButtonAction());

		// LOAD CATEGORIES BUTTON
		_loadCategoriesButton
				.addActionListener(new LoadCategoriesButtonAction());

		// SAVE CATEGORIES BUTTON
		_saveCategoriesButton.addActionListener(new SaveCategoriesAction());
	}

	/**
	 * Loads the grammar configuration from the file selected by the user.
	 */
	public static void loadGrammar() {

		// Creates and configures the file chooser
		JFileChooser chooser = new JFileChooser();
		AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s270"));
		filter.addExtension("jar");
		chooser.setFileFilter(filter);
		chooser.setCurrentDirectory(new File("./configuration/grammars/"));

		// Asks to the user
		int returnValue = chooser.showOpenDialog(null);

		// If OK
		if (returnValue == JFileChooser.APPROVE_OPTION) {

			// Gets the absolute path
			String absolutePath = chooser.getSelectedFile().getAbsolutePath();

			// Updates the ACIDE - A Configuration IDE current grammar
			// configuration
			AcideResourceManager.getInstance().setProperty(
					"currentGrammarConfiguration", absolutePath);

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s243")
							+ " " + absolutePath);

			// Gets the grammar name
			int lastIndexOfSlash = absolutePath.lastIndexOf("\\");
			if (lastIndexOfSlash == -1)
				lastIndexOfSlash = absolutePath.lastIndexOf("/");
			String grammarName = absolutePath.substring(lastIndexOfSlash + 1,
					absolutePath.length() - 4);

			// Updates the grammar message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setGrammarMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s248")
									+ " " + grammarName);

			// Updates the grammar configuration in the project configuration
			AcideProjectConfiguration.getInstance().setGrammarConfiguration(
					absolutePath);

			// Validates the changes in the main window
			AcideMainWindow.getInstance().validate();

			// Repaints the main window
			AcideMainWindow.getInstance().repaint();

			// The changes are saved
			_changesAreSaved = true;

			// Disables the save grammar menu item
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getGrammarMenu().getSaveGrammarMenuItem()
					.setEnabled(false);

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);
			}

		} else if (returnValue == JFileChooser.CANCEL_OPTION) {
			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s242"));
		}
	}

	/**
	 * Saves the grammar configuration.
	 */
	public static void saveGrammar() {

		try {

			// Gets the ACIDE - A Configurable IDE previous grammar
			// configuration
			String previousGrammarConfiguration = AcideResourceManager
					.getInstance().getProperty("previousGrammarConfiguration");

			// Gets the ACIDE - A Configurable IDE current grammar configuration
			String currentGrammarConfiguration = AcideResourceManager
					.getInstance().getProperty("currentGrammarConfiguration");

			// Gets the name
			int lastIndexOfSlash = previousGrammarConfiguration
					.lastIndexOf("\\");
			if (lastIndexOfSlash == -1)
				lastIndexOfSlash = previousGrammarConfiguration
						.lastIndexOf("/");
			String newName = previousGrammarConfiguration.substring(
					lastIndexOfSlash + 1,
					previousGrammarConfiguration.length() - 4);

			// Copies the files
			AcideByteFile.copy(currentGrammarConfiguration,
					previousGrammarConfiguration);

			// Updates the ACIDE - A Configurable IDE current grammar
			// configuration
			AcideResourceManager.getInstance()
					.setProperty("currentGrammarConfiguration",
							previousGrammarConfiguration);

			// Disables the save grammar menu item
			AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
					.getGrammarMenu().getSaveGrammarMenuItem()
					.setEnabled(false);

			// Updates the grammar message in the status bar
			AcideMainWindow
					.getInstance()
					.getStatusBar()
					.setGrammarMessage(
							AcideLanguageManager.getInstance().getLabels()
									.getString("s248")
									+ " " + newName);

			// Updates the grammar configuration in the project configuration
			AcideProjectConfiguration.getInstance().setGrammarConfiguration(
					previousGrammarConfiguration);

			// The changes are saved
			_changesAreSaved = true;

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s940")
							+ ": " + previousGrammarConfiguration);

		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s939"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}

	/**
	 * Saves the grammar configuration into a selected file.
	 */
	public static void saveAsGrammarWindow() {

		try {

			// Gets the ACIDE - A Configurable IDE current grammar configuration
			String currentGrammarConfiguration = AcideResourceManager
					.getInstance().getProperty("currentGrammarConfiguration");

			// Creates and configures the file chooser
			JFileChooser fileChooser = new JFileChooser();
			AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s270"));
			filter.addExtension("jar");
			fileChooser.setFileFilter(filter);
			fileChooser.setCurrentDirectory(new File(
					"./configuration/grammars/"));

			String fileAbsolutePath = "";

			// Asks to the user
			int returnValue = fileChooser.showSaveDialog(fileChooser);

			// If OK
			if (returnValue == JFileChooser.APPROVE_OPTION) {

				// Gets the absolute path
				fileAbsolutePath = fileChooser.getSelectedFile()
						.getAbsolutePath();

				// If it does not contains the .jar extension
				if (!fileAbsolutePath.endsWith(".jar"))

					// Adds it
					fileAbsolutePath += ".jar";

				// Copies the file
				AcideByteFile.copy(currentGrammarConfiguration,
						fileAbsolutePath);

				// Updates the ACIDE - A Configurable IDE current grammar
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentGrammarConfiguration", fileAbsolutePath);

				// Disables the save grammar menu item
				AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
						.getGrammarMenu().getSaveGrammarMenuItem()
						.setEnabled(false);

				// Gets the grammar name
				int lastIndexOfSlash = fileAbsolutePath.lastIndexOf("\\");
				if (lastIndexOfSlash == -1)
					lastIndexOfSlash = fileAbsolutePath.lastIndexOf("/");
				String grammarName = fileAbsolutePath.substring(
						lastIndexOfSlash + 1, fileAbsolutePath.length() - 4);

				// Updates the grammar message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setGrammarMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s248")
										+ " " + grammarName);

				// Updates the grammar configuration in the project
				// configuration
				AcideProjectConfiguration.getInstance()
						.setGrammarConfiguration(fileAbsolutePath);

				// The changes are saved
				_changesAreSaved = true;

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s941")
								+ ": " + fileAbsolutePath);
			} else if (returnValue == JFileChooser.CANCEL_OPTION) {
				fileChooser.cancelSelection();
				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s942"));
			}
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(
					null,
					exception.getMessage(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s943"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
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

			// Adds the rules
			String textContent = "header{\npackage acide.process.parser.grammar;\n}\n";
			textContent += "class GrammarLexer extends Lexer;\n";
			textContent += _categoriesTextArea.getText();
			textContent += "\nclass GrammarParser extends Parser;\n";
			textContent += "options{k=2;}\n";
			textContent += _rulesTextArea.getText();

			// Saves the grammar.g file
			boolean isSaved = AcideFileManager.getInstance()
					.write("grammar.g",
							textContent);

			// Saves the lexical categories file
			isSaved = isSaved
					&& AcideFileManager.getInstance()
							.write("lexicalCategories.txt",
									_categoriesTextArea.getText());
			
			// Saves the syntax rules file
			isSaved = isSaved
					&& AcideFileManager.getInstance()
							.write("syntaxRules.txt",
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

			// Closes the window
			dispose();
			
			String newGrammarName = "newGrammar";
			String newGrammarPath = "./configuration/grammars/newGrammar.jar";

			try {

				// Tries to generate the grammar
				AcideGrammarGenerator.getInstance().generate(newGrammarName);

				// Gets the ACIDE - A Configurable current grammar configuration
				String currentGrammarConfiguration = AcideResourceManager
						.getInstance().getProperty(
								"currentGrammarConfiguration");

				if (_changesAreSaved)

					// Updates the ACIDE - A Configurable IDE previous grammar
					// configuration
					AcideResourceManager.getInstance().setProperty(
							"previousGrammarConfiguration",
							currentGrammarConfiguration);

				// Updates the ACIDE - A Configurable IDE current grammar
				// configuration
				AcideResourceManager.getInstance().setProperty(
						"currentGrammarConfiguration", newGrammarPath);

				// Disables the save grammar menu item
				AcideMainWindow.getInstance().getMenu().getConfigurationMenu()
						.getGrammarMenu().getSaveGrammarMenuItem()
						.setEnabled(false);

				// Validates the changes in the main window
				AcideMainWindow.getInstance().validate();

				// Repaints the main window
				AcideMainWindow.getInstance().repaint();

				// The changes are saved
				_changesAreSaved = false;

				// Updates the grammar message in the status bar
				AcideMainWindow
						.getInstance()
						.getStatusBar()
						.setGrammarMessage(
								AcideLanguageManager.getInstance().getLabels()
										.getString("s248")
										+ " newGrammar (Not saved)");
				// Closes the window
				dispose();

				// Updates the log
				AcideLog.getLog().info(
						AcideLanguageManager.getInstance().getLabels()
								.getString("s935"));
			} catch (Exception exception) {

				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						AcideLanguageManager.getInstance().getLabels()
								.getString("s930"), JOptionPane.ERROR_MESSAGE);

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// The project has been modified
				AcideProjectConfiguration.getInstance().setIsModified(true);
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

			// Closes the window
			dispose();

			// Enables the main window
			AcideMainWindow.getInstance().setEnabled(true);

			// Brings it to the front
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

			// Asks the the file to the user
			String absolutePath = AcideFileManager.getInstance()
					.askAbsolutePath();

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
	 * ACIDE - A Configurable IDE grammar configuration window save button
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class SaveButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Asks to the user for the file path
			String absolutePath = AcideFileManager.getInstance()
					.askSavingFileEditorFile();

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

			// Asks the absolute path to the user
			String absolutePath = AcideFileManager.getInstance()
					.askAbsolutePath();

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
	class SaveCategoriesAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Asks the path to the user
			String absolutePath = AcideFileManager.getInstance()
					.askSavingFileEditorFile();

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

			// Closes the window
			dispose();

			// Updates the log
			AcideLog.getLog().info(
					AcideLanguageManager.getInstance().getLabels()
							.getString("s183"));
		}
	}
}
