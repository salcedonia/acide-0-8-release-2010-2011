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
package gui.menuBar.configurationMenu.grammarMenu.gui;

import gui.consolePanel.AcideConsolePanel;
import gui.mainWindow.MainWindow;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

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

import language.AcideLanguageManager;

import es.bytes.ByteFile;
import es.configuration.project.AcideProjectConfiguration;
import es.text.AcideFileManager;
import es.text.AcideTextFileExtensionFilterManager;

import operations.console.ConsoleThread;
import operations.log.AcideLog;
import operations.parser.AcideGrammarGenerator;
import resources.AcideResourceManager;

/**																
 * ACIDE - A Configurable IDE grammar configuration window.										
 *					
 * @version 0.8	
 * @see JFrame																													
 */
public class AcideGrammarConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE grammar configuration window class serial version UID.
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
	 * ACIDE - A Configurable IDE grammar configuration window categories button panel.
	 */
	private JPanel _categoriesButtonPanel;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window rules button panel.
	 */
	private JPanel _rulesButtonPanel;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window categories text area.
	 */
	private final JTextArea _categoriesTextArea;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window rules text area.
	 */
	private final JTextArea _rulesTextArea;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window categories scroll pane.
	 */
	private JScrollPane _categoriesScrollPane;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window rules scroll pane.
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
	 * ACIDE - A Configurable IDE grammar configuration window load categories button.
	 */
	private JButton _loadCategoriesButton;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window save categories button.
	 */
	private JButton _saveCategoriesButton;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window load rules button.
	 */
	private JButton _loadRulesButton;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window save rules button.
	 */
	private JButton _saveRulesButton;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window grammar name string.
	 */
	private String _grammarName;
	/**
	 * ACIDE - A Configurable IDE grammar configuration window flag that indicates if the changes are saved or not.
	 */
	private static boolean _changesAreSaved;

	/**
	 * Creates a new ACIDE - A Configurable IDE grammar configuration window.
	 * 
	 * @param modify indicates if the window is used for modify or
	 * for create a grammar.
	 */
	public AcideGrammarConfigurationWindow(boolean modify) {

		_changesAreSaved = true;
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		// Updates the log
		AcideLog.getLog().info(labels.getString("s173"));
		
		// FRAME
		setLayout(new GridBagLayout());
		setIconImage(new ImageIcon(ICON).getImage());
		
		if(modify){
			
			String currentGrammar = null;
			
			try {
				currentGrammar = AcideResourceManager.getInstance().getProperty("currentGrammar");
			} catch (Exception exception) {
				
				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						labels.getString("s936"), JOptionPane.ERROR_MESSAGE);
				
				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}
			
			// Gets the name
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");		
			_grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);
			
			// SET THE TITLE
			setTitle(labels.getString("s230") + " - " + _grammarName);
		}
		else
			setTitle(labels.getString("s184"));
		
		// CATEGORIES PANEL
		_categoriesPanel = new JPanel();
		_categoriesPanel.setLayout(new GridBagLayout());
		_categoriesPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s175"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		
		// RULES PANEL
		_rulesPanel = new JPanel();
		_rulesPanel.setLayout(new GridBagLayout());
		_rulesPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s176"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION, new Font("Tahoma", 1, 12),
				new Color(0, 0, 0)));
		
		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());
		
		// CATEGORIES BUTTON PANEL
		_categoriesButtonPanel = new JPanel();
		_categoriesButtonPanel.setLayout(new GridBagLayout());
		
		// RULES BUTTON PANEL
		_rulesButtonPanel = new JPanel();
		_rulesButtonPanel.setLayout(new GridBagLayout());
		
		// CATEGORIES TEXT AREA
		_categoriesTextArea = new JTextArea();
		_categoriesTextArea.setToolTipText(labels.getString("s179"));
		_categoriesScrollPane = new JScrollPane(_categoriesTextArea);
		
		// RULES TEXT AREA
		_rulesTextArea = new JTextArea();
		_rulesTextArea.setToolTipText(labels.getString("s180"));
		_rulesScrollPane = new JScrollPane(_rulesTextArea);
		
		// ACCEPT BUTTON
		_acceptButton = new JButton(labels.getString("s177"));
		_acceptButton.setToolTipText(labels.getString("s181"));
		
		// CANCEL BUTTON
		_cancelButton = new JButton(labels.getString("s178"));
		_cancelButton.setToolTipText(labels.getString("s182"));
		
		// LOAD CATEGORIES BUTTON
		_loadCategoriesButton = new JButton(labels.getString("s192"));
		_loadCategoriesButton.setToolTipText(labels.getString("s193"));
		
		// SAVE CATEGORIES
		_saveCategoriesButton = new JButton(labels.getString("s194"));
		_saveCategoriesButton.setToolTipText(labels.getString("s195"));
		
		// LOAD RULES BUTTON
		_loadRulesButton = new JButton(labels.getString("s196"));
		_loadRulesButton.setToolTipText(labels.getString("s197"));
		
		// SAVE RULES BUTTON
		_saveRulesButton = new JButton(labels.getString("s198"));
		_saveRulesButton.setToolTipText(labels.getString("s199"));
		
		// Listeners
		_acceptButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				// ADD THE RULES
				String textContent = "header{\npackage operations.parser.grammar;\n}\n";
				textContent += "class GrammarLexer extends Lexer;\n";
				textContent += _categoriesTextArea.getText();
				textContent += "\nclass GrammarParser extends Parser;\n";
				textContent += "options{k=2;}\n";
				textContent += _rulesTextArea.getText();
				AcideFileManager textFile = new AcideFileManager();
				
				boolean isSaved = textFile.write("grammar.g", textContent);
				
				isSaved = isSaved
						&& textFile.write("lexicalCats.txt",
								_categoriesTextArea.getText());
				isSaved = isSaved
						&& textFile.write("syntaxRules.txt", _rulesTextArea.getText());
				
				if (isSaved)
					
					// Updates the log
					AcideLog.getLog().info(labels.getString("s185"));
				else
					// Updates the log
					AcideLog.getLog().info(labels.getString("s186"));
				
				dispose();
				
				boolean isGenerated = false;
				String newGrammarName = "newGrammar";
				String newGrammarPath = "./configuration/grammars/newGrammar.jar";
				
				try {
					
					isGenerated = AcideGrammarGenerator.generate(newGrammarName);
					String previousGrammar = AcideResourceManager
							.getInstance().getProperty("currentGrammar");
					
					if (_changesAreSaved)
						// Updates the RESOURCE MANAGER
						AcideResourceManager.getInstance().setProperty("previousGrammar",
								previousGrammar);
					
					// Updates the RESOURCE MANAGER
					AcideResourceManager.getInstance().setProperty("currentGrammar",
							newGrammarPath);
					MainWindow.getInstance().getMenu().getConfiguration().getGrammar()
							.getSaveGrammar().setEnabled(false);
					MainWindow.getInstance().validate();
					MainWindow.getInstance().repaint();
					_changesAreSaved = false;
					MainWindow.getInstance().getStatusBar().setGrammarMessage(
							labels.getString("s248")
									+ " newGrammar (Not saved)");
					dispose();
					
					// Updates the log
					AcideLog.getLog().info(labels.getString("s935"));
				} catch (Exception exception) {
					
					// Error message
					JOptionPane.showMessageDialog(null, exception.getMessage(),
							labels.getString("s930"), JOptionPane.ERROR_MESSAGE);
					
					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
				}

				if (isGenerated)
					// Updates the log
					AcideLog.getLog().info(labels.getString("s208"));
				else
					// Updates the log
					AcideLog.getLog().error(labels.getString("s209"));
				
				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {
					
					// The project has been modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(true);
				}
			}
		});
		
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				dispose();
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s183"));
			}
		});
		
		_loadRulesButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				AcideFileManager file = new AcideFileManager();
				String path = file.askAbsolutePath();
				String text = null;
				text = file.load(path);
				_rulesTextArea.setText(text);
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s200"));
			}
		});
		
		_saveRulesButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				String text = _rulesTextArea.getText();
				AcideFileManager textFile = new AcideFileManager();
				String path = textFile.askSavingFileEditorFile();
				boolean saved = textFile.write(path, text);
				
				if (saved)
					// Updates the log
					AcideLog.getLog().info(labels.getString("s202") + path);
				else
					// Updates the log
					AcideLog.getLog().info(labels.getString("s203"));
			}
		});
		
		_loadCategoriesButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				AcideFileManager file = new AcideFileManager();
				String path = file.askAbsolutePath();
				
				String textContent = null;
				textContent = file.load(path);
				_categoriesTextArea.setText(textContent);
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s201"));
			}
		});
		
		_saveCategoriesButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				String text = _categoriesTextArea.getText();
				AcideFileManager textFile = new AcideFileManager();
				String path = textFile.askSavingFileEditorFile();
				boolean saved = textFile.write(path, text);
				
				if (saved)
					// Updates the log
					AcideLog.getLog().info(labels.getString("s204") + path);
				else
					// Updates the log
					AcideLog.getLog().info(labels.getString("s205"));
			}
		});
		
		ActionListener escPressed = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				dispose();
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s183"));
			}
		};
		
		_cancelButton.registerKeyboardAction(escPressed, "EscapeKey",
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0,
						true), JComponent.WHEN_IN_FOCUSED_WINDOW);

		// ADD THE COMPONENTS TO THE WINDOW WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.NONE;
		
		// CATEGORIES PANEL
		constraints.gridwidth = 1;
		constraints.gridheight = 1;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 300;
		constraints.ipady = 300;
		constraints.insets = new Insets(5, 5, 5, 5);
		_categoriesPanel.add(_categoriesScrollPane, constraints);
		
		// CATEGORIES BUTTONS PANEL
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridwidth = 1;
		_categoriesButtonPanel.add(_loadCategoriesButton, constraints);
		constraints.gridx = 1;
		_categoriesButtonPanel.add(_saveCategoriesButton, constraints);
		constraints.gridy = 1;
		constraints.gridx = 0;
		_categoriesPanel.add(_categoriesButtonPanel, constraints);
		
		// RULES PANEL
		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 400;
		constraints.ipady = 300;
		_rulesPanel.add(_rulesScrollPane, constraints);
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 1;
		_rulesButtonPanel.add(_loadRulesButton, constraints);
		constraints.gridx = 1;
		_rulesButtonPanel.add(_saveRulesButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_rulesPanel.add(_rulesButtonPanel, constraints);
		
		// BUTTON PANEL
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridwidth = 1;
		constraints.gridheight = 1;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		constraints.ipady = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_buttonPanel.add(_acceptButton, constraints);
		constraints.gridx = 1;
		_buttonPanel.add(_acceptButton, constraints);
		
		// FRAME
		constraints.gridx = 0;
		constraints.ipadx = 0;
		constraints.gridwidth = 1;
		constraints.gridy = 1;
		add(_categoriesPanel, constraints);
		constraints.gridx = 1;
		add(_rulesPanel, constraints);
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 2;
		add(_buttonPanel, constraints);
		setResizable(false);
		pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = getSize();
		setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);
		setVisible(true);
		
		// Updates the log
		AcideLog.getLog().info(labels.getString("s174"));
		
		if (modify){
			
			String jarPath = null;
			String currentPath = null;
			try {
				jarPath = AcideResourceManager.getInstance().getProperty("jarPath");
				
				// GET THE PATH
				currentPath = AcideResourceManager.getInstance().getProperty("currentGrammar");
				int index = currentPath.lastIndexOf("\\");
				if (index == -1)
					index = currentPath.lastIndexOf("/");			
				currentPath = currentPath.substring(0, index);
				
				AcideConsolePanel s = new AcideConsolePanel(false);
				ConsoleThread p = new ConsoleThread();
				p.executeCommand("cmd", currentPath, "\"" + jarPath + "\" xvf "
						+ _grammarName + ".jar syntaxRules.txt lexicalCats.txt",
						"exit", s);
				
				// Runtime.getRuntime().exec("\"" + jarPath + "\" xvf " +
				// grammarName + ".jar syntaxRules.txt lexicalCats.txt");
				
				Thread.sleep(200);
				
				// Gets its content
				String fileContent = AcideFileManager.getInstance().load(currentPath + "lexicalCats.txt");
				_categoriesTextArea.setText(fileContent);
				
				// Gets its content
				fileContent = AcideFileManager.getInstance().load(currentPath + "syntaxRules.txt");
				_rulesTextArea.setText(fileContent);
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s174"));
			} catch (Exception exception) {
				
				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						labels.getString("s938"), JOptionPane.ERROR_MESSAGE);
				
				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
			}
		}
	}

	/**
	 * Shows the load grammar GUI.
	 */
	public static void loadGrammarGUI() {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		JFileChooser chooser = new JFileChooser();
		AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(labels.getString("s270"));
		filter.addExtension("jar");
		chooser.setFileFilter(filter);
		chooser.setCurrentDirectory(new File("./configuration/grammars/"));
		
		int optionChosen = chooser.showOpenDialog(null);
		
		// IF OK
		if (optionChosen == JFileChooser.APPROVE_OPTION) {
			
			String grammarFile = chooser.getSelectedFile().getAbsolutePath();
			
			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("currentGrammar", grammarFile);
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s243") + " " + grammarFile);
			
			// GET THE GRAMMAR NAME
			int index = grammarFile.lastIndexOf("\\");		
			if (index == -1)
				index = grammarFile.lastIndexOf("/");		
			String grammarName = grammarFile.substring(index + 1,
					grammarFile.length() - 4);
			
			// Updates the status bar
			MainWindow mainWindow = MainWindow.getInstance();
			mainWindow.getStatusBar().setGrammarMessage(
					labels.getString("s248") + " " + grammarName);
			
			// Updates the project configuration
			AcideProjectConfiguration.getInstance().setGrammarConfiguration(grammarFile);
			mainWindow.validate();
			mainWindow.repaint();
			_changesAreSaved = true;
			
			// Updates the menu
			mainWindow.getMenu().getConfiguration().getGrammar().getSaveGrammar()
					.setEnabled(false);
			
			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {
				
				// The project has been modified
				AcideProjectConfiguration.getInstance()
						.setIsModified(true);
			}

		} else if (optionChosen == JFileChooser.CANCEL_OPTION) {
			// Updates the log
			AcideLog.getLog().info(labels.getString("s242"));
		}
	}

	/**
	 * Shows the save grammar window.
	 */
	public static void saveGrammarWindow() {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		try {
			String previous = AcideResourceManager.getInstance().getProperty("previousGrammar");
			String current = AcideResourceManager.getInstance().getProperty("currentGrammar");
			
			// GET THE NAME
			int index = previous.lastIndexOf("\\");
			if (index == -1)
				index = previous.lastIndexOf("/");
			String newName = previous.substring(index + 1,
					previous.length() - 4);
			
			ByteFile.copy(current, previous);
			
			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("currentGrammar", previous);
			
			MainWindow.getInstance().getMenu().getConfiguration().getGrammar().getSaveGrammar().setEnabled(false);
			MainWindow.getInstance().getStatusBar().setGrammarMessage(
					labels.getString("s248") + " " + newName);
			AcideProjectConfiguration.getInstance().setGrammarConfiguration(previous);
			_changesAreSaved = true;
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s940") + ": " + previous);
			
		} catch (Exception exception) {
			
			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s939"), JOptionPane.ERROR_MESSAGE);
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}

	/**
	 * Shows the save as grammar window.
	 */
	public static void saveAsGrammarWindow() {
		
		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		
		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		try {
			
			String current = AcideResourceManager.getInstance().getProperty("currentGrammar");
			JFileChooser selector = new JFileChooser();
			AcideTextFileExtensionFilterManager filter = new AcideTextFileExtensionFilterManager(labels.getString("s270"));
			filter.addExtension("jar");
			selector.setFileFilter(filter);
			selector.setCurrentDirectory(new File("./configuration/grammars/"));
			
			String fileName = "";
			int optionChosen = selector.showSaveDialog(selector);
			
			// IF OK
			if (optionChosen == JFileChooser.APPROVE_OPTION) {
				
				fileName = selector.getSelectedFile().getAbsolutePath();
				
				if (!fileName.endsWith(".jar"))
					fileName += ".jar";
				
				ByteFile.copy(current, fileName);
				
				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty("currentGrammar", fileName);
				
				MainWindow.getInstance().getMenu().getConfiguration().getGrammar().getSaveGrammar().setEnabled(false);
				
				// Gets the name
				int index = fileName.lastIndexOf("\\");	
				if (index == -1)
					index = fileName.lastIndexOf("/");			
				String grammarName = fileName.substring(index + 1,
						fileName.length() - 4);
				
				// Updates the status bar
				MainWindow.getInstance().getStatusBar().setGrammarMessage(
						labels.getString("s248") + " " + grammarName);
				AcideProjectConfiguration.getInstance().setGrammarConfiguration(
						fileName);
				_changesAreSaved = true;
				
				// Updates the log
				AcideLog.getLog().info(labels.getString("s941") + ": " + fileName);
			} else if (optionChosen == JFileChooser.CANCEL_OPTION) {
				selector.cancelSelection();
				// Updates the log
				AcideLog.getLog().info(labels.getString("s942"));
			}
		} catch (Exception exception) {
			
			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s943"), JOptionPane.ERROR_MESSAGE);
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}
