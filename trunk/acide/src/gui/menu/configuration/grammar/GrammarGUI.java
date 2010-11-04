package gui.menu.configuration.grammar;

import gui.MainWindow;
import gui.output.Output;

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

import language.Language;

import org.apache.log4j.Logger;

import es.bytes.ByteFile;
import es.text.TextFile;
import es.text.TextFileFilter;

import operations.log.Log;
import operations.output.ProcessThread;
import operations.parser.GrammarGenerator;
import properties.PropertiesManager;

/**
 * Grammar GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class GrammarGUI extends JFrame {

	/**
	 * _serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the icon of the window.
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Frame of the window.
	 */
	private static JFrame _frame;
	/**
	 * Categories panel.
	 */
	private JPanel _categoriesPanel;
	/**
	 * Rules panel.
	 */
	private JPanel _rulesPanel;
	/**
	 * Button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Categories button panel.
	 */
	private JPanel _categoriesButtonPanel;
	/**
	 * Rules button panel.
	 */
	private JPanel _rulesButtonPanel;
	/**
	 * Categories text area.
	 */
	private final JTextArea _categoriesTextArea;
	/**
	 * Rules text area.
	 */
	private final JTextArea _rulesTextArea;
	/**
	 * Categories scroll pane.
	 */
	private JScrollPane _categoriesScrollPane;
	/**
	 * Rules scroll pane.
	 */
	private JScrollPane _rulesScrollPane;
	/**
	 * Accept button.
	 */
	private JButton _acceptButton;
	/**
	 * Cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Load categories button.
	 */
	private JButton _loadCategoriesButton;
	/**
	 * Save categories button.
	 */
	private JButton _saveCategoriesButton;
	/**
	 * Load rules button.
	 */
	private JButton _loadRulesButton;
	/**
	 * Save rules button.
	 */
	private JButton _saveRulesButton;
	/**
	 * Grammar name string.
	 */
	private String _grammarName;
	/**
	 * Flag that indicates if the changes are saved or not.
	 */
	private static boolean _changesAreSaved;
	/**
	 * Log of the class.
	 */
	private static Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 * 
	 * @param modify
	 */
	public GrammarGUI(boolean modify) {

		_changesAreSaved = true;
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		final ResourceBundle labels = language.getLabels();
		
		_logger.info(labels.getString("s173"));
		
		// FRAME
		_frame = new JFrame();
		_frame.setLayout(new GridBagLayout());
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		
		if(modify){
			
			String currentGrammar = null;
			
			try {
				currentGrammar = PropertiesManager.getProperty("currentGrammar");
			} catch (Exception e2) {
				JOptionPane.showMessageDialog(null, e2.getMessage(),
						labels.getString("s936"), JOptionPane.ERROR_MESSAGE);
				_logger.error(e2.getMessage());
			}
			
			// GET THE NAME
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");		
			_grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);
			
			// SET THE TITLE
			_frame.setTitle(labels.getString("s230") + " - " + _grammarName);
		}
		else
			_frame.setTitle(labels.getString("s184"));
		
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
		
		// LISTENERS
		_acceptButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				// ADD THE RULES
				String textContent = "header{\npackage operations.parser.grammar;\n}\n";
				textContent += "class GrammarLexer extends Lexer;\n";
				textContent += _categoriesTextArea.getText();
				textContent += "\nclass GrammarParser extends Parser;\n";
				textContent += "options{k=2;}\n";
				textContent += _rulesTextArea.getText();
				TextFile textFile = new TextFile();
				
				boolean isSaved = textFile.save("grammar.g", textContent);
				
				isSaved = isSaved
						&& textFile.save("lexicalCats.txt",
								_categoriesTextArea.getText());
				isSaved = isSaved
						&& textFile.save("syntaxRules.txt", _rulesTextArea.getText());
				
				if (isSaved)
					_logger.info(labels.getString("s185"));
				else
					_logger.info(labels.getString("s186"));
				_frame.dispose();
				
				boolean isGenerated = false;
				String newGrammarName = "newGrammar";
				String newGrammarPath = "./configuration/grammars/newGrammar.jar";
				
				try {
					
					isGenerated = GrammarGenerator.generate(newGrammarName);
					String previousGrammar = PropertiesManager
							.getProperty("currentGrammar");
					if (_changesAreSaved)
						PropertiesManager.setProperty("previousGrammar",
								previousGrammar);
					PropertiesManager.setProperty("currentGrammar",
							newGrammarPath);
					MainWindow.getInstance().getMenu().getConfiguration().getGrammar()
							.getSaveGrammar().setEnabled(false);
					MainWindow.getInstance().validate();
					MainWindow.getInstance().repaint();
					_changesAreSaved = false;
					MainWindow.getInstance().getStatusBar().setMessageGrammar(
							labels.getString("s248")
									+ " newGrammar (Not saved)");
					_frame.dispose();
					_logger.info(labels.getString("s935"));
				} catch (Exception e1) {
					
					JOptionPane.showMessageDialog(null, e1.getMessage(),
							labels.getString("s930"), JOptionPane.ERROR_MESSAGE);
					_logger.error(e1.getMessage());
				}

				if (isGenerated)
					_logger.info(labels.getString("s208"));
				else
					_logger.error(labels.getString("s209"));
				
				// NOT DEFAULT PROJECT
				if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);
				}
			}
		});
		
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_frame.dispose();
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
				_logger.info(labels.getString("s183"));
			}
		});
		
		_loadRulesButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				TextFile file = new TextFile();
				String path = file.read();
				String text = null;
				text = file.load(path);
				_rulesTextArea.setText(text);
				_logger.info(labels.getString("s200"));
			}
		});
		
		_saveRulesButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				String text = _rulesTextArea.getText();
				TextFile textFile = new TextFile();
				String path = textFile.write();
				boolean saved = textFile.save(path, text);
				
				if (saved)
					_logger.info(labels.getString("s202") + path);
				else
					_logger.info(labels.getString("s203"));
			}
		});
		
		_loadCategoriesButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				TextFile file = new TextFile();
				String path = file.read();
				
				String textContent = null;
				textContent = file.load(path);
				_categoriesTextArea.setText(textContent);
				_logger.info(labels.getString("s201"));
			}
		});
		
		_saveCategoriesButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				String text = _categoriesTextArea.getText();
				TextFile textFile = new TextFile();
				String path = textFile.write();
				boolean saved = textFile.save(path, text);
				
				if (saved)
					_logger.info(labels.getString("s204") + path);
				else
					_logger.info(labels.getString("s205"));
			}
		});
		
		ActionListener escPressed = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_frame.dispose();
				_logger.info(labels.getString("s183"));
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
		_frame.add(_categoriesPanel, constraints);
		constraints.gridx = 1;
		_frame.add(_rulesPanel, constraints);
		constraints.gridwidth = 2;
		constraints.gridx = 0;
		constraints.gridy = 2;
		_frame.add(_buttonPanel, constraints);
		_frame.setResizable(false);
		_frame.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = _frame.getSize();
		_frame.setLocation((screenSize.width - frameSize.width) / 2,
				(screenSize.height - frameSize.height) / 2);
		_frame.setVisible(true);
		_logger.info(labels.getString("s174"));
		
		if (modify){
			
			String jarPath = null;
			String currentPath = null;
			try {
				jarPath = PropertiesManager.getProperty("jarPath");
				
				// GET THE PATH
				currentPath = PropertiesManager.getProperty("currentGrammar");
				int index = currentPath.lastIndexOf("\\");
				if (index == -1)
					index = currentPath.lastIndexOf("/");			
				currentPath = currentPath.substring(0, index);
				
				Output s = new Output(false);
				ProcessThread p = new ProcessThread();
				p.executeCommand("cmd", currentPath, "\"" + jarPath + "\" xvf "
						+ _grammarName + ".jar syntaxRules.txt lexicalCats.txt",
						"exit", s);
				
				// Runtime.getRuntime().exec("\"" + jarPath + "\" xvf " +
				// grammarName + ".jar syntaxRules.txt lexicalCats.txt");
				
				Thread.sleep(200);
				TextFile file = new TextFile();
				String txt = file.load(currentPath + "lexicalCats.txt");
				_categoriesTextArea.setText(txt);
				txt = file.load(currentPath + "syntaxRules.txt");
				_rulesTextArea.setText(txt);
				_logger.info(labels.getString("s174"));
			} catch (Exception e) {
				JOptionPane.showMessageDialog(null, e.getMessage(),
						labels.getString("s938"), JOptionPane.ERROR_MESSAGE);
				_logger.error(e.getMessage());
			}
		}
	}

	/**
	 * Shows the load grammar GUI.
	 */
	public static void loadGrammarGUI() {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		final ResourceBundle labels = language.getLabels();
		
		JFileChooser chooser = new JFileChooser();
		TextFileFilter filter = new TextFileFilter(labels.getString("s270"));
		filter.addExtension("jar");
		chooser.setFileFilter(filter);
		chooser.setCurrentDirectory(new File("./configuration/grammars/"));
		
		int optionChosen = chooser.showOpenDialog(null);
		
		// IF OK
		if (optionChosen == JFileChooser.APPROVE_OPTION) {
			
			String grammarFile = chooser.getSelectedFile().getAbsolutePath();
			PropertiesManager.setProperty("currentGrammar", grammarFile);
			_logger.info(labels.getString("s243") + " " + grammarFile);
			
			// GET THE GRAMMAR NAME
			int index = grammarFile.lastIndexOf("\\");		
			if (index == -1)
				index = grammarFile.lastIndexOf("/");		
			String grammarName = grammarFile.substring(index + 1,
					grammarFile.length() - 4);
			
			// UPDATES THE STATUS BAR
			MainWindow mainWindow = MainWindow.getInstance();
			mainWindow.getStatusBar().setMessageGrammar(
					labels.getString("s248") + " " + grammarName);
			
			// UPDATES THE CONFIGURATION
			mainWindow.getProjectConfiguration().setSyntacticConfiguration(grammarFile);
			mainWindow.validate();
			mainWindow.repaint();
			_changesAreSaved = true;
			
			// UPDATES THE MENU
			mainWindow.getMenu().getConfiguration().getGrammar().getSaveGrammar()
					.setEnabled(false);
			
			// NOT DEFAULT PROJECT
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {
				
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);
			}

		} else if (optionChosen == JFileChooser.CANCEL_OPTION) {
			_logger.info(labels.getString("s242"));
		}
	}

	/**
	 * Shows the save grammar GUI.
	 */
	public static void saveGrammarGUI() {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		final ResourceBundle labels = language.getLabels();
		
		try {
			String previous = PropertiesManager.getProperty("previousGrammar");
			String current = PropertiesManager.getProperty("currentGrammar");
			
			// GET THE NAME
			int index = previous.lastIndexOf("\\");
			if (index == -1)
				index = previous.lastIndexOf("/");
			String newName = previous.substring(index + 1,
					previous.length() - 4);
			
			ByteFile.copy(current, previous);
			PropertiesManager.setProperty("currentGrammar", previous);
			
			MainWindow.getInstance().getMenu().getConfiguration().getGrammar().getSaveGrammar().setEnabled(false);
			MainWindow.getInstance().getStatusBar().setMessageGrammar(
					labels.getString("s248") + " " + newName);
			MainWindow.getInstance().getProjectConfiguration().setSyntacticConfiguration(previous);
			_changesAreSaved = true;
			_logger.info(labels.getString("s940") + ": " + previous);
			
		} catch (Exception e) {
			JOptionPane.showMessageDialog(null, e.getMessage(),
					labels.getString("s939"), JOptionPane.ERROR_MESSAGE);
			_logger.error(e.getMessage());
		}
	}

	/**
	 * Shows the save as grammar GUI.
	 */
	public static void saveAsGrammarGUI() {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// GET THE LABELS
		final ResourceBundle labels = language.getLabels();
		try {
			
			String current = PropertiesManager.getProperty("currentGrammar");
			JFileChooser selector = new JFileChooser();
			TextFileFilter filter = new TextFileFilter(labels.getString("s270"));
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
				PropertiesManager.setProperty("currentGrammar", fileName);
				
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getConfiguration().getGrammar().getSaveGrammar().setEnabled(false);
				
				// GET THE GRAMMAR NAME
				int index = fileName.lastIndexOf("\\");	
				if (index == -1)
					index = fileName.lastIndexOf("/");			
				String grammarName = fileName.substring(index + 1,
						fileName.length() - 4);
				
				// UPDATES THE STATUS BAR
				mainWindow.getStatusBar().setMessageGrammar(
						labels.getString("s248") + " " + grammarName);
				mainWindow.getProjectConfiguration().setSyntacticConfiguration(
						fileName);
				_changesAreSaved = true;
				_logger.info(labels.getString("s941") + ": " + fileName);
			} else if (optionChosen == JFileChooser.CANCEL_OPTION) {
				selector.cancelSelection();
				_logger.info(labels.getString("s942"));
			}
		} catch (Exception e1) {
			JOptionPane.showMessageDialog(null, e1.getMessage(),
					labels.getString("s943"), JOptionPane.ERROR_MESSAGE);
			_logger.error(e1.getMessage());
		}
	}
}
