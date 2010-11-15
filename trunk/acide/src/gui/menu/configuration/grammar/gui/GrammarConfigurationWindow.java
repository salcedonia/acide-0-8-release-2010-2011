package gui.menu.configuration.grammar.gui;

import gui.mainWindow.MainWindow;
import gui.output.OutputPanel;

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

import es.bytes.ByteFile;
import es.text.TextFile;
import es.text.TextFileFilter;

import operations.log.Log;
import operations.output.OutputThread;
import operations.parser.GrammarGenerator;
import properties.PropertiesManager;

/************************************************************************																
 * Grammar configuration window of ACIDE - A Configurable IDE										
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
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
public class GrammarConfigurationWindow extends JFrame {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the window icon
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * Categories panel
	 */
	private JPanel _categoriesPanel;
	/**
	 * Rules panel
	 */
	private JPanel _rulesPanel;
	/**
	 * Button panel
	 */
	private JPanel _buttonPanel;
	/**
	 * Categories button panel
	 */
	private JPanel _categoriesButtonPanel;
	/**
	 * Rules button panel
	 */
	private JPanel _rulesButtonPanel;
	/**
	 * Categories text area
	 */
	private final JTextArea _categoriesTextArea;
	/**
	 * Rules text area
	 */
	private final JTextArea _rulesTextArea;
	/**
	 * Categories scroll pane
	 */
	private JScrollPane _categoriesScrollPane;
	/**
	 * Rules scroll pane
	 */
	private JScrollPane _rulesScrollPane;
	/**
	 * Accept button
	 */
	private JButton _acceptButton;
	/**
	 * Cancel button
	 */
	private JButton _cancelButton;
	/**
	 * Load categories button
	 */
	private JButton _loadCategoriesButton;
	/**
	 * Save categories button
	 */
	private JButton _saveCategoriesButton;
	/**
	 * Load rules button
	 */
	private JButton _loadRulesButton;
	/**
	 * Save rules button
	 */
	private JButton _saveRulesButton;
	/**
	 * Grammar name string
	 */
	private String _grammarName;
	/**
	 * Flag that indicates if the changes are saved or not
	 */
	private static boolean _changesAreSaved;

	/**
	 * Class constructor
	 * 
	 * @param modify indicates if the window is used for modify or
	 * for create a grammar
	 */
	public GrammarConfigurationWindow(boolean modify) {

		_changesAreSaved = true;
		
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
		final ResourceBundle labels = language.getLabels();
		
		// Updates the log
		Log.getLog().info(labels.getString("s173"));
		
		// FRAME
		setLayout(new GridBagLayout());
		setIconImage(new ImageIcon(ICON).getImage());
		
		if(modify){
			
			String currentGrammar = null;
			
			try {
				currentGrammar = PropertiesManager.getProperty("currentGrammar");
			} catch (Exception exception) {
				
				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						labels.getString("s936"), JOptionPane.ERROR_MESSAGE);
				
				// Updates the log
				Log.getLog().error(exception.getMessage());
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
				TextFile textFile = new TextFile();
				
				boolean isSaved = textFile.save("grammar.g", textContent);
				
				isSaved = isSaved
						&& textFile.save("lexicalCats.txt",
								_categoriesTextArea.getText());
				isSaved = isSaved
						&& textFile.save("syntaxRules.txt", _rulesTextArea.getText());
				
				if (isSaved)
					
					// Updates the log
					Log.getLog().info(labels.getString("s185"));
				else
					// Updates the log
					Log.getLog().info(labels.getString("s186"));
				
				dispose();
				
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
					MainWindow.getInstance().getStatusBar().setGrammarMessage(
							labels.getString("s248")
									+ " newGrammar (Not saved)");
					dispose();
					
					// Updates the log
					Log.getLog().info(labels.getString("s935"));
				} catch (Exception exception) {
					
					// Error message
					JOptionPane.showMessageDialog(null, exception.getMessage(),
							labels.getString("s930"), JOptionPane.ERROR_MESSAGE);
					
					// Updates the log
					Log.getLog().error(exception.getMessage());
				}

				if (isGenerated)
					// Updates the log
					Log.getLog().info(labels.getString("s208"));
				else
					// Updates the log
					Log.getLog().error(labels.getString("s209"));
				
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
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				dispose();
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
				
				// Updates the log
				Log.getLog().info(labels.getString("s183"));
			}
		});
		
		_loadRulesButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				TextFile file = new TextFile();
				String path = file.read();
				String text = null;
				text = file.load(path);
				_rulesTextArea.setText(text);
				
				// Updates the log
				Log.getLog().info(labels.getString("s200"));
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
				TextFile textFile = new TextFile();
				String path = textFile.write();
				boolean saved = textFile.save(path, text);
				
				if (saved)
					// Updates the log
					Log.getLog().info(labels.getString("s202") + path);
				else
					// Updates the log
					Log.getLog().info(labels.getString("s203"));
			}
		});
		
		_loadCategoriesButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				TextFile file = new TextFile();
				String path = file.read();
				
				String textContent = null;
				textContent = file.load(path);
				_categoriesTextArea.setText(textContent);
				
				// Updates the log
				Log.getLog().info(labels.getString("s201"));
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
				TextFile textFile = new TextFile();
				String path = textFile.write();
				boolean saved = textFile.save(path, text);
				
				if (saved)
					// Updates the log
					Log.getLog().info(labels.getString("s204") + path);
				else
					// Updates the log
					Log.getLog().info(labels.getString("s205"));
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
				Log.getLog().info(labels.getString("s183"));
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
		Log.getLog().info(labels.getString("s174"));
		
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
				
				OutputPanel s = new OutputPanel(false);
				OutputThread p = new OutputThread();
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
				
				// Updates the log
				Log.getLog().info(labels.getString("s174"));
			} catch (Exception exception) {
				
				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						labels.getString("s938"), JOptionPane.ERROR_MESSAGE);
				
				// Updates the log
				Log.getLog().error(exception.getMessage());
			}
		}
	}

	/**
	 * Shows the load grammar GUI
	 */
	public static void loadGrammarGUI() {
		
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
			
			// Updates the log
			Log.getLog().info(labels.getString("s243") + " " + grammarFile);
			
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
			// Updates the log
			Log.getLog().info(labels.getString("s242"));
		}
	}

	/**
	 * Shows the save grammar GUI.
	 */
	public static void saveGrammarGUI() {
		
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
			MainWindow.getInstance().getStatusBar().setGrammarMessage(
					labels.getString("s248") + " " + newName);
			MainWindow.getInstance().getProjectConfiguration().setSyntacticConfiguration(previous);
			_changesAreSaved = true;
			
			// Updates the log
			Log.getLog().info(labels.getString("s940") + ": " + previous);
			
		} catch (Exception exception) {
			
			JOptionPane.showMessageDialog(null, exception.getMessage(),
					labels.getString("s939"), JOptionPane.ERROR_MESSAGE);
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
		}
	}

	/**
	 * Shows the save as grammar GUI
	 */
	public static void saveAsGrammarGUI() {
		
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
				
				// Updates the status bar
				mainWindow.getStatusBar().setGrammarMessage(
						labels.getString("s248") + " " + grammarName);
				mainWindow.getProjectConfiguration().setSyntacticConfiguration(
						fileName);
				_changesAreSaved = true;
				
				// Updates the log
				Log.getLog().info(labels.getString("s941") + ": " + fileName);
			} else if (optionChosen == JFileChooser.CANCEL_OPTION) {
				selector.cancelSelection();
				// Updates the log
				Log.getLog().info(labels.getString("s942"));
			}
		} catch (Exception e1) {
			
			// Error message
			JOptionPane.showMessageDialog(null, e1.getMessage(),
					labels.getString("s943"), JOptionPane.ERROR_MESSAGE);
			
			// Updates the log
			Log.getLog().error(e1.getMessage());
		}
	}
}
