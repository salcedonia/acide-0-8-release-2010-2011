package gui.menu.configuration.grammar;

import gui.MainWindow;
import gui.output.Output;
import gui.pleaseWaitWindow.PleaseWaitWindow;

import java.awt.Dimension;
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
 * 
 */
public class GrammarGUI extends JFrame {

	/**
	 * _serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private static JFrame _frame;
	/**
	 * 
	 */
	private JPanel _categoriesPanel;
	/**
	 * 
	 */
	private JPanel _rulesPanel;
	/**
	 * 
	 */
	private JPanel _buttonPanel;
	/**
	 * 
	 */
	private JPanel _categoriesButtonPanel;
	/**
	 * 
	 */
	private JPanel _rulesButtonPanel;
	/**
	 * 
	 */
	private final JTextArea _taCategories;
	/**
	 * 
	 */
	private final JTextArea _taRules;
	/**
	 * 
	 */
	private JScrollPane _categoriesScrollPane;
	/**
	 * 
	 */
	private JScrollPane _rulesScrollPane;
	/**
	 * 
	 */
	private JButton _btnAccept;
	/**
	 * 
	 */
	private JButton _btnCancel;
	/**
	 * 
	 */
	private JButton _btnLoadCategories;
	/**
	 * 
	 */
	private JButton _btnSaveCategories;
	/**
	 * 
	 */
	private JButton _btnLoadRules;
	/**
	 * 
	 */
	private JButton _btnSaveRules;
	/**
	 * 
	 */
	private String _grammarName;
	/**
	 * 
	 */
	private static boolean _changesAreSaved;
	/**
	 *
	 */
	private static Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 * 
	 * @param modify
	 */
	public GrammarGUI(boolean modify) {

		_changesAreSaved = true;
		
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
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
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");
			
			_grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);
			
			_frame.setTitle(labels.getString("s230") + " - " + _grammarName);
		}
		else
			_frame.setTitle(labels.getString("s184"));
		
		// CATEGORIES PANEL
		_categoriesPanel = new JPanel();
		_categoriesPanel.setLayout(new GridBagLayout());
		_categoriesPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s175")));
		
		// RULES PANEL
		_rulesPanel = new JPanel();
		_rulesPanel.setLayout(new GridBagLayout());
		_rulesPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s176")));
		
		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());
		
		// CATEGORIES BUTTON PANEL
		_categoriesButtonPanel = new JPanel();
		_categoriesButtonPanel.setLayout(new GridBagLayout());
		
		// RULES BUTTON PANEL
		_rulesButtonPanel = new JPanel();
		_rulesButtonPanel.setLayout(new GridBagLayout());
		
		_taCategories = new JTextArea();
		_taCategories.setToolTipText(labels.getString("s179"));
		
		_taRules = new JTextArea();
		_taRules.setToolTipText(labels.getString("s180"));
		
		_categoriesScrollPane = new JScrollPane(_taCategories);
		_rulesScrollPane = new JScrollPane(_taRules);
		
		// ACCEPT BUTTON
		_btnAccept = new JButton(labels.getString("s177"));
		_btnAccept.setToolTipText(labels.getString("s181"));
		
		// CANCEL BUTTON
		_btnCancel = new JButton(labels.getString("s178"));
		_btnCancel.setToolTipText(labels.getString("s182"));
		
		// LOAD CATEGORIES BUTTON
		_btnLoadCategories = new JButton(labels.getString("s192"));
		_btnLoadCategories.setToolTipText(labels.getString("s193"));
		
		// SAVE CATEGORIES
		_btnSaveCategories = new JButton(labels.getString("s194"));
		_btnSaveCategories.setToolTipText(labels.getString("s195"));
		
		// LOAD RULES BUTTON
		_btnLoadRules = new JButton(labels.getString("s196"));
		_btnLoadRules.setToolTipText(labels.getString("s197"));
		
		// SAVE RULES BUTTON
		_btnSaveRules = new JButton(labels.getString("s198"));
		_btnSaveRules.setToolTipText(labels.getString("s199"));
		
		// LISTENERS
		_btnAccept.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				String txt = "header{\npackage operaciones.sintacticas.grammar;\n}\n";
				txt += "class GrammarLexer extends Lexer;\n";
				txt += _taCategories.getText();
				txt += "\nclass GrammarParser extends Parser;\n";
				txt += "options{k=2;}\n";
				txt += _taRules.getText();
				TextFile f = new TextFile();
				
				boolean save = f.save("grammar.g", txt);
				save = save
						&& f.save("lexicalCats.txt",
								_taCategories.getText());
				save = save
						&& f.save("syntaxRules.txt", _taRules.getText());
				if (save)
					_logger.info(labels.getString("s185"));
				else
					_logger.info(labels.getString("s186"));
				_frame.dispose();
				
				PleaseWaitWindow.showPleaseWaitWindow();
				PleaseWaitWindow.refreshPleaseWaitWindow();
				boolean generated = false;
				String newGrammarName = "newGrammar";
				String newGrammarPath = "./configuration/grammars/newGrammar.jar";
				try {
					generated = GrammarGenerator.generate(newGrammarName);
					String previousGrammar = PropertiesManager
							.getProperty("currentGrammar");
					if (_changesAreSaved)
						PropertiesManager.setProperty("previousGrammar",
								previousGrammar);
					PropertiesManager.setProperty("currentGrammar",
							newGrammarPath);
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getConfiguration().getGrammar()
							.getSaveGrammar().setEnabled(false);
					mainWindow.validate();
					mainWindow.repaint();
					_changesAreSaved = false;
					mainWindow.getStatusBar().setMessageGrammar(
							labels.getString("s248")
									+ " newGrammar (Not saved)");
					_frame.dispose();
					_logger.info(labels.getString("s935"));
				} catch (Exception e1) {
					JOptionPane.showMessageDialog(null, e1.getMessage(),
							labels.getString("s930"), JOptionPane.ERROR_MESSAGE);
					_logger.error(e1.getMessage());
				}
				PleaseWaitWindow.closePleaseWaitWindow();
				if (generated)
					_logger.info(labels.getString("s208"));
				else
					_logger.error(labels.getString("s209"));

				String prj = null;
				try {
					prj = PropertiesManager.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if (!(prj.equals("./configuration/default.acidePrj") && MainWindow
						.getInstance().getProjectConfiguration().getName()
						.equals(""))) {
					MainWindow.getInstance().getProjectConfiguration()
							.setModified(true);
				}

			}
		});
		
		_btnCancel.addActionListener(new ActionListener() {
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
		
		_btnLoadRules.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				TextFile file = new TextFile();
				String path = file.read();
				String text = null;
				text = file.load(path);
				_taRules.setText(text);
				_logger.info(labels.getString("s200"));
			}
		});
		
		_btnSaveRules.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				String text = _taRules.getText();
				TextFile f = new TextFile();
				String path = f.write();
				boolean saved = f.save(path, text);
				if (saved)
					_logger.info(labels.getString("s202") + path);
				else
					_logger.info(labels.getString("s203"));
			}
		});
		
		_btnLoadCategories.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				TextFile file = new TextFile();
				String path = file.read();
				String text = null;
				text = file.load(path);
				_taCategories.setText(text);
				_logger.info(labels.getString("s201"));
			}
		});
		
		_btnSaveCategories.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				String text = _taCategories.getText();
				TextFile f = new TextFile();
				String path = f.write();
				boolean saved = f.save(path, text);
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
		
		_btnCancel.registerKeyboardAction(escPressed, "EscapeKey",
				KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0,
						true), JComponent.WHEN_IN_FOCUSED_WINDOW);

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
		_categoriesButtonPanel.add(_btnLoadCategories, constraints);
		constraints.gridx = 1;
		_categoriesButtonPanel.add(_btnSaveCategories, constraints);
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
		_rulesButtonPanel.add(_btnLoadRules, constraints);
		constraints.gridx = 1;
		_rulesButtonPanel.add(_btnSaveRules, constraints);
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
		_buttonPanel.add(_btnAccept, constraints);
		constraints.gridx = 1;
		_buttonPanel.add(_btnAccept, constraints);
		
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
				currentPath = PropertiesManager.getProperty("currentGrammar");
				int ind = currentPath.lastIndexOf("\\");
				
				if (ind == -1)
					ind = currentPath.lastIndexOf("/");
				
				currentPath = currentPath.substring(0, ind + 1);
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
				_taCategories.setText(txt);
				txt = file.load(currentPath + "syntaxRules.txt");
				_taRules.setText(txt);
				_logger.info(labels.getString("s174"));
			} catch (Exception e) {
				JOptionPane.showMessageDialog(null, e.getMessage(),
						labels.getString("s938"), JOptionPane.ERROR_MESSAGE);
				_logger.error(e.getMessage());
			}
		}
	}

	/**
	 * 
	 */
	public static void loadGrammarGUI() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		final ResourceBundle labels = language.getLabels();
		
		JFileChooser chooser = new JFileChooser();
		TextFileFilter filter = new TextFileFilter(labels.getString("s270"));
		filter.addExtension("jar");
		chooser.setFileFilter(filter);
		chooser.setCurrentDirectory(new File("./configuration/grammars/"));
		
		int option = chooser.showOpenDialog(null);
		
		if (option == JFileChooser.APPROVE_OPTION) {
			
			String grammarFile = chooser.getSelectedFile().getAbsolutePath();
			PropertiesManager.setProperty("currentGrammar", grammarFile);
			_logger.info(labels.getString("s243") + " " + grammarFile);
			
			int index = grammarFile.lastIndexOf("\\");
			
			if (index == -1)
				index = grammarFile.lastIndexOf("/");
			
			String grammarName = grammarFile.substring(index + 1,
					grammarFile.length() - 4);
			
			MainWindow mainWindow = MainWindow.getInstance();
			mainWindow.getStatusBar().setMessageGrammar(
					labels.getString("s248") + " " + grammarName);
			mainWindow.getProjectConfiguration().setGrammarConfiguration(grammarFile);
			mainWindow.validate();
			mainWindow.repaint();
			_changesAreSaved = true;
			mainWindow.getMenu().getConfiguration().getGrammar().getSaveGrammar()
					.setEnabled(false);

			String prj = null;
			try {
				prj = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
					.getProjectConfiguration().getName().equals(""))) {
				MainWindow.getInstance().getProjectConfiguration()
						.setModified(true);
			}

		} else if (option == JFileChooser.CANCEL_OPTION) {
			_logger.info(labels.getString("s242"));
		}
	}

	/**
	 * 
	 */
	public static void saveGrammarGUI() {
		
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = language.getLabels();
		
		try {
			String previous = PropertiesManager.getProperty("previousGrammar");
			String current = PropertiesManager.getProperty("currentGrammar");
			int index = previous.lastIndexOf("\\");
			if (index == -1)
				index = previous.lastIndexOf("/");
			String newName = previous.substring(index + 1,
					previous.length() - 4);
			ByteFile.copy(current, previous);
			PropertiesManager.setProperty("currentGrammar", previous);
			
			MainWindow mainWindow = MainWindow.getInstance();
			mainWindow.getMenu().getConfiguration().getGrammar().getSaveGrammar().setEnabled(false);
			mainWindow.getStatusBar().setMessageGrammar(
					labels.getString("s248") + " " + newName);
			mainWindow.getProjectConfiguration().setGrammarConfiguration(previous);
			_changesAreSaved = true;
			_logger.info(labels.getString("s940") + ": " + previous);
		} catch (Exception e) {
			JOptionPane.showMessageDialog(null, e.getMessage(),
					labels.getString("s939"), JOptionPane.ERROR_MESSAGE);
			_logger.error(e.getMessage());
		}
	}

	/**
	 * 
	 */
	public static void saveAsGrammarGUI() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = language.getLabels();
		try {
			String current = PropertiesManager.getProperty("currentGrammar");
			JFileChooser selector = new JFileChooser();
			TextFileFilter filter = new TextFileFilter(labels.getString("s270"));
			filter.addExtension("jar");
			selector.setFileFilter(filter);
			selector.setCurrentDirectory(new File("./configuration/grammars/"));
			
			String fileName = "";
			int value = selector.showSaveDialog(selector);
			if (value == JFileChooser.APPROVE_OPTION) {
				
				fileName = selector.getSelectedFile().getAbsolutePath();
				
				if (!fileName.endsWith(".jar"))
					fileName += ".jar";
				
				ByteFile.copy(current, fileName);
				PropertiesManager.setProperty("currentGrammar", fileName);
				
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getConfiguration().getGrammar().getSaveGrammar().setEnabled(false);
				
				int index = fileName.lastIndexOf("\\");
				
				if (index == -1)
					index = fileName.lastIndexOf("/");
				
				String grammarName = fileName.substring(index + 1,
						fileName.length() - 4);
				mainWindow.getStatusBar().setMessageGrammar(
						labels.getString("s248") + " " + grammarName);
				mainWindow.getProjectConfiguration().setGrammarConfiguration(
						fileName);
				_changesAreSaved = true;
				_logger.info(labels.getString("s941") + ": " + fileName);
			} else if (value == JFileChooser.CANCEL_OPTION) {
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
