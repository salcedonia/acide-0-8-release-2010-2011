package gui.menu.configuration.lexical;

import es.configuration.programmingLanguage.ProgrammingLanguage;
import es.text.TextFileFilter;
import gui.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import org.apache.log4j.Logger;

import language.Language;

import operations.configuration.MenuConfiguration;
import operations.factory.GUIFactory;
import operations.log.Log;
import properties.PropertiesManager;

/**
 * 
 */
public class LexiconMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private JMenuItem _modifyLexicon;
	/**
	 * 
	 */
	private JMenuItem _newLexicon;
	/**
	 * 
	 */
	private JMenuItem _saveLexicon;
	/**
	 * 
	 */
	private JMenuItem _loadParameters;
	/**
	 * 
	 */
	private JMenuItem _saveAsLexicon;
	
	/**
	 * Constructor of the class.
	 */
	public LexiconMenu(){
				
		// MENU ITEM
		_modifyLexicon = new JMenuItem();
		_loadParameters = new JMenuItem();
		_newLexicon = new JMenuItem();
		_saveLexicon = new JMenuItem();
		_saveAsLexicon = new JMenuItem();
		
		setLanguageLabels();
	}

	/**
	 * 
	 */
	public void setLanguageLabels() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		ResourceBundle labels = language.getLabels();
		
		// MODIFY
		_modifyLexicon.setText(labels.getString("s29"));
		_modifyLexicon.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		
		// LOAD
		_loadParameters.setText(labels.getString("s35"));
		_loadParameters.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		
		// NEW
		_newLexicon.setText(labels.getString("s249"));
		
		// SAVE
		_saveLexicon.setText(labels.getString("s250"));
		
		// SAVE AS
		_saveAsLexicon.setText(labels.getString("s286"));
	}
	
	/**
	 * 
	 */
	public void setListeners(){
		
		// MODIFY
		_modifyLexicon.addActionListener(new ModifyLexiconListener());
		
		// LOAD
		_loadParameters.addActionListener(new LoadLexiconListener());
		
		// SAVE
		_saveLexicon.addActionListener(new SaveLexicalListener());
		
		// SAVE AS
		_saveAsLexicon.addActionListener(new SaveAslexicalListener());
		
		// NEW
		_newLexicon.addActionListener(new NewLexicalListener());
	}
	
	/**
	 * 
	 */
	public void buildMenu(){
		
		removeAll();
		
		if (MenuConfiguration.getNewLexical())
			add(_newLexicon);
		if (MenuConfiguration.getLoadParameters())
			add(_loadParameters);
		if (MenuConfiguration.getLexicon())
			add(_modifyLexicon);
		if (MenuConfiguration.getSaveLexical())
			add(_saveLexicon);
		if (MenuConfiguration.getSaveAslexical())
			add(_saveAsLexicon);
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getLoadParameters() {
		return _loadParameters;
	}

	/**
	 * 
	 * @param loadParameters
	 */
	public void setLoadParameters(JMenuItem loadParameters) {
		_loadParameters = loadParameters;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getModifyLexicon() {
		return _modifyLexicon;
	}

	/**
	 * 
	 * @param modifyLexicon
	 */
	public void setModifyLexicon(JMenuItem modifyLexicon) {
		_modifyLexicon = modifyLexicon;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getNewLexicon() {
		return _newLexicon;
	}

	/**
	 * 
	 * @param newLexicon
	 */
	public void setNewLexicon(JMenuItem newLexicon) {
		_newLexicon = newLexicon;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveLexicon() {
		return _saveLexicon;
	}

	/**
	 * 
	 * @param saveLexicon
	 */
	public void setSaveLexicon(JMenuItem saveLexicon) {
		_saveLexicon = saveLexicon;
	}
		
	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveAsLexicon() {
		return _saveAsLexicon;
	}

	/**
	 * 
	 * @param saveAsLexicon
	 */
	public void setSaveAsLexicon(JMenuItem saveAsLexicon) {
		_saveAsLexicon = saveAsLexicon;
	}
}

/**
 * 
 */
class ModifyLexiconListener implements ActionListener {
	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		GUIFactory.getInstance().buildLexiconGUI();
	}
}

/**
 * 
 */
class SaveAslexicalListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		Logger logger = Log.getLog();
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		JFileChooser chooser = new JFileChooser(labels.getString("s126"));
		TextFileFilter filtro = new TextFileFilter(labels.getString("s287"));
		filtro.addExtension("xml");
		chooser.setFileFilter(filtro);
		chooser.setCurrentDirectory(new File("./configuration/lexical"));
		int option = chooser.showSaveDialog(null);
		String archivo = " ";
		if (option == JFileChooser.APPROVE_OPTION) {
			archivo = chooser.getSelectedFile().getAbsolutePath();
		}
		if (archivo.equals(" ")) {
			logger.info(labels.getString("s92"));
		} else {
			int index = archivo.lastIndexOf("\\");
			String nombre = archivo.substring(index + 1, archivo.length());
			if (nombre.contains(".")) {
				index = nombre.lastIndexOf(".");
				nombre = nombre.substring(0, index);
			}
			ProgrammingLanguage l = ProgrammingLanguage.getInstance();
			boolean resultado = l.saveAs(nombre, false, archivo);
			if (resultado) {
				JOptionPane.showMessageDialog(null, labels.getString("s451"),
						labels.getString("s450"), 1);

			} else {
				JOptionPane.showMessageDialog(null, labels.getString("s452"),
						labels.getString("s450"), 0);
			}
		}
	}
}

/**
 * 
 */
class SaveLexicalListener implements ActionListener {

	/**
	 * 
	 */
	public void actionPerformed(ActionEvent e) {

		Logger logger = Log.getLog();
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		ProgrammingLanguage l = ProgrammingLanguage.getInstance();
		String archivo = l.getName();
		System.out.println(archivo);
		if (archivo.equals(" ")) {
			logger.info(labels.getString("s92"));
		} else {
			int index = archivo.lastIndexOf("\\");
			String nombre = archivo.substring(index + 1, archivo.length());
			if (nombre.contains(".")) {
				index = nombre.lastIndexOf(".");
				nombre = nombre.substring(0, index);
			}
			boolean resultado = l.save(nombre, false);
			if (resultado) {
				JOptionPane.showMessageDialog(null, labels.getString("s451"),
						labels.getString("s450"), 1);

			} else {
				JOptionPane.showMessageDialog(null, labels.getString("s452"),
						labels.getString("s450"), 0);
			}
		}
	}
}

/**
 * 
 */
class NewLexicalListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {

		MainWindow mainWindow = MainWindow.getInstance();
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		String s = "";
		String ss = "";
		ss = JOptionPane.showInputDialog(null, labels.getString("s453"),
				labels.getString("s454"), 2);
		s = ".\\configuration\\lexical\\" + ss + ".xml";
		if (!ss.trim().equals("")) {
			ProgrammingLanguage.getInstance().newLanguage(s);
			int editor = MainWindow.getInstance().getEditorBuilder()
					.getNumEditors();
			for (int j = 0; j < editor; j++) {
				MainWindow.getInstance().getEditorBuilder().getEditorAt(j)
						.resetDoc();
			}
			MainWindow
					.getInstance()
					.getStatusBar()
					.getMessagelexical()
					.setText(
							labels.getString("s449")
									+ " "
									+ ProgrammingLanguage.getInstance()
											.getName());
			MainWindow
					.getInstance()
					.getProjectConfiguration()
					.setLanguageConfiguration(
							ProgrammingLanguage.getInstance().getName());

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
		}
	}
}

/**
 * 
 */
class LoadLexiconListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		
		ResourceBundle labels = Language.getInstance().getLabels();
		TextFileFilter filter = new TextFileFilter(labels.getString("s327"));
		filter.addExtension(".xml");
		
		JFileChooser selector = new JFileChooser();
		selector.setFileSelectionMode(JFileChooser.FILES_ONLY);
		selector.addChoosableFileFilter(filter);
		selector.setCurrentDirectory(new File("./configuration/lexical"));
		
		int chosenOption = selector.showOpenDialog(selector);
		
		if (chosenOption == JFileChooser.APPROVE_OPTION) {
			
			ProgrammingLanguage l = ProgrammingLanguage.getInstance();
			l.load(selector.getSelectedFile().getAbsolutePath());
			int editor = MainWindow.getInstance().getEditorBuilder()
					.getNumEditors();
			for (int i = 0; i < editor; i++) {
				MainWindow.getInstance().getEditorBuilder().getEditorAt(i)
						.resetDoc();
			}

			MainWindow
					.getInstance()
					.getStatusBar()
					.getMessagelexical()
					.setText(
							labels.getString("s449")
									+ " "
									+ ProgrammingLanguage.getInstance()
											.getName());
			MainWindow
					.getInstance()
					.getProjectConfiguration()
					.setLanguageConfiguration(
							ProgrammingLanguage.getInstance().getlanguagePath());

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
	}
}



