package gui.menu.configuration.grammar;

import gui.pleaseWaitWindow.PleaseWaitWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import language.Language;

import operations.configuration.MenuConfiguration;
import operations.factory.GUIFactory;
import properties.PropertiesManager;

/**
 * 
 */
public class GrammarMenu extends JMenu {

	/**
	 * serialVersionUID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private JMenuItem _newGrammar;
	/**
	 * 
	 */
	private JMenuItem _loadGrammar;
	/**
	 * 
	 */
	private JMenuItem _modifyGrammar;
	/**
	 * 
	 */
	private JMenuItem _saveGrammar;
	/**
	 * 
	 */
	private JMenuItem _saveAsGrammar;
	/**
	 * 
	 */
	private JMenuItem _setPaths;
	/**
	 * 
	 */
	private JCheckBoxMenuItem _cbAutoGrammarAnalysis;
	
	/**
	 * Constructor of the class.
	 */
	public GrammarMenu(){
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		ResourceBundle labels = language.getLabels();
		
		// MENU ITEM
		_newGrammar = new JMenuItem();
		_loadGrammar = new JMenuItem();
		_modifyGrammar = new JMenuItem();
		_saveGrammar = new JMenuItem();
		_saveGrammar.setEnabled(false);
		_saveAsGrammar = new JMenuItem();
		_setPaths = new JMenuItem();
		_cbAutoGrammarAnalysis = new JCheckBoxMenuItem(labels.getString("s911"));
		_cbAutoGrammarAnalysis.setSelected(false);
		
		setLanguageLabels();
	}

	/**
	 * 
	 */
	public void setLanguageLabels() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		ResourceBundle labels = language.getLabels();
		
		// NEW
		_newGrammar.setText(labels.getString("s30"));
		_newGrammar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_T,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		
		// LOAD
		_loadGrammar.setText(labels.getString("s226"));
		
		// MODIFY
		_modifyGrammar.setText(labels.getString("s227"));
		
		// SAVE
		_saveGrammar.setText(labels.getString("s251"));
		
		// SAVE AS
		_saveAsGrammar.setText(labels.getString("s285"));
		
		// SET PATHS
		_setPaths.setText(labels.getString("s912"));
	
		// AUTO GRAMMAR ANALYSIS
		_cbAutoGrammarAnalysis.setText(labels.getString("s911"));
	}
	
	/**
	 * 
	 */
	public void setListeners(){
		
		// NEW GRAMMAR
		_newGrammar.addActionListener(new NewGrammarListener());
		
		// LOAD GRAMMAR
		_loadGrammar.addActionListener(new LoadGrammarListener());
		
		// MODIFY GRAMMAR
		_modifyGrammar.addActionListener(new ModifyGrammarListener());	
		
		// SAVE GRAMMAR
		_saveGrammar.addActionListener(new SaveGrammarListener());
		
		// SAVE AS GRAMMAR
		_saveAsGrammar.addActionListener(new SaveAsGrammarListener());
		
		// SET PATHS
		_setPaths.addActionListener(new SetPathsListener());
		
		// AUTO GRAMMAR ANALYSIS
		_cbAutoGrammarAnalysis.addActionListener(new AutoGrammarAnalysisListener());
	}
	
	/**
	 * 
	 */
	public void buildMenu() {
		
		removeAll();
		
		if (MenuConfiguration.getNewGrammar())
			add(_newGrammar);
		if (MenuConfiguration.getLoadGrammar())
			add(_loadGrammar);
		if (MenuConfiguration.getModifyGrammar())
			add(_modifyGrammar);
		if (MenuConfiguration.getSaveGrammar())
			add(_saveGrammar);
		if (MenuConfiguration.getSaveAsGrammar())
			add(_saveAsGrammar);
		if ((MenuConfiguration.getNewGrammar()
				|| MenuConfiguration.getLoadGrammar()
				|| MenuConfiguration.getModifyGrammar()
				|| MenuConfiguration.getSaveGrammar() || MenuConfiguration
				.getSaveAsGrammar()) && (MenuConfiguration.getSetPaths()))
			addSeparator();
		if (MenuConfiguration.getSetPaths())
			add(_setPaths);
		if ((MenuConfiguration.getNewGrammar()
				|| MenuConfiguration.getLoadGrammar()
				|| MenuConfiguration.getModifyGrammar()
				|| MenuConfiguration.getSaveGrammar()
				|| MenuConfiguration.getSaveAsGrammar() || MenuConfiguration
				.getSetPaths()) && (MenuConfiguration.getAutoGrammarAnalysis()))
			addSeparator();
		if (MenuConfiguration.getAutoGrammarAnalysis())
			add(_cbAutoGrammarAnalysis);		
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getLoadGrammar() {
		return _loadGrammar;
	}

	/**
	 * 
	 * @param loadGrammar
	 */
	public void setLoadGrammar(JMenuItem loadGrammar) {
		_loadGrammar = loadGrammar;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getModifyGrammar() {
		return _modifyGrammar;
	}

	/**
	 * 
	 * @param modifyGrammar
	 */
	public void setModifyGrammar(JMenuItem modifyGrammar) {
		_modifyGrammar = modifyGrammar;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getNewGrammar() {
		return _newGrammar;
	}

	/**
	 * 
	 * @param newGrammar
	 */
	public void setNewGrammar(JMenuItem newGrammar) {
		_newGrammar = newGrammar;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveGrammar() {
		return _saveGrammar;
	}

	/**
	 * 
	 * @param saveGrammar
	 */
	public void setSaveGrammar(JMenuItem saveGrammar) {
		_saveGrammar = saveGrammar;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveAsGrammar() {
		return _saveAsGrammar;
	}

	/**
	 * 
	 * @param saveAsGrammar
	 */
	public void setSaveAsGrammar(JMenuItem saveAsGrammar) {
		_saveAsGrammar = saveAsGrammar;
	}
}

/**
 * 
 */
class NewGrammarListener implements ActionListener{
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		GUIFactory.getInstance().buildGrammarGUI(false);
	}
}

/**
 * 
 */
class LoadGrammarListener implements ActionListener{
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		GrammarGUI.loadGrammarGUI();
	}
}

/**
 * 
 */
class ModifyGrammarListener implements ActionListener{
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		GUIFactory.getInstance().buildGrammarGUI(true);
	}
}

/**
 * 
 */
class SaveGrammarListener implements ActionListener{
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		GrammarGUI.saveGrammarGUI();
	}
}

/**
 * 
 */
class SaveAsGrammarListener implements ActionListener{	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		GrammarGUI.saveAsGrammarGUI();
	}
}

/**
 * 
 */
class SetPathsListener implements ActionListener{	
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		new SetPathsGUI();
	}
}

/**
 * 
 */
class AutoGrammarAnalysisListener implements ActionListener{
	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		PleaseWaitWindow.showPleaseWaitWindow();
	}
}