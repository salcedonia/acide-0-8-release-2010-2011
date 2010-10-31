package gui.menu.configuration.menu;

import es.bytes.ByteFile;
import es.text.TextFileFilter;
import gui.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;

import org.apache.log4j.Logger;

import language.Language;

import operations.configuration.MenuConfiguration;
import operations.factory.GUIFactory;
import operations.log.Log;
import properties.PropertiesManager;

/**
 * 
 */
public class MenuMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private JMenuItem _newMenu;
	/**
	 * 
	 */
	private JMenuItem _loadMenu;
	/**
	 * 
	 */
	private JMenuItem _modifyMenu;
	/**
	 * 
	 */
	private JMenuItem _saveMenu;
	/**
	 * 
	 */
	private JMenuItem _saveAsMenu;
	
	/**
	 * Constructor of the class.
	 */
	public MenuMenu(){
				
		// MENU ITEM
		_newMenu = new JMenuItem();
		_loadMenu = new JMenuItem();		
		_modifyMenu = new JMenuItem();
		_saveMenu = new JMenuItem();
		_saveMenu.setEnabled(false);
		_saveAsMenu = new JMenuItem();
				
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

		final ResourceBundle labels = language.getLabels();
		
		// NEW
		_newMenu.setText(labels.getString("s275"));
		
		// LOAD
		_loadMenu.setText(labels.getString("s276"));
		
		// MODIFY
		_modifyMenu.setText(labels.getString("s277"));
		
		// SAVE
		_saveMenu.setText(labels.getString("s278"));
		
		// SAVE AS
		_saveAsMenu.setText(labels.getString("s279"));	
	}
	
	/**
	 * 
	 */
	public void buildMenu() {

		removeAll();
		
		if (MenuConfiguration.getNewMenu())
			add(_newMenu);
		if (MenuConfiguration.getLoadMenu())
			add(_loadMenu);
		if (MenuConfiguration.getModifyMenu())
			add(_modifyMenu);
		if (MenuConfiguration.getSaveMenu())
			add(_saveMenu);
		if (MenuConfiguration.getSaveAsMenu())
			add(_saveAsMenu);
	}
	
	/**
	 * 
	 */
	public void setListeners(){
		
		// NEW MENU
		_newMenu.addActionListener(new NewMenuListener());
		
		// LOAD MENU
		_loadMenu.addActionListener(new LoadMenuListener());
		
		// MODIFY MENU
		_modifyMenu.addActionListener(new ModifyMenuListener());
		
		// SAVE MENU
		_saveMenu.addActionListener(new SaveMenuListener());
		
		// SAVE AS MENU
		_saveAsMenu.addActionListener(new SaveAsMenuListener());
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getLoadMenu() {
		return _loadMenu;
	}

	/**
	 * 
	 * @param loadMenu
	 */
	public void setLoadMenu(JMenuItem loadMenu) {
		_loadMenu = loadMenu;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getModifyMenu() {
		return _modifyMenu;
	}

	/**
	 * 
	 * @param modifyMenu
	 */
	public void setModifyMenu(JMenuItem modifyMenu) {
		_modifyMenu = modifyMenu;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getNewMenu() {
		return _newMenu;
	}

	/**
	 * 
	 * @param newMenu
	 */
	public void setNewMenu(JMenuItem newMenu) {
		_newMenu = newMenu;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveAsMenu() {
		return _saveAsMenu;
	}

	/**
	 * 
	 * @param saveAsMenu
	 */
	public void setSaveAsMenu(JMenuItem saveAsMenu) {
		_saveAsMenu = saveAsMenu;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveMenu() {
		return _saveMenu;
	}

	/**
	 * 
	 * @param saveMenu
	 */
	public void setSaveMenu(JMenuItem saveMenu) {
		_saveMenu = saveMenu;
	}
	
	/**
	 * 
	 */
	class NewMenuListener implements ActionListener{
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			GUIFactory.getInstance().buildMenuGUI(false);
		}
	}
	
	/**
	 * 
	 */
	class LoadMenuListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			Language language = Language.getInstance();
			
			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception ex) {
				ex.printStackTrace();
			}

			final ResourceBundle labels = language.getLabels();
			
			JFileChooser chooser = new JFileChooser();
			TextFileFilter filter = new TextFileFilter(labels
					.getString("s287"));
			filter.addExtension("menuCfg");
			chooser.setFileFilter(filter);
			chooser.setCurrentDirectory(new File("./configuration/menu/"));
			
			int option = chooser.showOpenDialog(null);
			if (option == JFileChooser.APPROVE_OPTION) {
				String menuFile = chooser.getSelectedFile()
						.getAbsolutePath();
				boolean[] valores = null;
				try {
					valores = MenuConfiguration
							.loadMenuConfigurationFile(menuFile);
					PropertiesManager.setProperty(
							"currentMenuConfiguration", menuFile);
					MenuConfiguration.setAll(valores);
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().buildMenu();
					mainWindow.validate();
					mainWindow.repaint();
					_saveMenu.setEnabled(false);
					_logger.info(labels.getString("s289"));
					MenuGUI.setChangesSaved(true);

					String prj = null;
					try {
						prj = PropertiesManager
								.getProperty("defaultAcideProject");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
							.getProjectConfiguration().getName().equals(""))) {
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);
					}

				} catch (Exception e1) {
					JOptionPane.showMessageDialog(null,
							labels.getString("s288") + " " + menuFile,
							labels.getString("289"),
							JOptionPane.ERROR_MESSAGE);
					_logger.error(labels.getString("s288") + " " + menuFile);
				}
			} else if (option == JFileChooser.CANCEL_OPTION) {
				_logger.info(labels.getString("s290"));
			}
		}
	}

	/**
	 * 
	 */
	class ModifyMenuListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			GUIFactory.getInstance().buildMenuGUI(true);
		}
	}

	/**
	 * 
	 */
	class SaveMenuListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			Language language = Language.getInstance();
			
			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception ex) {
				ex.printStackTrace();
			}

			final ResourceBundle labels = language.getLabels();
			
			try {
				String previous = PropertiesManager
						.getProperty("previousMenuConfiguration");
				String current = PropertiesManager
						.getProperty("currentMenuConfiguration");
				ByteFile.copy(current, previous);
				PropertiesManager.setProperty("currentMenuConfiguration",
						previous);
				_saveMenu.setEnabled(false);
				MenuGUI.setChangesSaved(true);
			} catch (Exception e1) {
				JOptionPane.showMessageDialog(null, e1.getMessage(),
						labels.getString("s293"), JOptionPane.ERROR_MESSAGE);
				_logger.error(e1.getMessage());
			}
		}
	}

	/**
	 * 
	 */
	class SaveAsMenuListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			Language language = Language.getInstance();
			
			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception ex) {
				ex.printStackTrace();
			}

			final ResourceBundle labels = language.getLabels();
			
			try {
				String currentMenu = PropertiesManager
						.getProperty("currentMenuConfiguration");
				JFileChooser selector = new JFileChooser();
				TextFileFilter filter = new TextFileFilter(labels
						.getString("s126"));
				filter.addExtension("menuCfg");
				selector.setFileFilter(filter);
				selector.setCurrentDirectory(new File(
						"./configuration/menu/"));
				String nombreFichero = "";
				int valor = selector.showSaveDialog(selector);
				if (valor == JFileChooser.APPROVE_OPTION) {
					nombreFichero = selector.getSelectedFile()
							.getAbsolutePath();
					if (!nombreFichero.endsWith(".menuCfg"))
						nombreFichero += ".menuCfg";
					ByteFile.copy(currentMenu, nombreFichero);
					PropertiesManager.setProperty(
							"currentMenuConfiguration", nombreFichero);
					_saveMenu.setEnabled(false);
					MenuGUI.setChangesSaved(true);
					_logger.info(labels.getString("s528") + nombreFichero
							+ labels.getString("s529"));
				} else if (valor == JFileChooser.CANCEL_OPTION) {
					selector.cancelSelection();
					_logger.info(labels.getString("s527"));
				}
			} catch (Exception e1) {
				JOptionPane.showMessageDialog(null, e1.getMessage(),
						labels.getString("s291"), JOptionPane.ERROR_MESSAGE);
				_logger.error(e1.getMessage());
			}
		}
	}
}

