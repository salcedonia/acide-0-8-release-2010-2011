package gui.menu.configuration.toolBar;

import es.bytes.ByteFile;
import es.configuration.menu.MenuConfiguration;
import es.configuration.toolBar.EditableToolBarCommandList;
import es.text.TextFileFilter;
import gui.MainWindow;
import gui.toolBarButton.ToolBarCommand;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;

import language.Language;

import org.apache.log4j.Logger;

import operations.factory.GUIFactory;
import operations.log.Log;
import properties.PropertiesManager;

/**
 * 
 */
public class ToolBarMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private JMenuItem _newToolBar;
	/**
	 * 
	 */
	private JMenuItem _loadToolBar;
	/**
	 * 
	 */
	private JMenuItem _modifyToolBar;
	/**
	 * 
	 */
	private JMenuItem _saveToolBar;
	/**
	 * 
	 */
	private JMenuItem _saveAsToolBar;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	
	/**
	 * Constructor of the class.
	 */
	public ToolBarMenu(){
				
		// MENU ITEM
		_newToolBar = new JMenuItem();
		_loadToolBar = new JMenuItem();	
		_modifyToolBar = new JMenuItem();
		_saveToolBar = new JMenuItem();
		_saveToolBar.setEnabled(false);
		_saveAsToolBar = new JMenuItem();
		
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
		_newToolBar.setText(labels.getString("s280"));
		
		// LOAD
		_loadToolBar.setText(labels.getString("s281"));
		
		// MODIFY
		_modifyToolBar.setText(labels.getString("s282"));
		
		// SAVE
		_saveToolBar.setText(labels.getString("s283"));
		
		// SAVE AS
		_saveAsToolBar.setText(labels.getString("s284"));
	}
	
	/**
	 * 
	 */
	public void setListeners(){
		
		// NEW
		_newToolBar.addActionListener(new NewToolBarListener());
		
		// LOAD
		_loadToolBar.addActionListener(new LoadToolBarListener());
		
		// MODIFY
		_modifyToolBar.addActionListener(new ModifyToolBarListener());	
		
		// SAVE
		_saveToolBar.addActionListener(new SaveToolBarListener());
		
		// SAVE AS
		_saveAsToolBar.addActionListener(new SaveAsToolBarListener());
	}

	/**
	 * 
	 */
	public void buildMenu(){
		
		removeAll();
		
		if (MenuConfiguration.getNewToolBar())
			add(_newToolBar);
		if (MenuConfiguration.getLoadToolBar())
			add(_loadToolBar);
		if (MenuConfiguration.getModifyToolBar())
			add(_modifyToolBar);
		if (MenuConfiguration.getSaveToolBar())
			add(_saveToolBar);
		if (MenuConfiguration.getSaveAsToolBar())
			add(_saveAsToolBar);
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getLoadToolBar() {
		return _loadToolBar;
	}

	/**
	 * 
	 * @param loadToolBar
	 */
	public void setLoadToolBar(JMenuItem loadToolBar) {
		_loadToolBar = loadToolBar;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getModifyToolBar() {
		return _modifyToolBar;
	}

	/**
	 * 
	 * @param modifyToolBar
	 */
	public void setModifyToolBar(JMenuItem modifyToolBar) {
		_modifyToolBar = modifyToolBar;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getNewToolBar() {
		return _newToolBar;
	}

	/**
	 * 
	 * @param newToolBar
	 */
	public void setNewToolBar(JMenuItem newToolBar) {
		_newToolBar = newToolBar;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveAsToolBar() {
		return _saveAsToolBar;
	}

	/**
	 * 
	 * @param saveAsToolBar
	 */
	public void setSaveAsToolBar(JMenuItem saveAsToolBar) {
		_saveAsToolBar = saveAsToolBar;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveToolBar() {
		return _saveToolBar;
	}

	/**
	 * 
	 * @param saveToolBar
	 */
	public void setSaveToolBar(JMenuItem saveToolBar) {
		_saveToolBar = saveToolBar;
	}
	
	/**
	 * 
	 */
	class NewToolBarListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			GUIFactory.getInstance().buildToolBarCommandGUI(false);
		}
	}
	
	/**
	 * 
	 */
	class LoadToolBarListener implements ActionListener{

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
					.getString("s904"));
			filter.addExtension("BHcfg");
			
			chooser.setFileFilter(filter);
			chooser.setCurrentDirectory(new File("./configuration/toolbar/"));
			
			int option = chooser.showOpenDialog(null);
			
			if (option == JFileChooser.APPROVE_OPTION) {
				
				String TBFile = chooser.getSelectedFile().getAbsolutePath();
				
				try {
					EditableToolBarCommandList.loadList(TBFile);
					EditableToolBarCommandList.loadAuxList(TBFile);
					ToolBarCommand.buildToolBar();
					ToolBarCommand.buildEditableToolBar();
					PropertiesManager.setProperty(
							"currentToolBarConfiguration", TBFile);
					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.validate();
					mainWindow.repaint();
					_saveToolBar.setEnabled(false);
					_logger.info(labels.getString("s905") + TBFile);
					ToolBarCommandGUI.setAreChangesSaved(true);
				
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
							labels.getString("s906") + TBFile,
							labels.getString("s907"),
							JOptionPane.ERROR_MESSAGE);
					_logger.error(labels.getString("s906") + TBFile);
				}
			} else if (option == JFileChooser.CANCEL_OPTION) {
				_logger.info(labels.getString("s908"));
			}
		}
	}

	/**
	 * 
	 */
	class ModifyToolBarListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			GUIFactory.getInstance().buildToolBarCommandGUI(true);
		}
	}

	class SaveToolBarListener implements ActionListener{
		
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
						.getProperty("previousToolBarConfiguration");
				String current = PropertiesManager
						.getProperty("currentToolBarConfiguration");
				ByteFile.copy(current, previous);
				PropertiesManager.setProperty(
						"currentToolBarConfiguration", previous);
				_saveToolBar.setEnabled(false);
				ToolBarCommandGUI.setAreChangesSaved(true);
			} catch (Exception e1) {
				JOptionPane.showMessageDialog(null, e1.getMessage(),
						labels.getString("s299"), JOptionPane.ERROR_MESSAGE);
			}
		}
	}

	class SaveAsToolBarListener implements ActionListener{
		
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
				
				String current = PropertiesManager
						.getProperty("currentToolBarConfiguration");
				JFileChooser selector = new JFileChooser();
				TextFileFilter filter = new TextFileFilter(labels
						.getString("s158"));
				filter.addExtension("BHcfg");
				selector.setFileFilter(filter);
				selector.setCurrentDirectory(new File(
						"./configuration/toolbar/"));
				String fileName = "";
				int valor = selector.showSaveDialog(selector);
				if (valor == JFileChooser.APPROVE_OPTION) {
					fileName = selector.getSelectedFile()
							.getAbsolutePath();
					if (!fileName.endsWith(".BHcfg"))
						fileName += ".BHcfg";
					ByteFile.copy(current, fileName);
					PropertiesManager.setProperty(
							"currentToolBarConfiguration", fileName);
					_saveToolBar.setEnabled(false);
					ToolBarCommandGUI.setAreChangesSaved(true);
					_logger.info(labels.getString("s900") + fileName
							+ labels.getString("s901"));
				} else if (valor == JFileChooser.CANCEL_OPTION) {
					selector.cancelSelection();
					_logger.info(labels.getString("s902"));
				}
			} catch (Exception e1) {
				JOptionPane.showMessageDialog(null, e1.getMessage(),
						labels.getString("s903"), JOptionPane.ERROR_MESSAGE);
				_logger.error(e1.getMessage());
			}
		}
	}
}