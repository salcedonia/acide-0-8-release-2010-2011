package gui.menu;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import language.Language;

import org.apache.log4j.Logger;

import es.configuration.menu.MenuConfiguration;
import es.explorer.ExplorerFile;

import operations.log.Log;
import properties.PropertiesManager;

import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.event.*;
import java.util.ResourceBundle;
import gui.MainWindow;
import gui.menu.configuration.ConfigurationMenu;
import gui.menu.edit.EditMenu;
import gui.menu.file.FileMenu;
import gui.menu.help.HelpMenu;
import gui.menu.project.ProjectMenu;
import gui.menu.view.ViewMenu;

/**
 * 
 */
public class Menu {

	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private JMenuBar _menuBar;
	/**
	 * 
	 */
	private FileMenu _file;
	/**
	 * 
	 */
	private EditMenu _edit;
	/**
	 * 
	 */
	private ProjectMenu _project;
	/**
	 * 
	 */
	private ViewMenu _view;
	/**
	 * 
	 */
	private ConfigurationMenu _configuration;
	/**
	 * 
	 */
	private HelpMenu _help;
	/**
	 * 
	 */
	private boolean _isShellFocus;
	/**
	 * 
	 */
	private boolean _isNPF;

	/**
	 * Constructor of the class.
	 */
	public Menu() {
		
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		final ResourceBundle labels = language.getLabels();
		
		_logger.info(labels.getString("s68"));
		
		// MENU BAR
		_menuBar = new JMenuBar();
		
		// MENUS
		_file = new FileMenu();
		_edit = new EditMenu();
		_project = new ProjectMenu();
		_view = new ViewMenu();
		_configuration = new ConfigurationMenu();
		_help = new HelpMenu();
								
		setLanguageLabels();
		buildMenu();
		
		_logger.info(labels.getString("s69"));
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
		
		// FILE
		_file.setText(labels.getString("s1"));
		_file.setLanguageLabels();
		
		// EDIT 
		_edit.setText(labels.getString("s2"));
		_edit.setLanguageLabels();
		
		// PROJECT
		_project.setText(labels.getString("s3"));
		_project.setLanguageLabels();
		
		// VIEW
		_view.setText(labels.getString("s4"));
		_view.setLanguageLabels();
		
		// CONFIGURATION
		_configuration.setText(labels.getString("s5"));
		_configuration.setLanguageLabels();
		
		// HELP
		_help.setText(labels.getString("s7"));	
		_help.setLanguageLabels();
		
		/*depurar.setText(labels.getString("s20"));
		depurar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D,
				ActionEvent.ALT_MASK));
		compilador.setText(labels.getString("s31"));
		compilador.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		interprete.setText(labels.getString("s32"));
		interprete.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		parser.setText(labels.getString("s33"));
		parser.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
				
		nuevaConfLenguaje.setText(labels.getString("s37"));
		nuevaConfLenguaje.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));*/	
		
	}

	/**
	 * 
	 */
	public void buildMenu() {
				
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		final ResourceBundle labels = language.getLabels();
		
		String currentMenu = null;
		
		try {
			currentMenu = PropertiesManager
					.getProperty("currentMenuConfiguration");
			boolean[] valores = MenuConfiguration
					.loadMenuConfigurationFile(currentMenu);
			MenuConfiguration.setAll(valores);
			PropertiesManager.setProperty("currentMenuConfiguration",
					currentMenu);
			_logger.info(labels.getString("s70") + " " + currentMenu);
		} catch (Exception e) {
			
			_logger.info(labels.getString("s71") + e.getMessage());
			
			// GET THE NAME
			String currentMenu2;
			int index = currentMenu.lastIndexOf("\\");
			if (index == -1)
				index = currentMenu.lastIndexOf("/");
			currentMenu2 = ".\\configuration\\menu\\"
					+ currentMenu.substring(index + 1, currentMenu.length());
			boolean[] values;
			try {
				values = MenuConfiguration.loadMenuConfigurationFile(currentMenu2);
				MenuConfiguration.setAll(values);
				PropertiesManager.setProperty("currentMenuConfiguration",
						currentMenu2);
				_logger.info(labels.getString("s70") + " " + currentMenu2);
				JOptionPane
						.showMessageDialog(null, labels.getString("s956")
								+ currentMenu + labels.getString("s957")
								+ currentMenu2);
			} catch (Exception e1) {
				try {
					values = MenuConfiguration
							.loadMenuConfigurationFile("./configuration/menu/defaultAllOn.menuCfg");
					MenuConfiguration.setAll(values);
					PropertiesManager.setProperty("currentMenuConfiguration",
							"./configuration/menu/defaultAllOn.menuCfg");
					JOptionPane.showMessageDialog(
							null,
							labels.getString("s956") + currentMenu
									+ labels.getString("s959"));
				} catch (HeadlessException e2) {
					e2.printStackTrace();
				} catch (Exception e2) {
					e2.printStackTrace();
				}
			}
		}

		_menuBar.removeAll();

		_file.buildMenu(labels,language);
		_edit.buildMenu();
		_project.buildMenu();
		_view.buildMenu();
		_configuration.buildMenu();		
		_help.buildMenu();
		
		if (MenuConfiguration.getFile()
				|| MenuConfiguration.getOpenFile()
				|| MenuConfiguration.getSaveFileAs()
				|| MenuConfiguration.getSaveFile()
				|| MenuConfiguration.getSaveAllFiles()
				|| MenuConfiguration.getPrintFile() || MenuConfiguration.getExit())
			_menuBar.add(_file);
		if (MenuConfiguration.getUndo() || MenuConfiguration.getRedo()
				|| MenuConfiguration.getCopy()
				|| MenuConfiguration.getPaste()
				|| MenuConfiguration.getCut()
				|| MenuConfiguration.getSelectAll()
				|| MenuConfiguration.getGoToLine() || MenuConfiguration.getSearch()
				|| MenuConfiguration.getReplace())
			_menuBar.add(_edit);
		if (MenuConfiguration.getProject()
				|| MenuConfiguration.getOpenProject()
				|| MenuConfiguration.getSaveProject()
				|| MenuConfiguration.getCloseProject()
				|| MenuConfiguration.getAddFile()
				|| MenuConfiguration.getRemoveFile()
				|| MenuConfiguration.getDeleteFile()
				|| MenuConfiguration.getNewProjectFile()
				|| MenuConfiguration.getAddFolder()
				|| MenuConfiguration.getRemoveFolder()
				|| MenuConfiguration.getCompile()
				|| MenuConfiguration.getExecute())
			_menuBar.add(_project);
		if (MenuConfiguration.getShowLog()
				|| MenuConfiguration.getShowBrowser()
				|| MenuConfiguration.getShowShellWindow())
			_menuBar.add(_view);
		
		_menuBar.add(_configuration);
		
		if (MenuConfiguration.getShowHelp()
				|| MenuConfiguration.getShowAboutUs())
			_menuBar.add(_help);
		
		_menuBar.setVisible(true);
	}

	/**
	 * 
	 */
	public void setListeners() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		
		// FILE
		_file.addMouseListener(new MouseClickMenu());
		_file.setListeners();
		
		// EDIT
		_edit.addMouseListener(new MouseClickMenu());
		_edit.setListeners();
		
		// PROJECT
		_project.addMouseListener(new MouseClickMenu());
		_project.setListeners();
		
		// VIEW
		_view.addMouseListener(new MouseClickMenu());
		_view.setListeners();
		
		// CONFIGURATION
		_configuration.addMouseListener(new MouseClickMenu());
		_configuration.setListeners();
		
		// HELP
		_help.addMouseListener(new MouseClickMenu());
		_help.setListeners();
		
		_logger.info(labels.getString("s72"));
	}

	/**
	 * 
	 * @return 
	 */
	public JMenuBar getMenuBar() {
		return _menuBar;
	}
	
	/**
	 * 
	 */
	public void enableFileMenu() {
		_file.enableMenu();
	}

	/**
	 * 
	 */
	public void disableFileMenu() {
		_file.disableMenu();	
	}
	
	/**
	 * 
	 */
	public void enableProjectMenu() {
		_project.enableMenu();
	}

	/**
	 * 
	 */
	public void deshabilitaMenuProyecto() {
		_project.disableMenu();		
	}

	/**
	 * 
	 * @param file
	 */
	public void setFile(FileMenu file) {
		_file = file;
	}
	
	/**
	 * 
	 * @return
	 */
	public HelpMenu getHelp() {
		return _help;
	}

	/**
	 * 
	 * @param help
	 */
	public void setHelp(HelpMenu help) {
		_help = help;
	}
	
	/**
	 * 
	 * @return
	 */
	public EditMenu getEdit() {
		return _edit;
	}

	/**
	 * 
	 * @param edit
	 */
	public void setEdit(EditMenu edit) {
		_edit = edit;
	}

	/**
	 * 
	 * @return
	 */
	public Logger getLogger() {
		return _logger;
	}

	/**
	 * 
	 * @param logger
	 */
	public void setLogger(Logger logger) {
		_logger = logger;
	}

	/**
	 * 
	 * @return
	 */
	public ConfigurationMenu getConfiguration() {
		return _configuration;
	}

	/**
	 * 
	 * @param configuration
	 */
	public void setConfiguration(ConfigurationMenu configuration) {
		_configuration = configuration;
	}

	/**
	 * 
	 * @return
	 */
	public ProjectMenu getProject() {
		return _project;
	}

	/**
	 * 
	 * @param project
	 */
	public void setProject(ProjectMenu project) {
		_project = project;
	}

	/**
	 * 
	 * @return
	 */
	public ViewMenu getView() {
		return _view;
	}

	/**
	 * 
	 * @param view
	 */
	public void setView(ViewMenu view) {
		_view = view;
	}

	/**
	 * 
	 * @param menuBar
	 */
	public void setMenuBar(JMenuBar menuBar) {
		_menuBar = menuBar;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isShellFocus() {
		return _isShellFocus;
	}

	/**
	 * 
	 * @param shellIsFocus
	 */
	public void setIsShellFocus(boolean shellIsFocus) {
		_isShellFocus = shellIsFocus;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isNPF() {
		return _isNPF;
	}

	/**
	 * 
	 * @param b
	 */
	public void setIsNPF(boolean b) {
		_isNPF = b;
	}

	/**
	 * 
	 * @return
	 */
	public FileMenu getFile() {
		
		return _file;
	}
	
	/**
	 * 
	 */
	public void enableEditMenu() {
		_edit.enableMenu();
	}
	
	/**
	 * 
	 */
	public void disableEditMenu() {
		_edit.disableMenu();
	}
}

/**
 * 
 */
class MouseClickMenu extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	public void mousePressed(MouseEvent arg0) {

		MainWindow mainWindow = MainWindow.getInstance();

		// Project Menu
		if (mainWindow.getMenu().getProject().isSelected()) {
			mainWindow.getMenu().getProject().getSaveProject().setEnabled(false);
			mainWindow.getMenu().getProject().getRemoveFile().setEnabled(false);
			mainWindow.getMenu().getProject().getDeleteFile().setEnabled(false);
			mainWindow.getMenu().getProject().getSetMain().setEnabled(false);
			mainWindow.getMenu().getProject().getUnsetMain().setEnabled(false);
			mainWindow.getMenu().getProject().getSetCompilable().setEnabled(false);
			mainWindow.getMenu().getProject().getUnsetCompilable().setEnabled(false);
			mainWindow.getMenu().getProject().getRemoveFolder().setEnabled(false);

			if (mainWindow.getProjectConfiguration().isModified()) {
				mainWindow.getMenu().getProject().getSaveProject().setEnabled(true);
			}

			TreePath path = mainWindow.getExplorer().getTree()
					.getSelectionPath();
			DefaultMutableTreeNode filePath;
			ExplorerFile fc;
			if (path != null) {

				filePath = (DefaultMutableTreeNode) path.getLastPathComponent();
				fc = (ExplorerFile) filePath.getUserObject();

				if (!fc.isDirectory()) {
					mainWindow.getMenu().getProject().getRemoveFile().setEnabled(true);
					mainWindow.getMenu().getProject().getDeleteFile().setEnabled(true);
					if (!fc.isMainFile())
						mainWindow.getMenu().getProject().getSetMain().setEnabled(true);
					if (fc.isMainFile())
						mainWindow.getMenu().getProject().getUnsetMain().setEnabled(true);
					if (!fc.isCompilableFile() || (fc.isCompilableFile() && fc.isMainFile()))
						mainWindow.getMenu().getProject().getSetCompilable()
								.setEnabled(true);
					if (fc.isCompilableFile() && !fc.isMainFile())
						mainWindow.getMenu().getProject().getUnsetCompilable()
								.setEnabled(true);
				} else {
					mainWindow.getMenu().getProject().getRemoveFolder().setEnabled(true);
				}
			}

			// No project
			String prj = null;
			try {
				prj = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			if ((prj.equals("./configuration/default.acidePrj") && mainWindow
					.getProjectConfiguration().getName().equals(""))) {
				int editor = mainWindow.getEditorBuilder().getNumEditors();
				if (editor > 0) {
					if (!mainWindow.getEditorBuilder().getSelectedEditor()
							.isMainFile())
						mainWindow.getMenu().getProject().getSetMain().setEnabled(true);
					if (mainWindow.getEditorBuilder().getSelectedEditor()
							.isMainFile())
						mainWindow.getMenu().getProject().getUnsetMain().setEnabled(true);
					if (!mainWindow.getEditorBuilder().getSelectedEditor()
							.isCompilerFile()
							|| (mainWindow.getEditorBuilder()
									.getSelectedEditor().isCompilerFile() && mainWindow
									.getEditorBuilder().getSelectedEditor()
									.isMainFile()))
						mainWindow.getMenu().getProject().getSetCompilable()
								.setEnabled(true);
					if (mainWindow.getEditorBuilder().getSelectedEditor()
							.isCompilerFile()
							&& !mainWindow.getEditorBuilder()
									.getSelectedEditor().isMainFile())
						mainWindow.getMenu().getProject().getUnsetCompilable()
								.setEnabled(true);
				}
			}

		}
		// File Menu
		if (mainWindow.getMenu().getFile().isSelected()) {
			mainWindow.getMenu().getFile().getSaveFile().setEnabled(false);
			mainWindow.getMenu().getFile().getSaveAllFiles().setEnabled(false);

			int editor = mainWindow.getEditorBuilder().getNumEditors();
			if (editor > 0) {

				if (mainWindow.getEditorBuilder().isRedButton() == true) {
					mainWindow.getMenu().getFile().getSaveFile().setEnabled(true);
				}
			}
			
			int eS = mainWindow.getEditorBuilder().getSelectedEditorIndex();
			mainWindow.getEditorBuilder().setSelectedEditorAt(editor - 1);
			for (int i = editor - 1; i >= 0; i--) {
				mainWindow.getEditorBuilder().setSelectedEditorAt(i);
				if (mainWindow.getEditorBuilder().isRedButton() == true) {
					mainWindow.getMenu().getFile().getSaveAllFiles().setEnabled(true);
				}
			}
			mainWindow.getEditorBuilder().setSelectedEditorAt(eS);
		}

		// Edit Menu
		if (mainWindow.getMenu().getEdit().isSelected()) {
			mainWindow.getMenu().getEdit().getUndo().setEnabled(false);
			mainWindow.getMenu().getEdit().getRepeat().setEnabled(false);
			mainWindow.getMenu().getEdit().getCopy().setEnabled(false);
			mainWindow.getMenu().getEdit().getPaste().setEnabled(false);
			mainWindow.getMenu().getEdit().getCut().setEnabled(false);

			if (mainWindow.getMenu().getEdit().getUndoManager().canUndo()) {
				mainWindow.getMenu().getEdit().getUndo().setEnabled(true);
			}
			if (mainWindow.getMenu().getEdit().getUndoManager().canRedo()) {
				mainWindow.getMenu().getEdit().getRepeat().setEnabled(true);
			}
			if (Toolkit.getDefaultToolkit().getSystemClipboard()
					.getContents(null) != null) {
				if (!mainWindow.getOutput().getTextComponent().hasFocus())
					mainWindow.getMenu().getEdit().getPaste().setEnabled(true);
				else if (mainWindow.getOutput().getTextComponent().getSelectionStart() >= mainWindow
						.getOutput().getMaxCaretPosition())
					mainWindow.getMenu().getEdit().getPaste().setEnabled(true);
			}
			int editor = mainWindow.getEditorBuilder().getNumEditors();
			if (editor > 0) {
				if (mainWindow.getOutput().getTextComponent().hasFocus()
						&& mainWindow.getOutput().getTextComponent().getSelectedText() != null) {
					mainWindow.getMenu().getEdit().getCopy().setEnabled(true);
					if (mainWindow.getOutput().getTextComponent().getSelectionStart() >= mainWindow
							.getOutput().getMaxCaretPosition())
						mainWindow.getMenu().getEdit().getCut().setEnabled(true);
				} else if (mainWindow.getEditorBuilder().getSelectedEditor()
						.getEditor().hasFocus()
						&& mainWindow.getEditorBuilder().getSelectedEditor()
								.getEditor().getSelectedText() != null) {
					mainWindow.getMenu().getEdit().getCopy().setEnabled(true);
					mainWindow.getMenu().getEdit().getCut().setEnabled(true);
				}
			} else {
				// We can copy from the output
				if (mainWindow.getOutput().getTextComponent().getSelectedText() != null) {
					mainWindow.getMenu().getEdit().getCopy().setEnabled(true);
					if (mainWindow.getOutput().getTextComponent().getSelectionStart() >= mainWindow
							.getOutput().getMaxCaretPosition())
						mainWindow.getMenu().getEdit().getCut().setEnabled(true);
				}
			}
		}
		if (mainWindow.getOutput().getTextComponent().isFocusOwner()) {
			mainWindow.getMenu().setIsShellFocus(true);
		} else
			mainWindow.getMenu().setIsShellFocus(false);
	}
}
