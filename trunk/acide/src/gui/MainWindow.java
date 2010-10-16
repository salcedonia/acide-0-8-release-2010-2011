package gui;

import es.text.TextFile;
import gui.editor.EditorBuilder;
import gui.editor.Editor;
import gui.explorer.Explorer;
import gui.menu.Menu;
import gui.menu.project.ProjectGUI;
import gui.output.Output;
import gui.splashScreen.SplashScreen;
import gui.statusBar.StatusBar;
import gui.toolBarButton.ToolBarCommand;

import java.awt.BorderLayout;
import java.awt.HeadlessException;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import language.Language;

import org.apache.log4j.Logger;

import operations.configuration.ExplorerFile;
import operations.configuration.EditableToolBarCommandList;
import operations.configuration.ProjectConfiguration;
import operations.factory.IOFactory;
import operations.factory.GUIFactory;
import operations.factory.OperationsFactory;
import operations.log.Log;
import properties.PropertiesManager;

/**
 * 
 */
public class MainWindow extends JFrame {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private static MainWindow _instance;
	/**
	 * 
	 */
	private EditorBuilder _editorBuilder;
	/**
	 * 
	 */
	private Menu _menu;
	/**
	 * 
	 */
	private Output _output;
	/**
	 * 
	 */
	private StatusBar _statusBar;
	/**
	 * 
	 */
	private Explorer _explorer;
	/**
	 * 
	 */
	private JToolBar _toolBar;
	/**
	 * 
	 */
	private ProjectConfiguration _projectConfiguration;
	/**
	 * 
	 */
	private ProjectGUI _projectGUI;
	/**
	 *
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private JSplitPane _splitPaneVertical;
	/**
	 * 
	 */
	private JSplitPane _splitPaneHorizontal;

	/**
	 * 
	 * @return
	 */
	public static MainWindow getInstance() {
		if (_instance == null)
			_instance = new MainWindow();
		return _instance;
	}

	/**
	 * Constructor of the class.
	 */
	public MainWindow() {

		Language language = Language.getInstance();

		SplashScreen.setProgressBar(20);

		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}

		SplashScreen.setProgressBar(22);
		setIconImage(new ImageIcon(ICON).getImage());
		SplashScreen.setProgressBar(25);
		final ResourceBundle labels = language.getLabels();
		SplashScreen.setProgressBar(27);
		_logger.info(labels.getString("s67"));
		SplashScreen.setProgressBar(32);
		JPanel contentPane;
		SplashScreen.setProgressBar(35);
		contentPane = (JPanel) getContentPane();
		SplashScreen.setProgressBar(40);
		setTitle(labels.getString("s425"));
		SplashScreen.setProgressBar(42);
		GUIFactory _guiFactory = GUIFactory.getInstance();
		SplashScreen.setProgressBar(43);
		_menu = _guiFactory.buildMenu();
		SplashScreen.setProgressBar(45);
		_explorer = _guiFactory.generaExplorer();
		SplashScreen.setProgressBar(47);
		_editorBuilder = _guiFactory.buildEditorBuilder();
		SplashScreen.setProgressBar(50);
		_output = _guiFactory.buildOutput();
		SplashScreen.setProgressBar(52);
		_statusBar = _guiFactory.buildStatusBar();
		SplashScreen.setProgressBar(55);
		contentPane.add(_statusBar.getStatusBar(), BorderLayout.SOUTH);
		SplashScreen.setProgressBar(57);
		buildToolBar();
		SplashScreen.setProgressBar(60);
		OperationsFactory operationsFactory = OperationsFactory.getInstance();
		SplashScreen.setProgressBar(62);
		_projectConfiguration = operationsFactory.buildProjectConfiguration();
		SplashScreen.setProgressBar(65);
		setJMenuBar(_menu.getMenuBar());
		SplashScreen.setProgressBar(67);
		contentPane.add(_toolBar, BorderLayout.NORTH);
		SplashScreen.setProgressBar(70);
		_splitPaneVertical = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
				_explorer, _editorBuilder.getTabbedPane());
		SplashScreen.setProgressBar(72);
		_splitPaneVertical.setResizeWeight(0.05);
		SplashScreen.setProgressBar(75);
		_splitPaneVertical.setContinuousLayout(true);
		SplashScreen.setProgressBar(77);
		_splitPaneHorizontal = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
				_splitPaneVertical, _output);
		SplashScreen.setProgressBar(80);
		_splitPaneHorizontal.setResizeWeight(0.9);
		SplashScreen.setProgressBar(82);
		_splitPaneHorizontal.setContinuousLayout(true);
		SplashScreen.setProgressBar(85);
		contentPane.add(_splitPaneHorizontal, BorderLayout.CENTER);
		SplashScreen.setProgressBar(87);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		SplashScreen.setProgressBar(90);
		setLocationRelativeTo(null);
		SplashScreen.setProgressBar(95);
		addWindowListener(new MainWindowListener());
		_logger.info(labels.getString("s66"));
		
		SplashScreen.closeStartingWindow();
	}

	/**
	 * 
	 * @return
	 */
	public Editor getEditor() {
		return (Editor) _editorBuilder.getEditorAt(0);
	}

	/**
	 * 
	 * @return
	 */
	public Output getOutput() {
		return _output;
	}
	
	/**
	 * 
	 */
	public void disableMainWindow() {
		setEnabled(false);
	}

	/**
	 * 
	 * @return
	 */
	public Menu getMenu() {
		return _menu;
	}

	/**
	 * 
	 * @return
	 */
	public EditorBuilder getEditorBuilder() {
		return _editorBuilder;
	}

	/**
	 * 
	 * @return
	 */
	public ProjectConfiguration getProjectConfiguration() {
		return _projectConfiguration;
	}

	/**
	 * 
	 */
	public void setListenersMenu() {
		_menu.setListeners();
	}

	/**
	 * 
	 */
	public void buildToolBar() {
		
		Language language = Language.getInstance();

		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		_toolBar = ToolBarCommand.buildToolBar();

		String currentToolBarConfiguration = null;
		try {
			
			EditableToolBarCommandList.clear();
			currentToolBarConfiguration = PropertiesManager.getProperty("currentToolBarConfiguration");
			EditableToolBarCommandList.loadList(currentToolBarConfiguration);
			PropertiesManager.setProperty("currentToolBarConfiguration", currentToolBarConfiguration);
		} 
		catch (Exception e) {
			
			String currentToolBarConfiguration2;
			int index = currentToolBarConfiguration.lastIndexOf("\\");
			if (index == -1)
				index = currentToolBarConfiguration.lastIndexOf("/");
			currentToolBarConfiguration2 = "./configuration/toolbar/"
					+ currentToolBarConfiguration.substring(index + 1, currentToolBarConfiguration.length());
			try {
				EditableToolBarCommandList.loadList(currentToolBarConfiguration2);
				JOptionPane.showMessageDialog(null, labels.getString("s958")
						+ currentToolBarConfiguration + labels.getString("s957")
						+ currentToolBarConfiguration2);
				PropertiesManager.setProperty("currentToolBarConfiguration", currentToolBarConfiguration2);
			} catch (Exception e1) {
				_logger.error(labels.getString("s127"));
				try {
					EditableToolBarCommandList
							.loadList("./configuration/toolbar/default.BHcfg");
					JOptionPane.showMessageDialog(
							null,
							labels.getString("s958") + currentToolBarConfiguration
									+ labels.getString("s959"));
					PropertiesManager.setProperty("currentToolBarConfiguration",
							"./configuration/toolbar/default.BHcfg");
				} catch (HeadlessException e2) {
					e2.printStackTrace();
				} catch (Exception e2) {
					e2.printStackTrace();
				}
			}
			_logger.error(labels.getString("s127"));
		}
		_toolBar = ToolBarCommand.buildEditableToolBar();
		ToolBarCommand.buildToolBar();
	}

	/**
	 * 
	 */
	public void closeDefaultProject() {

		try {
			String file = PropertiesManager.getProperty("defaultAcideProject");

			if (file.equals("./configuration/default.acidePrj")) {
				IOFactory fact = new IOFactory();
				TextFile f = fact.buildFile();
				getProjectConfiguration().removeFiles();

				getProjectConfiguration().setNumFiles(
						Integer.toString(getEditorBuilder().getNumEditors()));

				for (int i = 0; i < getEditorBuilder().getNumEditors(); i++) {
					ExplorerFile fic = new ExplorerFile();
					fic.setPath(getEditorBuilder().getEditorAt(i).getPath());
					fic.setSetFile(getEditorBuilder().getEditorAt(i)
							.isCompilerFile());
					fic.setMainFile(getEditorBuilder().getEditorAt(i)
							.isMainFile());
					fic.setName(getEditorBuilder().getEditorAt(i).getName());
					fic.setDirectory(false);
					getProjectConfiguration().setFile(fic);
					// MainWindow v = MainWindow.getInstance();
					/*
					 * for(int j=0; j<v.getProyecto().dameNumFich(); j++){
					 * if(v.getProyecto
					 * ().getfich(j).getPath().equals(getCreadorEditor
					 * ().dameEditorI(i).getPath()))
					 * fic.setSetFile(v.getProyecto().getfich(j).isSetFile());
					 * fic.setMainFile(v.getProyecto().getfich(j).isMainFile());
					 * }
					 */

				}
				getProjectConfiguration().setLanguage(
						PropertiesManager.getProperty("language"));
				getProjectConfiguration().setName("");
				String cad = getProjectConfiguration().save();
				f.save("./configuration/default.acidePrj", cad);
				PropertiesManager.setProperty("defaultAcideProject",
						"./configuration/default.acidePrj");
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * 
	 * @return
	 */
	public Explorer getExplorer() {
		return _explorer;
	}

	/**
	 * 
	 * @param explorer
	 */
	public void setExplorer(Explorer explorer) {
		_explorer = explorer;
	}

	/**
	 * 
	 * @return
	 */
	public ProjectGUI getProjectGUI() {
		return _projectGUI;
	}

	/**
	 * 
	 * @param projectGUI
	 */
	public void setProjectGUI(ProjectGUI projectGUI) {
		_projectGUI = projectGUI;
	}

	/**
	 * 
	 * @return
	 */
	public StatusBar getStatusBar() {
		return _statusBar;
	}

	/**
	 * 
	 * @return
	 */
	public JSplitPane getSplitPaneHorizontal() {
		return _splitPaneHorizontal;
	}

	/**
	 * 
	 * @param splitPaneH
	 */
	public void setSplitPaneHorizontal(JSplitPane splitPaneH) {
		_splitPaneHorizontal = splitPaneH;
	}

	/**
	 * 
	 * @param size
	 */
	public void setExplorerSize(int size) {
		_explorer.setExplorerSize(size);
	}

	/**
	 * 
	 * @return
	 */
	public JSplitPane getSplitPaneVertical() {
		return _splitPaneVertical;
	}

	/**
	 * 
	 * @param splitPaneV
	 */
	public void setSplitPaneVertical(JSplitPane splitPaneV) {
		_splitPaneVertical = splitPaneV;
	}

	/**
	 * 
	 */
	public class MainWindowListener extends WindowAdapter {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent)
		 */
		public void windowClosing(WindowEvent arg0) {

			Language language = Language.getInstance();
			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception e) {
				e.printStackTrace();
			}
			final ResourceBundle labels = language.getLabels();

			MainWindow mainWindow = MainWindow.getInstance();
			boolean c = false;

			if (mainWindow.getProjectConfiguration().isModified()) {

				int res = JOptionPane.showConfirmDialog(null,
						labels.getString("s657"), labels.getString("s953"),
						JOptionPane.YES_NO_OPTION);

				if (res == JOptionPane.OK_OPTION) {
					mainWindow.getMenu().getProject().getSaveProject().setEnabled(true);
					mainWindow.getMenu().getProject().getSaveProject().doClick();
				}

			}
			int eS = mainWindow.getEditorBuilder().getSelectedEditorIndex();
			int editor = mainWindow.getEditorBuilder().getNumEditors();
			mainWindow.getEditorBuilder().setSelectedEditorAt(editor - 1);
			for (int z = editor - 1; z >= 0; z--) {
				mainWindow.getEditorBuilder().setSelectedEditorAt(z);
				if (mainWindow.getEditorBuilder().isRedButton() == true) {
					int opt = JOptionPane.showConfirmDialog(null,
							labels.getString("s643"), labels.getString("s953"),
							JOptionPane.YES_NO_OPTION);

					if (opt == JOptionPane.OK_OPTION) {
						mainWindow.getMenu().getFile().saveOrSaveAS();
					}
				}
			}
			mainWindow.getEditorBuilder().setSelectedEditorAt(eS);
			if (!c) {

				getOutput().executeExitCommand();
				try {
					String currentMenu = PropertiesManager
							.getProperty("currentMenuConfiguration");
					if ((currentMenu.endsWith("lastModified.menuCfg"))
							|| (currentMenu.endsWith("newMenu.menuCfg"))) {
						String previous = PropertiesManager
								.getProperty("previousMenuConfiguration");
						PropertiesManager.setProperty("currentMenuConfiguration",
								previous);
					}
					String currentTB = PropertiesManager
							.getProperty("currentToolBarConfiguration");
					if ((currentTB.endsWith("lastModified.BHcfg"))
							|| currentTB.endsWith("newToolBar.BHcfg")) {
						String previous = PropertiesManager
								.getProperty("previousToolBarConfiguration");
						PropertiesManager.setProperty("currentToolBarConfiguration", previous);
					}
					String currentGrammar = PropertiesManager
							.getProperty("currentGrammar");
					if ((currentGrammar.endsWith("lastModified.jar"))
							|| (currentGrammar.endsWith("newGrammar.jar"))) {
						String previous = PropertiesManager
								.getProperty("previousGrammar");
						PropertiesManager.setProperty("currentGrammar",
								previous);
					}
				} catch (Exception e) {
					JOptionPane
							.showMessageDialog(null, e.getMessage(),
									labels.getString("s294"),
									JOptionPane.ERROR_MESSAGE);
				}

				closeDefaultProject();
				mainWindow.getProjectConfiguration().save2();
			}
		}
	}
}
