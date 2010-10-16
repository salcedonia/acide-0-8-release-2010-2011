package gui.menu.file;

import es.text.TextFile;
import gui.MainWindow;
import gui.editor.Editor;
import gui.editor.EditorBuilder;
import gui.menu.print.PrintGUI;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.undo.UndoableEdit;

import org.apache.log4j.Logger;

import language.Language;

import operations.configuration.DefaultConfiguration;
import operations.configuration.ExplorerFile;
import operations.configuration.MenuConfiguration;
import operations.factory.IOFactory;
import operations.factory.OperationsFactory;
import operations.log.Log;
import properties.PropertiesManager;

/**
 * 
 */
public class FileMenu extends JMenu {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private final static String NEW_FILE = "./resources/icons/menu/file/newFile.png";
	/**
	 * 
	 */
	private final static String OPEN_FILE = "./resources/icons/menu/file/openFile.png";
	/**
	 * 
	 */
	private final static String CLOSE_FILE = "./resources/icons/menu/file/closeFile.png";
	/**
	 * 
	 */
	private final static String CLOSE_ALL_FILES = "./resources/icons/menu/file/closeAllFiles.png";
	/**
	 * 
	 */
	private final static String SAVE_FILE = "./resources/icons/menu/file/saveFile.png";
	/**
	 * 
	 */
	private final static String SAVE_ALL_FILES = "./resources/icons/menu/file/saveAllFiles.png";
	/**
	 * 
	 */
	private final static String SAVE_FILE_AS = "./resources/icons/menu/file/saveFileAs.png";
	/**
	 * 
	 */
	private final static String PRINT_FILE = "./resources/icons/menu/file/printFile.png";
	/**
	 * 
	 */
	private final static String EXIT = "./resources/icons/menu/file/exit.png";
	
	/**
	 * 
	 */
	private JMenuItem _newFile;
	/**
	 * 
	 */
	private JMenuItem _openFile;
	/**
	 * 
	 */
	private JMenuItem _closeFile;
	/**
	 * 
	 */
	private JMenuItem _closeAllFiles;
	/**
	 * 
	 */
	private JMenuItem _saveFileAs;
	/**
	 * 
	 */
	private JMenuItem _saveFile;
	/**
	 * 
	 */
	private JMenuItem _saveAllFiles;
	/**
	 * 
	 */
	private JMenuItem _printFile;
	/**
	 * 
	 */
	private JMenuItem _exit;
	/**
	 * 
	 */
	private JMenuItem _setCompilable;
	/**
	 * 
	 */
	private JMenuItem _unsetCompilable;
	/**
	 * 
	 */
	private JMenuItem _setMain;
	/**
	 * 
	 */
	private JMenuItem _unsetMain;
	/**
	 * 
	 */
	private JMenuItem _removeFile;
	/**
	 * 
	 */
	private JMenuItem _deleteFile;
	/**
	 * 
	 */
	private JMenuItem _addFile;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	
	/**
	 * Constructor of the class.
	 */
	public FileMenu(){
				
		// MENU ITEM
		_newFile = new JMenuItem(new ImageIcon(NEW_FILE));
		_openFile = new JMenuItem(new ImageIcon(OPEN_FILE));	
		_closeFile = new JMenuItem(new ImageIcon(CLOSE_FILE));
		_closeAllFiles = new JMenuItem(new ImageIcon(CLOSE_ALL_FILES));		
		_saveFileAs = new JMenuItem(new ImageIcon(SAVE_FILE_AS));
		_saveAllFiles = new JMenuItem(new ImageIcon(SAVE_ALL_FILES));			
		_saveFile = new JMenuItem(new ImageIcon(SAVE_FILE));
		_printFile = new JMenuItem(new ImageIcon(PRINT_FILE));
		_exit = new JMenuItem(new ImageIcon(EXIT));
		
		_setCompilable = new JMenuItem();
		_unsetCompilable = new JMenuItem();
		_setMain = new JMenuItem();
		_unsetMain = new JMenuItem();		

		_removeFile = new JMenuItem();
		_deleteFile = new JMenuItem();
		_addFile = new JMenuItem();
			
		setLanguageLabels();
	}

	/**
	 * 
	 */
	public void setLanguageLabels(){
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}

		final ResourceBundle labels = language.getLabels();
		
		// NEW FILE
		_newFile.setText(labels.getString("s8"));
		_newFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.CTRL_MASK));
		
		// OPEN FILE
		_openFile.setText(labels.getString("s9"));
		if (language.getCurrentLocale().equals(new Locale("en", "EN")))
			_openFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
					ActionEvent.CTRL_MASK));
		else
			_openFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,
					ActionEvent.CTRL_MASK));

		// CLOSE FILE
		_closeFile.setText(labels.getString("s238"));
		
		// CLOSE ALL FILES
		_closeAllFiles.setText(labels.getString("s239"));

		// SAVE FILE
		_saveFileAs.setText(labels.getString("s10"));

		// SAVE AS
		_saveFile.setText(labels.getString("s617"));
		if (language.getCurrentLocale().equals(new Locale("en", "EN")))
			_saveFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
					ActionEvent.CTRL_MASK));
		else
			_saveFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
					ActionEvent.CTRL_MASK));

		// SAVE ALL FILES
		_saveAllFiles.setText(labels.getString("s217"));
		if (language.getCurrentLocale().equals(new Locale("en", "EN")))
			_saveAllFiles.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
					ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		else
			_saveAllFiles.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
					ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		// PRINT FILE
		_printFile.setText(labels.getString("s624"));
		_printFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
				ActionEvent.CTRL_MASK));

		// EXIT
		_exit.setText(labels.getString("s13"));
		_exit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
				ActionEvent.ALT_MASK));

		// SET COMPILABLE
		_setCompilable.setText(labels.getString("s254"));
		
		// UNSET COMPILABLE
		_unsetCompilable.setText(labels.getString("s255"));
		
		// SET MAIN
		_setMain.setText(labels.getString("s256"));
		
		// UNSET MAIN
		_unsetMain.setText(labels.getString("s952"));
		
		_closeFile.setEnabled(false);
		_closeAllFiles.setEnabled(false);
		_saveFileAs.setEnabled(false);
		_saveFile.setEnabled(false);
		_saveAllFiles.setEnabled(false);
		_printFile.setEnabled(false);
		_setMain.setEnabled(false);
		_unsetMain.setEnabled(false);
		_setCompilable.setEnabled(false);
		_unsetCompilable.setEnabled(false);
	}
	
	/**
	 * @param labels
	 * @param language 
	 * 
	 */
	public void buildMenu(ResourceBundle labels, Language language){
				
		removeAll();

		if (MenuConfiguration.getFile())
			add(_newFile);
		if (MenuConfiguration.getOpenFile())
			add(_openFile);
		if (MenuConfiguration.getCloseFile())
			add(_closeFile);
		if (MenuConfiguration.getCloseAll())
			add(_closeAllFiles);
		if ((MenuConfiguration.getFile()
				|| MenuConfiguration.getOpenFile()
				|| MenuConfiguration.getCloseFile() || MenuConfiguration
				.getCloseAll())
				&& (MenuConfiguration.getSaveFile()
						|| MenuConfiguration.getSaveFileAs() || MenuConfiguration
						.getSaveAllFiles()))
			addSeparator();
		if (MenuConfiguration.getSaveFile())
			add(_saveFile);
		if (MenuConfiguration.getSaveFileAs())
			add(_saveFileAs);
		if (MenuConfiguration.getSaveAllFiles())
			add(_saveAllFiles);
		if ((MenuConfiguration.getFile()
				|| MenuConfiguration.getOpenFile()
				|| MenuConfiguration.getCloseFile()
				|| MenuConfiguration.getCloseAll()
				|| MenuConfiguration.getSaveFile()
				|| MenuConfiguration.getSaveFileAs() || MenuConfiguration
				.getSaveAllFiles()) && (MenuConfiguration.getPrintFile()))
			addSeparator();

		if (MenuConfiguration.getSetFileFile())
			// archivo.add(setCompilable2);
			if (MenuConfiguration.getUnsetFileFile())
				// archivo.add(unsetCompilable2);
				if (MenuConfiguration.getSetMainFile())
					// archivo.add(setMain2);
					if (MenuConfiguration.getUnsetMainFile())
						// archivo.add(unsetMain2);
						// archivo.addSeparator();
						if (MenuConfiguration.getPrintFile())
							add(_printFile);
		
		if ((MenuConfiguration.getFile()
				|| MenuConfiguration.getOpenFile()
				|| MenuConfiguration.getSaveFileAs()
				|| MenuConfiguration.getSaveFile()
				|| MenuConfiguration.getSaveAllFiles()
				|| MenuConfiguration.getPrintFile()
				|| MenuConfiguration.getCloseFile() || MenuConfiguration
				.getCloseAll()) && MenuConfiguration.getExit())
			addSeparator();
		
		if (MenuConfiguration.getExit())
			add(_exit);
	}
	
	/**
	 * 
	 */
	public void setListeners(){
		
		// NEW FILE
		_newFile.addActionListener(new NewFileListener());
		
		// OPEN FILE
		_openFile.addActionListener(new OpenFileListener());
		
		// SAVE FILE AS
		_saveFileAs.addActionListener(new SaveFileAsListener());
		
		// SAVE FILE
		_saveFile.addActionListener(new SaveFileListener());
		
		// PRINT FILE
		_printFile.addActionListener(new PrintFileListener());
		
		// EXIT
		_exit.addActionListener(new ExitListener());	
		
		// REMOVE FILE
		_removeFile.addActionListener(new RemoveFileListener());
		
		// DELETE FILE
		_deleteFile.addActionListener(new DeleteFileListener());
		
		// ADD FILE
		_addFile.addActionListener(new AddFileListener());
		
		// CLOSE FILE
		_closeFile.addActionListener(new CloseFileListener());
		
		// CLOSE ALL FILES
		_closeAllFiles.addActionListener(new CloseAllFilesListener());
		
		// SAVE ALL FILES
		_saveAllFiles.addActionListener(new SaveAllFilesListener());
		
		// SET COMPILABLE
		_setCompilable.addActionListener(new SetCompilableFileListener());
		
		// UNSET COMPILABLE
		_unsetCompilable.addActionListener(new UnsetCompilableFileListener());
		
		// SET MAIN
		_setMain.addActionListener(new SetMainFileListener());
	
		// UNSET MAIN	
		_unsetMain.addActionListener(new UnsetMainFileListener());
	}
	
	/**
	 * 
	 * @return
	 */
	public int saveOrSaveAS() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		
		MainWindow mainWindow = MainWindow.getInstance();
		int editor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
		if (mainWindow.getEditorBuilder().isRedButton() == true) {
			if (mainWindow.getEditorBuilder().getSelectedEditor().getPath()
					.equals(labels.getString("s79")) == true) {

				IOFactory ioFactory = IOFactory.getInstance();
				TextFile textFile = ioFactory.buildFile();
				String f = " ";
				f = textFile.write();
				if (f.equals(" ")) {
					_logger.info(labels.getString("s92"));
				} else {
					
					boolean resultado = textFile.save(f, mainWindow.getEditorBuilder()
							.getSelectedEditor().getText());
					if (resultado) {
						_logger.info(labels.getString("s93") + f
								+ labels.getString("s94"));
						MainWindow.getInstance().getEditorBuilder()
								.greenButton();
						mainWindow.getEditorBuilder().getEditorAt(editor)
								.setPath(f);
						mainWindow.getEditorBuilder().getEditorAt(editor)
								.setToolTipText(f);
						int index = f.lastIndexOf("\\");
						index++;
						String file = f.substring(index, f.length());
						mainWindow.getEditorBuilder().getEditorAt(editor).setName(file);
						File fich = new File(mainWindow.getEditorBuilder()
								.getSelectedEditor().getPath());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor()
								.setLastChange(fich.lastModified());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor()
								.setLastSize(fich.length());

					} else {
						_logger.info(labels.getString("s95") + f);
					}
				}
			} else {
				_saveFile.setEnabled(true);
				_saveFile.doClick();
			}
		}

		return editor;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getNewFile() {
		return _newFile;
	}

	/**
	 * 
	 * @param newFile
	 */
	public void setNewFile(JMenuItem newFile) {
		_newFile = newFile;
	}
	
	/**
	 * 
	 * @param deleteFile
	 */
	public void setDeleteFile2(JMenuItem deleteFile) {
		this._deleteFile = deleteFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getDeleteFile2() {
		return _deleteFile;
	}

	/**
	 * 
	 * @param removeFile
	 */
	public void setRemoveFile2(JMenuItem removeFile) {
		this._removeFile = removeFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getRemoveFile2() {
		return _removeFile;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getAnadirFichero2() {
		return _addFile;
	}
	
	/**
	 * 
	 * @return
	 */
	public JMenuItem getExit() {
		return _exit;
	}

	/**
	 * 
	 * @param exit
	 */
	public void setExit(JMenuItem exit) {
		_exit = exit;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveFileAs() {
		return _saveFileAs;
	}

	/**
	 * 
	 * @param saveFileAs
	 */
	public void setSaveFileAs(JMenuItem saveFileAs) {
		_saveFileAs = saveFileAs;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveFile() {
		return _saveFile;
	}

	/**
	 * 
	 * @param saveFile
	 */
	public void setSaveFile(JMenuItem saveFile) {
		_saveFile = saveFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSaveAllFiles() {
		return _saveAllFiles;
	}

	/**
	 * 
	 * @param saveAllFiles
	 */
	public void setSaveAllFiles(JMenuItem saveAllFiles) {
		_saveAllFiles = saveAllFiles;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getPrintFile() {
		
		return _printFile;
	}
	
	/**
	 * 
	 * @param printFile
	 */
	public void setPrintFile(JMenuItem printFile) {
		_printFile = printFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getCloseAllFiles() {
		return _closeAllFiles;
	}

	/**
	 * 
	 * @param closeAllFiles
	 */
	public void setCloseAllFiles(JMenuItem closeAllFiles) {
		_closeAllFiles = closeAllFiles;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getCloseFile() {
		return _closeFile;
	}

	/**
	 * 
	 * @param closeFile
	 */
	public void setCloseFile(JMenuItem closeFile) {
		_closeFile = closeFile;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getOpenFile() {
		return _openFile;
	}

	/**
	 * 
	 * @param openFile
	 */
	public void setOpenFile(JMenuItem openFile) {
		_openFile = openFile;
	}
	/**
	 * 
	 * @return
	 */
	public JMenuItem getSetCompilable() {
		return _setCompilable;
	}

	/**
	 * 
	 * @param setCompilable
	 */
	public void setSetCompilable(JMenuItem setCompilable) {
		_setCompilable = setCompilable;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getUnsetCompilable() {
		return _unsetCompilable;
	}

	/**
	 * 
	 * @param unsetCompilable
	 */
	public void setUnsetCompilable(JMenuItem unsetCompilable) {
		_unsetCompilable = unsetCompilable;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getSetMain() {
		return _setMain;
	}

	/**
	 * 
	 * @param setMain
	 */
	public void setSetMain(JMenuItem setMain) {
		_setMain = setMain;
	}

	/**
	 * 
	 * @return
	 */
	public JMenuItem getUnsetMain() {
		return _unsetMain;
	}

	/**
	 * 
	 * @param unsetMain
	 */
	public void setUnsetMain(JMenuItem unsetMain) {
		_unsetMain = unsetMain;
	}
	
	/**
	 * 
	 */
	public void enableMenu() {
		
		_closeFile.setEnabled(true);
		_closeAllFiles.setEnabled(true);
		_saveFileAs.setEnabled(true);
		_saveFile.setEnabled(false);
		_saveAllFiles.setEnabled(false);
		_setMain.setEnabled(false);
		_unsetMain.setEnabled(false);
		_setCompilable.setEnabled(false);
		_unsetCompilable.setEnabled(false);
		_printFile.setEnabled(true);
	}
	
	/**
	 * 
	 */
	public void disableMenu() {
		_closeFile.setEnabled(false);
		_closeAllFiles.setEnabled(false);
		_saveFileAs.setEnabled(false);
		_saveFile.setEnabled(false);
		_saveAllFiles.setEnabled(false);
		_setMain.setEnabled(false);
		_unsetMain.setEnabled(false);
		_setCompilable.setEnabled(false);
		_unsetCompilable.setEnabled(false);
		_printFile.setEnabled(false);		
	}
	
	/**
	 * 
	 */
	public void enableSaveFileAs() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		_saveFileAs.setEnabled(true);
		_logger.info(labels.getString("s75"));
	}
	
	/**
	 * 
	 */
	class ExitListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			Language language = Language.getInstance();
			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			ResourceBundle labels = language.getLabels();
			
			Logger logger = Log.getLog();

			int opcion = JOptionPane.showConfirmDialog(null,
					labels.getString("s76"), labels.getString("s953"),
					JOptionPane.YES_NO_OPTION);

			if (opcion == JOptionPane.OK_OPTION) {
				logger.info(labels.getString("s77"));

				MainWindow mainWindow = MainWindow.getInstance();

				if (mainWindow.getProjectConfiguration().isModified()) {

					int res = JOptionPane.showConfirmDialog(null,
							labels.getString("s657"), labels.getString("s953"),
							JOptionPane.YES_NO_OPTION);

					if (res == JOptionPane.OK_OPTION) {
						mainWindow.getMenu().getProject().getSaveProject().setEnabled(true);
						mainWindow.getMenu().getProject().getSaveProject().doClick();
					}
				}
				int editorBuilder = mainWindow.getEditorBuilder().getSelectedEditorIndex();
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
				
				mainWindow.getEditorBuilder().setSelectedEditorAt(editorBuilder);
				MainWindow.getInstance().getOutput().executeExitCommand();
				
				try {
					String currentMenu = PropertiesManager
							.getProperty("currentMenuConfiguration");
					if ((currentMenu.endsWith("lastModified.menuCfg"))
							|| (currentMenu.endsWith("newMenu.menuCfg"))) {
						String previous = PropertiesManager
								.getProperty("previousMenuCfg");
						PropertiesManager.setProperty("currentMenuConfiguration",
								previous);
					}
					String currentTB = PropertiesManager
							.getProperty("currentToolBarConfiguration");
					if ((currentTB.endsWith("lastModified.BHcfg"))
							|| currentTB.endsWith("newToolBar.BHcfg")) {
						String previous = PropertiesManager
								.getProperty("previousToolBarConfiguration");
						PropertiesManager.setProperty(
								"currentToolBarConfiguration", previous);
					}
					String currentGrammar = PropertiesManager
							.getProperty("currentGrammar");
					if ((currentGrammar.endsWith("lastModified.jar"))
							|| (currentGrammar.endsWith("newGrammar.jar"))) {
						String previous = PropertiesManager
								.getProperty("previousGrammar");
						PropertiesManager.setProperty("currentGrammar", previous);
					}
				} catch (Exception e1) {
					JOptionPane.showMessageDialog(null, e1.getMessage(),
							labels.getString("s294"), JOptionPane.ERROR_MESSAGE);
				}
				// SAVE DEFAULT CONFIGURATION
				OperationsFactory facto = OperationsFactory.getInstance();
				DefaultConfiguration dc = facto.buildDefaultConfiguration();
				dc.save();
				// /SAVE DEFAULT PROJECT
				MainWindow.getInstance().closeDefaultProject();
				mainWindow.getProjectConfiguration().save2();

				System.exit(0);
			} else if ((opcion == JOptionPane.NO_OPTION)
					|| (opcion == JOptionPane.CANCEL_OPTION)) {
				logger.info(labels.getString("s78"));
			}
		}
	}

	/**
	 * Listener of the NewFile Menu.
	 */
	class NewFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			Logger logger = Log.getLog();
			MainWindow mainWindow = MainWindow.getInstance();
			Language language = Language.getInstance();

			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			ResourceBundle labels = language.getLabels();

			mainWindow.getMenu().enableFileMenu();
			mainWindow.getMenu().enableEditMenu();
			EditorBuilder ce = mainWindow.getEditorBuilder();
			ce.newTab(labels.getString("s79"), labels.getString("s79"), "", true, 0);
			logger.info(labels.getString("s80"));

			// UNDO REDO
			mainWindow.getMenu().enableFileMenu();
			mainWindow.getMenu().enableEditMenu();
			DefaultStyledDocument doc = mainWindow.getEditorBuilder()
					.getSelectedEditor().getDoc();
			// status
			mainWindow.getStatusBar().setMessage(labels.getString("s79"));
			doc.addUndoableEditListener(new UndoableEditListener() {

				public void undoableEditHappened(UndoableEditEvent evt) {

					MainWindow mainWindow = MainWindow.getInstance();
					UndoableEdit edit = evt.getEdit();

					if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
							.getType() == DefaultDocumentEvent.EventType.CHANGE))) {
						mainWindow.getMenu().getEdit().getUndoManager().addEdit(evt.getEdit());
					}
				}
			});
		}
	}

	/**
	 * 
	 */
	class OpenFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {

			Logger logger = Log.getLog();
			IOFactory ioFactory = IOFactory.getInstance();
			MainWindow mainWindow = MainWindow.getInstance();
			Language language = Language.getInstance();

			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			ResourceBundle labels = language.getLabels();
			TextFile textFile = ioFactory.buildFile();
			String f = " ";
			f = textFile.read();

			// If the text is empty
			if (f == null) {
				logger.info(labels.getString("s83"));
			} else {

				boolean isOpened = false;
				int fileIndex = -1;
				for (int j = 0; j < mainWindow.getEditorBuilder().getNumEditors(); j++) {
					if (mainWindow.getEditorBuilder().getEditorAt(j).getPath()
							.equals(f)) {
						isOpened = true;
						fileIndex = j;
					}
				}

				if (!isOpened) {

					String text = null;
					text = textFile.load(f);

					// If the file is not empty
					if (text != null) {

						int j = -1;
						for (int z = 0; z < mainWindow.getProjectConfiguration()
								.getNumFilesFromList(); z++) {
							if (mainWindow.getProjectConfiguration().getFile(z)
									.getPath().equals(f))
								j = z;
						}

						// If the file belongs to the project
						if (j > -1) {

							int t = 0;

							// status
							mainWindow.getStatusBar().setMessage(
									mainWindow.getProjectConfiguration().getFile(j)
											.getPath());

							if (mainWindow.getProjectConfiguration().getFile(j)
									.isSetFile()) {
								t = 2;
								// status
								mainWindow.getStatusBar().setMessage(
										mainWindow.getProjectConfiguration()
												.getFile(j).getPath()
												+ " <COMPILABLE>");
							}

							if (mainWindow.getProjectConfiguration().getFile(j)
									.isMainFile()) {
								t = 1;
								// status
								mainWindow.getStatusBar().setMessage(
										mainWindow.getProjectConfiguration()
												.getFile(j).getPath()
												+ " <MAIN>");
							}
							mainWindow.getEditorBuilder().newTab(f, f, text,
									true, t);
						} else {

							// If it doesn't belong to the project
							// We create a new tab with the original text on it
							mainWindow.getStatusBar().setMessage(f);
							mainWindow.getEditorBuilder().newTab(f, f, text,
									true, 0);
						}

						logger.info(labels.getString("s84") + f);
						logger.info(labels.getString("s85") + f
								+ labels.getString("s86"));

						// UNDO REDO
						mainWindow.getMenu().enableFileMenu();
						mainWindow.getMenu().enableEditMenu();
						DefaultStyledDocument doc = mainWindow.getEditorBuilder()
								.getSelectedEditor().getDoc();

						doc.addUndoableEditListener(new UndoableEditListener() {

							/*
							 * (non-Javadoc)
							 * @see javax.swing.event.UndoableEditListener#undoableEditHappened(javax.swing.event.UndoableEditEvent)
							 */
							public void undoableEditHappened(UndoableEditEvent evt) {

								MainWindow mainWindow = MainWindow.getInstance();
								UndoableEdit edit = evt.getEdit();

								if (edit instanceof DefaultDocumentEvent
										&& ((DefaultDocumentEvent) edit).getType() == DefaultDocumentEvent.EventType.CHANGE) {
									return;
								} else {
									mainWindow.getMenu().getEdit().getUndoManager()
											.addEdit(evt.getEdit());
								}
							}
						});

						Editor.getEditor().setCaretPosition(0);

						for (int z = 0; z < mainWindow.getProjectConfiguration()
								.getFileListSize(); z++) {
							if (mainWindow.getProjectConfiguration().getFile(z)
									.getPath().equals(f)) {
								mainWindow.getProjectConfiguration().getFile(z)
										.setOpened(true);
							}
						}
						String prj = null;
						try {
							prj = PropertiesManager
									.getProperty("defaultAcideProject");
						} catch (Exception e1) {
							e1.printStackTrace();
						}
						if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
								.getProjectConfiguration().getName().equals(""))) {
							mainWindow.getProjectConfiguration().setModified(true);
						}

					} else {
						logger.info(labels.getString("s88"));
					}

				} else {

					mainWindow.getEditorBuilder().setSelectedEditorAt(fileIndex);
				}
			}
		}
	}

	/**
	 * 
	 */
	class SaveFileAsListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			Language language = Language.getInstance();
			
			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			
			ResourceBundle labels = language.getLabels();
			Logger logger = Log.getLog();
			IOFactory ioFactory = IOFactory.getInstance();
			MainWindow mainWindow = MainWindow.getInstance();
			TextFile textFile = ioFactory.buildFile();
			String f = " ";
			
			if (mainWindow.getEditorBuilder().getNumEditors() == 0) {
				logger.info(labels.getString("s89"));

			} else {
				f = textFile.write();

				if (f.equals(" ")) {

					logger.info(labels.getString("s92"));
				} else {
					boolean result = textFile.save(f, mainWindow.getEditorBuilder()
							.getSelectedEditor().getText());

					if (result) {
						logger.info(labels.getString("s93") + f
								+ labels.getString("s94"));
						MainWindow.getInstance().getEditorBuilder().greenButton();
						int index = f.lastIndexOf("\\");
						index++;
						String file = f.substring(index, f.length());
						mainWindow.getEditorBuilder()
								.getPane()
								.setTitleAt(
										mainWindow.getEditorBuilder().getPane()
												.getSelectedIndex(), file);
						mainWindow.getEditorBuilder().getSelectedEditor().setPath(f);
						mainWindow.getEditorBuilder().getPane().setToolTipText(f);
					
						File explorerFile = new File(mainWindow.getEditorBuilder()
								.getSelectedEditor().getPath());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor()
								.setLastChange(explorerFile.lastModified());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setLastSize(explorerFile.length());

					} else {
						logger.info(labels.getString("s95") + f);
					}
				}
			}
		}
	}

	/**
	 * 
	 */
	class SaveFileListener implements ActionListener {
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			Language language = Language.getInstance();
			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			ResourceBundle labels = language.getLabels();

			Logger logger = Log.getLog();
			IOFactory ioFactory = IOFactory.getInstance();
			MainWindow mainWindow = MainWindow.getInstance();

			TextFile textFile = ioFactory.buildFile();
			String f = " ";
			
			if (mainWindow.getEditorBuilder().getNumEditors() == 0) {
				logger.info(labels.getString("s89"));
			} else {
				if (mainWindow.getEditorBuilder().getSelectedEditor().getPath()
						.equals(labels.getString("s79")) == false) {
					
					boolean result = textFile.save(mainWindow.getEditorBuilder()
							.getSelectedEditor().getPath(), mainWindow.getEditorBuilder()
							.getSelectedEditor().getText());

					if (result) {
						logger.info(labels.getString("s93") + f
								+ labels.getString("s94"));
						
						MainWindow.getInstance().getEditorBuilder().greenButton();
						
						File explorerFile = new File(mainWindow.getEditorBuilder()
								.getSelectedEditor().getPath());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor()
								.setLastChange(explorerFile.lastModified());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setLastSize(explorerFile.length());

					} else {
						
						 logger.info(labels.getString("s95") + f);
					}
				} else {
					mainWindow.getMenu().getFile().getSaveFileAs().setEnabled(true);
					mainWindow.getMenu().getFile().getSaveFileAs().doClick();
				}
			}
		}
	}

	/**
	 * 
	 */
	class PrintFileListener implements ActionListener {
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {

			PrintGUI.getInstance().getFrame().setVisible(true);
		}
	}

	/**
	 * 
	 */
	class AddFileListener implements ActionListener {
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			
			try {

				MainWindow mainWindow = MainWindow.getInstance();
				String file = "";
				file = mainWindow.getEditorBuilder()
						.getEditorAt(mainWindow.getEditorBuilder().getSelectedEditorIndex())
						.getPath();
				System.out.println("NO.  "
						+ mainWindow.getEditorBuilder().getSelectedEditorIndex());
				if (file != null) {
					
					TreePath path = mainWindow.getExplorer().getTree().getSelectionPath();
					DefaultMutableTreeNode filePath;
					ExplorerFile fc;
					
					// Folder Selected
					if (path != null) {
						filePath = (DefaultMutableTreeNode) path
								.getLastPathComponent();
						fc = (ExplorerFile) filePath.getUserObject();

						// File Selected
						if (!fc.isDirectory()) {
							filePath = mainWindow.getExplorer().getRoot().getNextNode();
							fc = (ExplorerFile) filePath.getUserObject();
						}

					} else {
						filePath = mainWindow.getExplorer().getRoot().getNextNode();
						fc = (ExplorerFile) filePath.getUserObject();
					}

					int in = file.lastIndexOf("\\");
					String fich = "";
					if (in != -1) {
						in++;
						fich = file.substring(in, file.length());
					} else {
						in = file.lastIndexOf("/");
						fich = file.substring(in, file.length());
					}
					ExplorerFile fic = new ExplorerFile();
					fic.setPath(file);
					fic.setName(fich);

					boolean isAdded = false;
					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration().getFile(i).getPath()
								.equals(fic.getPath())) {
							isAdded = true;
						}
					}

					if (!isAdded) {

						fic.setParent(mainWindow.getProjectConfiguration().getName());
						mainWindow.getProjectConfiguration().setFile(fic);
						mainWindow.getProjectConfiguration().setNumFiles(
								Integer.toString(mainWindow.getProjectConfiguration()
										.getNumFilesFromList()));
						mainWindow.getProjectConfiguration()
								.getFile(
										mainWindow.getProjectConfiguration()
												.getNumFilesFromList() - 1)
								.setOpened(true);
						DefaultMutableTreeNode defaultMutableTreeNode = new DefaultMutableTreeNode(fic);
						defaultMutableTreeNode.setAllowsChildren(false);
						filePath.add(defaultMutableTreeNode);
						mainWindow.validate();
						mainWindow.repaint();
						mainWindow.getExplorer().getTreeModel().reload();
						mainWindow.getExplorer().expandTree();
						mainWindow.getExplorer().setEnabledRemoveFile();
						mainWindow.getExplorer().setEnabledDeleteFile();
						mainWindow.getProjectConfiguration().setModified(true);
					}
				}
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}

	/**
	 * 
	 */
	class DeleteFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			
			Language language = Language.getInstance();
			
			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception e) {
				e.printStackTrace();
			}
			ResourceBundle labels = language.getLabels();
			MainWindow mainWindow = MainWindow.getInstance();

			int chosenOption = JOptionPane.showConfirmDialog(null,
					labels.getString("s951"));
			if (chosenOption == JOptionPane.OK_OPTION) {

				Toolkit toolkit = Toolkit.getDefaultToolkit();
				String prj = null;
				try {
					prj = PropertiesManager.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}

				// No project
				if ((prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					
					String fileRemove = mainWindow.getEditorBuilder().getSelectedEditor()
							.getPath();
					File fi = new File(fileRemove);
					fi.delete();
					mainWindow.getEditorBuilder().getPane()
							.remove(mainWindow.getEditorBuilder().getSelectedEditorIndex());
					mainWindow.getStatusBar().setMessage("");
					toolkit.beep();
				} else {

					ExplorerFile ff = new ExplorerFile();
					int y = -1;
					int editor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
					for (int j = 0; j < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); j++) {

						if (mainWindow.getProjectConfiguration()
								.getFile(j)
								.getPath()
								.equals(mainWindow.getEditorBuilder().getSelectedEditor()
										.getPath())) {

							ff = mainWindow.getProjectConfiguration().getFile(j);
							for (int m = 0; m < mainWindow.getProjectConfiguration()
									.getNumFilesFromList() + 1; m++) {

								if (mainWindow.getExplorer().getTree().getPathForRow(m)
										.getLastPathComponent().toString()
										.equals(ff.getLastPathComponent())) {

									y = m;
								}
							}
						}
					}

					TreePath currentSelection = mainWindow.getExplorer().getTree()
							.getPathForRow(y);

					if (currentSelection != null) {
						DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
								.getLastPathComponent());
						ExplorerFile p = (ExplorerFile) currentNode.getUserObject();
						if (!p.isDirectory()) {
							MutableTreeNode parent = (MutableTreeNode) (currentNode
									.getParent());
							if (parent != null) {
								mainWindow.getExplorer().getTreeModel()
										.removeNodeFromParent(currentNode);
								toolkit.beep();
								int cont = -1;
								for (int j = 0; j < mainWindow.getProjectConfiguration()
										.getNumFilesFromList(); j++) {
									if (mainWindow.getProjectConfiguration().getFile(j)
											.getPath().equals(p.getPath())) {
										System.out.println(mainWindow
												.getProjectConfiguration()
												.getFile(j).getPath());
										cont = j;
									}
								}
								ExplorerFile f = mainWindow.getProjectConfiguration()
										.getFile(cont);
								String fileRemove = f.getPath();
								mainWindow.getProjectConfiguration().removeFileAt(cont);
								mainWindow.getProjectConfiguration().setNumFiles(
										Integer.toString(mainWindow
												.getProjectConfiguration()
												.getNumFilesFromList()));

								System.out.println(fileRemove);
								File fi = new File(fileRemove);
								fi.delete();

								mainWindow.getStatusBar().setMessage("");
								mainWindow.getProjectConfiguration().setModified(true);
								mainWindow.getEditorBuilder().getPane().remove(editor);

								return;
							}
							toolkit.beep();
						}
					} else {// The file does not belong to the project
						String fileRemove = mainWindow.getEditorBuilder()
								.getSelectedEditor().getPath();
						File fi = new File(fileRemove);
						fi.delete();
						mainWindow.getEditorBuilder().getPane()
								.remove(mainWindow.getEditorBuilder().getSelectedEditorIndex());
						mainWindow.getStatusBar().setMessage("");
						toolkit.beep();
					}
					if (mainWindow.getProjectConfiguration().getNumFilesFromList() > 0) {
						mainWindow.getExplorer().setEnabledRemoveFile();
						mainWindow.getExplorer().setEnabledDeleteFile();
					} else {
						mainWindow.getExplorer().getRemoveFile().setEnabled(false);
						mainWindow.getExplorer().getDeleteFile().setEnabled(false);
					}
				}
			} 
		}
	}

	/**
	 * 
	 */
	class RemoveFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			
			Language language = Language.getInstance();
			
			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception e) {
				e.printStackTrace();
			}
			
			ResourceBundle labels = language.getLabels();
			MainWindow mainWindow = MainWindow.getInstance();
			
			int chosenOption = JOptionPane.showConfirmDialog(null,
					labels.getString("s623"));
			if (chosenOption == JOptionPane.OK_OPTION) {

				Toolkit toolkit = Toolkit.getDefaultToolkit();

				ExplorerFile f = new ExplorerFile();
				int y = -1;
				int editor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
				for (int j = 0; j < mainWindow.getProjectConfiguration()
						.getNumFilesFromList(); j++) {

					if (mainWindow.getProjectConfiguration()
							.getFile(j)
							.getPath()
							.equals(mainWindow.getEditorBuilder().getSelectedEditor()
									.getPath())) {
						f = mainWindow.getProjectConfiguration().getFile(j);
						for (int m = 0; m < mainWindow.getProjectConfiguration()
								.getNumFilesFromList() + 1; m++) {
							if (mainWindow.getExplorer().getTree().getPathForRow(m)
									.getLastPathComponent().toString()
									.equals(f.getLastPathComponent())) {

								y = m;
							}
						}
					}
				}

				TreePath currentSelection = mainWindow.getExplorer().getTree()
						.getPathForRow(y);

				if (currentSelection != null) {
					
					DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
							.getLastPathComponent());
					ExplorerFile p = (ExplorerFile) currentNode.getUserObject();
					
					if (!p.isDirectory()) {
						
						MutableTreeNode parent = (MutableTreeNode) (currentNode
								.getParent());
						
						if (parent != null) {
							mainWindow.getExplorer().getTreeModel()
									.removeNodeFromParent(currentNode);
							toolkit.beep();
							int cont = -1;
							for (int j = 0; j < mainWindow.getProjectConfiguration()
									.getNumFilesFromList(); j++) {
								if (mainWindow.getProjectConfiguration().getFile(j)
										.getPath().equals(p.getPath())) {
									System.out.println(mainWindow.getProjectConfiguration()
											.getFile(j).getPath());
									cont = j;
								}
							}
							mainWindow.getProjectConfiguration().removeFileAt(cont);
							mainWindow.getProjectConfiguration().setNumFiles(
									Integer.toString(mainWindow.getProjectConfiguration()
											.getNumFilesFromList()));
							mainWindow.getStatusBar().setMessage("");
							mainWindow.getProjectConfiguration().setModified(true);

							// Ask for saving
							if (mainWindow.getEditorBuilder().isRedButton(editor)) {
								int opt = JOptionPane.showConfirmDialog(null,
										labels.getString("s643"),
										labels.getString("s953"),
										JOptionPane.YES_NO_OPTION);

								if (opt == JOptionPane.OK_OPTION) {
									mainWindow.getMenu().getFile().getSaveFile().setEnabled(true);
									mainWindow.getMenu().getFile().getSaveFile().doClick();
								}
							}
							// Close
							mainWindow.getEditorBuilder().getPane().remove(editor);
							if (mainWindow.getEditorBuilder().getPane().getTabCount() == 0) {
								mainWindow.getMenu().disableFileMenu();
								mainWindow.getMenu().disableEditMenu();
							}

							return;

						}
						toolkit.beep();
					}

				}
				if (mainWindow.getProjectConfiguration().getNumFilesFromList() > 0) {
					mainWindow.getExplorer().setEnabledRemoveFile();
					mainWindow.getExplorer().setEnabledDeleteFile();
				} else {
					mainWindow.getExplorer().getRemoveFile().setEnabled(false);
					mainWindow.getExplorer().getDeleteFile().setEnabled(false);
				}
			} 
		}
	}

	/**
	 * 
	 */
	class CloseFileListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			Language language = Language.getInstance();
			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception ex) {
				ex.printStackTrace();
			}

			final ResourceBundle labels = language.getLabels();
			
			MainWindow mainWindow = MainWindow.getInstance();
			int editor = mainWindow.getEditorBuilder().getSelectedEditorIndex();
			
			if (mainWindow.getEditorBuilder().isRedButton() == true) {
				int opt = JOptionPane.showConfirmDialog(null,
						labels.getString("s643"));

				if (opt == JOptionPane.OK_OPTION) {

					if (mainWindow.getEditorBuilder().getSelectedEditor().getPath()
							.equals(labels.getString("s79")) == true) {

						IOFactory ioFactory = IOFactory.getInstance();
						TextFile textFile = ioFactory.buildFile();
						String f = " ";
						f = textFile.write();
						if (f.equals(" ")) {
							_logger.info(labels.getString("s92"));
						} else {
							
							boolean result = textFile.save(f, mainWindow
									.getEditorBuilder()
									.getSelectedEditor().getText());
							
							if (result) {
								_logger.info(labels.getString("s93")
										+ f + labels.getString("s94"));
								MainWindow.getInstance().getEditorBuilder()
										.greenButton();
								mainWindow.getEditorBuilder().getEditorAt(editor)
										.setPath(f);
								mainWindow.getEditorBuilder().getEditorAt(editor)
										.setToolTipText(f);
								int index = f.lastIndexOf("\\");
								index++;
								String file = f.substring(index,
										f.length());
								mainWindow.getEditorBuilder().getEditorAt(editor)
										.setName(file);
								File explorerFile = new File(mainWindow.getEditorBuilder()
										.getSelectedEditor().getPath());
								MainWindow.getInstance().getEditorBuilder()
										.getSelectedEditor()
										.setLastChange(explorerFile.lastModified());
								MainWindow.getInstance().getEditorBuilder()
										.getSelectedEditor()
										.setLastSize(explorerFile.length());
								mainWindow.getStatusBar().setMessage("");
							} else {
								_logger.info(labels.getString("s95")
										+ f);
							}
						}

					} else {
						_saveFile.setEnabled(true);
						_saveFile.doClick();
						mainWindow.getStatusBar().setMessage("");
					}

					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getFileListSize(); i++) {
						if (mainWindow.getProjectConfiguration()
								.getFile(i)
								.getPath()
								.equals(mainWindow.getEditorBuilder()
										.getEditorAt(editor).getPath())) {
							mainWindow.getProjectConfiguration().getFile(i)
									.setOpened(false);
						}
					}
					String prj = null;
					try {
						prj = PropertiesManager
								.getProperty("defaultAcideProject");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
							.getProjectConfiguration().getName().equals(""))) {
						mainWindow.getProjectConfiguration().setModified(true);
					}
					mainWindow.getEditorBuilder().getPane().remove(editor);

				} else if (opt == JOptionPane.NO_OPTION) {
					mainWindow.getEditorBuilder().getPane().remove(editor);
					mainWindow.getStatusBar().setMessage("");
				}
			} else {
				
				for (int i = 0; i < mainWindow.getProjectConfiguration()
						.getFileListSize(); i++) {
					if (mainWindow.getProjectConfiguration()
							.getFile(i)
							.getPath()
							.equals(mainWindow.getEditorBuilder()
									.getEditorAt(editor).getPath())) {
						mainWindow.getProjectConfiguration().getFile(i)
								.setOpened(false);
					}
				}
				String prj = null;
				try {
					prj = PropertiesManager
							.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					mainWindow.getProjectConfiguration().setModified(true);
				}
				mainWindow.getEditorBuilder().getPane().remove(editor);
				mainWindow.getStatusBar().setMessage("");
			}
			if (mainWindow.getEditorBuilder().getPane().getTabCount() == 0) {
				disableMenu();
				mainWindow.getMenu().disableEditMenu();
			}
		}
	}

	/**
	 * 
	 */
	class CloseAllFilesListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			Language language = Language.getInstance();
			try {
				language.getLanguage(Integer.parseInt(PropertiesManager
						.getProperty("language")));
			} catch (Exception ex) {
				ex.printStackTrace();
			}

			final ResourceBundle labels = language.getLabels();
			
			MainWindow mainWindow = MainWindow.getInstance();
			int editor = mainWindow.getEditorBuilder().getNumEditors();
			mainWindow.getEditorBuilder().setSelectedEditorAt(editor - 1);
			
			for (int i = editor - 1; i >= 0; i--) {
				
				mainWindow.getEditorBuilder().setSelectedEditorAt(i);
				
				if (mainWindow.getEditorBuilder().isRedButton() == true) {
					int chosenOption = JOptionPane.showConfirmDialog(null,
							labels.getString("s643"));

					if (chosenOption == JOptionPane.OK_OPTION) {

						if (mainWindow.getEditorBuilder().getSelectedEditor()
								.getPath().equals(labels.getString("s79")) == true) {

							IOFactory ioFactory = IOFactory.getInstance();
							TextFile textFile = ioFactory.buildFile();
							String f = " ";
							f = textFile.write();
							if (f.equals(" ")) {
								_logger.info(labels.getString("s92"));
							} else {
								
								boolean result = textFile.save(f, mainWindow
										.getEditorBuilder()
										.getSelectedEditor().getText());
								
								if (result) {
									_logger.info(labels.getString("s93")
											+ f
											+ labels.getString("s94"));
									MainWindow.getInstance()
											.getEditorBuilder()
											.greenButton();
									mainWindow.getEditorBuilder().getEditorAt(i)
											.setPath(f);
									mainWindow.getEditorBuilder().getEditorAt(i)
											.setToolTipText(f);
									int index = f.lastIndexOf("\\");
									index++;
									String file = f.substring(index,
											f.length());
									mainWindow.getEditorBuilder().getEditorAt(i)
											.setName(file);
									File explorerFile = new File(mainWindow
											.getEditorBuilder()
											.getSelectedEditor().getPath());
									MainWindow
											.getInstance()
											.getEditorBuilder()
											.getSelectedEditor()
											.setLastChange(
													explorerFile.lastModified());
									MainWindow.getInstance()
											.getEditorBuilder()
											.getSelectedEditor()
											.setLastSize(explorerFile.length());

								} else {
									_logger.info(labels.getString("s95")
											+ f);
								}
							}

						} else {
							_saveFile.setEnabled(true);
							_saveFile.doClick();
						}

						String prj = null;
						try {
							prj = PropertiesManager
									.getProperty("defaultAcideProject");
						} catch (Exception e1) {
							e1.printStackTrace();
						}
						if (!(prj
								.equals("./configuration/default.acidePrj") && mainWindow
								.getProjectConfiguration().getName()
								.equals(""))) {
							mainWindow.getProjectConfiguration().setModified(true);
						}

						for (int z = 0; z < mainWindow.getProjectConfiguration()
								.getFileListSize(); z++) {
							if (mainWindow.getProjectConfiguration()
									.getFile(z)
									.getPath()
									.equals(mainWindow.getEditorBuilder()
											.getEditorAt(i).getPath())) {
								mainWindow.getProjectConfiguration().getFile(z)
										.setOpened(false);
							}
						}
					} else {
						if (chosenOption == JOptionPane.CANCEL_OPTION) {
							return;
						}
					}
				}
				for (int z = 0; z < mainWindow.getProjectConfiguration()
						.getFileListSize(); z++) {
					if (mainWindow.getProjectConfiguration()
							.getFile(z)
							.getPath()
							.equals(mainWindow.getEditorBuilder().getEditorAt(i)
									.getPath())) {
						mainWindow.getProjectConfiguration().getFile(z)
								.setOpened(false);
					}
				}

			}
			for (int i = 0; i < editor; i++) {
				mainWindow.getEditorBuilder().setSelectedEditorAt(0);
				mainWindow.getEditorBuilder().getPane().remove(0);
				mainWindow.getEditorBuilder().getPane().validate();
			}

			disableMenu();
			mainWindow.getMenu().disableEditMenu();
		}
	}

	/**
	 * 
	 */
	class SaveAllFilesListener implements ActionListener{

		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent arg0) {
			
			MainWindow mainWindow = MainWindow.getInstance();
			int eS = mainWindow.getEditorBuilder().getSelectedEditorIndex();
			int editor = mainWindow.getEditorBuilder().getNumEditors();
			
			for (int i = 0; i < editor; i++) {
				mainWindow.getEditorBuilder().setSelectedEditorAt(i);
				saveOrSaveAS();
			}
			mainWindow.getEditorBuilder().setSelectedEditorAt(eS);
		}
	}

	/**
	 * 
	 */
	class SetCompilableFileListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			MainWindow mainWindow = MainWindow.getInstance();

			if (!mainWindow.getEditorBuilder().getSelectedEditor().isCompilerFile()
					|| (mainWindow.getEditorBuilder().getSelectedEditor()
							.isCompilerFile() && mainWindow.getEditorBuilder()
							.getSelectedEditor().isMainFile())) {

				String prj = null;
				try {
					prj = PropertiesManager
							.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					// No project
					mainWindow.getEditorBuilder().getSelectedEditor()
							.setCompilerFile(true);
					if (mainWindow.getEditorBuilder().getSelectedEditor()
							.isMainFile())
						mainWindow.getEditorBuilder().getSelectedEditor()
								.setMainFile(false);
					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration()
								.getFile(i)
								.getPath()
								.equals(mainWindow.getEditorBuilder()
										.getSelectedEditor().getPath()))
							mainWindow.getProjectConfiguration().getFile(i)
									.setSetFile(true);
						if (mainWindow.getProjectConfiguration().getFile(i)
								.isMainFile())
							mainWindow.getProjectConfiguration().getFile(i)
									.setMainFile(false);
					}
					// put icon in tab
					mainWindow.getEditorBuilder()
							.getPane()
							.setIconAt(
									mainWindow.getEditorBuilder()
											.getSelectedEditorIndex(),
									new ImageIcon(
											"./resources/icons/editor/compilable.PNG"));
					// status bar
					mainWindow.getStatusBar().setMessage(
							mainWindow.getEditorBuilder().getSelectedEditor()
									.getPath()
									+ " <COMPILABLE>");
				} else {

					mainWindow.getEditorBuilder().getSelectedEditor()
							.setCompilerFile(true);
					if (mainWindow.getEditorBuilder().getSelectedEditor()
							.isMainFile())
						mainWindow.getEditorBuilder().getSelectedEditor()
								.setMainFile(false);
					mainWindow.getProjectConfiguration().setModified(true);
					// status
					mainWindow.getStatusBar().setMessage(
							mainWindow.getEditorBuilder().getSelectedEditor()
									.getPath()
									+ " <COMPILABLE>");

					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration()
								.getFile(i)
								.getPath()
								.equals(mainWindow.getEditorBuilder()
										.getSelectedEditor().getPath())) {
							mainWindow.getProjectConfiguration().getFile(i)
									.setSetFile(true);
							if (mainWindow.getProjectConfiguration().getFile(i)
									.isMainFile())
								mainWindow.getProjectConfiguration().getFile(i)
										.setMainFile(false);
							// put icon in tab
							mainWindow.getEditorBuilder()
									.getPane()
									.setIconAt(
											mainWindow.getEditorBuilder()
													.getSelectedEditorIndex(),
											new ImageIcon(
													"./resources/icons/editor/compilable.PNG"));
						}
					}
				}
			}
		}
	}

	/**
	 * 
	 */
	class UnsetCompilableFileListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			MainWindow mainWindow = MainWindow.getInstance();

			if (mainWindow.getEditorBuilder().getSelectedEditor().isCompilerFile()
					&& !mainWindow.getEditorBuilder().getSelectedEditor()
							.isMainFile()) {

				String prj = null;
				try {
					prj = PropertiesManager
							.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					// No project
					mainWindow.getEditorBuilder().getSelectedEditor()
							.setCompilerFile(false);
					// quit status
					mainWindow.getStatusBar().setMessage(
							mainWindow.getEditorBuilder().getSelectedEditor()
									.getPath());
					// quit icon in tab
					mainWindow.getEditorBuilder()
							.getPane()
							.setIconAt(
									mainWindow.getEditorBuilder()
											.getSelectedEditorIndex(), null);

					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration()
								.getFile(i)
								.getPath()
								.equals(mainWindow.getEditorBuilder()
										.getSelectedEditor().getPath()))
							mainWindow.getProjectConfiguration().getFile(i)
									.setSetFile(false);
					}
				} else {

					mainWindow.getEditorBuilder().getSelectedEditor()
							.setCompilerFile(false);
					mainWindow.getProjectConfiguration().setModified(true);
					// quit status
					mainWindow.getStatusBar().setMessage(
							mainWindow.getEditorBuilder().getSelectedEditor()
									.getPath());
					// quit icon in tab
					mainWindow.getEditorBuilder()
							.getPane()
							.setIconAt(
									mainWindow.getEditorBuilder()
											.getSelectedEditorIndex(), null);
					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration()
								.getFile(i)
								.getPath()
								.equals(mainWindow.getEditorBuilder()
										.getSelectedEditor().getPath()))
							mainWindow.getProjectConfiguration().getFile(i)
									.setSetFile(false);
					}
				}
			}
		}
	}

	/**
	 * 
	 */
	class SetMainFileListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			MainWindow mainWindow = MainWindow.getInstance();

			if (!mainWindow.getEditorBuilder().getSelectedEditor().isMainFile()) {

				String prj = null;
				try {
					prj = PropertiesManager
							.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					// no project
					// quit previous main
					for (int i = 0; i < mainWindow.getEditorBuilder()
							.getNumEditors(); i++) {
						if (mainWindow.getEditorBuilder().getEditorAt(i)
								.isMainFile()) {
							mainWindow.getEditorBuilder().getEditorAt(i)
									.setMainFile(false);
							mainWindow.getEditorBuilder().getEditorAt(i)
									.setCompilerFile(false);
							mainWindow.getStatusBar().setMessage(
									mainWindow.getEditorBuilder().getEditorAt(i)
											.getPath());
							mainWindow.getEditorBuilder().getPane()
									.setIconAt(i, null);
						}
					}

					mainWindow.getEditorBuilder().getSelectedEditor()
							.setMainFile(true);
					mainWindow.getEditorBuilder().getSelectedEditor()
							.setCompilerFile(true);
					// status
					mainWindow.getStatusBar().setMessage(
							mainWindow.getEditorBuilder().getSelectedEditor()
									.getPath()
									+ " <MAIN>");
					// put icon in tab
					mainWindow.getEditorBuilder()
							.getPane()
							.setIconAt(
									mainWindow.getEditorBuilder()
											.getSelectedEditorIndex(),
									new ImageIcon(
											"./resources/icons/editor/main.PNG"));
				} else {
					// quit previous main
					for (int i = 0; i < mainWindow.getEditorBuilder()
							.getNumEditors(); i++) {
						if (mainWindow.getEditorBuilder().getEditorAt(i)
								.isMainFile()) {
							mainWindow.getEditorBuilder().getEditorAt(i)
									.setMainFile(false);
							mainWindow.getEditorBuilder().getEditorAt(i)
									.setCompilerFile(false);
							mainWindow.getStatusBar().setMessage(
									mainWindow.getEditorBuilder().getEditorAt(i)
											.getPath());
							mainWindow.getEditorBuilder().getPane()
									.setIconAt(i, null);
						}
					}

					mainWindow.getEditorBuilder().getSelectedEditor()
							.setMainFile(true);
					mainWindow.getEditorBuilder().getSelectedEditor()
							.setCompilerFile(true);
					mainWindow.getStatusBar().setMessage(
							mainWindow.getEditorBuilder().getSelectedEditor()
									.getPath()
									+ "  <MAIN>");

					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration()
								.getFile(i)
								.getPath()
								.equals(mainWindow.getEditorBuilder()
										.getSelectedEditor().getPath())) {

							for (int j = 0; j < mainWindow.getProjectConfiguration()
									.getFileListSize(); j++) {

								if (mainWindow.getProjectConfiguration().getFile(j)
										.isMainFile()) {
									mainWindow.getProjectConfiguration().getFile(j)
											.setMainFile(false);
									mainWindow.getProjectConfiguration().getFile(j)
											.setSetFile(false);
									for (int z = 0; z < mainWindow
											.getEditorBuilder()
											.getNumEditors(); z++) {
										if (mainWindow.getEditorBuilder()
												.getEditorAt(z)
												.getPath()
												.equals(mainWindow
														.getProjectConfiguration()
														.getFile(j)
														.getPath()))
											mainWindow.getEditorBuilder().getPane()
													.setIconAt(z, null);
									}
								}
							}

							mainWindow.getProjectConfiguration().getFile(i)
									.setMainFile(true);
							mainWindow.getProjectConfiguration().getFile(i)
									.setSetFile(true);
							mainWindow.getProjectConfiguration().setModified(true);

							// put icon in tab
							mainWindow.getEditorBuilder()
									.getPane()
									.setIconAt(
											mainWindow.getEditorBuilder()
													.getSelectedEditorIndex(),
											new ImageIcon(
													"./resources/icons/editor/main.PNG"));

						}
					}
				}
			}
		}
	}

	/**
	 * 
	 */
	class UnsetMainFileListener implements ActionListener{
		
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(ActionEvent e) {
			
			MainWindow mainWindow = MainWindow.getInstance();

			if (mainWindow.getEditorBuilder().getSelectedEditor().isMainFile()) {																// aplicar

				String prj = null;
				try {
					prj = PropertiesManager
							.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals("./configuration/default.acidePrj") && mainWindow
						.getProjectConfiguration().getName().equals(""))) {
					// No project
					mainWindow.getEditorBuilder().getSelectedEditor()
							.setMainFile(false);
					// quit status
					mainWindow.getStatusBar().setMessage(
							mainWindow.getEditorBuilder().getSelectedEditor()
									.getPath());
					// quit icon in tab
					mainWindow.getEditorBuilder()
							.getPane()
							.setIconAt(
									mainWindow.getEditorBuilder()
											.getSelectedEditorIndex(), null);
				} else {

					mainWindow.getEditorBuilder().getSelectedEditor()
							.setMainFile(false);
					mainWindow.getProjectConfiguration().setModified(true);
					// quit status
					mainWindow.getStatusBar().setMessage(
							mainWindow.getEditorBuilder().getSelectedEditor()
									.getPath());
					// quit icon in tab
					mainWindow.getEditorBuilder()
							.getPane()
							.setIconAt(
									mainWindow.getEditorBuilder()
											.getSelectedEditorIndex(), null);
					for (int i = 0; i < mainWindow.getProjectConfiguration()
							.getNumFilesFromList(); i++) {
						if (mainWindow.getProjectConfiguration()
								.getFile(i)
								.getPath()
								.equals(mainWindow.getEditorBuilder()
										.getSelectedEditor().getPath()))
							mainWindow.getProjectConfiguration().getFile(i)
									.setMainFile(false);
					}
				}
			}
		}
	}
}

