package gui.menu.file;

import es.configuration.menu.MenuConfiguration;
import es.text.TextFile;
import gui.MainWindow;
import gui.editor.EditorBuilder;
import gui.menu.print.PrintGUI;

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
import javax.swing.undo.UndoableEdit;

import org.apache.log4j.Logger;

import language.Language;

import operations.factory.IOFactory;
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
	protected Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 */
	public FileMenu() {

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

		_closeFile.setEnabled(false);
		_closeAllFiles.setEnabled(false);
		_saveFileAs.setEnabled(false);
		_saveFile.setEnabled(false);
		_saveAllFiles.setEnabled(false);
		_printFile.setEnabled(false);
	}

	/**
	 * @param labels
	 * @param language
	 * 
	 */
	public void buildMenu(ResourceBundle labels, Language language) {

		removeAll();

		if (MenuConfiguration.getFile())
			add(_newFile);
		if (MenuConfiguration.getOpenFile())
			add(_openFile);
		if (MenuConfiguration.getCloseFile())
			add(_closeFile);
		if (MenuConfiguration.getCloseAll())
			add(_closeAllFiles);
		if ((MenuConfiguration.getFile() || MenuConfiguration.getOpenFile()
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
		if ((MenuConfiguration.getFile() || MenuConfiguration.getOpenFile()
				|| MenuConfiguration.getCloseFile()
				|| MenuConfiguration.getCloseAll()
				|| MenuConfiguration.getSaveFile()
				|| MenuConfiguration.getSaveFileAs() || MenuConfiguration
				.getSaveAllFiles())
				&& (MenuConfiguration.getPrintFile()))
			addSeparator();

		if (MenuConfiguration.getPrintFile())
			add(_printFile);

		if ((MenuConfiguration.getFile() || MenuConfiguration.getOpenFile()
				|| MenuConfiguration.getSaveFileAs()
				|| MenuConfiguration.getSaveFile()
				|| MenuConfiguration.getSaveAllFiles()
				|| MenuConfiguration.getPrintFile()
				|| MenuConfiguration.getCloseFile() || MenuConfiguration
				.getCloseAll())
				&& MenuConfiguration.getExit())
			addSeparator();

		if (MenuConfiguration.getExit())
			add(_exit);
	}

	/**
	 * 
	 */
	public void setListeners() {

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

		// CLOSE FILE
		_closeFile.addActionListener(new CloseFileListener());

		// CLOSE ALL FILES
		_closeAllFiles.addActionListener(new CloseAllFilesListener());

		// SAVE ALL FILES
		_saveAllFiles.addActionListener(new SaveAllFilesListener());
	}

	/**
	 * Save the file the opened in the editor depending on the status of it. If
	 * it is red it will save it as. If it is green it will just save it.
	 * 
	 * @return The selected editor index of the operation.
	 */
	public int saveOrSaveAS() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception e) {
			e.printStackTrace();
		}

		// GET THE LABELS
		ResourceBundle labels = language.getLabels();

		// GET THE MAIN WINDOW
		MainWindow mainWindow = MainWindow.getInstance();

		int selectedEditorIndex = mainWindow.getEditorBuilder()
				.getSelectedEditorIndex();

		// IF THE FILE IS MODIFIED
		if (mainWindow.getEditorBuilder().isRedButton()) {

			// IF IT IS THE NEW DOCUMENT
			if (mainWindow.getEditorBuilder().getSelectedEditor().getAbsolutePath()
					.equals(labels.getString("s79"))) {

				// CREATES THE FILE IN DISK
				TextFile textFile = IOFactory.getInstance().buildFile();
				String filePath = " ";
				filePath = textFile.write();

				// IF IT IS NOT EMPTY
				if (!filePath.equals(" ")) {

					// SAVE THE FILE CONTENT
					boolean saveResult = textFile.save(filePath, mainWindow
							.getEditorBuilder().getSelectedEditor().getText());

					// IF IT COULD SAVE IT
					if (saveResult) {

						// UPDATES THE LOG
						_logger.info(labels.getString("s93") + filePath
								+ labels.getString("s94"));

						// SET THE BUTTON TO GREEN
						MainWindow.getInstance().getEditorBuilder()
								.setGreenButton();

						// SET THE NEW PATH
						mainWindow.getEditorBuilder().getEditorAt(
								selectedEditorIndex).setAbsolutePath(filePath);

						// SET THE TOOL TIP TEXT
						mainWindow.getEditorBuilder().getEditorAt(
								selectedEditorIndex).setToolTipText(filePath);

						// GET THE NAME
						int index = filePath.lastIndexOf("\\");
						if (index == -1)
							index = filePath.lastIndexOf("/");
						String name = filePath.substring(index + 1, filePath
								.length());

						// SET THE NAME
						mainWindow.getEditorBuilder().getEditorAt(
								selectedEditorIndex).setName(name);

						File file = new File(mainWindow.getEditorBuilder()
								.getSelectedEditor().getAbsolutePath());

						// SET THE LAST CHANGE
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setLastChange(
										file.lastModified());

						// SET THE SIZE
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setLastSize(file.length());

					} else

						// UPDATES THE LOG WITH THE ERROR
						_logger.info(labels.getString("s95") + filePath);

				} else

					// UPDATES THE LOG
					_logger.info(labels.getString("s92"));
			} else {

				// ENABLE THE MENU OPTION
				_saveFile.setEnabled(true);

				// SAVE THE FILE
				_saveFile.doClick();
			}
		}

		return selectedEditorIndex;
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
	 */
	public void enableMenu() {

		_closeFile.setEnabled(true);
		_closeAllFiles.setEnabled(true);
		_saveFileAs.setEnabled(true);
		_saveFile.setEnabled(false);
		_saveAllFiles.setEnabled(false);
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
		_printFile.setEnabled(false);
	}

	/**
	 * 
	 */
	public void enableSaveFileAs() {

		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
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
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent e) {
			MainWindow.getInstance().closingOperation();
			System.exit(0);
		}
	}

	/**
	 * Listener of the NewFile Menu.
	 */
	class NewFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent e) {

			// GET THE LANGUAGE
			Language language = Language.getInstance();

			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			// GET THE LABELS
			ResourceBundle labels = language.getLabels();

			MainWindow.getInstance().getMenu().enableFileMenu();
			MainWindow.getInstance().getMenu().enableEditMenu();
			EditorBuilder editorBuilder = MainWindow.getInstance()
					.getEditorBuilder();
			editorBuilder.newTab(labels.getString("s79"), labels
					.getString("s79"), "", true, 0);
			_logger.info(labels.getString("s80"));

			// UNDO REDO
			MainWindow.getInstance().getMenu().enableFileMenu();
			MainWindow.getInstance().getMenu().enableEditMenu();
			DefaultStyledDocument doc = MainWindow.getInstance()
					.getEditorBuilder().getSelectedEditor().getSyntaxDocument();
			doc.addUndoableEditListener(new UndoableEditListener() {
				/*
				 * (non-Javadoc)
				 * 
				 * @see
				 * javax.swing.event.UndoableEditListener#undoableEditHappened
				 * (javax.swing.event.UndoableEditEvent)
				 */
				public void undoableEditHappened(UndoableEditEvent evt) {

					UndoableEdit edit = evt.getEdit();

					if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
							.getType() == DefaultDocumentEvent.EventType.CHANGE))) {
						MainWindow.getInstance().getMenu().getEdit()
								.getUndoManager().addEdit(evt.getEdit());
					}
				}
			});

			// UPDATE THE STATUS BAR
			MainWindow.getInstance().getStatusBar().setMessage(
					labels.getString("s79"));
		}
	}

	/**
	 * 
	 */
	class OpenFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent e) {

			// GET THE LANGUAGE
			Language language = Language.getInstance();

			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			// GET THE LABELS
			ResourceBundle labels = language.getLabels();

			TextFile textFile = IOFactory.getInstance().buildFile();
			String filePath = " ";
			filePath = textFile.read();

			// IF THE FILE EXISTS
			if (filePath != null) {

				boolean isOpened = false;

				// CHECK IF THE FILE IS ALREADY OPENED
				int fileIndex = -1;
				for (int j = 0; j < MainWindow.getInstance().getEditorBuilder()
						.getNumEditors(); j++) {
					if (MainWindow.getInstance().getEditorBuilder()
							.getEditorAt(j).getAbsolutePath().equals(filePath)) {
						isOpened = true;
						fileIndex = j;
					}
				}

				// IF IS NOT OPENED IN THE EDITOR
				if (!isOpened) {

					String text = null;
					text = textFile.load(filePath);

					// IF THE TEXT IS NOT EMPTY
					if (text != null) {

						// CHECK IF THE FILE BELONGS TO A PROJECT
						int fileProjectIndex = -1;
						for (int pos = 0; pos < MainWindow.getInstance()
								.getProjectConfiguration()
								.getNumFilesFromList(); pos++) {
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(pos)
									.getPath().equals(filePath))
								fileProjectIndex = pos;
						}

						// IF THE FILE BELONGS TO A PROJECT
						if (fileProjectIndex > -1) {

							int type = 0;

							// UPDATES THE STATUS BAR
							MainWindow.getInstance().getStatusBar().setMessage(
									MainWindow.getInstance()
											.getProjectConfiguration()
											.getFileAt(fileProjectIndex)
											.getPath());

							// IS COMPILABLE FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(
											fileProjectIndex)
									.isCompilableFile()) {
								type = 2;

								// UPDATES THE STATUS BAR
								MainWindow
										.getInstance()
										.getStatusBar()
										.setMessage(
												MainWindow
														.getInstance()
														.getProjectConfiguration()
														.getFileAt(
																fileProjectIndex)
														.getPath()
														+ " <COMPILABLE>");
							}

							// IS MAIN FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(
											fileProjectIndex).isMainFile()) {
								type = 1;

								// UPDATES THE STATUS BAR
								MainWindow
										.getInstance()
										.getStatusBar()
										.setMessage(
												MainWindow
														.getInstance()
														.getProjectConfiguration()
														.getFileAt(
																fileProjectIndex)
														.getPath()
														+ " <MAIN>");
							}

							// OPEN A NEW TAB
							MainWindow.getInstance().getEditorBuilder().newTab(
									filePath, filePath, text, true, type);
						} else {

							// IF IT DOES NOT BELONG TO THE PROJECT

							// UPDATES THE STATUS BAR
							MainWindow.getInstance().getStatusBar().setMessage(
									filePath);

							// OPEN A NEW TAB WITH THE CONTENT
							MainWindow.getInstance().getEditorBuilder().newTab(
									filePath, filePath, text, true, 0);
						}

						// UPDATES THE LOG
						_logger.info(labels.getString("s84") + filePath);
						_logger.info(labels.getString("s85") + filePath
								+ labels.getString("s86"));

						// UNDO REDO
						MainWindow.getInstance().getMenu().enableFileMenu();
						MainWindow.getInstance().getMenu().enableEditMenu();
						DefaultStyledDocument doc = MainWindow.getInstance()
								.getEditorBuilder().getSelectedEditor()
								.getSyntaxDocument();

						doc.addUndoableEditListener(new UndoableEditListener() {

							/*
							 * (non-Javadoc)
							 * 
							 * @seejavax.swing.event.UndoableEditListener#
							 * undoableEditHappened
							 * (javax.swing.event.UndoableEditEvent)
							 */
							public void undoableEditHappened(
									UndoableEditEvent evt) {

								UndoableEdit edit = evt.getEdit();

								if (edit instanceof DefaultDocumentEvent
										&& ((DefaultDocumentEvent) edit)
												.getType() == DefaultDocumentEvent.EventType.CHANGE) {
									return;
								} else {
									MainWindow.getInstance().getMenu()
											.getEdit().getUndoManager()
											.addEdit(evt.getEdit());
								}
							}
						});

						// SET THE CARET IN THE FIRST POSITION OF THE SELECTED
						// EDITOR
						MainWindow.getInstance().getEditorBuilder().getSelectedEditor().getEditor().setCaretPosition(0);

						// SET THE NEW FILE STATE TO OPENED
						for (int filePosition = 0; filePosition < MainWindow
								.getInstance().getProjectConfiguration()
								.getFileListSize(); filePosition++) {
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(
											filePosition).getPath().equals(
											filePath)) {
								MainWindow.getInstance()
										.getProjectConfiguration().getFileAt(
												filePosition).setIsOpened(true);
							}
						}

						// NOT DEFAULT PROJECT
						if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
							
							// THE PROJECT IS MODIFIED
							MainWindow.getInstance().getProjectConfiguration()
									.setIsModified(true);
	
					} else {

						// EMPTY FILE
						_logger.info(labels.getString("s88"));
					}

				} else {

					// AS IS ALREADY OPENED, PUT THE FOCUS IN THE OPENED EDITOR
					MainWindow.getInstance().getEditorBuilder()
							.setSelectedEditorAt(fileIndex);
				}
			} else

				// FILE DOESN'T EXISTS
				_logger.info(labels.getString("s83"));
		}
	}

	/**
	 * 
	 */
	class SaveFileAsListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent e) {

			// GET THE LANGUAGE
			Language language = Language.getInstance();

			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			// GET THE LABELS
			ResourceBundle labels = language.getLabels();

			TextFile textFile = IOFactory.getInstance().buildFile();
			String f = " ";

			// IF THERE ARE OPENED FILES
			if (MainWindow.getInstance().getEditorBuilder().getNumEditors() != 0) {

				f = textFile.write();

				// IF THE FILE IS NOT EMPTY
				if (!f.equals(" ")) {

					boolean result = textFile.save(f, MainWindow.getInstance()
							.getEditorBuilder().getSelectedEditor().getText());

					// IF IT COULD SAVE IT
					if (result) {
						
						// UPDATE THE LOG
						_logger.info(labels.getString("s93") + f
								+ labels.getString("s94"));
						
						// SET THE GREEN BUTTON TO THE EDITOR
						MainWindow.getInstance().getEditorBuilder()
								.setGreenButton();
						
						// GET THE NAME OF THE FILE
						int index = f.lastIndexOf("\\");
						if(index == -1)
							index = f.lastIndexOf("/");
						index++;
						String file = f.substring(index, f.length());
						
						// SET THE TITLE
						MainWindow.getInstance().getEditorBuilder().getPane()
								.setTitleAt(
										MainWindow.getInstance()
												.getEditorBuilder().getPane()
												.getSelectedIndex(), file);
						
						// SET THE PATH
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setAbsolutePath(f);
						
						// SET THE TOOL TIP TEXT
						MainWindow.getInstance().getEditorBuilder().getPane()
								.setToolTipText(f);

						// SAVES THE ORIGINAL FILE
						File explorerFile = new File(MainWindow.getInstance()
								.getEditorBuilder().getSelectedEditor()
								.getAbsolutePath());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setLastChange(
										explorerFile.lastModified());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setLastSize(
										explorerFile.length());

					} else
						_logger.info(labels.getString("s95") + f);
				} else
					_logger.info(labels.getString("s92"));
			} else
				_logger.info(labels.getString("s89"));
		}
	}

	/**
	 * 
	 */
	class SaveFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent e) {

			// GET THE LANGUAGE
			Language language = Language.getInstance();

			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			// GET THE LABELS
			ResourceBundle labels = language.getLabels();

			TextFile textFile = IOFactory.getInstance().buildFile();
			String filePath = " ";

			// IF THERE ARE OPENED FILES
			if (MainWindow.getInstance().getEditorBuilder().getNumEditors() != 0) {
				
				// IF IS NOT THE NEW FILE
				if (!MainWindow.getInstance().getEditorBuilder()
						.getSelectedEditor().getAbsolutePath().equals(
								labels.getString("s79"))) {

					// SAVE 
					boolean result = textFile.save(MainWindow.getInstance()
							.getEditorBuilder().getSelectedEditor().getAbsolutePath(),
							MainWindow.getInstance().getEditorBuilder()
									.getSelectedEditor().getText());

					// IF IT COULD SAVE IT
					if (result) {
						
						// UPDATES THE LOG
						_logger.info(labels.getString("s93") + filePath
								+ labels.getString("s94"));

						// SET THE GREEN BUTTON
						MainWindow.getInstance().getEditorBuilder()
								.setGreenButton();

						// SAVE THE ORIGINAL FILE
						File explorerFile = new File(MainWindow.getInstance()
								.getEditorBuilder().getSelectedEditor()
								.getAbsolutePath());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setLastChange(
										explorerFile.lastModified());
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().setLastSize(
										explorerFile.length());

					} else {
						_logger.info(labels.getString("s95") + filePath);
					}
				} else {
					MainWindow.getInstance().getMenu().getFile()
							.getSaveFileAs().setEnabled(true);
					MainWindow.getInstance().getMenu().getFile()
							.getSaveFileAs().doClick();
				}
			}
			else
				_logger.info(labels.getString("s89"));
		}
	}

	/**
	 * 
	 */
	class PrintFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent arg0) {
			PrintGUI.getInstance().getFrame().setVisible(true);
		}
	}

	/**
	 * 
	 */
	class CloseFileListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent e) {

			// GET THE LANGUAGE
			Language language = Language.getInstance();

			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception ex) {
				ex.printStackTrace();
			}

			// GET THE LABELS
			final ResourceBundle labels = language.getLabels();

			int selectedEditorIndex = MainWindow.getInstance().getEditorBuilder()
					.getSelectedEditorIndex();

			// IF IS RED BUTTON
			if (MainWindow.getInstance().getEditorBuilder().isRedButton()) {
				
				// ASK THE USER FOR SAVING THE FILE
				int chosenOption = JOptionPane.showConfirmDialog(null, labels
						.getString("s643"));

				// IF OK
				if (chosenOption == JOptionPane.OK_OPTION) {

					// IF IT IS THE NEW FILE
					if (MainWindow.getInstance().getEditorBuilder()
							.getSelectedEditor().getAbsolutePath().equals(
									labels.getString("s79"))) {

						TextFile textFile = IOFactory.getInstance().buildFile();
						String filePath = " ";
						filePath = textFile.write();
						
						if (!filePath.equals(" ")) {
							
							// SAVE
							boolean result = textFile.save(filePath, MainWindow
									.getInstance().getEditorBuilder()
									.getSelectedEditor().getText());

							// IF IT COULD SAVE IT
							if (result) {
								
								// UPDATES THE LOG
								_logger.info(labels.getString("s93") + filePath
										+ labels.getString("s94"));
								
								// SET THE GREEN BUTTON
								MainWindow.getInstance().getEditorBuilder()
										.setGreenButton();
								
								// SET THE PATH
								MainWindow.getInstance().getEditorBuilder()
										.getEditorAt(selectedEditorIndex).setAbsolutePath(filePath);
								
								// SET THE TOOL TIP TEXT
								MainWindow.getInstance().getEditorBuilder()
										.getEditorAt(selectedEditorIndex).setToolTipText(filePath);
								
								// GET THE NAME
								int index = filePath.lastIndexOf("\\");
								if(index == -1)
									index = filePath.lastIndexOf("/");
								index++;
								String file = filePath.substring(index, filePath.length());
								
								// SET THE TITLE
								MainWindow.getInstance().getEditorBuilder()
										.getEditorAt(selectedEditorIndex).setName(file);
								
								// SAVE THE ORIGINAL FILE
								File explorerFile = new File(MainWindow
										.getInstance().getEditorBuilder()
										.getSelectedEditor().getAbsolutePath());
								MainWindow.getInstance().getEditorBuilder()
										.getSelectedEditor().setLastChange(
												explorerFile.lastModified());
								MainWindow.getInstance().getEditorBuilder()
										.getSelectedEditor().setLastSize(
												explorerFile.length());
								
								// UPDATES THE STATUS BAR
								MainWindow.getInstance().getStatusBar()
										.setMessage("");
							}
							else
								_logger.info(labels.getString("s92"));
						} 
						else
							_logger.info(labels.getString("s95") + filePath);
					}		
					else {
						_saveFile.setEnabled(true);
						_saveFile.doClick();
						MainWindow.getInstance().getStatusBar().setMessage("");
					}

					// UPDATES THE STATE OF THE PROJECT FILES
					for (int i = 0; i < MainWindow.getInstance()
							.getProjectConfiguration().getFileListSize(); i++) {
						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).getPath().equals(
										MainWindow.getInstance()
												.getEditorBuilder()
												.getEditorAt(selectedEditorIndex).getAbsolutePath())) {
							MainWindow.getInstance().getProjectConfiguration()
									.getFileAt(i).setIsOpened(false);
						}
					}
								
					// NOT DEFAULT PROJECT
					if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
						
						// THE PROJECT IS MODIFIED
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);
					
					// REMOVE THE TAB
					MainWindow.getInstance().getEditorBuilder().getPane()
							.remove(selectedEditorIndex);

				} else 
					if (chosenOption == JOptionPane.NO_OPTION) {
						
						// REMOVE THE TAB
						MainWindow.getInstance().getEditorBuilder().getPane()
							.remove(selectedEditorIndex);
						
						// UPDATES THE STATUS BAR
						MainWindow.getInstance().getStatusBar().setMessage("");
				}
			} else {

				// UPDATES THE STATE OF THE FILE IN THE PROJECT CONFIGURATION
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getFileListSize(); i++) {
					
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorBuilder()
											.getEditorAt(selectedEditorIndex).getAbsolutePath())) {
						
						// IS NOT OPENED
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsOpened(false);
					}
				}
				
				// NOT DEFAULT PROJECT
				if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
					MainWindow.getInstance().getProjectConfiguration()
							.setIsModified(true);
				
				// REMOVES THE TAB
				MainWindow.getInstance().getEditorBuilder().getPane().remove(
						selectedEditorIndex);
				
				// UPDATES THE STATUS BAR
				MainWindow.getInstance().getStatusBar().setMessage("");
			}
			
			// NO MORE TABS OPENED
			if (MainWindow.getInstance().getEditorBuilder().getPane()
					.getTabCount() == 0) {
				
				// DISABLE THE FILE MENU
				disableMenu();
				
				// DISABLE THE EDIT MENU AS WELL
				MainWindow.getInstance().getMenu().disableEditMenu();
			}
		}
	}

	/**
	 * 
	 */
	class CloseAllFilesListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent e) {

			// GET THE LANGUAGE
			Language language = Language.getInstance();
			
			try {
				language.getLanguage(PropertiesManager.getProperty("language"));
			} catch (Exception ex) {
				ex.printStackTrace();
			}

			// GET THE LABELS
			final ResourceBundle labels = language.getLabels();

			MainWindow mainWindow = MainWindow.getInstance();
			int numEditors = mainWindow.getEditorBuilder().getNumEditors();
			mainWindow.getEditorBuilder().setSelectedEditorAt(numEditors - 1);

			for (int i = numEditors - 1; i >= 0; i--) {

				mainWindow.getEditorBuilder().setSelectedEditorAt(i);

				if (mainWindow.getEditorBuilder().isRedButton() == true) {
					int chosenOption = JOptionPane.showConfirmDialog(null,
							labels.getString("s643"));

					if (chosenOption == JOptionPane.OK_OPTION) {

						if (mainWindow.getEditorBuilder().getSelectedEditor()
								.getAbsolutePath().equals(labels.getString("s79")) == true) {

							IOFactory ioFactory = IOFactory.getInstance();
							TextFile textFile = ioFactory.buildFile();
							String f = " ";
							f = textFile.write();
							if (f.equals(" ")) {
								_logger.info(labels.getString("s92"));
							} else {

								boolean result = textFile.save(f, mainWindow
										.getEditorBuilder().getSelectedEditor()
										.getText());

								if (result) {
									_logger.info(labels.getString("s93") + f
											+ labels.getString("s94"));
									MainWindow.getInstance().getEditorBuilder()
											.setGreenButton();
									mainWindow.getEditorBuilder()
											.getEditorAt(i).setAbsolutePath(f);
									mainWindow.getEditorBuilder()
											.getEditorAt(i).setToolTipText(f);
									int index = f.lastIndexOf("\\");
									if (index == -1)
										index = f.lastIndexOf("/");	
									index++;
									index++;
									String file = f
											.substring(index, f.length());
									mainWindow.getEditorBuilder()
											.getEditorAt(i).setName(file);
									File explorerFile = new File(mainWindow
											.getEditorBuilder()
											.getSelectedEditor().getAbsolutePath());
									MainWindow
											.getInstance()
											.getEditorBuilder()
											.getSelectedEditor()
											.setLastChange(
													explorerFile.lastModified());
									MainWindow.getInstance().getEditorBuilder()
											.getSelectedEditor().setLastSize(
													explorerFile.length());

								} else {
									_logger.info(labels.getString("s95") + f);
								}
							}

						} else {
							_saveFile.setEnabled(true);
							_saveFile.doClick();
						}
						
						// NOT THE DEFAULT CONFIGURATION
						if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject())
							
							// NOW IS MODIFIED
							mainWindow.getProjectConfiguration().setIsModified(
									true);

						for (int z = 0; z < mainWindow
								.getProjectConfiguration().getFileListSize(); z++) {
							if (mainWindow.getProjectConfiguration().getFileAt(
									z).getPath().equals(
									mainWindow.getEditorBuilder()
											.getEditorAt(i).getAbsolutePath())) {
								mainWindow.getProjectConfiguration().getFileAt(
										z).setIsOpened(false);
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
					if (mainWindow.getProjectConfiguration().getFileAt(z)
							.getPath().equals(
									mainWindow.getEditorBuilder()
											.getEditorAt(i).getAbsolutePath())) {
						mainWindow.getProjectConfiguration().getFileAt(z)
								.setIsOpened(false);
					}
				}

			}
			for (int i = 0; i < numEditors; i++) {
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
	class SaveAllFilesListener implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent arg0) {

			int selectedEditorIndex = MainWindow.getInstance().getEditorBuilder().getSelectedEditorIndex();		
			int numEditors = MainWindow.getInstance().getEditorBuilder().getNumEditors();

			for (int i = 0; i < numEditors; i++) {
				MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(i);
				saveOrSaveAS();
			}
			
			MainWindow.getInstance().getEditorBuilder().setSelectedEditorAt(selectedEditorIndex);
		}
	}
}
