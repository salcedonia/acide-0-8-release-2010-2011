package gui.menu.file;

import es.configuration.menu.MenuConfiguration;
import es.text.TextFile;
import gui.mainWindow.MainWindow;
import gui.menu.file.listeners.CloseAllFilesListener;
import gui.menu.file.listeners.CloseFileListener;
import gui.menu.file.listeners.ExitListener;
import gui.menu.file.listeners.NewFileListener;
import gui.menu.file.listeners.OpenFileListener;
import gui.menu.file.listeners.PrintFileListener;
import gui.menu.file.listeners.SaveAllFilesListener;
import gui.menu.file.listeners.SaveFileAsListener;
import gui.menu.file.listeners.SaveFileListener;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import language.Language;

import operations.factory.IOFactory;
import operations.log.Log;
import properties.PropertiesManager;

/************************************************************************																
 * File menu of ACIDE - A Configurable IDE											
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
 * @see JMenu 																													
 ***********************************************************************/
public class FileMenu extends JMenu {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image file for the new file menu item
	 */
	private final static String NEW_FILE = "./resources/icons/menu/file/newFile.png";
	/**
	 * Image file for the open file menu item
	 */
	private final static String OPEN_FILE = "./resources/icons/menu/file/openFile.png";
	/**
	 * Image file for the open file menu item
	 */
	private final static String CLOSE_FILE = "./resources/icons/menu/file/closeFile.png";
	/**
	 * Image file for the close all files menu item
	 */
	private final static String CLOSE_ALL_FILES = "./resources/icons/menu/file/closeAllFiles.png";
	/**
	 * Image file for the save file menu item
	 */
	private final static String SAVE_FILE = "./resources/icons/menu/file/saveFile.png";
	/**
	 * Image file for the save all files menu item
	 */
	private final static String SAVE_ALL_FILES = "./resources/icons/menu/file/saveAllFiles.png";
	/**
	 * Image file for the save file as menu item
	 */
	private final static String SAVE_FILE_AS = "./resources/icons/menu/file/saveFileAs.png";
	/**
	 * Image file for the print file menu item
	 */
	private final static String PRINT_FILE = "./resources/icons/menu/file/printFile.png";
	/**
	 * Image file for the exit menu item
	 */
	private final static String EXIT = "./resources/icons/menu/file/exit.png";
	/**
	 * New file menu item
	 */
	private JMenuItem _newFile;
	/**
	 * Open file menu item
	 */
	private JMenuItem _openFile;
	/**
	 * Close file menu item
	 */
	private JMenuItem _closeFile;
	/**
	 * Close all files menu item
	 */
	private JMenuItem _closeAllFiles;
	/**
	 * Save file as menu item
	 */
	private JMenuItem _saveFileAs;
	/**
	 * Save file menu item
	 */
	private JMenuItem _saveFile;
	/**
	 * Save all file menu item
	 */
	private JMenuItem _saveAllFiles;
	/**
	 * Print file menu item
	 */
	private JMenuItem _printFile;
	/**
	 * Exit menu item
	 */
	private JMenuItem _exit;

	/**
	 * Class constructor
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
	 * Sets the labels to display in the selected language
	 */
	public void setLanguageLabels() {

		// Gets the language
		Language language = Language.getInstance();
		
		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			exception.printStackTrace();
		}

		// Gets the labels
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
	 * Builds the file menu
	 * 
	 * @param labels labels to display in the selected language
	 * @param language selected language
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
	 * Sets the file menu item Listeners
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
	 * Saves the file the opened in the editor depending on the status of it. If
	 * it is red it will save it as. If it is green it will just save it
	 * 
	 * @return the selected editor index of the operation
	 */
	public int saveOrSaveAS() {

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
		ResourceBundle labels = language.getLabels();

		int selectedEditorIndex = MainWindow.getInstance().getEditorManager()
				.getSelectedEditorIndex();

		// If the file has been modified 
		if (MainWindow.getInstance().getEditorManager().isRedButton()) {

			// If it is the NEW FILE
			if (MainWindow.getInstance().getEditorManager().getSelectedEditor().getAbsolutePath()
					.equals(labels.getString("s79"))) {

				// Creates the file in the disk
				TextFile textFile = IOFactory.getInstance().buildFile();
				String filePath = " ";
				filePath = textFile.write();

				// If it is not empty
				if (!filePath.equals(" ")) {

					// Saves the file
					boolean saveResult = textFile.save(filePath, MainWindow.getInstance()
							.getEditorManager().getSelectedEditor().getText());

					// If it could save it
					if (saveResult) {

						// Updates the log
						Log.getLog().info(labels.getString("s93") + filePath
								+ labels.getString("s94"));

						// Sets the green button
						MainWindow.getInstance().getEditorManager()
								.setGreenButton();

						// Sets the new file path
						MainWindow.getInstance().getEditorManager().getEditorAt(
								selectedEditorIndex).setAbsolutePath(filePath);

						// Sets the tool tip text
						MainWindow.getInstance().getEditorManager().getEditorAt(
								selectedEditorIndex).setToolTipText(filePath);

						// Gets the file name
						int index = filePath.lastIndexOf("\\");
						if (index == -1)
							index = filePath.lastIndexOf("/");
						String name = filePath.substring(index + 1, filePath
								.length());

						// Sets the name
						MainWindow.getInstance().getEditorManager().getEditorAt(
								selectedEditorIndex).setName(name);

						File file = new File(MainWindow.getInstance().getEditorManager()
								.getSelectedEditor().getAbsolutePath());

						// Sets the last change
						MainWindow.getInstance().getEditorManager()
								.getSelectedEditor().setLastChange(
										file.lastModified());

						// Sets the size
						MainWindow.getInstance().getEditorManager()
								.getSelectedEditor().setLastSize(file.length());

					} else

						// Updates the log
						Log.getLog().info(labels.getString("s95") + filePath);

				} else

					// Updates the log
					Log.getLog().info(labels.getString("s92"));
			} else {

				// Enables the SAVE FILE menu
				_saveFile.setEnabled(true);

				// Saves the file
				_saveFile.doClick();
			}
		}

		return selectedEditorIndex;
	}

	/**
	 * Enables the file menu
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
	 * Disables the file menu
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
	 * Enables the save file as menu item
	 */
	public void enableSaveFileAs() {

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
		ResourceBundle labels = language.getLabels();
		
		_saveFileAs.setEnabled(true);
		
		// Updates the log
		Log.getLog().info(labels.getString("s75"));
	}
	
	/**
	 * Returns the new file menu item
	 * 
	 * @return the new file menu item
	 */
	public JMenuItem getNewFile() {
		return _newFile;
	}

	/**
	 * Sets a new value to the new file menu item
	 * 
	 * @param newFile new value to set
	 */
	public void setNewFile(JMenuItem newFile) {
		_newFile = newFile;
	}

	/**
	 * Returns the exit menu item
	 * 
	 * @return the exit menu item
	 */
	public JMenuItem getExit() {
		return _exit;
	}

	/**
	 * Sets a new value to the exit menu item
	 * 
	 * @param exit new value to set
	 */
	public void setExit(JMenuItem exit) {
		_exit = exit;
	}

	/**
	 * Returns the save file as menu item
	 * 
	 * @return the save file as menu item
	 */
	public JMenuItem getSaveFileAs() {
		return _saveFileAs;
	}

	/**
	 * Sets a new value to the save file as menu item
	 * 
	 * @param saveFileAs new value to set
	 */
	public void setSaveFileAs(JMenuItem saveFileAs) {
		_saveFileAs = saveFileAs;
	}

	/**
	 * Returns the save file menu item
	 * 
	 * @return the save file menu item
	 */
	public JMenuItem getSaveFile() {
		return _saveFile;
	}

	/**
	 * Sets a new value to the save file menu item
	 * 
	 * @param saveFile new value to set
	 */
	public void setSaveFile(JMenuItem saveFile) {
		_saveFile = saveFile;
	}

	/**
	 * Returns the save all files menu item
	 * 
	 * @return the save all files menu item
	 */
	public JMenuItem getSaveAllFiles() {
		return _saveAllFiles;
	}

	/**
	 * Sets a new value to the save all files menu item
	 * 
	 * @param saveAllFiles new value to set
	 */
	public void setSaveAllFiles(JMenuItem saveAllFiles) {
		_saveAllFiles = saveAllFiles;
	}

	/**
	 * Returns the print file menu item
	 * 
	 * @return the print file menu item
	 */
	public JMenuItem getPrintFile() {
		return _printFile;
	}

	/**
	 * Sets a new value to the print file menu item
	 * 
	 * @param printFile new value to set
	 */
	public void setPrintFile(JMenuItem printFile) {
		_printFile = printFile;
	}

	/**
	 * Returns the close all files menu item
	 * 
	 * @return the close all files menu item
	 */
	public JMenuItem getCloseAllFiles() {
		return _closeAllFiles;
	}

	/**
	 * Sets a new value to the close all files menu item
	 * 
	 * @param closeAllFiles new value to set
	 */
	public void setCloseAllFiles(JMenuItem closeAllFiles) {
		_closeAllFiles = closeAllFiles;
	}

	/**
	 * Returns the close file menu item
	 * 
	 * @return the close file menu item
	 */
	public JMenuItem getCloseFile() {
		return _closeFile;
	}

	/**
	 * Sets a new value to the close file menu item
	 * 
	 * @param closeFile new value to set
	 */
	public void setCloseFile(JMenuItem closeFile) {
		_closeFile = closeFile;
	}

	/**
	 * Returns the open file menu item
	 * 
	 * @return the open file menu item
	 */
	public JMenuItem getOpenFile() {
		return _openFile;
	}

	/**
	 * Sets a new value to the open file menu item
	 * 
	 * @param openFile new value to set
	 */
	public void setOpenFile(JMenuItem openFile) {
		_openFile = openFile;
	}
}
