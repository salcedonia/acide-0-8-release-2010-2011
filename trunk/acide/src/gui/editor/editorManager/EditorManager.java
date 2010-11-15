package gui.editor.editorManager;

import gui.editor.editorManager.listeners.EditorManagerMouseClickListener;
import gui.editor.editorManager.utils.gui.DragAndDropTabbedPane;
import gui.editor.editorManager.utils.logic.testPlaf.TestPlaf;
import gui.editor.editorPanel.EditorPanel;
import gui.editor.editorPanel.popup.EditorPopupMenu;
import gui.mainWindow.MainWindow;

import javax.swing.*;

import language.Language;
import operations.log.Log;

import properties.PropertiesManager;

import java.io.File;
import java.util.ResourceBundle;

/************************************************************************																
 * Handles the creation and destruction of the different editor tabs of 
 * ACIDE - A Configurable IDE											
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
 ***********************************************************************/
public class EditorManager {

	/**
	 * Path where all the resources of the application are
	 */
	private static final String RESOURCE_PATH = "./resources/icons/editor/";
	/**
	 * TabbedPane for the editor
	 */
	private DragAndDropTabbedPane _tabbedPane;
	/**
	 * TestPlaf for the editor
	 */
	private TestPlaf _testPlaf;
	/**
	 * Editor panel popup menu
	 */
	private EditorPopupMenu _popup;
	
	/**
	 * Class constructor
	 */
	public EditorManager() {

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
		final ResourceBundle labels = language.getLabels();

		try {
			
			_tabbedPane = new DragAndDropTabbedPane();
			_testPlaf = new TestPlaf();
			_tabbedPane.setUI(_testPlaf);
			_tabbedPane.addMouseListener(new EditorManagerMouseClickListener());
			// POPUP
			buildPopupMenu();
			
		} catch (RuntimeException exception) {
			
			// Updates the log
			Log.getLog().info(labels.getString("s315"));
			exception.printStackTrace();
		}
	}
	
	/**
	 * Builds the popup menu
	 */
	public void buildPopupMenu() {
		_popup = new EditorPopupMenu();
	}
	
	/**
	 * Creates an editor tab with the name, tool tip and type specified
	 * 
	 * @param name
	 *            name of the tab
	 * @param toolTip
	 *            tool tip for the tab
	 * @param type
	 *            type of the file
	 */
	public void newEditor(String name, String toolTip, int type) {

		EditorPanel editorPanel = new EditorPanel();

		switch (type) {
		case 0: {
			_tabbedPane.addTab(name, null, editorPanel, toolTip);
			editorPanel.setIcon(null);
			break;
		}
		case 1: {
			_tabbedPane.addTab(name, new ImageIcon(RESOURCE_PATH + "main.PNG"),
					editorPanel, toolTip);
			editorPanel.setIcon(new ImageIcon(RESOURCE_PATH + "main.PNG"));
			break;
		}
		case 2: {
			_tabbedPane.addTab(name, new ImageIcon(RESOURCE_PATH
					+ "compilable.PNG"), editorPanel, toolTip);
			new ImageIcon(RESOURCE_PATH + "compilable.PNG");
			break;
		}
		}
		
		// Adds the popup menu
		_tabbedPane.addMouseListener(new EditorManagerMouseClickListener());
		_tabbedPane.revalidate();
		_tabbedPane.repaint();
	}

	/**
	 * Closes a tab in the position at the list given as a parameter
	 * 
	 * @param pos tab position to be closed
	 */
	public void removeTab(int pos) {
		_testPlaf.getCloseButtonAt(pos).doClick();
	}

	/**
	 * Returns the editor at the position of the list given as a parameter
	 * 
	 * @param pos
	 *            position of the editor to return
	 * 
	 * @return the editor at the position of the list given as a parameter
	 */
	public EditorPanel getEditorAt(int pos) {
		if ((pos < _tabbedPane.getComponentCount()) && (pos >= 0)) {
			return (EditorPanel) _tabbedPane.getComponentAt(pos);
		} else {
			return null;
		}
	}

	/**
	 * Returns the number of editors of the tabbedPane
	 * 
	 * @return the number of editors of the tabbedPane
	 */
	public int getNumEditors() {
		return _tabbedPane.getTabCount();
	}

	/**
	 * Creates a new tab in the tabbedPane
	 * 
	 * @param path
	 *            path of the file to open
	 * @param textContent
	 *            text type
	 * @param text
	 *            content of the text to display
	 * @param modifiable
	 *            indicates if the editor is modifiable or not
	 */
	public void newTab(String path, String textContent, String text,
			boolean modifiable, int type) {

		// Checks if the file is already opened
		boolean found = false;
		int pos = 0;

		for (int i = 0; i < getNumEditors(); i++) {
			if (getEditorAt(i).getAbsolutePath() == textContent) {
				found = true;
				pos = i;
			}
		}

		// if it is not opened yet
		if (!found) {

			File file = new File(textContent);

			// Gets the name
			int index = path.lastIndexOf("\\");
			if (index == -1)
				index = path.lastIndexOf("/");
			String subName = path.substring(index + 1);

			newEditor(subName, textContent, type);

			// Starts from the last opened editor
			int n = getNumEditors() - 1;
			getEditorAt(n).loadText(text);
			getEditorAt(n).setEditable(modifiable);
			getTabbedPane().setSelectedIndex(n);
			getEditorAt(n).setAbsolutePath(textContent);
			getEditorAt(n).setLastChange(file.lastModified());
			getEditorAt(n).setLastSize(file.length());

		} else {

			// If it is already opened, sets the focus on it
			setSelectedEditorAt(pos);
		}
	}

	/**
	 * Sets the selected editor at the position given as a parameter
	 * 
	 * @param position position to select
	 */
	public void setSelectedEditorAt(int position) {
		getTabbedPane().setSelectedIndex(position);
	}

	/**
	 * Returns the the selected editor index
	 * 
	 * @return the the selected editor index
	 */
	public int getSelectedEditorIndex() {
		return getTabbedPane().getSelectedIndex();
	}

	/**
	 * Returns the selected editor panel
	 * 
	 * @return the selected editor panel
	 */
	public EditorPanel getSelectedEditor() {
		return getEditorAt(getTabbedPane().getSelectedIndex());
	}

	/**
	 * Returns the editor builder tabbedPane
	 * 
	 * @return the editor builder tabbedPane
	 */
	public JTabbedPane getTabbedPane() {
		return _tabbedPane;
	}

	/**
	 * Returns the popup menu
	 * 
	 * @return the popup menu
	 */
	public EditorPopupMenu getPopupMenu() {
		return _popup;
	}
	
	/**
	 * Returns the editor marked as Main File
	 * 
	 * @return the main editor marked as Main File
	 */
	public EditorPanel getMainEditor() {

		for (int i = 0; i < getNumEditors(); i++) {

			if (getEditorAt(i).isMainFile())
				return getEditorAt(i);
		}

		return null;
	}

	/**
	 * Returns the drag and drop tabbed panel
	 * 
	 * @return The drag and drop tabbed panel
	 * @see DragAndDropTabbedPane
	 */
	public DragAndDropTabbedPane getPane() {
		return _tabbedPane;
	}

	/**
	 * Returns the TestPlaf of the tabbed panel
	 * 
	 * @return the TestPlaf of the tabbed panel
	 * @see TestPlaf
	 */
	public TestPlaf getTestPlaf() {
		return _testPlaf;
	}

	/**
	 * Sets the close button to green
	 */
	public void setGreenButton() {
		MainWindow.getInstance().getEditorManager().getTestPlaf()
				.getCloseButtonAt(
						MainWindow.getInstance().getEditorManager()
								.getSelectedEditorIndex()).setGreenButton();
	}

	/**
	 * Returns true if the button is red and false in the other case
	 * 
	 * @return true if the button is red and false in the other case
	 */
	public boolean isRedButton() {
		return MainWindow.getInstance().getEditorManager().getTestPlaf()
				.getCloseButtonAt(
						MainWindow.getInstance().getEditorManager()
								.getSelectedEditorIndex()).isRedButton();
	}

	/**
	 * Sets the green button of the editor at the list position given as a
	 * parameter.
	 * 
	 * @param position
	 *            list position of the button
	 */
	public void setGreenButtonAt(int position) {
		MainWindow.getInstance().getEditorManager().getTestPlaf()
				.getCloseButtonAt(position).setGreenButton();
	}

	/**
	 * Returns true if the button is red and false in the other case
	 * 
	 * @param position
	 *            list position of the button
	 * 
	 * @return True if the button is red and false in the other case
	 */
	public boolean isRedButton(int position) {
		return MainWindow.getInstance().getEditorManager().getTestPlaf()
				.getCloseButtonAt(position).isRedButton();
	}

	/**
	 * Sets the file in the editor as compilable
	 */
	public void setCompilableFile() {

		if (!MainWindow.getInstance().getEditorManager().getSelectedEditor()
				.isCompilerFile()
				|| (MainWindow.getInstance().getEditorManager()
						.getSelectedEditor().isCompilerFile() && MainWindow
						.getInstance().getEditorManager().getSelectedEditor()
						.isMainFile())) {

			// Default project
			if (MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {

				// Sets the file as compiled
				MainWindow.getInstance().getEditorManager().getSelectedEditor()
						.setCompilerFile(true);

				// If it is already a MAIN FILE
				if (MainWindow.getInstance().getEditorManager()
						.getSelectedEditor().isMainFile())
					// Removes the main file property
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().setMainFile(false);

				// Searches for the file into the project configuration file list
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// If Exists
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorManager()
											.getSelectedEditor()
											.getAbsolutePath()))
						// Marks it as COMPILABLE FILE
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsCompilableFile(true);

					// Is it already a MAIN FILE?
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).isMainFile())
						// Removes the main property
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsMainFile(false);
				}

				// Puts the COMPILABLE icon in the tab
				MainWindow
						.getInstance()
						.getEditorManager()
						.getPane()
						.setIconAt(
								MainWindow.getInstance().getEditorManager()
										.getSelectedEditorIndex(),
								new ImageIcon(
										"./resources/icons/editor/compilable.PNG"));

				// Updates the status bar
				MainWindow.getInstance().getStatusBar().setMessage(
						MainWindow.getInstance().getEditorManager()
								.getSelectedEditor().getAbsolutePath()
								+ " <COMPILABLE>");
			} else {

				// Not default project

				MainWindow.getInstance().getEditorManager().getSelectedEditor()
						.setCompilerFile(true);

				if (MainWindow.getInstance().getEditorManager()
						.getSelectedEditor().isMainFile())
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().setMainFile(false);

				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);

				// Updates the status bar
				MainWindow.getInstance().getStatusBar().setMessage(
						MainWindow.getInstance().getEditorManager()
								.getSelectedEditor().getAbsolutePath()
								+ " <COMPILABLE>");

				// Search for the file into the project configuration file list
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// If exists
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorManager()
											.getSelectedEditor()
											.getAbsolutePath())) {

						// Marks it as COMPILABLE FILE
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsCompilableFile(true);

						// It is MAIN FILE
						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).isMainFile())

							// Removes the main file property
							MainWindow.getInstance().getProjectConfiguration()
									.getFileAt(i).setIsMainFile(false);

						// Put the COMPILABLE icon in the tab
						MainWindow
								.getInstance()
								.getEditorManager()
								.getPane()
								.setIconAt(
										MainWindow.getInstance()
												.getEditorManager()
												.getSelectedEditorIndex(),
										new ImageIcon(
												"./resources/icons/editor/compilable.PNG"));
					}
				}
			}
		}
	}

	/**
	 * Unsets the file in the editor as compilable
	 */
	public void unsetCompilableFile() {

		// If it is COMPILABLE FILE and not MAIN FILE
		if (MainWindow.getInstance().getEditorManager().getSelectedEditor()
				.isCompilerFile()
				&& !MainWindow.getInstance().getEditorManager()
						.getSelectedEditor().isMainFile()) {

			// Sets COMPILER FILE to false
			MainWindow.getInstance().getEditorManager().getSelectedEditor()
					.setCompilerFile(false);

			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setMessage(
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().getAbsolutePath());

			// Quits the icon tab
			MainWindow.getInstance().getEditorManager().getPane().setIconAt(
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditorIndex(), null);

			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {

				// The project has been modified
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);

				// Searches for the file into the project configuration file list
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorManager()
											.getSelectedEditor()
											.getAbsolutePath()))
						// Sets the COMPILABLE FILE as false
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsCompilableFile(false);
				}
			}
		}
	}

	/**
	 * Sets a file of the editor as a main file
	 */
	public void setMainFile() {

		// IF it is not MAIN FILE
		if (!MainWindow.getInstance().getEditorManager().getSelectedEditor()
				.isMainFile()) {

			// Removes the previous MAIN FILE
			for (int i = 0; i < MainWindow.getInstance().getEditorManager()
					.getNumEditors(); i++) {

				// Finds the previous MAIN FILE
				if (MainWindow.getInstance().getEditorManager().getEditorAt(i)
						.isMainFile()) {

					// Sets MAIN FILE as false
					MainWindow.getInstance().getEditorManager().getEditorAt(i)
							.setMainFile(false);

					// Sets COMPILER FILE as false
					MainWindow.getInstance().getEditorManager().getEditorAt(i)
							.setCompilerFile(false);

					// Updates the status bar
					MainWindow.getInstance().getStatusBar().setMessage(
							MainWindow.getInstance().getEditorManager()
									.getEditorAt(i).getAbsolutePath());

					// Removes the tab icon
					MainWindow.getInstance().getEditorManager().getPane()
							.setIconAt(i, null);
				}
			}

			// Sets MAIN FILE as true
			MainWindow.getInstance().getEditorManager().getSelectedEditor()
					.setMainFile(true);

			// Sets COMPILER FILE as true
			MainWindow.getInstance().getEditorManager().getSelectedEditor()
					.setCompilerFile(true);

			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setMessage(
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().getAbsolutePath()
							+ " <MAIN>");

			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {

				// Updates the file into the project configuration
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// If exists
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorManager()
											.getSelectedEditor()
											.getAbsolutePath())) {

						for (int j = 0; j < MainWindow.getInstance()
								.getProjectConfiguration().getFileListSize(); j++) {

							// MAIN FILE?
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(j)
									.isMainFile()) {

								// Sets MAIN FILE to false
								MainWindow.getInstance()
										.getProjectConfiguration().getFileAt(j)
										.setIsMainFile(false);

								// Sets COMPILABLE FILE to false
								MainWindow.getInstance()
										.getProjectConfiguration().getFileAt(j)
										.setIsCompilableFile(false);

								for (int position = 0; position < MainWindow.getInstance()
										.getEditorManager().getNumEditors(); position++) {

									if (MainWindow
											.getInstance()
											.getEditorManager()
											.getEditorAt(position)
											.getAbsolutePath()
											.equals(
													MainWindow
															.getInstance()
															.getProjectConfiguration()
															.getFileAt(j)
															.getPath()))

										// REMOVE THE ICON FROM THE TAB
										MainWindow.getInstance()
												.getEditorManager().getPane()
												.setIconAt(position, null);
								}
							}
						}

						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsMainFile(true);
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsCompilableFile(true);
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);

						// Puts the tab icon
						MainWindow
								.getInstance()
								.getEditorManager()
								.getPane()
								.setIconAt(
										MainWindow.getInstance()
												.getEditorManager()
												.getSelectedEditorIndex(),
										new ImageIcon(
												"./resources/icons/editor/main.PNG"));
					}
				}
			}
		}
	}

	/**
	 * Unsets the file in the editor as a main file
	 */
	public void unsetMainFile() {

		// If it is MAIN FILE
		if (MainWindow.getInstance().getEditorManager().getSelectedEditor()
				.isMainFile()) {

			// Sets the MAIN FILE as false
			MainWindow.getInstance().getEditorManager().getSelectedEditor()
					.setMainFile(false);

			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setMessage(
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditor().getAbsolutePath());
			
			// Quits the tab icon
			MainWindow.getInstance().getEditorManager().getPane().setIconAt(
					MainWindow.getInstance().getEditorManager()
							.getSelectedEditorIndex(), null);

			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration().isDefaultProject()) {

				// The project has been modified
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);

				// Searches for the file into the project configuration file list
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// If exists
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorManager()
											.getSelectedEditor()
											.getAbsolutePath()))
						// Sets MAIN FILE as false
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsMainFile(false);
				}
			}
		}
	}
}
