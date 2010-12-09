package gui.fileEditor.fileEditorManager;

import gui.fileEditor.fileEditorManager.listeners.AcideFileEditorManagerChangeListener;
import gui.fileEditor.fileEditorManager.listeners.AcideFileEditorManagerMouseClickListener;
import gui.fileEditor.fileEditorManager.utils.gui.DragAndDropTabbedPane;
import gui.fileEditor.fileEditorManager.utils.logic.testPlaf.TestPlaf;
import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.fileEditor.fileEditorPanel.popup.AcideEditorPopupMenu;
import gui.mainWindow.MainWindow;

import javax.swing.*;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;


import java.io.File;
import java.util.ResourceBundle;

/************************************************************************
 * Handles the creation and destruction of the different editor tabs of ACIDE -
 * A Configurable IDE.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
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
public class AcideFileEditorManager {

	/**
	 * Path where all the resources of the application are.
	 */
	private static final String RESOURCE_PATH = "./resources/icons/editor/";
	/**
	 * TabbedPane for the editor.
	 */
	private DragAndDropTabbedPane _tabbedPane;
	/**
	 * TestPlaf for the editor.
	 */
	private TestPlaf _testPlaf;
	/**
	 * Editor panel popup menu.
	 */
	private AcideEditorPopupMenu _popupMenu;

	/**
	 * Creates a new file editor manager.
	 */
	public AcideFileEditorManager() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();

		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		try {

			_tabbedPane = new DragAndDropTabbedPane();
			_testPlaf = new TestPlaf();
			_tabbedPane.setUI(_testPlaf);
			_tabbedPane.addMouseListener(new AcideFileEditorManagerMouseClickListener());
			_tabbedPane.addChangeListener(new AcideFileEditorManagerChangeListener());

			// POPUP
			buildPopupMenu();

		} catch (RuntimeException exception) {

			// Updates the log
			AcideLog.getLog().info(labels.getString("s315"));
			exception.printStackTrace();
		}
	}

	/**
	 * Builds the file editor manager popup menu.
	 */
	public void buildPopupMenu() {
		_popupMenu = new AcideEditorPopupMenu();
	}

	/**
	 * Creates a new file editor panel tab with the name, tool tip and type specified.
	 * 
	 * @param name
	 *            name of the tab.
	 * @param toolTip
	 *            tool tip for the tab.
	 * @param type
	 *            type of the file.
	 */
	public void newAcideFileEditorPanel(String name, String toolTip, int type) {

		AcideFileEditorPanel editorPanel = new AcideFileEditorPanel();

		switch (type) {
		case 0:
			_tabbedPane.addTab(name, null, editorPanel, toolTip);
			editorPanel.setIcon(null);
			break;

		case 1:
			_tabbedPane.addTab(name, new ImageIcon(RESOURCE_PATH + "main.PNG"),
					editorPanel, toolTip);
			editorPanel.setIcon(new ImageIcon(RESOURCE_PATH + "main.PNG"));
			break;

		case 2:
			_tabbedPane.addTab(name, new ImageIcon(RESOURCE_PATH
					+ "compilable.PNG"), editorPanel, toolTip);
			new ImageIcon(RESOURCE_PATH + "compilable.PNG");
			break;
		}

		// Adds the popup menu
		_tabbedPane.addMouseListener(new AcideFileEditorManagerMouseClickListener());
		
		// The new tab is the selected one
		_tabbedPane.setSelectedIndex(_tabbedPane.getTabCount() - 1);
	}

	/**
	 * Closes a tab in the position at the list given as a parameter.
	 * 
	 * @param pos
	 *            tab position to be closed.
	 */
	public void removeTab(int pos) {
		_testPlaf.getCloseButtonAt(pos).doClick();
	}

	/**
	 * Returns the file editor panel at the position of the list given as a parameter.
	 * 
	 * @param pos
	 *            position of the editor to return.
	 * 
	 * @return the editor at the position of the list given as a parameter.
	 */
	public AcideFileEditorPanel getFileEditorPanelAt(int pos) {
		if ((pos < _tabbedPane.getComponentCount()) && (pos >= 0)) {
			return (AcideFileEditorPanel) _tabbedPane.getComponentAt(pos);
		} else {
			return null;
		}
	}

	/**
	 * Returns the number of the tabbed pane file editor panels.
	 * 
	 * @return the number of the tabbed pane file editor panels.
	 */
	public int getNumFileEditorPanels() {
		return _tabbedPane.getTabCount();
	}

	/**
	 * Creates a new tab in the tabbedPane.
	 * 
	 * @param path
	 *            path of the file to open.
	 * @param textContent
	 *            text type.
	 * @param text
	 *            content of the text to display.
	 * @param modifiable
	 *            indicates if the editor is modifiable or not.
	 */
	public void newTab(String path, String textContent, String text,
			boolean modifiable, int type) {

		// Checks if the file is already opened
		int position = -1;

		for (int index = 0; index < getNumFileEditorPanels(); index++)
			if (getFileEditorPanelAt(index).getAbsolutePath() == textContent)
				position = index;

		// if it is not opened yet
		if (position == -1) {

			File file = new File(textContent);

			// Gets the name
			int lastIndexOfSlash = path.lastIndexOf("\\");
			if (lastIndexOfSlash == -1)
				lastIndexOfSlash = path.lastIndexOf("/");
			String fileName = path.substring(lastIndexOfSlash + 1);

			// Creates the new text editor internal panel
			newAcideFileEditorPanel(fileName, textContent, type);

			// Starts from the last opened editor
			int posEditor = getNumFileEditorPanels() - 1;
			getFileEditorPanelAt(posEditor).loadText(text);
			getFileEditorPanelAt(posEditor).setEditable(modifiable);
			getTabbedPane().setSelectedIndex(posEditor);
			getFileEditorPanelAt(posEditor).setAbsolutePath(textContent);
			getFileEditorPanelAt(posEditor).setLastChange(file.lastModified());
			getFileEditorPanelAt(posEditor).setLastSize(file.length());

		} else {

			// If it is already opened, sets the focus on it
			setSelectedFileEditorPanelAt(position);
		}
		
		// Sets the caret position in the first position of the text pane
		getSelectedFileEditorPanel().getActiveTextEditionArea()
				.setCaretPosition(0);
	}

	/**
	 * Sets the selected file editor panel at the position given as a parameter.
	 * 
	 * @param position
	 *            position to select.
	 */
	public void setSelectedFileEditorPanelAt(int position) {
		_tabbedPane.setSelectedIndex(position);
	}

	/**
	 * Returns the the selected file editor panel index.
	 * 
	 * @return the the selected file editor panel index.
	 */
	public int getSelectedFileEditorPanelIndex() {
		return getTabbedPane().getSelectedIndex();
	}

	/**
	 * Returns the selected file editor panel.
	 * 
	 * @return the selected file editor panel.
	 */
	public AcideFileEditorPanel getSelectedFileEditorPanel() {
		return getFileEditorPanelAt(getTabbedPane().getSelectedIndex());
	}

	/**
	 * Returns the editor builder tabbedPane.
	 * 
	 * @return the editor builder tabbedPane.
	 */
	public DragAndDropTabbedPane getTabbedPane() {
		return _tabbedPane;
	}

	/**
	 * Returns the popup menu.
	 * 
	 * @return the popup menu.
	 */
	public AcideEditorPopupMenu getPopupMenu() {
		return _popupMenu;
	}

	/**
	 * Returns the editor marked as Main File.
	 * 
	 * @return the main editor marked as Main File.
	 */
	public AcideFileEditorPanel getMainEditor() {

		for (int i = 0; i < getNumFileEditorPanels(); i++) {

			if (getFileEditorPanelAt(i).isMainFile())
				return getFileEditorPanelAt(i);
		}

		return null;
	}

	/**
	 * Returns the TestPlaf of the tabbed panel.
	 * 
	 * @return the TestPlaf of the tabbed panel.
	 * @see TestPlaf
	 */
	public TestPlaf getTestPlaf() {
		return _testPlaf;
	}

	/**
	 * Sets the close button to green.
	 */
	public void setGreenButton() {
		MainWindow
				.getInstance()
				.getFileEditorManager()
				.getTestPlaf()
				.getCloseButtonAt(
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex())
				.setGreenCloseButton();
	}

	/**
	 * Returns true if the button is red and false in the other case.
	 * 
	 * @return true if the button is red and false in the other case.
	 */
	public boolean isRedButton() {
		return MainWindow
				.getInstance()
				.getFileEditorManager()
				.getTestPlaf()
				.getCloseButtonAt(
						MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex()).isRedButton();
	}

	/**
	 * Sets the green button of the editor at the list position given as a
	 * parameter.
	 * 
	 * @param position
	 *            list position of the button.
	 */
	public void setGreenButtonAt(int position) {
		MainWindow.getInstance().getFileEditorManager().getTestPlaf()
				.getCloseButtonAt(position).setGreenCloseButton();
	}

	/**
	 * Returns true if the button is red and false in the other case.
	 * 
	 * @param position
	 *            list position of the button.
	 * 
	 * @return True if the button is red and false in the other case.
	 */
	public boolean isRedButton(int position) {
		return MainWindow.getInstance().getFileEditorManager().getTestPlaf()
				.getCloseButtonAt(position).isRedButton();
	}

	/**
	 * Sets the file in the editor as compilable.
	 */
	public void setCompilableFile() {

		if (!MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
				.isCompilerFile()
				|| (MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isCompilerFile() && MainWindow
						.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
						.isMainFile())) {

			// Default project
			if (MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// Sets the file as compiled
				MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
						.setCompilerFile(true);

				// If it is already a MAIN FILE
				if (MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile())
					// Removes the main file property
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setMainFile(false);

				// Searches for the file into the project configuration file
				// list
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// If Exists
					if (MainWindow
							.getInstance()
							.getProjectConfiguration()
							.getFileAt(i)
							.getPath()
							.equals(MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getAbsolutePath()))
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
						.getFileEditorManager()
						.getTabbedPane()
						.setIconAt(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex(),
								new ImageIcon(
										"./resources/icons/editor/compilable.PNG"));

				// Updates the status bar
				MainWindow
						.getInstance()
						.getStatusBar()
						.setMessage(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel().getAbsolutePath()
										+ " <COMPILABLE>");
			} else {

				// Not default project

				MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
						.setCompilerFile(true);

				if (MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile())
					MainWindow.getInstance().getFileEditorManager()
							.getSelectedFileEditorPanel().setMainFile(false);

				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);

				// Updates the status bar
				MainWindow
						.getInstance()
						.getStatusBar()
						.setMessage(
								MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel().getAbsolutePath()
										+ " <COMPILABLE>");

				// Search for the file into the project configuration file list
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// If exists
					if (MainWindow
							.getInstance()
							.getProjectConfiguration()
							.getFileAt(i)
							.getPath()
							.equals(MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getAbsolutePath())) {

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
								.getFileEditorManager()
								.getTabbedPane()
								.setIconAt(
										MainWindow.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex(),
										new ImageIcon(
												"./resources/icons/editor/compilable.PNG"));
					}
				}
			}
		}
	}

	/**
	 * Unsets the file in the editor as compilable.
	 */
	public void unsetCompilableFile() {

		// If it is COMPILABLE FILE and not MAIN FILE
		if (MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
				.isCompilerFile()
				&& !MainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().isMainFile()) {

			// Sets COMPILER FILE to false
			MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
					.setCompilerFile(false);

			// Updates the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setMessage(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getAbsolutePath());

			// Quits the icon tab
			MainWindow
					.getInstance()
					.getFileEditorManager()
					.getTabbedPane()
					.setIconAt(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanelIndex(), null);

			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// The project has been modified
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);

				// Searches for the file into the project configuration file
				// list
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					if (MainWindow
							.getInstance()
							.getProjectConfiguration()
							.getFileAt(i)
							.getPath()
							.equals(MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getAbsolutePath()))
						// Sets the COMPILABLE FILE as false
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsCompilableFile(false);
				}
			}
		}
	}

	/**
	 * Sets a file of the editor as a main file.
	 */
	public void setMainFile() {

		// IF it is not MAIN FILE
		if (!MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
				.isMainFile()) {

			// Removes the previous MAIN FILE
			for (int i = 0; i < MainWindow.getInstance().getFileEditorManager()
					.getNumFileEditorPanels(); i++) {

				// Finds the previous MAIN FILE
				if (MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(i)
						.isMainFile()) {

					// Sets MAIN FILE as false
					MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(i)
							.setMainFile(false);

					// Sets COMPILER FILE as false
					MainWindow.getInstance().getFileEditorManager().getFileEditorPanelAt(i)
							.setCompilerFile(false);

					// Updates the status bar
					MainWindow
							.getInstance()
							.getStatusBar()
							.setMessage(
									MainWindow.getInstance().getFileEditorManager()
											.getFileEditorPanelAt(i).getAbsolutePath());

					// Removes the tab icon
					MainWindow.getInstance().getFileEditorManager().getTabbedPane()
							.setIconAt(i, null);
				}
			}

			// Sets MAIN FILE as true
			MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
					.setMainFile(true);

			// Sets COMPILER FILE as true
			MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
					.setCompilerFile(true);

			// Updates the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setMessage(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getAbsolutePath()
									+ " <MAIN>");

			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// Updates the file into the project configuration
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// If exists
					if (MainWindow
							.getInstance()
							.getProjectConfiguration()
							.getFileAt(i)
							.getPath()
							.equals(MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getAbsolutePath())) {

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

								for (int position = 0; position < MainWindow
										.getInstance().getFileEditorManager()
										.getNumFileEditorPanels(); position++) {

									if (MainWindow
											.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(position)
											.getAbsolutePath()
											.equals(MainWindow.getInstance()
													.getProjectConfiguration()
													.getFileAt(j).getPath()))

										// Removes the icon from the tab
										MainWindow.getInstance()
												.getFileEditorManager()
												.getTabbedPane()
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
								.getFileEditorManager()
								.getTabbedPane()
								.setIconAt(
										MainWindow.getInstance()
												.getFileEditorManager()
												.getSelectedFileEditorPanelIndex(),
										new ImageIcon(
												"./resources/icons/editor/main.PNG"));
					}
				}
			}
		}
	}

	/**
	 * Unsets the file in the editor as a main file.
	 */
	public void unsetMainFile() {

		// If it is MAIN FILE
		if (MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
				.isMainFile()) {

			// Sets the MAIN FILE as false
			MainWindow.getInstance().getFileEditorManager().getSelectedFileEditorPanel()
					.setMainFile(false);

			// Updates the status bar
			MainWindow
					.getInstance()
					.getStatusBar()
					.setMessage(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getAbsolutePath());

			// Quits the tab icon
			MainWindow
					.getInstance()
					.getFileEditorManager()
					.getTabbedPane()
					.setIconAt(
							MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanelIndex(), null);

			// Not default project
			if (!MainWindow.getInstance().getProjectConfiguration()
					.isDefaultProject()) {

				// The project has been modified
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);

				// Searches for the file into the project configuration file
				// list
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// If exists
					if (MainWindow
							.getInstance()
							.getProjectConfiguration()
							.getFileAt(i)
							.getPath()
							.equals(MainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanel().getAbsolutePath()))
						// Sets MAIN FILE as false
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsMainFile(false);
				}
			}
		}
	}

	/**
	 * Updates the button icons of the selected file editor in the tabbed pane.
	 */
	public void updatesButtonIcons() {

		if (MainWindow.getInstance().getFileEditorManager()
				.getNumFileEditorPanels() > 0) {

			// Gets the selected editor index
			int selectedEditorIndex = MainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanelIndex();

			// Gets the current image icon
			ImageIcon icon = (ImageIcon) MainWindow.getInstance()
					.getFileEditorManager().getTabbedPane()
					.getIconAt(selectedEditorIndex);

			// Sets the current image icon
			MainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().setIcon(icon);
		}
	}
}
