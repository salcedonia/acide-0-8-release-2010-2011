package operations.configuration;

import java.util.ArrayList;

import operations.factory.IOFactory;
import properties.PropertiesManager;
import es.text.TextFile;
import gui.MainWindow;

/**
 * Handle the default configuration of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class DefaultConfiguration {

	/**
	 * Flag the indicates if the explorer browser is showed or not.
	 */
	private boolean _isExplorerShowed;
	/**
	 * Flag the indicates if the shell is showed or not.
	 */
	private boolean _isShellShowed;
	/**
	 * Window width.
	 */
	private int _windowWidth;
	/**
	 * Window height.
	 */
	private int _windowHeight;
	/**
	 * Position X of the main window.
	 */
	private int _posX;
	/**
	 * Position Y of the main window.
	 */
	private int _posY;
	/**
	 * Position of the split pane of the output.
	 */
	private int _splitPaneHorizontalPosition;
	/**
	 * Position of the split pane of the explorer.
	 */
	private int _splitPaneVerticalPosition;
	/**
	 * Number of editors of the main window.
	 */
	private int _numEditor;
	/**
	 * Editors of the application.
	 */
	private ArrayList<String> _editors;

	/**
	 * Load the default configuration from a text file.
	 */
	public void load() {

		TextFile textFile = IOFactory.getInstance().buildFile();
		String fileContent = "";
		try {
			fileContent = textFile.load(PropertiesManager
					.getProperty("DefaultConfiguration"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		int cont = 0;
		int contf = 0;

		// EXPLORER
		contf = fileContent.indexOf("\n", cont);
		String cond = fileContent.substring(cont, contf);
		if (cond.equals("true"))
			_isExplorerShowed = true;
		else
			_isExplorerShowed = false;
		cont = contf + 1;

		// SHELL
		contf = fileContent.indexOf("\n", cont);
		cond = fileContent.substring(cont, contf);
		if (cond.equals("true"))
			_isShellShowed = true;
		else
			_isShellShowed = false;
		cont = contf + 1;

		// WINDOW WIDTH
		contf = fileContent.indexOf("\n", cont);
		cond = fileContent.substring(cont, contf);
		_windowWidth = Integer.parseInt(cond);
		cont = contf + 1;

		// WINDOW HEIGHT
		contf = fileContent.indexOf("\n", cont);
		cond = fileContent.substring(cont, contf);
		_windowHeight = Integer.parseInt(cond);
		cont = contf + 1;

		// POSITION X
		contf = fileContent.indexOf("\n", cont);
		cond = fileContent.substring(cont, contf);
		_posX = Integer.parseInt(cond);
		cont = contf + 1;

		// POSITION Y
		contf = fileContent.indexOf("\n", cont);
		cond = fileContent.substring(cont, contf);
		_posY = Integer.parseInt(cond);
		cont = contf + 1;

		// POSITION OF THE HORIZONTAL SPLIT PANE
		contf = fileContent.indexOf("\n", cont);
		cond = fileContent.substring(cont, contf);
		_splitPaneHorizontalPosition = Integer.parseInt(cond);
		cont = contf + 1;

		// POSITION OF THE VERTICAL SPLIT PANE
		contf = fileContent.indexOf("\n", cont);
		cond = fileContent.substring(cont, contf);
		_splitPaneVerticalPosition = Integer.parseInt(cond);
		cont = contf + 1;

		// NUMBER OF EDITORS
		contf = fileContent.indexOf("\n", cont);
		cond = fileContent.substring(cont, contf);
		_numEditor = Integer.parseInt(cond);
		cont = contf + 1;

		// EDITORS
		_editors = new ArrayList<String>(_numEditor);
		for (int i = 0; i < _numEditor; i++) {
			contf = fileContent.indexOf("\n", cont);
			cond = fileContent.substring(cont, contf);
			cont = contf + 1;
			_editors.add(cond);
		}
	}

	/**
	 * Save the default configuration in a text file.
	 */
	public void save() {

		String fileContent = "";

		// IS EXPLORER SHOWED
		if (MainWindow.getInstance().getMenu().getView().getShowBrowser()
				.isSelected())
			_isExplorerShowed = true;
		else
			_isExplorerShowed = false;

		fileContent = fileContent + Boolean.toString(_isExplorerShowed) + "\n";

		// IS SHELL SHOWED
		if (MainWindow.getInstance().getMenu().getView().getShowShellWindow()
				.isSelected())
			_isShellShowed = true;
		else
			_isShellShowed = false;
		fileContent = fileContent + Boolean.toString(_isShellShowed) + "\n";

		// WINDOW WIDTH
		fileContent = fileContent
				+ Integer.toString(MainWindow.getInstance().getWidth()) + "\n";

		// WINDOW HEIGHT
		fileContent = fileContent
				+ Integer.toString(MainWindow.getInstance().getHeight()) + "\n";

		// POS X
		fileContent = fileContent
				+ Integer.toString(MainWindow.getInstance().getX()) + "\n";

		// POS Y
		fileContent = fileContent
				+ Integer.toString(MainWindow.getInstance().getY()) + "\n";

		// EXPLORER WIDTH
		fileContent = fileContent
				+ Integer.toString(MainWindow.getInstance().getExplorer()
						.getWidth()) + "\n";

		// OUTPUT HEIGHT
		fileContent = fileContent
				+ Integer.toString(MainWindow.getInstance().getOutput()
						.getHeight()) + "\n";

		// NUM ASSOCIATED FILES TO THE PROJECT
		fileContent = fileContent
				+ Integer.toString(MainWindow.getInstance().getEditorBuilder()
						.getNumEditors()) + "\n";

		// ADD EACH ONE OF THE FILES
		for (int i = 0; i < MainWindow.getInstance().getEditorBuilder()
				.getNumEditors(); i++) {

			fileContent = fileContent
					+ MainWindow.getInstance().getEditorBuilder()
							.getEditorAt(i).getAbsolutePath() + "\n";
		}

		// SAVE IT IN THE CONFIGURATION FILE
		TextFile file = IOFactory.getInstance().buildFile();
		file.save("./configuration/defaultConfiguration.acide", fileContent);
	}

	/**
	 * Return the window height.
	 * 
	 * @return The window height.
	 */
	public int getWindowHeight() {
		return _windowHeight;
	}

	/**
	 * Set a new value to the window height.
	 * 
	 * @param windowHeight
	 *            New value to set.
	 */
	public void setHeightWindow(int windowHeight) {
		_windowHeight = windowHeight;
	}

	/**
	 * Get the number of editors.
	 * 
	 * @return The number of editors.
	 */
	public int getNumEditor() {
		return _numEditor;
	}

	/**
	 * Set a new value to the number of editors.
	 * 
	 * @param numEditor
	 *            New value to set.
	 */
	public void setNumEditor(int numEditor) {
		_numEditor = numEditor;
	}

	/**
	 * Get the position X of the window.
	 * 
	 * @return The position X of the window.
	 */
	public int getPosX() {
		return _posX;
	}

	/**
	 * Set a new value to the position X of the window.
	 * 
	 * @param posX
	 *            New value to set.
	 */
	public void setPosX(int posX) {
		_posX = posX;
	}

	/**
	 * Get the position Y of the window.
	 * 
	 * @return The position Y of the window.
	 */
	public int getPosY() {
		return _posY;
	}

	/**
	 * Set a new value to the position Y of the window.
	 * 
	 * @param posY
	 *            New value to set.
	 */
	public void setPosY(int posY) {
		_posY = posY;
	}

	/**
	 * Return true if the shell is showed and false in other case.
	 * 
	 * @return True if the shell is showed and false in other case.
	 */
	public boolean isShellShowed() {
		return _isShellShowed;
	}

	/**
	 * Set a new value to the shellShowed.
	 * 
	 * @param shell
	 *            New value to set.
	 */
	public void setShell(boolean shell) {
		_isShellShowed = shell;
	}

	/**
	 * Get the window width.
	 * 
	 * @return The window width.
	 */
	public int getWindowWidth() {
		return _windowWidth;
	}

	/**
	 * Set a new value to the window width.
	 * 
	 * @param windowWidth
	 *            New value to set.
	 */
	public void setWidthWindow(int windowWidth) {
		_windowWidth = windowWidth;
	}

	/**
	 * Returns the position of the horizontal split pane.
	 * 
	 * @return The position of the horizontal split pane.
	 */
	public int getSplitPaneHorizontalPosition() {

		return _splitPaneHorizontalPosition;
	}

	/**
	 * Returns the position of the vertical split pane.
	 * 
	 * @return The position of the vertical split pane.
	 */
	public int getSplitPaneVerticalPosition() {
		return _splitPaneVerticalPosition;
	}

	/**
	 * Set a new value to the splitPaneHorizontalPosition.
	 * 
	 * @param splitPaneHorizontalPosition
	 *            New value to set.
	 */
	public void setSplitPaneHorizontalPosition(int splitPaneHorizontalPosition) {

		_splitPaneHorizontalPosition = splitPaneHorizontalPosition;
	}

	/**
	 * Set a new value to the splitPaneVerticalPosition.
	 * 
	 * @param splitPaneVerticalPosition
	 *            New value to set.
	 */
	public void setSplitPaneVerticalPosition(int splitPaneVerticalPosition) {

		_splitPaneVerticalPosition = splitPaneVerticalPosition;
	}
}