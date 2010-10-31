package operations.factory;

import es.text.TextFile;

/**
 * Build the most relevant Input/Output objects of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class IOFactory {
	
	/**
	 * Instance of the class.
	 */
	private static IOFactory _instance;

	/**
	 * Returns the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public static IOFactory getInstance() {
		if (_instance == null)
			_instance = new IOFactory();
		return _instance;
	}

	/**
	 * Build a text file for the application.
	 * 
	 * @return A text file for the application.
	 */
	public TextFile buildFile() {
		return new TextFile();
	}
}
