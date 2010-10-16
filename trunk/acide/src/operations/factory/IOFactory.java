package operations.factory;

import es.text.TextFile;

/**
 * 
 */
public class IOFactory {
	
	/**
	 * 
	 */
	private static IOFactory _instance;

	/**
	 * 
	 * @return
	 */
	public static IOFactory getInstance() {
		if (_instance == null)
			_instance = new IOFactory();
		return _instance;
	}

	/**
	 * 
	 * @return
	 */
	public TextFile buildFile() {
		return new TextFile();
	}
}
