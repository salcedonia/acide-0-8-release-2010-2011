package es.text;

import java.util.StringTokenizer;

import operations.lexicon.ObjectList;

/**
 * Handle the valid extensions of the files for the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ValidExtensions implements java.io.Serializable {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * List of extensions.
	 */
	private ObjectList _list;
	/**
	 * Instance of the class.
	 */
	private static ValidExtensions _instance;

	/**
	 * Constructor of the class.
	 */
	public ValidExtensions() {
		super();
		_list = new ObjectList();
	}

	/**
	 * Load a new valid extension set.
	 * 
	 * @param extensions
	 *            New value to set.
	 */
	public void load(ValidExtensions extensions) {
		_instance = extensions;
	}

	/**
	 * Returns the extension a the position <b>POS</b> in the list.
	 * 
	 * @param pos
	 *            Position.
	 * 
	 * @return the extension a the position <b>POS</b> in the list.
	 */
	public Object getExtensionAt(int pos) {
		return _list.getObject(pos);
	}

	/**
	 * Set the new extension for the value at the position <b>POS</b> at the
	 * list.
	 * 
	 * @param element
	 *            New value to set.
	 */
	public void setExtensionAt(Object element) {
		_list.insert(_list.size(), element);
	}

	/**
	 * Return the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public static ValidExtensions getInstance() {
		if (_instance == null)
			_instance = new ValidExtensions();
		return _instance;
	}

	/**
	 * Check if the extension given as a parameter is a valid or invalid
	 * extension, returning true in for the first case and false in another.
	 * 
	 * @param extension
	 *            Extension to check.
	 * 
	 * @return True if it is valid extension and false in other case.
	 */
	public boolean isValidExtension(String extension) {

		for (int i = 0; i < _list.size(); i++)
			if (extension.endsWith((String) _list.getObject(i)))
				return true;

		return false;
	}

	/**
	 * Convert the list of extensions into different tokens and save them into a
	 * string separated by commas.
	 * 
	 * @param string
	 *            String to save.
	 */
	public void tokenizeExtensions(String string) {

		StringTokenizer tokens = new StringTokenizer(string, ",");

		for (int i = 0; i < tokens.countTokens(); i++)
			setExtensionAt(tokens.nextToken());
	}
}
