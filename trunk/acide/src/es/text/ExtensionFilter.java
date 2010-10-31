package es.text;

import java.io.File;
import javax.swing.filechooser.*;

/**
 * Handle the extensions of the files for the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ExtensionFilter extends FileFilter {

	/**
	 * Valid extensions for the application.
	 */
	private String[] _extensions;
	/**
	 * Description of the extensions.
	 */
	private String _description;

	/**
	 * Constructor of the class.
	 * 
	 * @param extension
	 *            New extension.
	 */
	public ExtensionFilter(String extension) {

		this(new String[] { extension }, null);
	}

	/**
	 * Constructor of the class.
	 * 
	 * @param extensions
	 *            Extensions.
	 * @param description
	 *            Description.
	 */
	public ExtensionFilter(String[] extensions, String description) {
		_extensions = new String[extensions.length];
		for (int i = extensions.length - 1; i >= 0; i--) {
			_extensions[i] = extensions[i].toLowerCase();
		}
		_description = (description == null ? extensions[0] + " files"
				: description);
	}

	/**
	 * Returns true if the file contains a valid extension and false in other
	 * case.
	 * 
	 * @param file
	 *            File to check.
	 */
	public boolean accept(File file) {

		if (file.isDirectory()) {
			return true;
		}
		String name = file.getName().toLowerCase();

		for (int i = _extensions.length - 1; i >= 0; i--) {
			if (name.endsWith(_extensions[i])) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the description.
	 * 
	 * @return The description.
	 */
	public String getDescription() {
		return _description;
	}
}
