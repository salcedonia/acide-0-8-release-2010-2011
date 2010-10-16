package es.text;

import java.io.File;
import javax.swing.filechooser.*;

/**
 * 
 */
public class ExtensionFilter extends FileFilter {

	/**
	 * 
	 */
	private String[] _extensions;
	/**
	 * 
	 */
	private String _description;

	/**
	 * Constructor of the class.
	 * 
	 * @param ext
	 */
	public ExtensionFilter(String ext) {
		
		this(new String[] { ext }, null);
	}

	/**
	 * Constructor of the class.
	 * 
	 * @param exts
	 * @param descr
	 */
	public ExtensionFilter(String[] exts, String descr) {
		_extensions = new String[exts.length];
		for (int i = exts.length - 1; i >= 0; i--) {
			_extensions[i] = exts[i].toLowerCase();
		}
		_description = (descr == null ? exts[0] + " files" : descr);
	}

	/**
	 * 
	 */
	public boolean accept(File f) {
		if (f.isDirectory()) {
			return true;
		}
		String name = f.getName().toLowerCase();
		for (int i = _extensions.length - 1; i >= 0; i--) {
			if (name.endsWith(_extensions[i])) {
				return true;
			}
		}
		return false;
	}

	/**
	 * 
	 */
	public String getDescription() {
		return _description;
	}
}
