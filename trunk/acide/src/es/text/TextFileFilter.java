package es.text;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import javax.swing.filechooser.FileFilter;

import language.Language;

/**
 * 
 */
public class TextFileFilter extends FileFilter {

	/**
	 * 
	 */
	private String _description;
	/**
	 * 
	 */
	private List<String> _extensions;

	/**
	 * Constructor of the class.
	 * 
	 * @param description
	 */
	public TextFileFilter(String description) {
		ResourceBundle labels = Language.getInstance().getLabels();
		if (description == null) {
			throw new NullPointerException(labels.getString("s312"));
		}
		_description = description;
		_extensions = new ArrayList<String>();
	}

	/**
	 * 
	 */
	public boolean accept(File file) {
		if (file.isDirectory() || _extensions.size() == 0) {
			return true;
		}
		String fileName = file.getName().toLowerCase();
		for (String extension : _extensions) {
			if (fileName.endsWith(extension)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * 
	 */
	public String getDescription() {
		StringBuffer buffer = new StringBuffer(_description);
		buffer.append(" (");
		for (String extension : _extensions) {
			buffer.append(extension).append(" ");
		}
		return buffer.append(")").toString();
	}

	/**
	 * 
	 * @param description
	 */
	public void setDescription(String description) {
		ResourceBundle labels = Language.getInstance().getLabels();
		if (description == null) {
			throw new NullPointerException(labels.getString("s313"));
		}
		this._description = description;
	}

	/**
	 * 
	 * @param extension
	 */
	public void addExtension(String extension) {
		ResourceBundle labels = Language.getInstance().getLabels();
		if (extension == null) {
			throw new NullPointerException(labels.getString("s314"));
		}
		_extensions.add(extension.toLowerCase());
	}

	/**
	 * 
	 * @param extension
	 */
	public void removeExtension(String extension) {
		_extensions.remove(extension);
	}

	/**
	 * 
	 */
	public void clearExtensions() {
		_extensions.clear();
	}

	/**
	 * 
	 * @return
	 */
	public List<String> getExtensions() {
		return _extensions;
	}
}
