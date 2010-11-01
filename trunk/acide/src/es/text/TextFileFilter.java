package es.text;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import javax.swing.filechooser.FileFilter;

import properties.PropertiesManager;

import language.Language;

/**
 * Handle the text files filters of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class TextFileFilter extends FileFilter {

	/**
	 * Description.
	 */
	private String _description;
	/**
	 * Extensions.
	 */
	private List<String> _extensions;

	/**
	 * Constructor of the class.
	 * 
	 * @param description New description.
	 */
	public TextFileFilter(String description) {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager
					.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		if (description == null)
			throw new NullPointerException(labels.getString("s312"));
		
		_description = description;
		_extensions = new ArrayList<String>();
	}

	/**
	 * Returns true if the file contains a valid extension and false in other case.
	 * 
	 * @param file File to check.
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
	 * Returns the description.
	 * 
	 * @return The description.
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
	 * Set a new value for the description.
	 * 
	 * @param description New value to set.
	 */
	public void setDescription(String description) {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager
					.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		if (description == null) {
			throw new NullPointerException(labels.getString("s313"));
		}
		_description = description;
	}

	/**
	 * Add a new extension to the list.
	 * 
	 * @param extension New extension to add.
	 */
	public void addExtension(String extension) {
		
		// GET THE LANGUAGE
		Language language = Language.getInstance();
		try {
			language.getLanguage(PropertiesManager
					.getProperty("language"));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		// GET THE LABELS
		ResourceBundle labels = language.getLabels();
		
		if (extension == null) {
			throw new NullPointerException(labels.getString("s314"));
		}
		_extensions.add(extension.toLowerCase());
	}

	/** 
	 * Remove an extension from the list of available extensions.
	 * 
	 * @param extension Extension to remove.
	 */
	public void removeExtension(String extension) {
		_extensions.remove(extension);
	}

	/**
	 * Delete all the extensions.
	 */
	public void clearExtensions() {
		_extensions.clear();
	}

	/**
	 * Returns the list of extensions.
	 * 
	 * @return The list of extensions.
	 */
	public List<String> getExtensions() {
		return _extensions;
	}
}
