package es.text;

import gui.MainWindow;

import javax.swing.*;

import language.Language;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

import java.io.*;
import java.util.ResourceBundle;

/**
 * Handle the text files of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class TextFile {

	/**
	 * Logger of the application.
	 */
	private Logger _logger = Log.getLog();
	/**
	 * File chooser to choose a file text.
	 */
	private JFileChooser _fileChooser;

	/**
	 * Constructor of the class.
	 */
	public TextFile() {

		_fileChooser = new JFileChooser();
	}

	/**
	 * Returns the name of a text file.
	 * 
	 * @return The name of a text file.
	 */
	public String read() {

		ResourceBundle labels = Language.getInstance().getLabels();
		String fileName = null;
		String text = null;
		File file = null;

		try {
			text = PropertiesManager.getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setDialogTitle(labels.getString("s9"));
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception e) {
			e.printStackTrace();
		}

		int value = _fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {

			fileName = _fileChooser.getSelectedFile().getAbsolutePath();
			_logger.info(labels.getString("s300") + fileName);
			PropertiesManager.setProperty("defaultPath", fileName);

		} else if (value == JFileChooser.CANCEL_OPTION) {
			_fileChooser.cancelSelection();
			_logger.info(labels.getString("s302"));
		}
		return fileName;

	}

	/**
	 * Read the path of a text file selected in a file chooser.
	 * 
	 * @return The path of a text file selected in a file chooser.
	 */
	public String readPath() {

		ResourceBundle labels = Language.getInstance().getLabels();

		String path = " ";
		String text = null;
		File file = null;

		try {
			text = PropertiesManager.getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception e) {
			e.printStackTrace();
		}
		_fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		int value = _fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {
			path = _fileChooser.getSelectedFile().getAbsolutePath();
			_logger.info(labels.getString("s303") + path);
			PropertiesManager.setProperty("defaultPath", path);
		} else if (value == JFileChooser.CANCEL_OPTION) {
			_fileChooser.cancelSelection();
			_logger.info(labels.getString("s304"));
		}
		return path;
	}

	/**
	 * Returns the name of a text file.
	 * 
	 * @param filter
	 *            Filter for the text files.
	 * 
	 * @return The name of a text file.
	 */
	public String read(TextFileFilter filter) {

		ResourceBundle labels = Language.getInstance().getLabels();
		JFileChooser sel = new JFileChooser();
		String fileName = " ";
		String text = null;
		File file = null;
		try {
			text = PropertiesManager.getProperty("defaultPath");
			file = new File(text);
			sel.setFileFilter(filter);
			sel.setCurrentDirectory(file.getParentFile());
		} catch (Exception e) {
			e.printStackTrace();
		}

		int value = sel.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {

			fileName = sel.getSelectedFile().getAbsolutePath();
			_logger.info(labels.getString("s305") + fileName);
			PropertiesManager.setProperty("defaultPath", fileName);
		} else if (value == JFileChooser.CANCEL_OPTION) {
			sel.cancelSelection();
			_logger.info(labels.getString("s306"));
		}
		return fileName;
	}

	/**
	 * Write
	 * 
	 * @return
	 */
	public String write() {

		ResourceBundle labels = Language.getInstance().getLabels();
		String fileName = " ";
		String text = null;
		File file = null;
		MainWindow mainWindow = MainWindow.getInstance();
		try {
			text = PropertiesManager.getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setCurrentDirectory(file);
		} catch (Exception e) {
			e.printStackTrace();
		}

		boolean isApproved = true;

		int value = _fileChooser.showSaveDialog(_fileChooser);
		File selectedFile = _fileChooser.getSelectedFile();

		if (value == JFileChooser.APPROVE_OPTION) {
			if (selectedFile.exists()
					&& _fileChooser.getDialogType() == JFileChooser.SAVE_DIALOG) {
				int result = JOptionPane.showConfirmDialog(null,
						labels.getString("s954"), labels.getString("s953"),
						JOptionPane.YES_NO_OPTION);
				if (result == JOptionPane.YES_OPTION) {
					isApproved = true;
				}
				if (result == JOptionPane.NO_OPTION) {
					isApproved = false;

					if (mainWindow.getMenu().isNPF()) {
						mainWindow
								.getEditorBuilder()
								.getPane()
								.remove(mainWindow.getEditorBuilder()
										.getSelectedEditorIndex());
						mainWindow.getEditorBuilder().getPane().validate();
					}
				}

			} else
				isApproved = true;
		} else if (value == JFileChooser.CANCEL_OPTION) {
			_fileChooser.cancelSelection();
			_logger.info(labels.getString("s308"));
			isApproved = false;

			if (mainWindow.getMenu().isNPF()) {
				mainWindow
						.getEditorBuilder()
						.getPane()
						.remove(mainWindow.getEditorBuilder()
								.getSelectedEditorIndex());
				mainWindow.getEditorBuilder().getPane().validate();
			}
		}
		if (isApproved) {
			fileName = _fileChooser.getSelectedFile().getAbsolutePath();
			_logger.info(labels.getString("s307") + fileName);
			PropertiesManager.setProperty("defaultPath", fileName);
		}

		return fileName;
	}

	/**
	 * Load the text file content into a string.
	 * 
	 * @param fileName
	 *            File name.
	 * 
	 * @return The file content.
	 * 
	 * @throws IOException
	 *             Just in case something wrong happens.
	 */
	public String load(String fileName) {

		ResourceBundle labels = Language.getInstance().getLabels();
		try {
			BufferedReader reader;
			reader = new BufferedReader(new InputStreamReader(
					new FileInputStream(fileName)));
			StringBuffer b = new StringBuffer("");
			String cad;

			while (true) {
				cad = reader.readLine();
				if (cad == null)
					break;
				b.append(cad + "\n");
			}
			reader.close();
			System.gc();
			return b.toString();

		} catch (IOException e) {
			e.printStackTrace();
			_logger.error(labels.getString("s309") + fileName);
			return null;
		}

	}

	/**
	 * Save the file content given as a parameter into a file given as a
	 * parameter as well.
	 * 
	 * @param file
	 *            File to save in.
	 * @param fileContent
	 *            File content to store into the file.
	 * 
	 * @return True if the operation was succeed and false in other case.
	 */
	public boolean save(String file, String fileContent) {

		ResourceBundle labels = Language.getInstance().getLabels();

		try {
			PrintWriter salida = new PrintWriter(new BufferedWriter(
					new FileWriter(file)));
			salida.print(fileContent);
			salida.close();
			_logger.info(labels.getString("s310") + file);
			return true;
		} catch (java.io.IOException e) {
			_logger.error(labels.getString("s311") + file);
			return false;
		}
	}

	/**
	 * 
	 * @return
	 */
	public JFileChooser getFileChooser() {
		return _fileChooser;
	}
}
