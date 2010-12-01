package es.text;

import gui.mainWindow.MainWindow;

import javax.swing.*;

import operations.log.AcideLog;

import resources.ResourceManager;

import java.io.*;
import java.util.ResourceBundle;

import language.AcideLanguage;

/************************************************************************																
 * Handles the text files of ACIDE - A Configurable IDE.
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
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
public class TextFile {

	/**
	 * File chooser.
	 */
	private JFileChooser _fileChooser;

	/**
	 * Creates a new text file.
	 */
	public TextFile() {
		_fileChooser = new JFileChooser();
	}

	/**
	 * Returns the text file name.
	 * 
	 * @return the text file name.
	 */
	public String read() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		String fileName = null;
		String text = null;
		File file = null;

		try {
			text = ResourceManager.getInstance().getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setDialogTitle(labels.getString("s9"));
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		int value = _fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {

			fileName = _fileChooser.getSelectedFile().getAbsolutePath();
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s300") + fileName);
			
			// Updates the RESOURCE MANAGER
			ResourceManager.getInstance().setProperty("defaultPath", fileName);

		} else if (value == JFileChooser.CANCEL_OPTION) {
			
			_fileChooser.cancelSelection();
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s302"));
		}
		return fileName;

	}

	/**
	 * Returns the text file path selected in a file chooser.
	 * 
	 * @return the text file path selected in a file chooser.
	 */
	public String readPath() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		String path = " ";
		String text = null;
		File file = null;

		try {
			text = ResourceManager.getInstance().getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}
		_fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		int value = _fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {
			path = _fileChooser.getSelectedFile().getAbsolutePath();
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s303") + path);
			
			// Updates the RESOURCE MANAGER
			ResourceManager.getInstance().setProperty("defaultPath", path);
		} else if (value == JFileChooser.CANCEL_OPTION) {
			
			_fileChooser.cancelSelection();
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s304"));
		}
		return path;
	}

	/**
	 * Returns the text file name.
	 * 
	 * @param filter
	 *            filter for the text files.
	 * 
	 * @return the text file name.
	 */
	public String read(TextFileFilter filter) {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		JFileChooser fileChooser = new JFileChooser();
		String fileName = " ";
		String text = null;
		File file = null;
		try {
			text = ResourceManager.getInstance().getProperty("defaultPath");
			file = new File(text);
			fileChooser.setFileFilter(filter);
			fileChooser.setCurrentDirectory(file.getParentFile());
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		int value = fileChooser.showOpenDialog(null);
		if (value == JFileChooser.APPROVE_OPTION) {

			fileName = fileChooser.getSelectedFile().getAbsolutePath();
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s305") + fileName);
			
			// Updates the RESOURCE MANAGER
			ResourceManager.getInstance().setProperty("defaultPath", fileName);
		} else if (value == JFileChooser.CANCEL_OPTION) {
			
			fileChooser.cancelSelection();
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s306"));
		}
		return fileName;
	}

	/**
	 * Writes on a text file
	 * 
	 * @return the absolute file path
	 */
	public String write() {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		String absoluteFilePath = " ";
		String text = null;
		File file = null;
		
		try {
			text = ResourceManager.getInstance().getProperty("defaultPath");
			file = new File(text);
			_fileChooser.setCurrentDirectory(file);
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		boolean isApproved = true;

		int value = _fileChooser.showSaveDialog(_fileChooser);
		File selectedFile = _fileChooser.getSelectedFile();

		// Ask the user for saving it
		if (value == JFileChooser.APPROVE_OPTION) {
			
			// If exists
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

					if (MainWindow.getInstance().getMenu().isNPF()) {
						MainWindow.getInstance()
								.getFileEditorManager()
								.getTabbedPane()
								.remove(MainWindow.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanelIndex());
						MainWindow.getInstance().getFileEditorManager().getTabbedPane().validate();
					}
				}

			} else
				isApproved = true;
		} else if (value == JFileChooser.CANCEL_OPTION) {
			_fileChooser.cancelSelection();
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s308"));
			
			isApproved = false;

			if (MainWindow.getInstance().getMenu().isNPF()) {
				MainWindow.getInstance()
						.getFileEditorManager()
						.getTabbedPane()
						.remove(MainWindow.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanelIndex());
				MainWindow.getInstance().getFileEditorManager().getTabbedPane().validate();
			}
		}
		if (isApproved) {
			
			absoluteFilePath = _fileChooser.getSelectedFile().getAbsolutePath();
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s307") + absoluteFilePath);
			
			// Updates the RESOURCE MANAGER
			ResourceManager.getInstance().setProperty("defaultPath", absoluteFilePath);
		}

		return absoluteFilePath;
	}

	/**
	 * Loads the text file content into a string.
	 * 
	 * @param fileName
	 *            file name.
	 * @return the file content.
	 * @throws IOException.
	 */
	public String load(String fileName) {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		try {
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					new FileInputStream(fileName)));
			StringBuffer stringBuffer = new StringBuffer("");
			String string;

			while (true) {
				string = reader.readLine();
				if (string == null)
					break;
				stringBuffer.append(string + "\n");
			}
			reader.close();
			System.gc();
			return stringBuffer.toString();

		} catch (IOException exception) {
			
			// Updates the log
			AcideLog.getLog().error(labels.getString("s309") + fileName);
			exception.printStackTrace();
			
			return null;
		}
	}

	/**
	 * Saves the file content given as a parameter into a file given as a
	 * parameter as well.
	 * 
	 * @param file
	 *            file to save in.
	 * @param fileContent
	 *            file content to store into the file.
	 * 
	 * @return true if the operation was succeed and false in other case.
	 */
	public boolean save(String file, String fileContent) {

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		try {
			PrintWriter printerWriter = new PrintWriter(new BufferedWriter(
					new FileWriter(file)));
			printerWriter.print(fileContent);
			printerWriter.close();
			
			// Updates the log
			AcideLog.getLog().info(labels.getString("s310") + file);
			
			return true;
		} catch (IOException exception) {
			
			// Updates the log
			AcideLog.getLog().error(labels.getString("s311") + file);
			exception.printStackTrace();
				
			return false;
		}
	}

	/**
	 * Returns the file chooser.
	 * 
	 * @return the file chooser.
	 */
	public JFileChooser getFileChooser() {
		return _fileChooser;
	}
}
