/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package acide.process.parser;

import java.io.File;

import javax.swing.JOptionPane;

import acide.files.AcideFileManager;
import acide.files.bytes.AcideByteFileManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.process.parser.gui.AcideProgressWindow;
import acide.resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE grammar file creation process.
 * 
 * @version 0.8
 * @see Thread
 */
public class AcideGrammarFileCreationProcess extends Thread {

	/**
	 * ACIDE - A Configurable IDE grammar file creation process grammar name.
	 */
	private String _grammarName;
	/**
	 * ACIDE - A Configurable IDE grammar file creation process verbose process.
	 */
	private boolean _verboseProcess;

	/**
	 * Creates a new ACIDE - A Configurable IDE grammar file creation process.
	 * 
	 * @param grammarName
	 *            grammar name.
	 * @param verboseProcess
	 *            verbose process flag.
	 */
	public AcideGrammarFileCreationProcess(String grammarName,
			boolean verboseProcess) {
		_grammarName = grammarName;
		_verboseProcess = verboseProcess;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Thread#run()
	 */
	@Override
	public void run() {

		// If the verbose flag is true then
		if (_verboseProcess)
			// Shows the progress window
			AcideProgressWindow.getInstance().showWindow();

		// Executes the antlr to obtain the .java files from the grammar
		executeAntlr();

		// Modifies the generated GrammarParser.java
		modifyGrammarParserFile();

		// Compiles the generated files to obtain the .class files
		compileGeneratedFiles();

		// Reallocates the generated files in the correspondent folder
		reallocateGeneratedFiles();

		// Generates the .jar file
		generateJarFile();

		// Deletes the generated files
		deleteGeneratedFiles();

		// Reallocates the generated .jar file into the correspondent folder
		reallocateJarFile();

		// If the verbose flag is true then
		if (_verboseProcess)
			// Enables the close button in the progress window
			AcideProgressWindow.getInstance().enableCloseButton();
		else
			// Success message
			JOptionPane.showMessageDialog(
					AcideMainWindow.getInstance(),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s1065"), AcideLanguageManager
							.getInstance().getLabels().getString("s1066"),
					JOptionPane.INFORMATION_MESSAGE);
	}

	/**
	 * Executes ANTLR for generating the required files to generate the .jar
	 * file.
	 */
	private void executeAntlr() {

		String javaPath = null;

		try {

			// Gets the java path from the ACIDE - A Configurable IDE resource
			// manager
			javaPath = AcideResourceManager.getInstance().getProperty(
					"javaPath");
			if (javaPath.equals("null"))
				throw new Exception(AcideLanguageManager.getInstance()
						.getLabels().getString("s927"));
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s928"),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s934"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Closes the progress window
			AcideProgressWindow.getInstance().closeWindow();

			return;
		}

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1049"));

		// Updates the progress window
		AcideProgressWindow
				.getInstance()
				.setText(
						"\"" + javaPath
								+ "\" -cp ./lib/antlr.jar antlr.Tool grammar.g");

		// Executes antlr to generate the files which will be at the .jar
		Process process = null;

		try {

			// Executes the command
			process = Runtime.getRuntime().exec(
					"\"" + javaPath
							+ "\" -cp ./lib/antlr.jar antlr.Tool grammar.g");

			// Waits for the process to finish
			process.waitFor();
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Closes the progress window
			AcideProgressWindow.getInstance().closeWindow();

			return;
		}

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1050"));
	}

	/**
	 * Adds the exceptions to the generated GrammarParser.java file.
	 */
	private void modifyGrammarParserFile() {

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1051"));

		File file = new File("GrammarParser.java");
		if (!file.exists()) {
			return;
		}

		// Gets the file content
		String fileContent = null;
		fileContent = AcideFileManager.getInstance().load("GrammarParser.java");
		String exception = "throw new RecognitionException();";

		// Inserts the first exception
		String aux = "";
		int index = fileContent.indexOf("recover(ex");
		index++;
		aux = fileContent.substring(index);
		int indexAux = aux.indexOf(";");
		index += indexAux + 1;
		String head = fileContent.substring(0, index);
		String tail = fileContent.substring(index);
		fileContent = head + exception + tail;

		// Inserts the following exceptions
		boolean finished = false;
		while (!finished) {
			index = fileContent.lastIndexOf(exception);
			aux = fileContent.substring(index);
			indexAux = aux.indexOf("recover(ex");
			if (indexAux == -1)
				finished = true;
			else {
				String aux2 = aux.substring(indexAux);
				int indexAux2 = aux2.indexOf(";");
				index = index + indexAux + indexAux2 + 1;
				head = fileContent.substring(0, index);
				tail = fileContent.substring(index);
				fileContent = head + exception + tail;
			}
		}

		// Updates the grammar parser file with the new content
		AcideFileManager.getInstance().write("GrammarParser.java", fileContent);

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1052"));
	}

	/**
	 * Compiles the generated files by ANTLR to obtain the .class files.
	 */
	private void compileGeneratedFiles() {

		String javacPath = null;

		try {

			// Gets the javac path from the ACIDE - A Configurable IDE resource
			// manager
			javacPath = AcideResourceManager.getInstance().getProperty(
					"javacPath");
			if (javacPath.equals("null"))
				throw new Exception(AcideLanguageManager.getInstance()
						.getLabels().getString("s929"));
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s929"),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s933"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Closes the progress window
			AcideProgressWindow.getInstance().closeWindow();

			return;
		}

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1053"));

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				"\"" + javacPath + "\" -cp .;c:\\classes .\\*.java -d .");

		Process process = null;
		try {

			// Executes the command
			process = Runtime.getRuntime().exec(
					"\"" + javacPath + "\" -cp .;c:\\classes .\\*.java -d .");

			// Waits for the process to finish
			process.waitFor();
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Closes the progress window
			AcideProgressWindow.getInstance().closeWindow();

			return;
		}

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1054"));
	}

	/**
	 * Reallocates the generated files into the correspondent folder in the
	 * source folder of ACIDE - A Configurable IDE.
	 */
	private void reallocateGeneratedFiles() {

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1055"));

		// Reallocates the GrammarLexer.java file
		AcideByteFileManager.getInstance().reallocateFile("GrammarLexer.java",
				"src/acide/process/parser/grammar/GrammarLexer.java");

		// Reallocates the GrammarLexer.smap file
		AcideByteFileManager.getInstance().reallocateFile("GrammarLexer.smap",
				"src/acide/process/parser/grammar/GrammarLexer.smap");

		// Reallocates the GrammarLexerTokenTypes.java file
		AcideByteFileManager.getInstance().reallocateFile(
				"GrammarLexerTokenTypes.java",
				"src/acide/process/parser/grammar/GrammarLexerTokenTypes.java");

		// Reallocates the GrammarLexerTokenTypes.txt file
		AcideByteFileManager.getInstance().reallocateFile(
				"GrammarLexerTokenTypes.txt",
				"src/acide/process/parser/grammar/GrammarLexerTokenTypes.txt");

		// Reallocates the GrammarParser.java file
		AcideByteFileManager.getInstance().reallocateFile("GrammarParser.java",
				"src/acide/process/parser/grammar/GrammarParser.java");

		// Reallocates the GrammarParser.smap file
		AcideByteFileManager.getInstance().reallocateFile("GrammarParser.smap",
				"src/acide/process/parser/grammar/GrammarParser.smap");

		// Reallocates the grammar.g file
		AcideByteFileManager.getInstance().reallocateFile("grammar.g",
				"src/acide/process/parser/grammar/grammar.g");

		// Reallocates the syntaxRules.txt file
		AcideByteFileManager.getInstance().reallocateFile("syntaxRules.txt",
				"src/acide/process/parser/grammar/syntaxRules.txt");

		// Reallocates the lexicalCategories.txt file
		AcideByteFileManager.getInstance().reallocateFile(
				"lexicalCategories.txt",
				"src/acide/process/parser/grammar/lexicalCategories.txt");

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1056"));
	}

	/**
	 * Generates the .jar file which contains the grammar configuration.
	 */
	private void generateJarFile() {

		String jarPath = null;
		try {

			// Gets the jar path from the ACIDE - A Configurable IDE resource
			// manager
			jarPath = AcideResourceManager.getInstance().getProperty("jarPath");

			if (jarPath.equals("null"))
				throw new Exception(AcideLanguageManager.getInstance()
						.getLabels().getString("s930"));
		} catch (Exception exception) {

			// Error message
			JOptionPane.showMessageDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s930"),
					AcideLanguageManager.getInstance().getLabels()
							.getString("s932"), JOptionPane.ERROR_MESSAGE);

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1057"));

		Process process = null;
		try {

			// Executes the command
			process = Runtime.getRuntime().exec(
					"\"" + jarPath + "\" cfm " + _grammarName + ".jar "
							+ "acide/process/parser/grammar/manifest.txt "
							+ "acide/process/parser/grammar");

			// Waits for the process to finish
			process.waitFor();
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();

			// Closes the progress window
			AcideProgressWindow.getInstance().closeWindow();

			return;
		}

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1058"));
	}

	/**
	 * Deletes the generated files by ANTLR once the .jar file is generated and
	 * they have been reallocated in the correspondent folder.
	 */
	private void deleteGeneratedFiles() {

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1059"));

		// Deletes the GrammarLexer.java file
		File file = new File("GrammarLexer.java");
		file.delete();

		// Deletes the GrammarLexer.class file
		file = new File("GrammarLexer.class");
		file.delete();

		// Deletes the GrammarLexerTokenTypes.java file
		file = new File("GrammarLexerTokenTypes.java");
		file.delete();

		// Deletes the GrammarLexerTokenTypes.class file
		file = new File("GrammarLexerTokenTypes.class");
		file.delete();

		// Deletes the GrammarParser.java file
		file = new File("GrammarParser.java");
		file.delete();

		// Deletes the GrammarParser.class file
		file = new File("GrammarParser.class");
		file.delete();

		// Deletes the GrammarLexer.smap file
		file = new File("GrammarLexer.smap");
		file.delete();

		// Deletes the GrammarParser.smap file
		file = new File("GrammarParser.smap");
		file.delete();

		// Deletes the GrammarLexerTokenTypes.txt
		file = new File("GrammarLexerTokenTypes.txt");
		file.delete();

		// Deletes the grammar.g
		file = new File("grammar.g");
		file.delete();

		// Deletes the syntaxRules.txt
		file = new File("syntaxRules.txt");
		file.delete();

		// Deletes the lexicalCategories.txt
		file = new File("lexicalCategories.txt");
		file.delete();

		// Deletes the acide folder
		file = new File("acide");
		file.delete();

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1060"));
	}

	/**
	 * Reallocates the generated .jar file in the the grammar configuration
	 * folder.
	 */
	private void reallocateJarFile() {

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1061"));

		// Reallocates the .jar file
		AcideByteFileManager.getInstance().reallocateFile(
				_grammarName + ".jar",
				"./configuration/grammars/" + _grammarName + ".jar");

		// Updates the progress window
		AcideProgressWindow.getInstance().setText(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s1062"));
	}
}
