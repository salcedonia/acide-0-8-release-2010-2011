package es.configuration.programmingLanguage;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import org.apache.log4j.Logger;

import language.Language;

import operations.configuration.GrammarConfiguration;
import operations.lexicon.Comments;
import operations.lexicon.DividerList;
import operations.lexicon.TokenTypeList;
import operations.log.Log;
import properties.PropertiesManager;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import es.text.ValidExtensions;
import gui.MainWindow;

/**
 * Handles the Lexical configuration for a programming language in the
 * application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ProgrammingLanguage {

	/**
	 * Log of the class.
	 */
	private Logger _logger = Log.getLog();
	/**
	 * Name of the lexical for a programming language.
	 */
	private String _languageName;
	/**
	 * Path of the lexical configuration for a programming language.
	 */
	private String _languagePath;
	/**
	 * Flag that indicates if the lexical for a programming language is compiled
	 * or interpreted.
	 */
	private boolean _isCompiledOrInterpreted;
	/**
	 * Token type list for the lexical configuration for a programming language.
	 */
	private TokenTypeList _tokenList;
	/**
	 * Grammar path.
	 */
	private String _grammarPath;
	/**
	 * Valid extensions for the lexical configuration for a programming
	 * language.
	 */
	private ValidExtensions _validExtensions;
	/**
	 * Instance of the class.
	 */
	private static ProgrammingLanguage _instance;
	/**
	 * Divider list for the lexical configuration for a programming language.
	 */
	private DividerList _dividerList;
	/**
	 * Comments for the lexical configuration for a programming language.
	 */
	private Comments _comments;

	/**
	 * Constructor of the class.
	 */
	public ProgrammingLanguage() {
		super();
	}

	/**
	 * Returns the unique instance of the class.
	 * 
	 * @return The unique instance of the class.
	 */
	public static ProgrammingLanguage getInstance() {
		if (_instance == null)
			_instance = new ProgrammingLanguage();
		return _instance;
	}

	/**
	 * Creates a new lexical configuration for a programming language.
	 * 
	 * @param path
	 *            Path which contains the configuration.
	 */
	public void newLexical(String path) {

		// GET THE NAME OF THE LEXICAL
		int index = path.lastIndexOf("\\");
		if(index == -1)
			index = path.lastIndexOf("/");
		String name = path.substring(index + 1, path.length());

		if (name.contains(".")) {
			index = name.lastIndexOf(".");
			name = name.substring(0, index);
		}
		_languageName = name;

		// RESET THE PARAMETERS OF THE LEXICAL CONFIGURATION
		TokenTypeList.getInstance().reset();
		DividerList.getInstance().reset();
		Comments.getInstance().reset();
		_languagePath = path;
		PropertiesManager.setProperty("languagePath", path);
		save(_languageName, false);
	}

	/**
	 * Save the lexical configuration for a programming language in a XML file
	 * and returns true if the operation was succeed or false in other case.
	 * 
	 * @param name
	 *            Name of the lexical configuration for the language.
	 * @param isCompiledOrInterpreted
	 *            Indicates if the programming language is compiled or
	 *            interpreted.
	 * 
	 * @return True if the operation was succeed or false in other case.
	 */
	public boolean save(String name, boolean isCompiledOrInterpreted) {

		// IF THE NAME IS ALREADY SET BY THE USER
		if ((_languageName != null)
				&& (!_languageName.trim().equalsIgnoreCase(""))) {

			_languageName = name;
			_isCompiledOrInterpreted = isCompiledOrInterpreted;
			_tokenList = TokenTypeList.getInstance();
			_grammarPath = GrammarConfiguration._path;
			_validExtensions = ValidExtensions.getInstance();
			_dividerList = DividerList.getInstance();
			_comments = Comments.getInstance();

			XStream xStream = new XStream();
			try {
				FileOutputStream file = new FileOutputStream(_languagePath);
				xStream.toXML(this, file);
				file.close();
			} catch (Exception e) {
				e.printStackTrace();
				return false;
			}
			PropertiesManager.setProperty("languagePath", _languagePath);
		}
		return true;
	}

	/**
	 * Save the lexical configuration for a programming language in a XML file
	 * in a defined path given as a parameter, returning true if the operation
	 * was succeed or false in other case.
	 * 
	 * @param name
	 *            Name of the lexical configuration for the language.
	 * @param isCompiledOrInterpreted
	 *            Indicates if the programming language is compiled or
	 *            interpreted.
	 * @param path
	 *            Path for the new file.
	 * 
	 * @return True if the operation was succeed or false in other case.
	 */
	public boolean saveAs(String name, boolean IsCompiledOrInterpreted,
			String path) {

		// IF THE NAME IS ALREADY SET BY THE USER
		if ((_languageName != null)
				&& (!_languageName.trim().equalsIgnoreCase(""))) {

			_languageName = name;
			_isCompiledOrInterpreted = IsCompiledOrInterpreted;
			_tokenList = TokenTypeList.getInstance();
			_grammarPath = GrammarConfiguration._path;
			_validExtensions = ValidExtensions.getInstance();
			_dividerList = DividerList.getInstance();
			_comments = Comments.getInstance();

			XStream x = new XStream();

			try {
				FileOutputStream f = new FileOutputStream(path);
				x.toXML(this, f);
				f.close();
			} catch (Exception e) {
				e.printStackTrace();
				return false;
			}
		}
		return true;
	}

	/**
	 * Save the lexical configuration of a programming language in a temporal
	 * XML file, returning true if the operation was succeed or false in other
	 * case.
	 * 
	 * @param name
	 *            Name of the lexical configuration for the language.
	 * @param isCompiledOrInterpreted
	 *            Indicates if the programming language is compiled or
	 *            interpreted.
	 * 
	 * @return True if the operation was succeed or false in other case.
	 */
	public String saveTemp(String name, boolean isCompiledOrInterpreted) {

		File xmlTmp = null;

		// IF THE NAME IS ALREADY SET BY THE USER
		if ((_languageName != null)
				&& (!_languageName.trim().equalsIgnoreCase(""))) {

			_languageName = name;
			_isCompiledOrInterpreted = isCompiledOrInterpreted;
			_tokenList = TokenTypeList.getInstance();
			_grammarPath = GrammarConfiguration._path;
			_validExtensions = ValidExtensions.getInstance();
			_dividerList = DividerList.getInstance();
			_comments = Comments.getInstance();

			XStream xStream = new XStream();

			try {
				xmlTmp = File.createTempFile("TMP", ".xml", new File(
						"./configuration/lexical/temp/"));
				xmlTmp.deleteOnExit();

				FileOutputStream file = new FileOutputStream(xmlTmp);

				xStream.toXML(this, file);
				file.close();

			} catch (Exception e) {
				e.printStackTrace();
				return null;
			}

			return xmlTmp.getAbsolutePath();
		}

		return "./configuration/lexical/temp/NULL.xml";
	}

	/**
	 * Load the lexical configuration for a programming language in the
	 * application from a temporal XML file which is located in a path given as
	 * a parameter.
	 * 
	 * @param path
	 *            File path of the file to extract the configuration from.
	 */
	public void loadTemp(String path) {

		// IF THE NAME IS ALREADY SET BY THE USER
		if ((path != null) && (!path.trim().equalsIgnoreCase(""))) {

			try {
				XStream x = new XStream();
				FileInputStream f = new FileInputStream(path);
				ProgrammingLanguage l = (ProgrammingLanguage) x.fromXML(f);
				String n = l._languageName;
				Boolean ci = l._isCompiledOrInterpreted;
				TokenTypeList lt = l._tokenList;
				String pg = l._grammarPath;
				ValidExtensions ex = l._validExtensions;
				DividerList dl = l._dividerList;
				Comments c = l._comments;
				f.close();

				_languageName = n;
				_isCompiledOrInterpreted = ci;
				_tokenList = lt;
				_grammarPath = pg;
				_validExtensions = ex;
				_dividerList = dl;
				_comments = c;
				_languagePath = path;

				DividerList.getInstance().load(_dividerList);
				TokenTypeList.getInstance().load(_tokenList);
				GrammarConfiguration.setPath(_grammarPath);
				ValidExtensions.getInstance().load(_validExtensions);
				Comments.getInstance().load(_comments);

			} catch (Exception e) {
				
				e.printStackTrace();
				
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
				
				// DISPLAY AN ERROR MESSAGE
				JOptionPane.showMessageDialog(null, labels.getString("s526"),
						labels.getString("s526"), 0);
			}
		}
	}

	/**
	 * Load the lexical configuration for a programming language in the
	 * application from a XML file which is located in a path given as a
	 * parameter.
	 * 
	 * @param path
	 *            File path of the file to extract the configuration from.
	 */
	public void load(String path) {

		// IF THE NAME IS ALREADY SET BY THE USER
		if ((path != null) && (!path.trim().equalsIgnoreCase(""))) {
			try {

				XStream x = new XStream(new DomDriver());
				FileInputStream f = new FileInputStream(path);

				ProgrammingLanguage programmingLanguage = (ProgrammingLanguage) x
						.fromXML(f);

				String name = programmingLanguage._languageName;
				Boolean isCompiledOrInterpreted = programmingLanguage._isCompiledOrInterpreted;
				TokenTypeList tokenTypeList = programmingLanguage._tokenList;
				String grammarPath = programmingLanguage._grammarPath;
				ValidExtensions validExtensions = programmingLanguage._validExtensions;
				DividerList dividerList = programmingLanguage._dividerList;
				Comments comments = programmingLanguage._comments;
				f.close();

				_languageName = name;
				_isCompiledOrInterpreted = isCompiledOrInterpreted;
				_tokenList = tokenTypeList;
				_grammarPath = grammarPath;
				_validExtensions = validExtensions;
				_dividerList = dividerList;
				_comments = comments;
				_languagePath = path; // ABSOLUTE PATH

				DividerList.getInstance().load(_dividerList);
				TokenTypeList.getInstance().load(_tokenList);
				GrammarConfiguration.setPath(_grammarPath);
				ValidExtensions.getInstance().load(_validExtensions);
				Comments.getInstance().load(_comments);

				PropertiesManager.setProperty("languagePath", _languagePath);

			} catch (Exception e) {
				
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
				JOptionPane
						.showMessageDialog(null, labels.getString("s968")
								+ path + labels.getString("s957")
								+ "./configuration/lexical/default.xml");
				
				// IF THE FILE DOESN'T EXISTS LOAD THE DEFAULT CONFIGURATION
				load("./configuration/lexical/default.xml");
				
				MainWindow.getInstance().getProjectConfiguration()
				.setLexicalConfiguration("./configuration/lexical/default.xml");
			
				_logger.info(labels.getString("s201") + " " + path);
			}
		}
	}

	/**
	 * Returns the valid extensions of a lexical configuration for a programming
	 * language.
	 * 
	 * @return The valid extensions of a lexical configuration for a programming
	 *         language.
	 */
	public ValidExtensions getValidExtensions() {
		return _validExtensions;
	}

	/**
	 * Set the new value for the valid extensions of a lexical configuration for
	 * a programming language.
	 * 
	 * @param validExtensions
	 *            New value to set.
	 */
	public void setValidExtensions(ValidExtensions validExtensions) {
		_validExtensions = validExtensions;
	}

	/**
	 * Returns the token list of a lexical configuration for a programming
	 * language.
	 * 
	 * @return The token list of a lexical configuration for a programming
	 *         language.
	 */
	public TokenTypeList getTokenList() {
		return _tokenList;
	}

	/**
	 * Set the new value for the token list of a lexical configuration for a
	 * programming language.
	 * 
	 * @param tokenList
	 *            New value to set.
	 */
	public void setTokenList(TokenTypeList tokenList) {
		_tokenList = tokenList;
	}

	/**
	 * Returns the name of a lexical configuration for a programming language.
	 * 
	 * @return The name of a lexical configuration for a programming language.
	 */
	public String getName() {
		return _languageName;
	}

	/**
	 * Set the new value for the name of a lexical configuration for a
	 * programming language.
	 * 
	 * @param name
	 *            New value to set.
	 */
	public void setName(String name) {
		_languageName = name;
	}

	/**
	 * Returns the grammar path of a lexical configuration for a programming
	 * language.
	 * 
	 * @return The grammar path of a lexical configuration for a programming
	 *         language.
	 */
	public String getGrammarPath() {
		return _grammarPath;
	}

	/**
	 * Set the new value for the grammar path of a lexical configuration for a
	 * programming language.
	 * 
	 * @param grammarPath
	 *            New value to set.
	 */
	public void setGrammarPath(String grammarPath) {
		_grammarPath = grammarPath;
	}

	/**
	 * Returns the divider list of a lexical configuration for a programming
	 * language.
	 * 
	 * @return The divider list of a lexical configuration for a programming
	 *         language.
	 */

	public DividerList getDividerList() {
		return _dividerList;
	}

	/**
	 * Set the new value for the divider list of a lexical configuration for a
	 * programming language.
	 * 
	 * @param dividerList
	 *            New value to set.
	 */
	public void setDividerList(DividerList dividerList) {
		_dividerList = dividerList;
	}

	/**
	 * Returns the path of a lexical configuration for a programming language.
	 * 
	 * @return The path of a lexical configuration for a programming language.
	 */
	public String getPath() {
		return _languagePath;
	}
}