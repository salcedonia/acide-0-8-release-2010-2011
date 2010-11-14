package es.configuration.lexicon;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.Language;

import operations.lexicon.Comments;
import operations.lexicon.DelimiterList;
import operations.lexicon.TokenTypeList;
import operations.log.Log;
import properties.PropertiesManager;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import es.configuration.grammar.GrammarConfiguration;
import es.text.ValidExtensions;
import gui.mainWindow.MainWindow;

 /************************************************************************																
 * Lexicon configuration of ACIDE - A Configurable IDE											
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
public class LexiconConfiguration {

	/**
	 * Name of the lexical for a programming language
	 */
	private String _languageName;
	/**
	 * Path of the lexical configuration for a programming language
	 */
	private String _languagePath;
	/**
	 * Flag that indicates if the lexical for a programming language is compiled
	 * or interpreted
	 */
	private boolean _isCompiledOrInterpreted;
	/**
	 * Token type list for the lexical configuration for a programming language
	 */
	private TokenTypeList _tokenList;
	/**
	 * Grammar configuration file path
	 */
	private String _grammarPath;
	/**
	 * Valid extensions for the lexical configuration for a programming
	 * language
	 */
	private ValidExtensions _validExtensions;
	/**
	 * Class instance
	 */
	private static LexiconConfiguration _instance;
	/**
	 * Delimiter list for the lexical configuration for a programming language
	 */
	private DelimiterList _delimiterList;
	/**
	 * Comments for the lexical configuration for a programming language
	 */
	private Comments _comments;

	/**
	 * Class constructor
	 */
	public LexiconConfiguration() {
		super();
	}

	/**
	 * Returns the unique class instance
	 * 
	 * @return the unique class instance
	 */
	public static LexiconConfiguration getInstance() {
		if (_instance == null)
			_instance = new LexiconConfiguration();
		return _instance;
	}

	/**
	 * Creates a new lexical configuration for a programming language
	 * 
	 * @param path
	 *            path which contains the configuration
	 */
	public void newLexical(String path) {

		// Gets the name
		int index = path.lastIndexOf("\\");
		if(index == -1)
			index = path.lastIndexOf("/");
		_languageName = path.substring(index + 1, path.length());

		// RESET THE PARAMETERS OF THE LEXICAL CONFIGURATION
		TokenTypeList.getInstance().reset();
		DelimiterList.getInstance().reset();
		Comments.getInstance().reset();
		_languagePath = path;
		PropertiesManager.setProperty("languagePath", path);
		save(_languageName, false);
	}

	/**
	 * Saves the lexical configuration for a programming language in a XML file
	 * and returns true if the operation was succeed or false in other case
	 * 
	 * @param name
	 *            name of the lexical configuration for the language
	 * @param isCompiledOrInterpreted
	 *            indicates if the programming language is compiled or
	 *            interpreted
	 * 
	 * @return true if the operation was succeed or false in other case
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
			_delimiterList = DelimiterList.getInstance();
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
	 * Saves the lexical configuration for a programming language in a XML file
	 * in a defined path given as a parameter, returning true if the operation
	 * was succeed or false in other case
	 * 
	 * @param name
	 *            name of the lexical configuration for the language.
	 * @param isCompiledOrInterpreted
	 *            indicates if the programming language is compiled or
	 *            interpreted
	 * @param path
	 *            path for the new file
	 * 
	 * @return true if the operation was succeed or false in other case
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
			_delimiterList = DelimiterList.getInstance();
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
	 * Saves the lexical configuration of a programming language in a temporal
	 * XML file, returning true if the operation was succeed or false in other
	 * case
	 * 
	 * @param name
	 *            name of the lexical configuration for the language
	 * @param isCompiledOrInterpreted
	 *            indicates if the programming language is compiled or
	 *            interpreted
	 * 
	 * @return True if the operation was succeed or false in other case
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
			_delimiterList = DelimiterList.getInstance();
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

			return "./configuration/lexical/temp/" + xmlTmp.getName();
		}

		return "./configuration/lexical/temp/NULL.xml";
	}

	/**
	 * Loads the lexical configuration for a programming language in the
	 * application from a temporal XML file which is located in a path given as
	 * a parameter
	 * 
	 * @param path
	 *            file path of the file to extract the configuration from
	 */
	public void loadTemp(String path) {

		// IF THE NAME IS ALREADY SET BY THE USER
		if ((path != null) && (!path.trim().equalsIgnoreCase(""))) {

			try {
				
				XStream x = new XStream();
				
				FileInputStream f = new FileInputStream(path);
				LexiconConfiguration programmingLanguage = (LexiconConfiguration) x.fromXML(f);
				
				String name = programmingLanguage._languageName;
				Boolean isCompiledOrInterpreted = programmingLanguage._isCompiledOrInterpreted;
				TokenTypeList tokenTypeList = programmingLanguage._tokenList;
				String grammarPath = programmingLanguage._grammarPath;
				ValidExtensions validExtensions = programmingLanguage._validExtensions;
				DelimiterList dividerList = programmingLanguage._delimiterList;
				Comments comments = programmingLanguage._comments;
				f.close();

				_languageName = name;
				_isCompiledOrInterpreted = isCompiledOrInterpreted;
				_tokenList = tokenTypeList;
				_grammarPath = grammarPath;
				_validExtensions = validExtensions;
				_delimiterList = dividerList;
				_comments = comments;
				_languagePath = path;

				DelimiterList.getInstance().load(_delimiterList);
				TokenTypeList.getInstance().load(_tokenList);
				GrammarConfiguration.setPath(_grammarPath);
				ValidExtensions.getInstance().load(_validExtensions);
				Comments.getInstance().load(_comments);

			} catch (Exception e) {		
				e.printStackTrace();
			}
		}
	}

	/**
	 * Loads the lexical configuration for a programming language in the
	 * application from a XML file which is located in a path given as a
	 * parameter
	 * 
	 * @param path
	 *            File path of the file to extract the configuration from
	 */
	public void load(String path) {

		// IF THE NAME IS ALREADY SET BY THE USER
		if ((path != null) && (!path.trim().equalsIgnoreCase(""))) {
			try {

				XStream x = new XStream(new DomDriver());
				FileInputStream f = new FileInputStream(path);

				LexiconConfiguration programmingLanguage = (LexiconConfiguration) x
						.fromXML(f);

				String name = programmingLanguage._languageName;
				Boolean isCompiledOrInterpreted = programmingLanguage._isCompiledOrInterpreted;
				TokenTypeList tokenTypeList = programmingLanguage._tokenList;
				String grammarPath = programmingLanguage._grammarPath;
				ValidExtensions validExtensions = programmingLanguage._validExtensions;
				DelimiterList dividerList = programmingLanguage._delimiterList;
				Comments comments = programmingLanguage._comments;
				f.close();

				_languageName = name;
				_isCompiledOrInterpreted = isCompiledOrInterpreted;
				_tokenList = tokenTypeList;
				_grammarPath = grammarPath;
				_validExtensions = validExtensions;
				_delimiterList = dividerList;
				_comments = comments;
				_languagePath = path; // ABSOLUTE PATH

				DelimiterList.getInstance().load(_delimiterList);
				TokenTypeList.getInstance().load(_tokenList);
				GrammarConfiguration.setPath(_grammarPath);
				ValidExtensions.getInstance().load(_validExtensions);
				Comments.getInstance().load(_comments);

				PropertiesManager.setProperty("languagePath", _languagePath);

			} catch (Exception exception) {
								
				// Gets the language TO DISPLAY
				Language language = Language.getInstance();
				
				try {
					language.getLanguage(PropertiesManager.getProperty("language"));
				} catch (Exception exception1) {
					
					// Updates the log
					Log.getLog().error(exception1.getMessage());
					exception1.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();
				
				JOptionPane
						.showMessageDialog(null, labels.getString("s968")
								+ path + labels.getString("s957")
								+ "./configuration/lexical/default.xml");
				
				// IF THE FILE DOESN'T EXISTS LOAD THE DEFAULT CONFIGURATION
				load("./configuration/lexical/default.xml");
				
				MainWindow.getInstance().getProjectConfiguration()
				.setLexicalConfiguration("./configuration/lexical/default.xml");
			
				// Updates the log
				Log.getLog().info(labels.getString("s201") + " " + path);
			}
		}
	}

	/**
	 * Returns the valid extensions of a lexical configuration for a programming
	 * language
	 * 
	 * @return the valid extensions of a lexical configuration for a programming
	 *         language
	 */
	public ValidExtensions getValidExtensions() {
		return _validExtensions;
	}

	/**
	 * Sets the new value for the valid extensions of a lexical configuration for
	 * a programming language
	 * 
	 * @param validExtensions
	 *            new value to set
	 */
	public void setValidExtensions(ValidExtensions validExtensions) {
		_validExtensions = validExtensions;
	}

	/**
	 * Returns the token list of a lexical configuration for a programming
	 * language
	 * 
	 * @return the token list of a lexical configuration for a programming
	 *         language
	 */
	public TokenTypeList getTokenList() {
		return _tokenList;
	}

	/**
	 * Sets the new value for the token list of a lexical configuration for a
	 * programming language
	 * 
	 * @param tokenList
	 *            new value to set
	 */
	public void setTokenList(TokenTypeList tokenList) {
		_tokenList = tokenList;
	}

	/**
	 * Returns the name of a lexical configuration for a programming language
	 * 
	 * @return the name of a lexical configuration for a programming language
	 */
	public String getName() {
		return _languageName;
	}

	/**
	 * Sets the new value for the name of a lexical configuration for a
	 * programming language
	 * 
	 * @param name
	 *            new value to set
	 */
	public void setName(String name) {
		_languageName = name;
	}

	/**
	 * Returns the grammar path of a lexical configuration for a programming
	 * language
	 * 
	 * @return the grammar path of a lexical configuration for a programming
	 *         language
	 */
	public String getGrammarPath() {
		return _grammarPath;
	}

	/**
	 * Sets the new value for the grammar path of a lexical configuration for a
	 * programming language
	 * 
	 * @param grammarPath
	 *            ew value to set
	 */
	public void setGrammarPath(String grammarPath) {
		_grammarPath = grammarPath;
	}

	/**
	 * Returns the divider list of a lexical configuration for a programming
	 * language
	 * 
	 * @return the divider list of a lexical configuration for a programming
	 *         language
	 */

	public DelimiterList getDividerList() {
		return _delimiterList;
	}

	/**
	 * Sets the new value for the divider list of a lexical configuration for a
	 * programming language
	 * 
	 * @param dividerList
	 *            new value to set
	 */
	public void setDividerList(DelimiterList dividerList) {
		_delimiterList = dividerList;
	}

	/**
	 * Returns the path of a lexical configuration for a programming language
	 * 
	 * @return the path of a lexical configuration for a programming language
	 */
	public String getPath() {
		return _languagePath;
	}
}
