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
package es.configuration.lexicon;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.AcideLanguageManager;

import operations.lexicon.Remarks;
import operations.lexicon.DelimiterList;
import operations.lexicon.TokenTypeList;
import operations.log.AcideLog;
import resources.AcideResourceManager;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import es.configuration.grammar.AcideGrammarConfiguration;
import es.configuration.project.AcideProjectConfiguration;
import es.text.AcideValidExtensionsManager;


/**
 * ACIDE - A Configurable IDE lexicon configuration.
 * 
 * @version 0.8
 */
public class AcideLexiconConfiguration {

	/**
	 * ACIDE - A Configurable IDE lexicon configuration name.
	 */
	private String _languageName;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration path.
	 */
	private String _languagePath;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration flag that indicates if
	 * it is compiled or interpreted.
	 */
	private boolean _isCompiledOrInterpreted;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration token type list.
	 */
	private TokenTypeList _tokenList;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration grammar configuration
	 * file path.
	 */
	private String _grammarPath;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration valid extensions.
	 */
	private AcideValidExtensionsManager _validExtensions;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration unique class instance.
	 */
	private static AcideLexiconConfiguration _instance;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration delimiter list.
	 */
	private DelimiterList _delimiterList;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration remarks.
	 */
	private Remarks _remarks;

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration.
	 */
	public AcideLexiconConfiguration() {
		super();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration unique class
	 *         instance.
	 */
	public static AcideLexiconConfiguration getInstance() {
		if (_instance == null)
			_instance = new AcideLexiconConfiguration();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE lexicon configuration.
	 * 
	 * @param path
	 *            path which contains the configuration.
	 */
	public void newLexicon(String path) {

		// Gets the name
		int index = path.lastIndexOf("\\");
		if (index == -1)
			index = path.lastIndexOf("/");
		_languageName = path.substring(index + 1, path.length());

		// Resets the lexicon configuration parameters
		TokenTypeList.getInstance().reset();
		DelimiterList.getInstance().reset();
		Remarks.getInstance().reset();
		_languagePath = path;

		// Updates the RESOURCE MANAGER
		AcideResourceManager.getInstance().setProperty("languagePath", path);
		save(_languageName, false);
	}

	/**
	 * Saves the lexicon configuration for a programming language in a XML file
	 * and returns true if the operation was succeed or false in other case.
	 * 
	 * @param name
	 *            name of the lexicon configuration for the language.
	 * @param isCompiledOrInterpreted
	 *            indicates if the programming language is compiled or
	 *            interpreted.
	 * 
	 * @return true if the operation was succeed or false in other case.
	 */
	public boolean save(String name, boolean isCompiledOrInterpreted) {

		// If the name is already set by the user
		if ((_languageName != null)
				&& (!_languageName.trim().equalsIgnoreCase(""))) {

			_languageName = name;
			_isCompiledOrInterpreted = isCompiledOrInterpreted;
			_tokenList = TokenTypeList.getInstance();
			_grammarPath = AcideGrammarConfiguration._path;
			_validExtensions = AcideValidExtensionsManager.getInstance();
			_delimiterList = DelimiterList.getInstance();
			_remarks = Remarks.getInstance();

			XStream xStream = new XStream();
			try {
				FileOutputStream file = new FileOutputStream(_languagePath);
				xStream.toXML(this, file);
				file.close();
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
				return false;
			}

			// Updates the RESOURCE MANAGER
			AcideResourceManager.getInstance().setProperty("languagePath",
					_languagePath);
		}
		return true;
	}

	/**
	 * Saves the lexicon configuration for a programming language in a XML file
	 * in a defined path given as a parameter, returning true if the operation
	 * was succeed or false in other case.
	 * 
	 * @param name
	 *            name of the lexicon configuration for the language.
	 * @param isCompiledOrInterpreted
	 *            indicates if the programming language is compiled or
	 *            interpreted.
	 * @param path
	 *            path for the new file.
	 * 
	 * @return true if the operation was succeed or false in other case.
	 */
	public boolean saveAs(String name, boolean IsCompiledOrInterpreted,
			String path) {

		// If the name is already set by the user
		if ((_languageName != null)
				&& (!_languageName.trim().equalsIgnoreCase(""))) {

			_languageName = name;
			_isCompiledOrInterpreted = IsCompiledOrInterpreted;
			_tokenList = TokenTypeList.getInstance();
			_grammarPath = AcideGrammarConfiguration._path;
			_validExtensions = AcideValidExtensionsManager.getInstance();
			_delimiterList = DelimiterList.getInstance();
			_remarks = Remarks.getInstance();

			// Creates the XStream object to write in the XML file
			XStream xStream = new XStream();

			try {
				FileOutputStream fileOutputStream = new FileOutputStream(path);
				xStream.toXML(this, fileOutputStream);
				fileOutputStream.close();
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
				return false;
			}
		}
		return true;
	}

	/**
	 * Saves the lexicon configuration of a programming language in a temporal
	 * XML file, returning true if the operation was succeed or false in other
	 * case.
	 * 
	 * @param name
	 *            name of the lexicon configuration for the language.
	 * @param isCompiledOrInterpreted
	 *            indicates if the programming language is compiled or
	 *            interpreted.
	 * 
	 * @return True if the operation was succeed or false in other case.
	 */
	public String saveTemp(String name, boolean isCompiledOrInterpreted) {

		File xmlTmp = null;

		// If the name is already set by the user
		if ((_languageName != null)
				&& (!_languageName.trim().equalsIgnoreCase(""))) {

			_languageName = name;
			_isCompiledOrInterpreted = isCompiledOrInterpreted;
			_tokenList = TokenTypeList.getInstance();
			_grammarPath = AcideGrammarConfiguration._path;
			_validExtensions = AcideValidExtensionsManager.getInstance();
			_delimiterList = DelimiterList.getInstance();
			_remarks = Remarks.getInstance();

			XStream xStream = new XStream();

			try {
				xmlTmp = File.createTempFile("TMP", ".xml", new File(
						"./configuration/lexicon/temp/"));
				xmlTmp.deleteOnExit();

				FileOutputStream file = new FileOutputStream(xmlTmp);

				xStream.toXML(this, file);
				file.close();

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
				return null;
			}

			return "./configuration/lexicon/temp/" + xmlTmp.getName();
		}

		return "./configuration/lexicon/temp/NULL.xml";
	}

	/**
	 * Loads the lexicon configuration for a programming language in the
	 * application from a temporal XML file which is located in a path given as
	 * a parameter.
	 * 
	 * @param path
	 *            file path of the file to extract the configuration from.
	 */
	public void loadTemp(String path) {

		// If the name is already set by the user
		if ((path != null) && (!path.trim().equalsIgnoreCase(""))) {

			try {

				XStream x = new XStream();

				FileInputStream f = new FileInputStream(path);
				AcideLexiconConfiguration lexiconConfiguration = (AcideLexiconConfiguration) x
						.fromXML(f);

				String name = lexiconConfiguration._languageName;
				Boolean isCompiledOrInterpreted = lexiconConfiguration._isCompiledOrInterpreted;
				TokenTypeList tokenTypeList = lexiconConfiguration._tokenList;
				String grammarPath = lexiconConfiguration._grammarPath;
				AcideValidExtensionsManager validExtensions = lexiconConfiguration._validExtensions;
				DelimiterList dividerList = lexiconConfiguration._delimiterList;
				Remarks remarks = lexiconConfiguration._remarks;
				f.close();

				_languageName = name;
				_isCompiledOrInterpreted = isCompiledOrInterpreted;
				_tokenList = tokenTypeList;
				_grammarPath = grammarPath;
				_validExtensions = validExtensions;
				_delimiterList = dividerList;
				_remarks = remarks;
				_languagePath = path;

				DelimiterList.getInstance().load(_delimiterList);
				TokenTypeList.getInstance().load(_tokenList);
				AcideGrammarConfiguration.setPath(_grammarPath);
				AcideValidExtensionsManager.getInstance().load(_validExtensions);
				Remarks.getInstance().load(_remarks);

			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
		}
	}

	/**
	 * Loads the lexicon configuration for a programming language in the
	 * application from a XML file which is located in a path given as a
	 * parameter.
	 * 
	 * @param path
	 *            File path of the file to extract the configuration from.
	 */
	public void load(String path) {

		// If the name is already set by the user
		if ((path != null) && (!path.trim().equalsIgnoreCase(""))) {
			try {

				XStream x = new XStream(new DomDriver());
				FileInputStream f = new FileInputStream(path);

				// Gets the lexicon configuration from the XML file
				AcideLexiconConfiguration lexiconConfiguration = (AcideLexiconConfiguration) x
						.fromXML(f);

				// NAME
				String name = lexiconConfiguration._languageName;
				
				// IS COMPILED OR INTERPRETED
				Boolean isCompiledOrInterpreted = lexiconConfiguration._isCompiledOrInterpreted;
				
				// TOKEN TYPE LIST
				TokenTypeList tokenTypeList = lexiconConfiguration._tokenList;
				
				// GRAMMAR PATH
				String grammarPath = lexiconConfiguration._grammarPath;
				
				// VALID EXTENSIONS
				AcideValidExtensionsManager validExtensions = lexiconConfiguration._validExtensions;
				
				// DELIMITER LIST
				DelimiterList delimiterList = lexiconConfiguration._delimiterList;
				
				// REMARKS
				Remarks remarks = lexiconConfiguration._remarks;
				f.close();

				_languageName = name;
				_isCompiledOrInterpreted = isCompiledOrInterpreted;
				_tokenList = tokenTypeList;
				_grammarPath = grammarPath;
				_validExtensions = validExtensions;
				_delimiterList = delimiterList;
				_remarks = remarks;
				_languagePath = path; // ABSOLUTE PATH

				DelimiterList.getInstance().load(_delimiterList);
				TokenTypeList.getInstance().load(_tokenList);
				AcideGrammarConfiguration.setPath(_grammarPath);
				AcideValidExtensionsManager.getInstance().load(_validExtensions);
				Remarks.getInstance().load(_remarks);

				// Updates the RESOURCE MANAGER
				AcideResourceManager.getInstance().setProperty("languagePath",
						_languagePath);

			} catch (Exception exception) {

				// Gets the language
				AcideLanguageManager language = AcideLanguageManager
						.getInstance();

				try {
					language.getLanguage(AcideResourceManager.getInstance()
							.getProperty("language"));
				} catch (Exception exception1) {

					// Updates the log
					AcideLog.getLog().error(exception1.getMessage());
					exception1.printStackTrace();
				}

				// Gets the labels
				ResourceBundle labels = language.getLabels();

				JOptionPane.showMessageDialog(null, labels.getString("s968")
						+ path + labels.getString("s957")
						+ "./configuration/lexicon/default.xml");

				// If the file does not exist, loads the default configuration
				load("./configuration/lexicon/default.xml");

				// Updates the lexicon configuration in the project configuration
				AcideProjectConfiguration.getInstance().
						setLexiconConfiguration(
								"./configuration/lexicon/default.xml");

				// Updates the log
				AcideLog.getLog().info(labels.getString("s201") + " " + path);
			}
		}
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration valid
	 * extensions.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration valid
	 *         extensions.
	 */
	public AcideValidExtensionsManager getValidExtensions() {
		return _validExtensions;
	}

	/**
	 * Sets the a value to the ACIDE - A Configurable IDE lexicon configuration
	 * valid extensions.
	 * 
	 * @param validExtensions
	 *            new value to set.
	 */
	public void setValidExtensions(AcideValidExtensionsManager validExtensions) {
		_validExtensions = validExtensions;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration token type
	 * list.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration token type
	 *         list.
	 */
	public TokenTypeList getTokenList() {
		return _tokenList;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * token type list.
	 * 
	 * @param tokenList
	 *            new value to set.
	 */
	public void setTokenList(TokenTypeList tokenList) {
		_tokenList = tokenList;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration name.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration name.
	 */
	public String getName() {
		return _languageName;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * name.
	 * 
	 * @param name
	 *            new value to set.
	 */
	public void setName(String name) {
		_languageName = name;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration grammar
	 * path.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration grammar
	 *         path.
	 */
	public String getGrammarPath() {
		return _grammarPath;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * grammar.
	 * 
	 * @param grammarPath
	 *            new value to set.
	 */
	public void setGrammarPath(String grammarPath) {
		_grammarPath = grammarPath;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration divider
	 * list.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration divider
	 *         list.
	 */
	public DelimiterList getDividerList() {
		return _delimiterList;
	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE lexicon configuration
	 * divider list.
	 * 
	 * @param dividerList
	 *            new value to set.
	 */
	public void setDividerList(DelimiterList dividerList) {
		_delimiterList = dividerList;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE lexicon configuration path.
	 * 
	 * @return the ACIDE - A Configurable IDE lexicon configuration path.
	 */
	public String getPath() {
		return _languagePath;
	}
}
