package es.configuration.programmingLanguage;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;

import language.Language;

import operations.configuration.GrammarConfiguration;
import operations.lexicon.Comments;
import operations.lexicon.DividerList;
import operations.lexicon.TokenTypeList;
import properties.PropertiesManager;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

import es.text.ValidExtensions;

/**
 * 
 */
public class ProgrammingLanguage {

	/**
	 * 
	 */
	private String _languageName;
	/**
	 * 
	 */
	private String _languagePath;
	/**
	 * 
	 */
	private boolean _isCompiledOrInterpreted;
	/**
	 * 
	 */
	private TokenTypeList _tokenList;
	/**
	 * 
	 */
	private String _grammarPath;
	/**
	 * 
	 */
	private ValidExtensions _validExtensions;
	/**
	 * 
	 */
	private static ProgrammingLanguage _instance;
	/**
	 * 
	 */
	private DividerList _dividerList;
	/**
	 * 
	 */
	private Comments _comments;

	/**
	 * Constructor of the class.
	 */
	public ProgrammingLanguage() {
		super();
	}

	/**
	 *
	 * 
	 * @return
	 */
	public static ProgrammingLanguage getInstance() {
		if (_instance == null)
			_instance = new ProgrammingLanguage();
		return _instance;
	}

	/**
	 * 
	 * @param pl
	 */
	public void newLanguage(String pl) {
		
		int index = pl.lastIndexOf("\\");
		String n = pl.substring(index + 1, pl.length());
		
		if (n.contains(".")) {
			index = n.lastIndexOf(".");
			n = n.substring(0, index);
		}
		_languageName = n;
		TokenTypeList.getInstance().reset();
		DividerList.getInstance().reset();
		Comments.getInstance().reset();
		_languagePath = pl;
		PropertiesManager.setProperty("languagePath", pl);
		save(_languageName, false);
	}

	/**
	 * 
	 * @param nom
	 * @param CompInt
	 * @return
	 */
	public boolean save(String nom, boolean CompInt) {
		if ((_languageName != null) && (!_languageName.trim().equalsIgnoreCase(""))) {
			_languageName = nom;
			_isCompiledOrInterpreted = CompInt;
			_tokenList = TokenTypeList.getInstance();
			_grammarPath = GrammarConfiguration._path;
			_validExtensions = ValidExtensions.getInstance();
			_dividerList = DividerList.getInstance();
			_comments = Comments.getInstance();
			XStream x = new XStream();
			try {
				FileOutputStream f = new FileOutputStream(_languagePath);
				x.toXML(this, f);
				f.close();
			} catch (Exception e) {
				e.printStackTrace();
				return false;
			}
			PropertiesManager.setProperty("languagePath", _languagePath);
		}
		return true;
	}

	/**
	 * 
	 * @param nom
	 * @param CompInt
	 * @param path
	 * @return
	 */
	public boolean saveAs(String nom, boolean CompInt, String path) {
		if ((_languageName != null) && (!_languageName.trim().equalsIgnoreCase(""))) {
			_languageName = nom;
			_isCompiledOrInterpreted = CompInt;
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
	 * 
	 * @param nom
	 * @param CompInt
	 * @return
	 */
	public String saveTemp(String nom, boolean CompInt) {
		
		File xmlTmp = null;
		
		// IF THE NAME IS ALREADY SET BY THE USER
		if ((_languageName != null) && (!_languageName.trim().equalsIgnoreCase(""))) {
			_languageName = nom;
			_isCompiledOrInterpreted = CompInt;
			_tokenList = TokenTypeList.getInstance();
			_grammarPath = GrammarConfiguration._path;
			_validExtensions = ValidExtensions.getInstance();
			_dividerList = DividerList.getInstance();
			_comments = Comments.getInstance();
			XStream x = new XStream();

			try {
				xmlTmp = File.createTempFile("TMP", ".xml", new File(
						"./configuration/lexical/temp/"));
				xmlTmp.deleteOnExit();

				FileOutputStream f = new FileOutputStream(xmlTmp);

				x.toXML(this, f);
				f.close();
				
			} catch (Exception e) {
				e.printStackTrace();
				return null;
			}
			
			return xmlTmp.getAbsolutePath();
		}
		else{
			
		}
		return "./configuration/lexical/temp/NULL.xml";
	}

	/**
	 * 
	 * @param pathFile
	 */
	public void loadTemp(String pathFile) {
		if ((pathFile != null) && (!pathFile.trim().equalsIgnoreCase(""))) {

			try {
				XStream x = new XStream();
				FileInputStream f = new FileInputStream(pathFile);
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
				_languagePath = pathFile;

				DividerList.getInstance().load(_dividerList);
				TokenTypeList.getInstance().load(_tokenList);
				GrammarConfiguration.setPath(_grammarPath);
				ValidExtensions.getInstance().carga(_validExtensions);
				Comments.getInstance().load(_comments);

			} catch (Exception e) {
				e.printStackTrace();
				Language i = Language.getInstance();
				try {
					i.getLanguage(Integer.parseInt(PropertiesManager
							.getProperty("language")));
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				ResourceBundle labels = i.getLabels();
				JOptionPane.showMessageDialog(null, labels.getString("s526"),
						labels.getString("s526"), 0);
			}
		}
	}

	/**
	 * 
	 * @param pathFile
	 */
	public void load(String pathFile) {
		
		if ((pathFile != null) && (!pathFile.trim().equalsIgnoreCase(""))) {
			try {
				XStream x = new XStream(new DomDriver());
				FileInputStream f = new FileInputStream(pathFile);
				ProgrammingLanguage programmingLanguage = (ProgrammingLanguage) x.fromXML(f);
				String name = programmingLanguage._languageName;
				Boolean ci = programmingLanguage._isCompiledOrInterpreted;
				TokenTypeList lt = programmingLanguage._tokenList;
				String pg = programmingLanguage._grammarPath;
				ValidExtensions ex = programmingLanguage._validExtensions;
				DividerList dl = programmingLanguage._dividerList;
				Comments c = programmingLanguage._comments;
				f.close();

				_languageName = name;
				_isCompiledOrInterpreted = ci;
				_tokenList = lt;
				_grammarPath = pg;
				_validExtensions = ex;
				_dividerList = dl;
				_comments = c;
				_languagePath = pathFile;

				DividerList.getInstance().load(_dividerList);
				TokenTypeList.getInstance().load(_tokenList);
				GrammarConfiguration.setPath(_grammarPath);
				ValidExtensions.getInstance().carga(_validExtensions);
				Comments.getInstance().load(_comments);

				PropertiesManager.setProperty("languagePath", pathFile);

			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * 
	 * @return
	 */
	public ValidExtensions getValidExtensions() {
		return _validExtensions;
	}

	/**
	 * 
	 * @param validExtensions
	 */
	public void setValidExtensions(ValidExtensions validExtensions) {
		_validExtensions = validExtensions;
	}

	/**
	 * 
	 * @return
	 */
	public TokenTypeList getTokenList() {
		return _tokenList;
	}

	/**
	 * 
	 * @param tokenList
	 */
	public void setTokenList(TokenTypeList tokenList) {
		_tokenList = tokenList;
	}

	/**
	 * 
	 * @return
	 */
	public String getName() {
		return _languageName;
	}

	/**
	 * 
	 * @param name
	 */
	public void setName(String name) {
		_languageName = name;
	}

	/**
	 * 
	 * @return
	 */
	public String getGrammarPath() {
		return _grammarPath;
	}

	/**
	 * 
	 * @param grammarPath
	 */
	public void setGrammarPath(String grammarPath) {
		_grammarPath = grammarPath;
	}

	/**
	 * 
	 * @return
	 */
	public DividerList getDividerList() {
		return _dividerList;
	}

	/**
	 * 
	 * @param dividerList
	 */
	public void setDividerList(DividerList dividerList) {
		_dividerList = dividerList;
	}

	/**
	 * 
	 * @return
	 */
	public String getlanguagePath() {
		return _languagePath;
	}
}
