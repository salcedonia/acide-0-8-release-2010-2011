package es.configuration.menu;


import java.util.ResourceBundle;

import properties.PropertiesManager;

import language.Language;

import es.text.TextFile;

/**
 * Handle the menu configuration of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class MenuConfiguration {

	/**
	 * 
	 */
	private static boolean _file;
	/**
	 * 
	 */
	private static boolean _openFile;
	/**
	 * 
	 */
	private static boolean _saveFileAs;
	/**
	 * 
	 */
	private static boolean _saveFile;
	/**
	 * 
	 */
	private static boolean _saveAllFiles;
	/**
	 * 
	 */
	private static boolean _printFile;
	/**
	 * 
	 */
	private static boolean _exit;
	/**
	 * 
	 */
	private static boolean _undo;
	/**
	 * 
	 */
	private static boolean _redo;
	/**
	 * 
	 */
	private static boolean _copy;
	/**
	 * 
	 */
	private static boolean _paste;
	/**
	 * 
	 */
	private static boolean _cut;
	/**
	 * 
	 */
	private static boolean _selectAll;
	/**
	 * 
	 */
	private static boolean _goToLine;
	/**
	 * 
	 */
	private static boolean _search;
	/**
	 * 
	 */
	private static boolean _replace;
	/**
	 * 
	 */
	private static boolean _project;
	/**
	 * 
	 */
	private static boolean _openProject;
	/**
	 * 
	 */
	private static boolean _saveProject;
	/**
	 * 
	 */
	private static boolean _addFile;
	/**
	 * 
	 */
	private static boolean _removeFile;
	/**
	 * 
	 */
	private static boolean _addFolder;
	/**
	 * 
	 */
	private static boolean _removeFolder;
	/**
	 * 
	 */
	private static boolean _compile;
	/**
	 * 
	 */
	private static boolean _execute;
	/**
	 * 
	 */
	private static boolean _showLog;
	/**
	 * 
	 */
	private static boolean _showBrowser;
	/**
	 * 
	 */
	private static boolean _showShellWindow;
	/**
	 * 
	 */
	private static boolean _loadParameters;
	/**
	 * 
	 */
	private static boolean _lexicon;
	/**
	 * 
	 */
	private static boolean _newGrammar;
	/**
	 * 
	 */
	private static boolean _loadGrammar;
	/**
	 * 
	 */
	private static boolean _modifyGrammar;
	/**
	 * 
	 */
	private static boolean _configure;
	/**
	 * 
	 */
	private static boolean _externalCommand;
	/**
	 * 
	 */
	private static boolean _spanish;
	/**
	 * 
	 */
	private static boolean _english;
	/**
	 * 
	 */
	private static boolean _menu;
	/**
	 * 
	 */
	private static boolean _toolBar;
	/**
	 * 
	 */
	private static boolean _showHelp;
	/**
	 * 
	 */
	private static boolean _showAboutUs;
	/**
	 * 
	 */
	private static boolean _closeProject;
	/**
	 * 
	 */
	private static boolean _closeFile;
	/**
	 * 
	 */
	private static boolean _closeAll;
	/**
	 * 
	 */
	private static boolean _compiler;
	/**
	 * 
	 */
	private static boolean _newlexical;
	/**
	 * 
	 */
	private static boolean _saveLexical;
	/**
	 * 
	 */
	private static boolean _saveGrammar;
	/**
	 * 
	 */
	private static boolean _setFile;
	/**
	 * 
	 */
	private static boolean _unsetFile;
	/**
	 * 
	 */
	private static boolean _setMain;
	/**
	 * 
	 */
	private static boolean _newMenu;
	/**
	 * 
	 */
	private static boolean _loadMenu;
	/**
	 * 
	 */
	private static boolean _modifyMenu;
	/**
	 * 
	 */
	private static boolean _saveMenu;
	/**
	 * 
	 */
	private static boolean _saveAsMenu;
	/**
	 * 
	 */
	private static boolean _newToolBar;
	/**
	 * 
	 */
	private static boolean _loadToolBar;
	/**
	 * 
	 */
	private static boolean _modifyToolBar;
	/**
	 * 
	 */
	private static boolean _saveToolBar;
	/**
	 * 
	 */
	private static boolean _saveAsToolBar;
	/**
	 * 
	 */
	private static boolean _saveAsGrammar;
	/**
	 * 
	 */
	private static boolean _saveAslexical;
	/**
	 * 
	 */
	private static boolean _setPaths;
	/**
	 * 
	 */
	private static boolean _autoGrammarAnalysis;
	/**
	 * 
	 */
	private static boolean _saveAsProject;
	/**
	 * 
	 */
	private static boolean _newProjectFile;
	/**
	 * 
	 */
	private static boolean _deleteFile;
	/**
	 * 
	 */
	private static boolean _unsetMain;
	/**
	 * 
	 */
	private static boolean _setFileFile;
	/**
	 * 
	 */
	private static boolean _unsetFileFile;
	/**
	 * 
	 */
	private static boolean _setMainFile;
	/**
	 * 
	 */
	private static boolean _unsetMainFile;

	/**
	 * Constructor of the class.
	 */
	public MenuConfiguration() {
	
	}

	// ///////////////////////////////////////////////////////////////
	// SOLO PARA COMPROBAR QUE FUNCIONA LA CONFIGURACION
	/*public static void muestraValores() {
		System.out.println("nuevoFich " + nuevoFich);
		System.out.println("abrirFich " + abrirFich);
		System.out.println("salvarFich " + salvarFich);
		System.out.println("salir " + salir);
		System.out.println("deshacer " + deshacer);
		System.out.println("repetir " + repetir);
		System.out.println("copiar " + copiar);
		System.out.println("pegar " + pegar);
		System.out.println("cortar " + cortar);
		System.out.println("buscar " + buscar);
		System.out.println("reemplazar " + reemplazar);
		System.out.println("nuevoProyecto " + nuevoProyecto);
		System.out.println("abrirProyecto " + abrirProyecto);
		System.out.println("guardarProyecto " + guardarProyecto);
		System.out.println("añadirFichero " + añadirFichero);
		System.out.println("compilar " + compilar);
		System.out.println("ejecutar " + ejecutar);
		System.out.println("depurar " + depurar);
		System.out.println("mostrarLog " + mostrarLog);
		System.out.println("nuevaConfLenguaje " + nuevaConfLenguaje);
		System.out.println("lexica " + lexica);
		System.out.println("sintactica " + sintactica);
		System.out.println("compilador " + compilador);
		System.out.println("interprete " + interprete);
		System.out.println("parser " + parser);
		System.out.println("menu " + menu);
		System.out.println("cargarParam " + cargarParam);
		System.out.println("salvarParam " + salvarParam);
		System.out.println("español " + español);
		System.out.println("english " + english);
		System.out.println("mostrarAyuda " + mostrarAyuda);
		System.out.println("acercade " + acercade);
		System.out.println("configurar " + configurar);
		System.out.println("comadoExterno " + comandoExterno);
	}

	public static void setPruebaConfigMenu() {
		nuevoFich = false;
		abrirFich = false;
		salvarFich = false;
		salir = false;
		deshacer = true;
		repetir = true;
		copiar = true;
		pegar = true;
		cortar = true;
		buscar = true;
		reemplazar = true;
		nuevoProyecto = false;
		abrirProyecto = false;
		guardarProyecto = false;
		añadirFichero = false;
		compilar = true;
		ejecutar = true;
		depurar = true;
		mostrarLog = true;
		nuevaConfLenguaje = true;
		lexica = true;
		sintactica = true;
		compilador = true;
		interprete = true;
		parser = true;
		menu = true;
		cargarParam = false;
		salvarParam = true;
		español = true;
		english = true;
		mostrarAyuda = false;
		acercade = true;
		configurar = true;
		comandoExterno = true;
	}*/

	//
	// /////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Set all the values to a value given as a parameter.
	 * 
	 * @param values New values to set.
	 */
	public static void setAll(boolean[] values) {
		
		_file = values[0];
		_openFile = values[1];
		_saveFileAs = values[2];
		_saveFile = values[3];
		_saveAllFiles = values[4];
		_printFile = values[5];
		_exit = values[6];
		_undo = values[7];
		_redo = values[8];
		_copy = values[9];
		_paste = values[10];
		_cut = values[11];
		_selectAll = values[12];
		_goToLine = values[13];
		_search = values[14];
		_replace = values[15];
		_project = values[16];
		_openProject = values[17];
		_saveProject = values[18];
		_addFile = values[19];
		_removeFile = values[20];
		_addFolder = values[21];
		_removeFolder = values[22];
		_compile = values[23];
		_execute = values[24];
		_showLog = values[25];
		_showBrowser = values[26];
		_showShellWindow = values[27];
		_loadParameters = values[28];
		_lexicon = values[29];
		_newGrammar = values[30];
		_loadGrammar = values[31];
		_modifyGrammar = values[32];
		_configure = values[33];
		_externalCommand = values[34];
		_spanish = values[35];
		_english = values[36];
		_menu = values[37];
		_toolBar = values[38];
		_showHelp = values[39];
		_showAboutUs = values[40];
		_closeProject = values[41];
		_closeFile = values[42];
		_closeAll = values[43];
		_compiler = values[44];
		_newlexical = values[45];
		_saveLexical = values[46];
		_saveGrammar = values[47];
		_setFile = values[48];
		_unsetFile = values[49];
		_setMain = values[50];
		_newMenu = values[51];
		_loadMenu = values[52];
		_modifyMenu = values[53];
		_saveMenu = values[54];
		_saveAsMenu = values[55];
		_newToolBar = values[56];
		_loadToolBar = values[57];
		_modifyToolBar = values[58];
		_saveToolBar = values[59];
		_saveAsToolBar = values[60];
		_saveAsGrammar = values[61];
		_saveAslexical = values[62];
		_setPaths = values[63];
		_autoGrammarAnalysis = values[64];
		_saveAsProject = values[65];
		_newProjectFile = values[66];
		_deleteFile = values[67];
		_unsetMain = values[68];
		_setFileFile = values[69];
		_unsetFileFile = values[70];
		_setMainFile = values[71];
		_unsetMainFile = values[72];
	}

	/**
	 * Save the configuration in a file.
	 * 
	 * @param name Name of the file.        
	 * @param values Values to save.
	 */
	public static void saveMenuConfigurationFile(String name, boolean values[]) {
		
		String txt = "";
		txt += "newFile = " + values[0] + "\n";
		txt += "openFile = " + values[1] + "\n";
		txt += "saveFileAs = " + values[2] + "\n";
		txt += "saveFile = " + values[3] + "\n";
		txt += "saveAllFiles = " + values[4] + "\n";
		txt += "printFile = " + values[5] + "\n";
		txt += "exit = " + values[6] + "\n";
		txt += "undo = " + values[7] + "\n";
		txt += "redo = " + values[8] + "\n";
		txt += "copy = " + values[9] + "\n";
		txt += "paste = " + values[10] + "\n";
		txt += "cut = " + values[11] + "\n";
		txt += "selectAll = " + values[12] + "\n";
		txt += "goToLine = " + values[13] + "\n";
		txt += "search = " + values[14] + "\n";
		txt += "replace = " + values[15] + "\n";
		txt += "newProject = " + values[16] + "\n";
		txt += "opneProject = " + values[17] + "\n";
		txt += "saveProject = " + values[18] + "\n";
		txt += "addFile = " + values[19] + "\n";
		txt += "removeFile = " + values[20] + "\n";
		txt += "addFolder = " + values[21] + "\n";
		txt += "removeFolder = " + values[22] + "\n";
		txt += "compile = " + values[23] + "\n";
		txt += "execute = " + values[24] + "\n";
		txt += "showLog = " + values[25] + "\n";
		txt += "showBrowser = " + values[26] + "\n";
		txt += "ShowShellWindow = " + values[27] + "\n";
		txt += "loadParameters = " + values[28] + "\n";
		txt += "lexicon = " + values[29] + "\n";
		txt += "newGrammar = " + values[30] + "\n";
		txt += "loadGrammar = " + values[31] + "\n";
		txt += "modifyGrammar = " + values[32] + "\n";
		txt += "configure = " + values[33] + "\n";
		txt += "externalCommand = " + values[34] + "\n";
		txt += "spanish = " + values[35] + "\n";
		txt += "english = " + values[36] + "\n";
		txt += "menu = " + values[37] + "\n";
		txt += "toolBar = " + values[38] + "\n";
		txt += "showHelp = " + values[39] + "\n";
		txt += "showAboutUs = " + values[40] + "\n";
		txt += "closeProject = " + values[41] + "\n";
		txt += "closeFile = " + values[42] + "\n";
		txt += "closeAll = " + values[43] + "\n";
		txt += "compiler = " + values[44] + "\n";
		txt += "newlexical = " + values[45] + "\n";
		txt += "savelexical = " + values[46] + "\n";
		txt += "saveGrammar = " + values[47] + "\n";
		txt += "setFile = " + values[48] + "\n";
		txt += "unsetFile = " + values[49] + "\n";
		txt += "setMain = " + values[50] + "\n";
		txt += "newMenu = " + values[51] + "\n";
		txt += "loadMenu = " + values[52] + "\n";
		txt += "modifyMenu = " + values[53] + "\n";
		txt += "saveMenu = " + values[54] + "\n";
		txt += "saveAsMenu = " + values[55] + "\n";
		txt += "newToolBar = " + values[56] + "\n";
		txt += "loadToolBar = " + values[57] + "\n";
		txt += "modifyToolBar = " + values[58] + "\n";
		txt += "saveToolBar = " + values[59] + "\n";
		txt += "saveAsToolBar = " + values[60] + "\n";
		txt += "saveAsGrammar = " + values[61] + "\n";
		txt += "saveAslexical = " + values[62] + "\n";
		txt += "setPaths = " + values[63] + "\n";
		txt += "autoSyntaxAnalysis = " + values[64] + "\n";
		txt += "saveAsProject = " + values[65] + "\n";
		txt += "newProjectFile = " + values[66] + "\n";
		txt += "deleteFile = " + values[67] + "\n";
		txt += "unsetMain = " + values[68] + "\n";
		txt += "setFileFile = " + values[69] + "\n";
		txt += "unsetFileFile = " + values[70] + "\n";
		txt += "setMainFile = " + values[71] + "\n";
		txt += "unsetMainFile = " + values[72] + "\n";
		TextFile f = new TextFile();
		f.save(name, txt);
	}

	/**
	 * Load the menu configuration from a file.
	 * 
	 * @param path Path of the file.
	 * 
	 * @return Values loaded.
	 * 
	 * @throws Exception
	 */
	public static boolean[] loadMenuConfigurationFile(String path) throws Exception {
		
		boolean[] values = new boolean[73];
		TextFile f = new TextFile();
		String txt = f.load(path);
		
		int atribute = 0;
		char c;
		for (int index = 0; index < txt.length(); index++) {
			c = txt.charAt(index);
			if (c == '=') {
				index += 2;
				c = txt.charAt(index);
				if (c == 'f') {
					values[atribute] = false;
					atribute++;
				}
				else if (c == 't') {
					values[atribute] = true;
					atribute++;
				}
				else {
					Language language = Language.getInstance();
					try {
						language.getLanguage(PropertiesManager.getProperty("language"));
					}
					catch (Exception e) {
						e.printStackTrace();
					}
					ResourceBundle labels = language.getLabels();
					throw new Exception(labels.getString("s533"));
				}
			}
		}
		return values;
	}

	/**
	 * Enable all the options.
	 */
	public static void allEnabled() {
		_file = true;
		_openFile = true;
		_saveFileAs = true;
		_saveFile = true;
		_saveAllFiles = true;
		_printFile = true;
		_exit = true;
		_undo = true;
		_redo = true;
		_copy = true;
		_paste = true;
		_cut = true;
		_selectAll = true;
		_goToLine = true;
		_search = true;
		_replace = true;
		_project = true;
		_openProject = true;
		_saveProject = true;
		_addFile = true;
		_removeFile = true;
		_addFolder = true;
		_removeFolder = true;
		_compile = true;
		_execute = true;
		_showLog = true;
		_showBrowser = true;
		_showShellWindow = true;
		_loadParameters = true;
		_lexicon = true;
		_newGrammar = true;
		_loadGrammar = true;
		_modifyGrammar = true;
		_configure = true;
		_externalCommand = true;
		_spanish = true;
		_english = true;
		_menu = true;
		_toolBar = true;		
		_showHelp = true;
		_showAboutUs = true;		
		_closeProject = true;
		_closeFile = true;
		_closeAll = true;
		_compiler = true;
		_newlexical = true;
		_saveLexical = true;
		_saveGrammar = true;
		_setFile = true;
		_unsetFile = true;
		_setMain = true;
		_newMenu = true;
		_loadMenu = true;
		_modifyMenu = true;
		_saveMenu = true;
		_saveAsMenu = true;
		_newToolBar = true;
		_loadToolBar = true;
		_modifyToolBar = true;
		_saveToolBar = true;
		_saveAsToolBar = true;
		_saveAsGrammar = true;
		_saveAslexical = true;
		_setPaths = true;
		_autoGrammarAnalysis = true;
		_saveAsProject = true;
		_newProjectFile = true;
		_deleteFile = true;
		_unsetMain = true;
		_setFileFile = true;
		_unsetFileFile = true;
		_setMainFile = true;
		_unsetMainFile = true;
	}

	/**
	 * Disable all the options.
	 */
	public static void allDisable() {
		_file = false;
		_openFile = false;
		_saveFileAs = false;
		_saveFile = false;
		_saveAllFiles = false;
		_printFile = false;
		_exit = false;
		_undo = false;
		_redo = false;
		_copy = false;
		_paste = false;
		_cut = false;
		_selectAll = false;
		_goToLine = false;
		_search = false;
		_replace = false;
		_project = false;
		_openProject = false;
		_saveProject = false;
		_addFile = false;
		_removeFile = false;
		_addFolder = false;
		_removeFolder = false;
		_compile = false;
		_execute = false;
		_showLog = false;
		_showBrowser = false;
		_showShellWindow = false;
		_loadParameters = false;
		_lexicon = false;
		_newGrammar = false;
		_loadGrammar = false;
		_modifyGrammar = false;
		_configure = false;
		_externalCommand = false;
		_spanish = false;
		_english = false;
		_menu = true;// Menu option has to be always switched on
		_toolBar = false;	
		_showHelp = false;
		_showAboutUs = false;	
		_closeProject = false;
		_closeFile = false;
		_closeAll = false;
		_compiler = false;
		_newlexical = false;
		_saveLexical = false;
		_saveGrammar = false;
		_setFile = false;
		_unsetFile = false;
		_setMain = false;
		_newMenu = false;
		_loadMenu = false;
		_modifyMenu = false;
		_saveMenu = false;
		_saveAsMenu = false;
		_newToolBar = false;
		_loadToolBar = false;
		_modifyToolBar = false;
		_saveToolBar = false;
		_saveAsToolBar = false;
		_saveAsGrammar = false;
		_saveAslexical = false;
		_setPaths = false;
		_autoGrammarAnalysis = false;
		_saveAsProject = false;
		//mig
		_newProjectFile = false;
		_deleteFile = false;
		_unsetMain = false;
		_setFileFile = false;
		_unsetFileFile = false;
		_setMainFile = false;
		_unsetMainFile = false;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getOpenFile() {
		return _openFile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getNewLexical() {
		return _newlexical;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveGrammar() {
		return _saveGrammar;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveLexical() {
		return _saveLexical;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSearch() {
		return _search;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getLoadParameters() {
		return _loadParameters;
	}
	
	/**
	 * 
	 * @return
	 */
	public static boolean getCopy() {
		return _copy;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getCut() {
		return _cut;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getUndo() {
		return _undo;
	}
	
	/**
	 * 
	 * @return
	 */
	public static boolean getEnglish() {
		return _english;
	}
	
	/**
	 * 
	 * @return
	 */
	public static boolean getSpanish() {
		return _spanish;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getLexicon() {
		return _lexicon;
	}
	
	/**
	 * 
	 * @return
	 */
	public static boolean getMenu() {
		return _menu;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getShowLog() {
		return _showLog;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getFile() {
		return _file;
	}
	
	/**
	 * 
	 * @return
	 */
	public static boolean getProject() {
		return _project;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getPaste() {
		return _paste;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getReplace() {
		return _replace;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getRedo() {
		return _redo;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getExit() {
		return _exit;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveFileAs() {
		return _saveFileAs;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getNewGrammar() {
		return _newGrammar;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getOpenProject() {
		return _openProject;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getShowAboutUs() {
		return _showAboutUs;
	}
	
	/**
	 * 
	 * @return
	 */
	public static boolean getNewProjectFile() {
		return _newProjectFile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getAddFile() {
		return _addFile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getCompile() {
		return _compile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getExecute() {
		return _execute;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveProject() {
		return _saveProject;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getShowHelp() {
		return _showHelp;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getConfigure() {
		return _configure;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getExternalCommand() {
		return _externalCommand;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getAddFolder() {
		return _addFolder;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getToolBar() {
		return _toolBar;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getGoToLine() {
		return _goToLine;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getLoadGrammar() {
		return _loadGrammar;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getModifyGrammar() {
		return _modifyGrammar;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getPrintFile() {
		return _printFile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getRemoveFile() {
		return _removeFile;
	}
	
	/**
	 * 
	 * @return
	 */
	public static boolean getDeleteFile() {
		return _deleteFile;
	}
	
	/**
	 * 
	 * @return
	 */
	public static boolean getRemoveFolder() {
		return _removeFolder;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveFile() {
		return _saveFile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveAllFiles() {
		return _saveAllFiles;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSelectAll() {
		return _selectAll;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getShowBrowser() {
		return _showBrowser;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getShowShellWindow() {
		return _showShellWindow;
	}
	
	/**
	 * 
	 * @return
	 */
	public static boolean getCloseProject() {
		return _closeProject;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getCloseAll() {
		return _closeAll;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getCloseFile() {
		return _closeFile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getCompiler() {
		return _compiler;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSetFile() {
		return _setFile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSetMain() {
		return _setMain;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getUnsetFile() {
		return _unsetFile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getLoadMenu() {
		return _loadMenu;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getLoadToolBar() {
		return _loadToolBar;
	}
	
	/**
	 * 
	 * @return
	 */
	public static boolean getModifyMenu() {
		return _modifyMenu;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getModifyToolBar() {
		return _modifyToolBar;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getNewMenu() {
		return _newMenu;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getNewToolBar() {
		return _newToolBar;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveAsGrammar() {
		return _saveAsGrammar;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveAslexical() {
		return _saveAslexical;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveAsMenu() {
		return _saveAsMenu;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveAsToolBar() {
		return _saveAsToolBar;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveMenu() {
		return _saveMenu;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveToolBar() {
		return _saveToolBar;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getAutoGrammarAnalysis() {
		return _autoGrammarAnalysis;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSetPaths() {
		return _setPaths;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSaveAsProject() {
		return _saveAsProject;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getUnsetMain() {
		return _unsetMain;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSetFileFile() {
		return _setFileFile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getUnsetFileFile() {
		return _unsetFileFile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getSetMainFile() {
		return _setMainFile;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean getUnsetMainFile() {
		return _unsetMainFile;
	}
}
