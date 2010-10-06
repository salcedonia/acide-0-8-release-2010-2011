package operaciones.configuracion;

import idioma.Idioma;

import java.util.ResourceBundle;

import principal.almacenPropiedades;
import es.texto.Fichero;

/**
 * Clase que guarda los datos del menu para su configuracion por parte del
 * usuario
 */
public class MenuConfig {

	private static boolean nuevoFich;
	
	private static boolean abrirFich;

	private static boolean salvarFich;
	
	private static boolean save;
	
	private static boolean saveAll;
	
	private static boolean print;

	private static boolean salir;

	private static boolean deshacer;

	private static boolean repetir;

	private static boolean copiar;

	private static boolean pegar;

	private static boolean cortar;
	
	private static boolean selectAll;
	
	private static boolean goTo;

	private static boolean buscar;

	private static boolean reemplazar;

	private static boolean nuevoProyecto;

	private static boolean abrirProyecto;

	private static boolean guardarProyecto;
	
	private static boolean añadirFichero;
	
	private static boolean removeFile;
	
	private static boolean addFolder;
	
	private static boolean removeFolder;

	private static boolean compilar;

	private static boolean ejecutar;

	private static boolean mostrarLog;
	
	private static boolean showBrowserCBox;
	
	private static boolean showShellWindowCBox;
	
	private static boolean cargarParam;

	private static boolean lexica;

	private static boolean sintactica;
	
	private static boolean loadSyntax;
	
	private static boolean modifySyntax;

	private static boolean configurar;
	
	private static boolean comandoExterno;

	private static boolean español;

	private static boolean english;
	
	private static boolean menu;
	
	private static boolean edicionIconos;

	private static boolean mostrarAyuda;

	private static boolean acercade;
	
	private static boolean closeProject;
	
	private static boolean closeFile;
	
	private static boolean closeAll;
	
	private static boolean compiler;
	
	private static boolean newLexical;
	
	private static boolean saveLexical;
	
	private static boolean saveGrammar;
	
	private static boolean setFile;
	
	private static boolean unsetFile;
	
	private static boolean setMain;
	
	private static boolean newMenu;
	
	private static boolean loadMenu;
	
	private static boolean modifyMenu;
	
	private static boolean saveMenu;
	
	private static boolean saveAsMenu;
	
	private static boolean newTB;
	
	private static boolean loadTB;
	
	private static boolean modifyTB;
	
	private static boolean saveTB;
	
	private static boolean saveAsTB;
	
	private static boolean saveAsGrammar;
	
	private static boolean saveAsLexical;
	
	private static boolean setPaths;
	
	private static boolean autoSyntaxAnalysis;
	
	private static boolean saveAsProject;
	
	//mig
	private static boolean newProjectFile;
	
	private static boolean deleteFile;
	
	private static boolean unsetMain;
	
	private static boolean setFile2;
	
	private static boolean unsetFile2;
	
	private static boolean setMain2;
	
	private static boolean unsetMain2;

	public MenuConfig() {
		// Cargar ultima configuracion
		// Por defecto todos activados
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
	 * Metodo para configurar todos los atributos a la vez por medio de un array
	 * de valores booleanos
	 * 
	 * @param valores
	 *            Array de valores booleanos para configurar los atributos de la
	 *            clase en este orden: nuevoFich, abrirFich, salvarFich, save, 
	 *            saveAll, print, salir, deshacer, repetir, copiar, pegar, cortar, 
	 *            selectAll, goTo, buscar, reemplazar, nuevoProyecto, abrirProyecto, 
	 *            guardarProyecto, añadirFichero, removeFile, addFolder, removeFolder,
	 *            compilar, ejecutar, mostrarLog, showBrowserCBox, showShellWindowCBox,
	 *            cargarParam, lexica, sintactica, loadSyntax, modifySyntax, 
	 *            configurar, comandoExterno, español, english, menu, edicionIconos,
	 *            mostrarAyuda, acercade, closeProject, closeFile, closeAll, compiler,
	 *            newLexical, saveLexical, saveGrammar, setFile, unsetFile, setMain,
	 *            newMenu, loadMenu, modifyMenu, saveMenu, saveAsMenu, newTB, loadTB,
	 *            saveTB, saveAsTB, saveAsGrammar, saveAsLexical, setPaths, autoSyntaxAnalysis,
	 *            saveAsProject
	 */
	public static void setAll(boolean[] valores) {
		nuevoFich = valores[0];
		abrirFich = valores[1];
		salvarFich = valores[2];
		save = valores[3];
		saveAll = valores[4];
		print = valores[5];
		salir = valores[6];
		deshacer = valores[7];
		repetir = valores[8];
		copiar = valores[9];
		pegar = valores[10];
		cortar = valores[11];
		selectAll = valores[12];
		goTo = valores[13];
		buscar = valores[14];
		reemplazar = valores[15];
		nuevoProyecto = valores[16];
		abrirProyecto = valores[17];
		guardarProyecto = valores[18];
		añadirFichero = valores[19];
		removeFile = valores[20];
		addFolder = valores[21];
		removeFolder = valores[22];
		compilar = valores[23];
		ejecutar = valores[24];
		mostrarLog = valores[25];
		showBrowserCBox = valores[26];
		showShellWindowCBox = valores[27];
		cargarParam = valores[28];
		lexica = valores[29];
		sintactica = valores[30];
		loadSyntax = valores[31];
		modifySyntax = valores[32];
		configurar = valores[33];
		comandoExterno = valores[34];
		español = valores[35];
		english = valores[36];
		menu = valores[37];
		edicionIconos = valores[38];
		mostrarAyuda = valores[39];
		acercade = valores[40];
		closeProject = valores[41];
		closeFile = valores[42];
		closeAll = valores[43];
		compiler = valores[44];
		newLexical = valores[45];
		saveLexical = valores[46];
		saveGrammar = valores[47];
		setFile = valores[48];
		unsetFile = valores[49];
		setMain = valores[50];
		newMenu = valores[51];
		loadMenu = valores[52];
		modifyMenu = valores[53];
		saveMenu = valores[54];
		saveAsMenu = valores[55];
		newTB = valores[56];
		loadTB = valores[57];
		modifyTB = valores[58];
		saveTB = valores[59];
		saveAsTB = valores[60];
		saveAsGrammar = valores[61];
		saveAsLexical = valores[62];
		setPaths = valores[63];
		autoSyntaxAnalysis = valores[64];
		saveAsProject = valores[65];
		//mig
		newProjectFile = valores[66];
		deleteFile = valores[67];
		unsetMain = valores[68];
		setFile2 = valores[69];
		unsetFile2 = valores[70];
		setMain2 = valores[71];
		unsetMain2 = valores[72];
	}

	/**
	 * Método que guarda la configuración de menú en un fichero cuyo nombre
	 * recibe por parámetro
	 * 
	 * @param nombre
	 *            Nombre con el que se guardará el fichero de configuración
	 * @param valores
	 *            Lista de valores booleanos que indica los valores que están
	 *            activados o desactivados en este orden: nuevoFich, abrirFich, salvarFich, 
	 *            save, saveAll, print, salir, deshacer, repetir, copiar, pegar, cortar, 
	 *            selectAll, goTo, buscar, reemplazar, nuevoProyecto, abrirProyecto, 
	 *            guardarProyecto, añadirFichero, removeFile, addFolder, removeFolder,
	 *            compilar, ejecutar, mostrarLog, showBrowserCBox, showShellWindowCBox,
	 *            cargarParam, lexica, sintactica, loadSyntax, modifySyntax, 
	 *            configurar, comandoExterno, español, english, menu, edicionIconos,
	 *            mostrarAyuda, acercade, closeProject, closeFile, closeAll, compiler,
	 *            newLexical, saveLexical, saveGrammar, setFile, unsetFile, setMain,
	 *            newMenu, loadMenu, modifyMenu, saveMenu, saveAsMenu, newTB, loadTB,
	 *            saveTB, saveAsTB, saveAsGrammar, saveAsLexical, setPaths, autoSyntaxAnalysis,
	 *            saveAsProject
	 */
	public static void guardarMenuCfgFich(String nombre, boolean valores[]) {
		String txt = "";
		txt += "nuevoFich = " + valores[0] + "\n";
		txt += "abrirFich = " + valores[1] + "\n";
		txt += "salvarFich = " + valores[2] + "\n";
		txt += "save = " + valores[3] + "\n";
		txt += "saveAll = " + valores[4] + "\n";
		txt += "print = " + valores[5] + "\n";
		txt += "salir = " + valores[6] + "\n";
		txt += "deshacer = " + valores[7] + "\n";
		txt += "repetir = " + valores[8] + "\n";
		txt += "copiar = " + valores[9] + "\n";
		txt += "pegar = " + valores[10] + "\n";
		txt += "cortar = " + valores[11] + "\n";
		txt += "selectAll = " + valores[12] + "\n";
		txt += "goTo = " + valores[13] + "\n";
		txt += "buscar = " + valores[14] + "\n";
		txt += "reemplazar = " + valores[15] + "\n";
		txt += "nuevoProyecto = " + valores[16] + "\n";
		txt += "abrirProyecto = " + valores[17] + "\n";
		txt += "guardarProyecto = " + valores[18] + "\n";
		txt += "añadirFichero = " + valores[19] + "\n";
		txt += "removeFile = " + valores[20] + "\n";
		txt += "addFolder = " + valores[21] + "\n";
		txt += "removeFolder = " + valores[22] + "\n";
		txt += "compilar = " + valores[23] + "\n";
		txt += "ejecutar = " + valores[24] + "\n";
		txt += "mostrarLog = " + valores[25] + "\n";
		txt += "showBrowserCBox = " + valores[26] + "\n";
		txt += "ShowShellWindowCBox = " + valores[27] + "\n";
		txt += "cargarParam = " + valores[28] + "\n";
		txt += "lexica = " + valores[29] + "\n";
		txt += "sintactica = " + valores[30] + "\n";
		txt += "loadSyntax = " + valores[31] + "\n";
		txt += "modifySyntax = " + valores[32] + "\n";
		txt += "configurar = " + valores[33] + "\n";
		txt += "comandoExterno = " + valores[34] + "\n";
		txt += "español = " + valores[35] + "\n";
		txt += "english = " + valores[36] + "\n";
		txt += "menu = " + valores[37] + "\n";
		txt += "edicionIconos = " + valores[38] + "\n";
		txt += "mostrarAyuda = " + valores[39] + "\n";
		txt += "acercade = " + valores[40] + "\n";
		txt += "closeProject = " + valores[41] + "\n";
		txt += "closeFile = " + valores[42] + "\n";
		txt += "closeAll = " + valores[43] + "\n";
		txt += "compiler = " + valores[44] + "\n";
		txt += "newLexical = " + valores[45] + "\n";
		txt += "saveLexical = " + valores[46] + "\n";
		txt += "saveGrammar = " + valores[47] + "\n";
		txt += "setFile = " + valores[48] + "\n";
		txt += "unsetFile = " + valores[49] + "\n";
		txt += "setMain = " + valores[50] + "\n";
		txt += "newMenu = " + valores[51] + "\n";
		txt += "loadMenu = " + valores[52] + "\n";
		txt += "modifyMenu = " + valores[53] + "\n";
		txt += "saveMenu = " + valores[54] + "\n";
		txt += "saveAsMenu = " + valores[55] + "\n";
		txt += "newTB = " + valores[56] + "\n";
		txt += "loadTB = " + valores[57] + "\n";
		txt += "modifyTB = " + valores[58] + "\n";
		txt += "saveTB = " + valores[59] + "\n";
		txt += "saveAsTB = " + valores[60] + "\n";
		txt += "saveAsGrammar = " + valores[61] + "\n";
		txt += "saveAsLexical = " + valores[62] + "\n";
		txt += "setPaths = " + valores[63] + "\n";
		txt += "autoSyntaxAnalysis = " + valores[64] + "\n";
		txt += "saveAsProject = " + valores[65] + "\n";
		//mig
		txt += "newProjectFile = " + valores[66] + "\n";
		txt += "deleteFile = " + valores[67] + "\n";
		txt += "unsetMain = " + valores[68] + "\n";
		txt += "setFile2 = " + valores[69] + "\n";
		txt += "unsetFile2 = " + valores[70] + "\n";
		txt += "setMain2 = " + valores[71] + "\n";
		txt += "unsetMain2 = " + valores[72] + "\n";
		Fichero f = new Fichero();
		f.salvar(nombre, txt);
	}

	/**
	 * Método que carga una configuración de menú previamente guardada
	 * 
	 * @param ruta
	 *            Ruta absoluta en que se encuentra el archivo de configuración
	 * @return Array de booleanos que indica los valores que están activados o
	 *         desactivados en el siguiente orden: nuevoFich, abrirFich, salvarFich, 
	 *         save, saveAll, print, salir, deshacer, repetir, copiar, pegar, cortar, 
	 *         selectAll, goTo, buscar, reemplazar, nuevoProyecto, abrirProyecto, 
	 *         guardarProyecto, añadirFichero, removeFile, addFolder, removeFolder,
	 *         compilar, ejecutar, mostrarLog, showBrowserCBox, showShellWindowCBox,
	 *         cargarParam, lexica, sintactica, loadSyntax, modifySyntax, 
	 *         configurar, comandoExterno, español, english, menu, edicionIconos,
	 *         mostrarAyuda, acercade, closeProject, closeFile, closeAll, compiler,
	 *         newLexical, saveLexical, saveGrammar, setFile, unsetFile, setMain,
	 *         newMenu, loadMenu, modifyMenu, saveMenu, saveAsMenu, newTB, loadTB,
	 *         saveTB, saveAsTB, saveAsGrammar, saveAsLexical, setPaths, autoSyntaxAnalysis,
	 *         saveAsProject
	 * @throws Exception
	 *             Errores en alguno de los parámetros de configuración que
	 *             contiene el archivo
	 */
	public static boolean[] cargarMenuCfgFich(String ruta) throws Exception {
		boolean[] valores = new boolean[73];
		Fichero f = new Fichero();
		String txt = f.cargar(ruta);
		int atributo = 0;
		char c;
		for (int index = 0; index < txt.length(); index++) {
			c = txt.charAt(index);
			if (c == '=') {
				index += 2;
				c = txt.charAt(index);
				if (c == 'f') {
					valores[atributo] = false;
					atributo++;
				}
				else if (c == 't') {
					valores[atributo] = true;
					atributo++;
				}
				else {
					Idioma i = Idioma.getInstance();
					try {
						i.seleccionIdioma(Integer.parseInt(almacenPropiedades
								.getPropiedad("idioma")));
					}
					catch (Exception e) {
						e.printStackTrace();
					}
					ResourceBundle labels = i.getLabels();
					throw new Exception(labels.getString("s533"));
				}
			}
		}
		return valores;
	}

	/**
	 * Método que activa todas las opciones del menú
	 */
	public static void todosActivados() {
		nuevoFich = true;
		abrirFich = true;
		salvarFich = true;
		save = true;
		saveAll = true;
		print = true;
		salir = true;
		deshacer = true;
		repetir = true;
		copiar = true;
		pegar = true;
		cortar = true;
		selectAll = true;
		goTo = true;
		buscar = true;
		reemplazar = true;
		nuevoProyecto = true;
		abrirProyecto = true;
		guardarProyecto = true;
		añadirFichero = true;
		removeFile = true;
		addFolder = true;
		removeFolder = true;
		compilar = true;
		ejecutar = true;
		mostrarLog = true;
		showBrowserCBox = true;
		showShellWindowCBox = true;
		cargarParam = true;
		lexica = true;
		sintactica = true;
		loadSyntax = true;
		modifySyntax = true;
		configurar = true;
		comandoExterno = true;
		español = true;
		english = true;
		menu = true;
		edicionIconos = true;		
		mostrarAyuda = true;
		acercade = true;		
		closeProject = true;
		closeFile = true;
		closeAll = true;
		compiler = true;
		newLexical = true;
		saveLexical = true;
		saveGrammar = true;
		setFile = true;
		unsetFile = true;
		setMain = true;
		newMenu = true;
		loadMenu = true;
		modifyMenu = true;
		saveMenu = true;
		saveAsMenu = true;
		newTB = true;
		loadTB = true;
		modifyTB = true;
		saveTB = true;
		saveAsTB = true;
		saveAsGrammar = true;
		saveAsLexical = true;
		setPaths = true;
		autoSyntaxAnalysis = true;
		saveAsProject = true;
		//mig
		newProjectFile = true;
		deleteFile = true;
		unsetMain = true;
		setFile2 = true;
		unsetFile2 = true;
		setMain2 = true;
		unsetMain2 = true;
	}

	/**
	 * Método que desactiva todas las opciones de menú
	 */
	public static void todosDesactivados() {
		nuevoFich = false;
		abrirFich = false;
		salvarFich = false;
		save = false;
		saveAll = false;
		print = false;
		salir = false;
		deshacer = false;
		repetir = false;
		copiar = false;
		pegar = false;
		cortar = false;
		selectAll = false;
		goTo = false;
		buscar = false;
		reemplazar = false;
		nuevoProyecto = false;
		abrirProyecto = false;
		guardarProyecto = false;
		añadirFichero = false;
		removeFile = false;
		addFolder = false;
		removeFolder = false;
		compilar = false;
		ejecutar = false;
		mostrarLog = false;
		showBrowserCBox = false;
		showShellWindowCBox = false;
		cargarParam = false;
		lexica = false;
		sintactica = false;
		loadSyntax = false;
		modifySyntax = false;
		configurar = false;
		comandoExterno = false;
		español = false;
		english = false;
		// Menu option has to be always switched on
		menu = true;
		edicionIconos = false;	
		mostrarAyuda = false;
		acercade = false;	
		closeProject = false;
		closeFile = false;
		closeAll = false;
		compiler = false;
		newLexical = false;
		saveLexical = false;
		saveGrammar = false;
		setFile = false;
		unsetFile = false;
		setMain = false;
		newMenu = false;
		loadMenu = false;
		modifyMenu = false;
		saveMenu = false;
		saveAsMenu = false;
		newTB = false;
		loadTB = false;
		modifyTB = false;
		saveTB = false;
		saveAsTB = false;
		saveAsGrammar = false;
		saveAsLexical = false;
		setPaths = false;
		autoSyntaxAnalysis = false;
		saveAsProject = false;
		//mig
		newProjectFile = false;
		deleteFile = false;
		unsetMain = false;
		setFile2 = false;
		unsetFile2 = false;
		setMain2 = false;
		unsetMain2 = false;
	}

	public static boolean getAbrirFich() {
		return abrirFich;
	}

	public static boolean getNewLexical() {
		return newLexical;
	}

	public static void setNewLexical(boolean newLexical) {
		MenuConfig.newLexical = newLexical;
	}

	public static boolean getSaveGrammar() {
		return saveGrammar;
	}

	public static void setSaveGrammar(boolean saveGrammar) {
		MenuConfig.saveGrammar = saveGrammar;
	}

	public static boolean getSaveLexical() {
		return saveLexical;
	}

	public static void setSaveLexical(boolean saveLexical) {
		MenuConfig.saveLexical = saveLexical;
	}

	public static void setAbrirFich(boolean abrirFich) {
		MenuConfig.abrirFich = abrirFich;
	}

	public static boolean getBuscar() {
		return buscar;
	}

	public static void setBuscar(boolean buscar) {
		MenuConfig.buscar = buscar;
	}

	public static boolean getCargarParam() {
		return cargarParam;
	}

	public static void setCargarParam(boolean cargarParam) {
		MenuConfig.cargarParam = cargarParam;
	}
	
	public static boolean getCopiar() {
		return copiar;
	}

	public static void setCopiar(boolean copiar) {
		MenuConfig.copiar = copiar;
	}

	public static boolean getCortar() {
		return cortar;
	}

	public static void setCortar(boolean cortar) {
		MenuConfig.cortar = cortar;
	}

	public static boolean getDeshacer() {
		return deshacer;
	}

	public static void setDeshacer(boolean deshacer) {
		MenuConfig.deshacer = deshacer;
	}

	public static boolean getEnglish() {
		return english;
	}

	public static void setEnglish(boolean english) {
		MenuConfig.english = english;
	}

	public static boolean getEspañol() {
		return español;
	}

	public static void setEspañol(boolean español) {
		MenuConfig.español = español;
	}

	public static boolean getLexica() {
		return lexica;
	}

	public static void setLexica(boolean lexica) {
		MenuConfig.lexica = lexica;
	}

	public static boolean getMenu() {
		return menu;
	}

	public static void setMenu(boolean menu) {
		MenuConfig.menu = menu;
	}

	public static boolean getMostrarLog() {
		return mostrarLog;
	}

	public static void setMostrarLog(boolean mostrarLog) {
		MenuConfig.mostrarLog = mostrarLog;
	}

	public static boolean getNuevoFich() {
		return nuevoFich;
	}

	public static void setNuevoFich(boolean nuevoFich) {
		MenuConfig.nuevoFich = nuevoFich;
	}
	
	public static boolean getNuevoProyecto() {
		return nuevoProyecto;
	}

	public static void setNuevoProyecto(boolean nuevoProyecto) {
		MenuConfig.nuevoProyecto = nuevoProyecto;
	}

	public static boolean getPegar() {
		return pegar;
	}

	public static void setPegar(boolean pegar) {
		MenuConfig.pegar = pegar;
	}

	public static boolean getReemplazar() {
		return reemplazar;
	}

	public static void setReemplazar(boolean reemplazar) {
		MenuConfig.reemplazar = reemplazar;
	}

	public static boolean getRepetir() {
		return repetir;
	}

	public static void setRepetir(boolean repetir) {
		MenuConfig.repetir = repetir;
	}

	public static boolean getSalir() {
		return salir;
	}

	public static void setSalir(boolean salir) {
		MenuConfig.salir = salir;
	}

	public static boolean getSalvarFich() {
		return salvarFich;
	}

	public static void setSalvarFich(boolean salvarFich) {
		MenuConfig.salvarFich = salvarFich;
	}

	public static boolean getSintactica() {
		return sintactica;
	}

	public static void setSintactica(boolean sintactica) {
		MenuConfig.sintactica = sintactica;
	}

	public static boolean getAbrirProyecto() {
		return abrirProyecto;
	}

	public static void setAbrirProyecto(boolean abrirProyecto) {
		MenuConfig.abrirProyecto = abrirProyecto;
	}

	public static boolean getAcercade() {
		return acercade;
	}

	public static void setAcercade(boolean acercade) {
		MenuConfig.acercade = acercade;
	}

	//mig
	public static boolean getNewProjectFile() {
		return newProjectFile;
	}
	
	public static void setNewProjectFile(boolean newProjectFile) {
		MenuConfig.newProjectFile = newProjectFile;
	}
	//
	
	public static boolean getAñadirFichero() {
		return añadirFichero;
	}

	public static void setAñadirFichero(boolean añadirFichero) {
		MenuConfig.añadirFichero = añadirFichero;
	}

	public static boolean getCompilar() {
		return compilar;
	}

	public static void setCompilar(boolean compilar) {
		MenuConfig.compilar = compilar;
	}

	public static boolean getEjecutar() {
		return ejecutar;
	}

	public static void setEjecutar(boolean ejecutar) {
		MenuConfig.ejecutar = ejecutar;
	}

	public static boolean getGuardarProyecto() {
		return guardarProyecto;
	}

	public static void setGuardarProyecto(boolean guardarProyecto) {
		MenuConfig.guardarProyecto = guardarProyecto;
	}

	public static boolean getMostrarAyuda() {
		return mostrarAyuda;
	}

	public static void setMostrarAyuda(boolean mostrarAyuda) {
		MenuConfig.mostrarAyuda = mostrarAyuda;
	}
	
	public static boolean getConfigurar() {
		return configurar;
	}

	public static void setConfigurar(boolean configurar) {
		MenuConfig.configurar = configurar;
	}
	
	public static boolean getComandoExterno() {
		return comandoExterno;
	}

	public static void setComandoExterno(boolean comandoExterno) {
		MenuConfig.comandoExterno = comandoExterno;
	}

	public static boolean getAddFolder() {
		return addFolder;
	}

	public static void setAddFolder(boolean addFolder) {
		MenuConfig.addFolder = addFolder;
	}

	public static boolean getEdicionIconos() {
		return edicionIconos;
	}

	public static void setEdicionIconos(boolean edicionIconos) {
		MenuConfig.edicionIconos = edicionIconos;
	}

	public static boolean getGoTo() {
		return goTo;
	}

	public static void setGoTo(boolean goTo) {
		MenuConfig.goTo = goTo;
	}

	public static boolean getLoadSyntax() {
		return loadSyntax;
	}

	public static void setLoadSyntax(boolean loadSyntax) {
		MenuConfig.loadSyntax = loadSyntax;
	}

	public static boolean getModifySyntax() {
		return modifySyntax;
	}

	public static void setModifySyntax(boolean modifySyntax) {
		MenuConfig.modifySyntax = modifySyntax;
	}

	public static boolean getPrint() {
		return print;
	}

	public static void setPrint(boolean print) {
		MenuConfig.print = print;
	}

	public static boolean getRemoveFile() {
		return removeFile;
	}
	
	//mig
	public static boolean getDeleteFile() {
		return deleteFile;
	}
	
	public static void setDeleteFile(boolean b) {
		MenuConfig.deleteFile=deleteFile;
	}
	//
	
	public static void setRemoveFile(boolean removeFile) {
		MenuConfig.removeFile = removeFile;
	}

	public static boolean getRemoveFolder() {
		return removeFolder;
	}

	public static void setRemoveFolder(boolean removeFolder) {
		MenuConfig.removeFolder = removeFolder;
	}

	public static boolean getSave() {
		return save;
	}

	public static void setSave(boolean save) {
		MenuConfig.save = save;
	}

	public static boolean getSaveAll() {
		return saveAll;
	}

	public static void setSaveAll(boolean saveAll) {
		MenuConfig.saveAll = saveAll;
	}

	public static boolean getSelectAll() {
		return selectAll;
	}

	public static void setSelectAll(boolean selectAll) {
		MenuConfig.selectAll = selectAll;
	}

	public static boolean getShowBrowserCBox() {
		return showBrowserCBox;
	}

	public static void setShowBrowserCBox(boolean showBrowserCBox) {
		MenuConfig.showBrowserCBox = showBrowserCBox;
	}

	public static boolean getShowShellWindowCBox() {
		return showShellWindowCBox;
	}

	public static void setShowShellWindowCBox(boolean showShellWindowCBox) {
		MenuConfig.showShellWindowCBox = showShellWindowCBox;
	}

	public static boolean getCloseProject() {
		return closeProject;
	}

	public static void setCloseProject(boolean closeProject) {
		MenuConfig.closeProject = closeProject;
	}

	public static boolean getCloseAll() {
		return closeAll;
	}

	public static void setCloseAll(boolean closeAll) {
		MenuConfig.closeAll = closeAll;
	}

	public static boolean getCloseFile() {
		return closeFile;
	}

	public static void setCloseFile(boolean closeFile) {
		MenuConfig.closeFile = closeFile;
	}

	public static boolean getCompiler() {
		return compiler;
	}

	public static void setCompiler(boolean compiler) {
		MenuConfig.compiler = compiler;
	}
	
	public static boolean getSetFile() {
		return setFile;
	}

	public static void setSetFile(boolean setFile) {
		MenuConfig.setFile = setFile;
	}

	public static boolean getSetMain() {
		return setMain;
	}

	public static void setSetMain(boolean setMain) {
		MenuConfig.setMain = setMain;
	}

	public static boolean getUnsetFile() {
		return unsetFile;
	}

	public static void setUnsetFile(boolean unsetFile) {
		MenuConfig.unsetFile = unsetFile;
	}

	public static boolean isLoadMenu() {
		return loadMenu;
	}

	public static void setLoadMenu(boolean loadMenu) {
		MenuConfig.loadMenu = loadMenu;
	}

	public static boolean isLoadTB() {
		return loadTB;
	}

	public static void setLoadTB(boolean loadTB) {
		MenuConfig.loadTB = loadTB;
	}

	public static boolean isModifyMenu() {
		return modifyMenu;
	}

	public static void setModifyMenu(boolean modifyMenu) {
		MenuConfig.modifyMenu = modifyMenu;
	}

	public static boolean isModifyTB() {
		return modifyTB;
	}

	public static void setModifyTB(boolean modifyTB) {
		MenuConfig.modifyTB = modifyTB;
	}

	public static boolean isNewMenu() {
		return newMenu;
	}

	public static void setNewMenu(boolean newMenu) {
		MenuConfig.newMenu = newMenu;
	}

	public static boolean isNewTB() {
		return newTB;
	}

	public static void setNewTB(boolean newTB) {
		MenuConfig.newTB = newTB;
	}

	public static boolean isSaveAsGrammar() {
		return saveAsGrammar;
	}

	public static void setSaveAsGrammar(boolean saveAsGrammar) {
		MenuConfig.saveAsGrammar = saveAsGrammar;
	}

	public static boolean isSaveAsLexical() {
		return saveAsLexical;
	}

	public static void setSaveAsLexical(boolean saveAsLexical) {
		MenuConfig.saveAsLexical = saveAsLexical;
	}

	public static boolean isSaveAsMenu() {
		return saveAsMenu;
	}

	public static void setSaveAsMenu(boolean saveAsMenu) {
		MenuConfig.saveAsMenu = saveAsMenu;
	}

	public static boolean isSaveAsTB() {
		return saveAsTB;
	}

	public static void setSaveAsTB(boolean saveAsTB) {
		MenuConfig.saveAsTB = saveAsTB;
	}

	public static boolean isSaveMenu() {
		return saveMenu;
	}

	public static void setSaveMenu(boolean saveMenu) {
		MenuConfig.saveMenu = saveMenu;
	}

	public static boolean isSaveTB() {
		return saveTB;
	}

	public static void setSaveTB(boolean saveTB) {
		MenuConfig.saveTB = saveTB;
	}

	public static boolean isAutoSyntaxAnalysis() {
		return autoSyntaxAnalysis;
	}

	public static void setAutoSyntaxAnalysis(boolean autoSyntaxAnalysis) {
		MenuConfig.autoSyntaxAnalysis = autoSyntaxAnalysis;
	}

	public static boolean isSetPaths() {
		return setPaths;
	}

	public static void setSetPaths(boolean setPaths) {
		MenuConfig.setPaths = setPaths;
	}

	public static boolean isSaveAsProject() {
		return saveAsProject;
	}

	public static void setSaveAsProject(boolean saveAsProject) {
		MenuConfig.saveAsProject = saveAsProject;
	}

	//mig
	public static boolean getUnsetMain() {
		return unsetMain;
	}

	public static void setUnsetMain(boolean b) {
		MenuConfig.unsetMain = b;
	}
	public static boolean getSetFile2() {
		return setFile2;
	}

	public static void setSetFile2(boolean b) {
		MenuConfig.setFile2 = b;
	}
	public static boolean getUnsetFile2() {
		return unsetFile2;
	}

	public static void setUnsetFile2(boolean b) {
		MenuConfig.unsetFile2 = b;
	}
	public static boolean getSetMain2() {
		return setMain2;
	}

	public static void setSetMain2(boolean b) {
		MenuConfig.setMain2 = b;
	}
	public static boolean getUnsetMain2() {
		return unsetMain2;
	}

	public static void setUnsetMain2(boolean b) {
		MenuConfig.unsetMain2 = b;
	}
	//
}
