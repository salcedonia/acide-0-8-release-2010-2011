package operaciones.configuracion;

import es.configuracion.lenguajeprog.Lenguaje;
import es.texto.Fichero;
import gui.Ventana;
import gui.parametrizacion.SintacticaGUI;

import java.util.ArrayList;

import principal.almacenPropiedades;

/**Clase que almacena la configuración del proyecto
 * 
 *
 */
public class ProyectoConfig {

/**Nombre del proyecto
 * 
 */
	
private String nombreProy;

/**Path del proyecto
 * 
 */
private String pathProy;

/**Configuracion del lenguaje asociado al proyecto
 * Incluye también el nombre de la configuracion del lenguaje
 */
private String lenguajeConfig;


private String grammarConfig;

/**El Path del compilador
 * 
 */
private String pathEjecu;

/**Argumentos del compilador
 * 
 */
private String arg;

/**El Path del  intérprette
 * 
 */

//mig
private String shellPath;

private String echoCommand;

private String exitCommand;

private String shellDir;

private boolean checkCompiler;

private String separatorFile;

private String extensionFile;

private String lenguaje;

private String currentMenu;

private String currentTB;

private boolean firstSave;

private boolean modified;

//atributos de la antigua defaulfconf

private boolean explorer;

private boolean shell;

private int widthWindow;

private int heightWindow;

//Position x window
private int posx;

//position y windows
private int posy;

//panels
private int width1;
private int height1;

/**Ficheros asociados al proyecto
 * 
 */
private ArrayList fich;

/**Almancena el numero de ficheros asociados
 * 
 */
private String numFich;

public ProyectoConfig(){
	fich=new ArrayList();
}

//GETTERS
public String getnombreProy(){
	return nombreProy;
}

public String getpathProy(){
	return pathProy;
}

public String getNumFich() {
	if (numFich!=null) return numFich;
	return "0";
}
//mig
public int getFichSize() {
	
	return fich.size();
}

public void setNumFich(String numFich) {
	this.numFich = numFich;
}

public String getpathEjecu(){
	return pathEjecu;
}
//mig
public String getShellPath() {
	return shellPath;
}

public String getarg(){
	return arg;
}
public Fich getfich(int indice){
	return (Fich) fich.get(indice);
}
public String getLenguajeConfig() {
	return lenguajeConfig;
}

//SETTERS
public void setnombreProy(String np){
	nombreProy=np;
}

public void setpathProy(String pp){
	pathProy=pp;
}


public void setpathEjecu(String pe){
	pathEjecu=pe;
}

public void setarg(String a){
	arg=a;
}
//mig
public void setShellPath(String s){
	shellPath=s;
}
public void setfich(Fich f){
	fich.add(f);
}


public void setLenguajeConfig(String lenguajeConfig) {
	this.lenguajeConfig = lenguajeConfig;
}

/**Salva parámetros del proyecto en un documento de texto
 * 
 *
 *@return String
 */
public String salvarProy(){
	Ventana v = Ventana.getInstance();
	String texto="";
	texto=texto+getnombreProy()+"\n";
	texto=texto+lenguajeConfig+"\n";
	texto=texto+grammarConfig+"\n";
    texto=texto+getpathEjecu()+"\n";
	texto=texto+getarg()+"\n";
	texto=texto+getShellPath()+"\n";
	texto=texto+getShellDir()+"\n";
    texto=texto+getLenguaje()+"\n";
    texto=texto+getCurrentMenu()+"\n";
    texto=texto+getCurrentTB()+"\n";
    //antigua defaultconf
    if (v.getnuevoMenu().getShowBrowserCBox().isSelected()) explorer=true;
		else explorer=false;
    texto=texto+isExplorer()+"\n";
    if (v.getnuevoMenu().getShowShellWindowCBox().isSelected()) shell=true;
		else shell=false;
    texto=texto+isShell()+"\n";
    texto=texto+v.getWidth()+"\n";
    texto=texto+v.getHeight()+"\n";
    texto=texto+v.getX()+"\n";
    texto=texto+v.getY()+"\n";
    texto=texto+v.getSplitPaneV().getDividerLocation()+"\n";
    texto=texto+v.getSplitPaneH().getDividerLocation()+"\n";
    //
	//Numero de ficheros asociados
	texto=texto+dameNumFich()+"\n";
	//Añade los ficheros asociados
	for(int i=0;i<fich.size();i++){
           Fich f=(Fich)fich.get(i);		
		texto=texto+f.getPath()+"\n"+f.getName()+"\n"+f.getPadre()+"\n"+f.isDirectory()+"\n"+f.isSetFile()+"\n"+f.isMainFile()+"\n"+f.isOpened()+"\n";
}
		return texto;
}

/**Salva parámetros del proyecto en un documento de texto
 * Posición y tamaño de la ventana, tamaño de los paneles
 *
 *@return String
 */
public void salvarProy2(){
	
	String prj=null;
	try {
		prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
	} catch (Exception e1) {
		e1.printStackTrace();
	}
	if(!(prj.equals(".//configuration/Default.acidePrj") && Ventana.getInstance().getProyecto().getnombreProy().equals(""))){
		//Hay proyecto
		Ventana v = Ventana.getInstance();
		String t = null;
		String ruta = null;
		Fichero f = new Fichero();
		
		try {
			ruta = almacenPropiedades.getPropiedad("DefaultAcidePrj");
			t = f.cargar(ruta);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		//dividir texto en lineas
		String[] lines = t.split("\n");
		
		//sustituir valores
		lines[12] = Integer.toString(v.getWidth());
		lines[13] = Integer.toString(v.getHeight());
		lines[14] = Integer.toString(v.getX());
		lines[15] = Integer.toString(v.getY());
		lines[16] = Integer.toString(v.getSplitPaneV().getDividerLocation());
		lines[17] = Integer.toString(v.getSplitPaneH().getDividerLocation());
		
		//reconstruir texto
		String t2 = "";
		for(int i=0; i<lines.length; i++){
			t2 = t2+lines[i]+"\n";
		}
		
		//salvar texto en archivo
		try {
			f.salvar(almacenPropiedades.getPropiedad("DefaultAcidePrj"), t2);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
	}

}

/**
 * 
 * @param t
 */
public void cargaProy(String t){
int cont=0;
int contf=0;
	//Nombre del proyecto
   contf=t.indexOf("\n",cont);
   nombreProy=t.substring(cont,contf);
   cont=contf+1;
   //Path Lenguaje
   contf=t.indexOf("\n",cont);
   lenguajeConfig=t.substring(cont,contf);
   if (!lenguajeConfig.equals("null")){
    Lenguaje l=Lenguaje.getInstance();
   //String leng=".//configuration/Lexical/"+lenguajeConfig+".xml";
   // l.cargar(leng);
   }
   cont=contf+1;
   
   //Path Grammar
   contf=t.indexOf("\n",cont);
   grammarConfig=t.substring(cont,contf);
   if (!grammarConfig.equals("null")){
	  // SintacticaGUI.loadGrammarGUI();
   }
   cont=contf+1;
   
   //path Ejecutable
   contf=t.indexOf("\n",cont);
   pathEjecu=t.substring(cont,contf);
   cont=contf+1;
   
  //Argumentos
   contf=t.indexOf("\n",cont);
   arg=t.substring(cont,contf);
   cont=contf+1;
   
   //path shell
   contf=t.indexOf("\n",cont);
   shellPath=t.substring(cont,contf);
   cont=contf+1;
   
   //dir shell
   contf=t.indexOf("\n",cont);
   shellDir=t.substring(cont,contf);
   cont=contf+1;
   
   //carga idioma
   contf=t.indexOf("\n",cont);
   lenguaje=t.substring(cont,contf);
   cont=contf+1;
   
   //carga current menu
   contf=t.indexOf("\n",cont);
   currentMenu=t.substring(cont,contf);
   cont=contf+1;
   //carga current TB
   contf=t.indexOf("\n",cont);
   currentTB=t.substring(cont,contf);
   cont=contf+1;
   
   //carga atributos antigua defaultconf////////////
  //Explorer
   contf=t.indexOf("\n",cont);
   String cond=t.substring(cont,contf);
   if (cond.equals("true")) explorer=true;
   else explorer=false;
   cont=contf+1;
   
   //Shell
   contf=t.indexOf("\n",cont);
    cond=t.substring(cont,contf);
   if (cond.equals("true")) shell=true;
   else shell=false;
   cont=contf+1;	
   
   //Width
   contf=t.indexOf("\n",cont);
   cond=t.substring(cont,contf);
   widthWindow=Integer.parseInt(cond);
   cont=contf+1;
   //Height
   
   contf=t.indexOf("\n",cont);
   cond=t.substring(cont,contf);
   heightWindow=Integer.parseInt(cond);
   cont=contf+1;
   
   //Position x Window
   contf=t.indexOf("\n",cont);
   cond=t.substring(cont,contf);
   posx=Integer.parseInt(cond);
   cont=contf+1;
   
   //Position y Window
   contf=t.indexOf("\n",cont);
   cond=t.substring(cont,contf);
   posy=Integer.parseInt(cond);
   cont=contf+1;
   
   //Width1
   contf=t.indexOf("\n",cont);
   cond=t.substring(cont,contf);
   width1=Integer.parseInt(cond);
   cont=contf+1;
   //Height1
   contf=t.indexOf("\n",cont);
   cond=t.substring(cont,contf);
   height1=Integer.parseInt(cond);
   cont=contf+1;
   /////////////////////////////////////////////
   
   //Carga numero de ficheros asociados
   contf=t.indexOf("\n",cont);
   numFich=t.substring(cont,contf);
   cont=contf+1;
   //Ficheros adjuntos
    boolean check;
    boolean main;
    String name;
    String  path;
    String padre;
    boolean dir;
    boolean opened;
   
    fich.clear();
    for(int i=0;i<Integer.parseInt(this.getNumFich());i++){
    	Fich fi=new Fich();
    	contf=t.indexOf("\n",cont);
      path=t.substring(cont,contf);
       cont=contf+1;
      contf=t.indexOf("\n",cont);
      name=t.substring(cont,contf);
       cont=contf+1;
      contf=t.indexOf("\n",cont);
      padre=t.substring(cont,contf);
       cont=contf+1;
      contf=t.indexOf("\n",cont);
      if (Boolean.parseBoolean(t.substring(cont,contf))==true) dir=true; 
        else dir=false;
      cont=contf+1;
      contf=t.indexOf("\n",cont);
      if (Boolean.parseBoolean(t.substring(cont,contf))==true) check=true; 
      	else check=false;
      cont=contf+1; 
     contf=t.indexOf("\n",cont);
     if (Boolean.parseBoolean(t.substring(cont,contf))==true) main=true; 
     	else main=false;
      cont=contf+1;
      //mig
      contf=t.indexOf("\n",cont);
     if (Boolean.parseBoolean(t.substring(cont,contf))==true) opened=true; 
      	else opened=false;
       cont=contf+1; 
      fi.setMainFile(main);
      fi.setSetFile(check);
      fi.setPath(path);
      fi.setPadre(padre);
      fi.setName(name);
      fi.setDirectory(dir);
      fi.setOpened(opened);
      fich.add(fi);
	 }
}

/**Devuelve el numero de ficheros asociados al proyecto
 * 
 * @return
 */
public int dameNumFich(){
	return fich.size();
}
/**Inicializa el array de ficheros asociados al proyecto
 * 
 *
 */
public void borraFich(){
	fich.clear();
}

public void removeFich(int cont) {
	// TODO Auto-generated method stub
	fich.remove(cont);
}

public String getGrammarConfig() {
	return grammarConfig;
}

public void setGrammarConfig(String grammarConfig) {
	this.grammarConfig = grammarConfig;
}

public boolean isCheckCompiler() {
	return checkCompiler;
}

public void setCheckCompiler(boolean checkCompiler) {
	this.checkCompiler = checkCompiler;
}

public String getSeparatorFile() {
	return separatorFile;
}

public void setSeparatorFile(String separatorFile) {
	this.separatorFile = separatorFile;
}

public String getExtensionFile() {
	return extensionFile;
}

public void setExtensionFile(String extensionFile) {
	this.extensionFile = extensionFile;
}

public boolean isFirstSave() {
	return firstSave;
}

public void setFirstSave(boolean firstSave) {
	this.firstSave = firstSave;
}

public String getCurrentMenu() {
	return currentMenu;
}

public void setCurrentMenu(String currentMenu) {
	this.currentMenu = currentMenu;
}

public String getCurrentTB() {
	return currentTB;
}

public void setCurrentTB(String currentTB) {
	this.currentTB = currentTB;
}

public String getLenguaje() {
	return lenguaje;
}

public void setLenguaje(String lenguaje) {
	this.lenguaje = lenguaje;
}

public boolean isModified() {
	return modified;
}

public void setModified(boolean b) {
	this.modified = b;
}

public void setEchoCommand(String string) {
	echoCommand = string;
}

public void setExitCommand(String text) {
	exitCommand = text;
}

public void setShellDir(String text) {
	shellDir = text;
}

public String getEchoCommand() {
	return echoCommand;
}

public String getExitCommand() {
	return exitCommand;
}

public String getShellDir() {
	return shellDir;
}

public int getHeightWindow() {
	return heightWindow;
}
public void setHeightWindow(int heightWindow) {
	this.heightWindow = heightWindow;
}

public int getPosx() {
	return posx;
}
public void setPosx(int posx) {
	this.posx = posx;
}
public int getPosy() {
	return posy;
}
public void setPosy(int posy) {
	this.posy = posy;
}
public boolean isShell() {
	return shell;
}
public void setShell(boolean shell) {
	this.shell = shell;
}
public boolean isExplorer() {
	return explorer;
}
public void setExplorer(boolean e) {
	this.explorer = e;
}
public int getWidthWindow() {
	return widthWindow;
}
public void setWidthWindow(int widthWindow) {
	this.widthWindow = widthWindow;
}
public int getHeight1() {
	return height1;
}
public void setHeight1(int h) {
	height1 = h;
}
public int getWidth1() {
	return width1;
}
public void setWidth1(int h) {
	width1 = h;
}
}