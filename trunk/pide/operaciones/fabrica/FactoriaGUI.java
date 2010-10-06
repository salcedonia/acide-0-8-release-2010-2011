package operaciones.fabrica;

import idioma.Idioma;
import gui.StatusBar;
import gui.Ventana;
import gui.editor.CreadorEditor;
import gui.explorador.Explorador;
import gui.iconos.Iconos;
import gui.log.LogTab;
import gui.menus.About;
import gui.menus.PrintGUI;
import gui.menus.SearchGUI;
import gui.menus.Menu;
import gui.menus.ReplaceGUI;
import gui.parametrizacion.ComandoExternoGUI;
import gui.parametrizacion.CompilerGUI;
import gui.parametrizacion.ConfigurarConsolaGUI;
import gui.parametrizacion.EdicionIconosGUI;
import gui.parametrizacion.ExecutionGUI;
import gui.parametrizacion.LenguajeGUI;
import gui.parametrizacion.LexicaGUI;
import gui.parametrizacion.MenuGUI;
import gui.parametrizacion.ProyectoGUI;
import gui.parametrizacion.SintacticaGUI;
import gui.salida.Salida;

/**Clase que crea las clases interfaz por medio de Factorias Abstractas singleton
 * 
 *
 */
public class FactoriaGUI {

	private static FactoriaGUI instancia;

	//Crea una unica instancia de FactoriaGUI
	public static FactoriaGUI getInstance() {
		if (instancia == null)
			instancia = new FactoriaGUI();
		return instancia;
	}

	public Ventana generaVentana() {
		return new Ventana();
	}

	public Menu generaMenu() {
		return new Menu();
	}
	
	public About buildAbout(){
		return new About();
	}
	public ReplaceGUI generaReemplazarGUI(){
		return new ReplaceGUI();
	}
	
	public SearchGUI generaBuscarGUI(){
		return new SearchGUI();
	}
	
 	public Iconos generaIconos() {
		return new Iconos();
	}

	public Salida generaSalida() {
		return new Salida(true);
	}

	public Explorador generaExplorador() {
		return new Explorador();
	}

	public MenuGUI generaMenuGUI() {
		return new MenuGUI(false);
	}
	public SintacticaGUI generaSintacticaGUI() {
		return new SintacticaGUI(false);
	}
	public LexicaGUI generaLexicaGUI() {
		return new LexicaGUI();
	}
	public CreadorEditor generaCreadorEditor() {
		return new CreadorEditor();
	}
	public LogTab generaLogTab() {
		return new LogTab();
	}
	
	public Idioma generaIdioma(){
		return new Idioma();
	}
	
	public LenguajeGUI generaLenguajeGUI() {
		return new LenguajeGUI();
	}
	
	public ProyectoGUI generaProyectoGUI() {
		return new ProyectoGUI();
	}
	
	public ConfigurarConsolaGUI generaConfigurarConsolaGUI(){
		return new ConfigurarConsolaGUI();
	}
	
	public ComandoExternoGUI generaComandoExternoGUI(){
		return new ComandoExternoGUI();
	}
	
	public EdicionIconosGUI generaEdicionIconosGUI() {
		return new EdicionIconosGUI(false);
	}
    public StatusBar buildStatusBar(){
    	return new StatusBar();
    }
   public PrintGUI buildPrintGUI(){
	   return new PrintGUI();
   }
  public ExecutionGUI buildExecutionGUI(){
	  return new ExecutionGUI();
  }
  public CompilerGUI buildCompilerGUI(){
	  return new CompilerGUI();
  }
}
