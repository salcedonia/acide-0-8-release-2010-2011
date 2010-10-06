package gui;

import es.texto.Fichero;
import gui.editor.CreadorEditor;
import gui.editor.Editor;
import gui.explorador.Explorador;
import gui.menus.Menu;
import gui.parametrizacion.ProyectoGUI;
import gui.iconos.Iconos;
import gui.salida.Salida;
import idioma.Idioma;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.event.KeyListener;
import java.util.ResourceBundle;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;

import org.apache.log4j.Logger;

import operaciones.configuracion.DefaultConfiguration;
import operaciones.configuracion.Fich;
import operaciones.configuracion.ListaIconosEditables;
import operaciones.configuracion.ProyectoConfig;
import operaciones.fabrica.FactoriaES;
import operaciones.fabrica.FactoriaGUI;
import operaciones.fabrica.FactoriaOperaciones;
import operaciones.log.Log;
import principal.almacenPropiedades;

/**
 * Clase de construye la ventana de trabajo y agrega componentes como por
 * ejemplo area de texto, explorador, menus,etc
 */
public class Ventana extends JFrame{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Atributo: Instancia Ventana
	 * 
	 */
	private static Ventana instancia;

	// Crea una unica instancia de Ventana
	public static Ventana getInstance() {
		if (instancia == null) instancia = new Ventana();
		return instancia;
	}

	/**
	 * Atributo: Editor
	 * 
	 */
	private CreadorEditor cEditor;

	/*
	 * Atributo:Menu
	 * 
	 */
	private Menu nuevoMenu;

	/**
	 * Atributo: Salida
	 * 
	 */
	private Salida nuevaSalida;

	/**
	 * Atributo StatusBar;
	 * 
	 */
	private StatusBar statusBar;

	/**
	 * Atributo:Explorador
	 * 
	 */
	private Explorador nuevoExplorador;

	/**
	 * Atributo:Iconos
	 * 
	 */
	// private Iconos nuevoIcono;
	private JToolBar barraHerrFija;

	// private JToolBar barraHerrEditable;

	/**
	 * Atributo: Configuracion del proyecto
	 * 
	 */
	private ProyectoConfig nuevoproy;

	/**
	 * Atributo: ProyectoGUI
	 * 
	 */
	private ProyectoGUI proyGUI;


	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */

	private Logger logger = Log.getLog();

	private JSplitPane splitPaneV;

	private JSplitPane splitPaneH;

	public Ventana() {
		
		Idioma i = Idioma.getInstance();
		StartingWindow.setIniciandoLabel("22");
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		StartingWindow.setIniciandoLabel("25");
		final ResourceBundle labels = i.getLabels();
		StartingWindow.setIniciandoLabel("27");
		this.addWindowListener(new java.awt.event.WindowAdapter() {
			// TODO Añadir operaciones para recuperar las configuraciones
			// anteriores de menu, gramatica, lexico, toolbar si no ha sido
			// salvadas
			public void windowClosing(java.awt.event.WindowEvent evt) {
				nuevaSalida.ejecutaComandoSalida();
				try {
					String currentMenu = almacenPropiedades.getPropiedad("currentMenuCfg");
					if((currentMenu.endsWith("lastModified.menuCfg")) || (currentMenu.endsWith("newMenu.menuCfg"))) {
						String previous = almacenPropiedades.getPropiedad("previousMenuCfg");
						almacenPropiedades.setPropiedad("currentMenuCfg",previous);
					}
					String currentTB = almacenPropiedades.getPropiedad("currentTBCfg");
					if((currentTB.endsWith("lastModified.BHcfg")) || currentTB.endsWith("newTB.BHcfg")) {
						String previous = almacenPropiedades.getPropiedad("previousTBCfg");
						almacenPropiedades.setPropiedad("currentTBCfg",previous);
					}
					String currentGrammar = almacenPropiedades.getPropiedad("currentGrammar");
					if((currentGrammar.endsWith("lastModified.jar")) || (currentGrammar.endsWith("newGrammar.jar"))) {
						String previous = almacenPropiedades.getPropiedad("previousGrammar");
						almacenPropiedades.setPropiedad("currentGrammar",previous);
					}
				}
				catch (Exception e) {
					JOptionPane.showMessageDialog(null, e.getMessage(), labels
							.getString("s294"), JOptionPane.ERROR_MESSAGE);
				}
			}
		});		
		StartingWindow.setIniciandoLabel("30");
		logger.info(labels.getString("s67"));
		StartingWindow.setIniciandoLabel("32");
		JPanel contentPane;
		StartingWindow.setIniciandoLabel("35");
		contentPane = (JPanel) this.getContentPane();
		StartingWindow.setIniciandoLabel("37");
		//this.setSize(new Dimension(800, 700));
		StartingWindow.setIniciandoLabel("40");
		this.setTitle(labels.getString("s425"));
		StartingWindow.setIniciandoLabel("42");
		// Al menu se le paso la ventana (Objeto Oyente)
		FactoriaGUI fact = FactoriaGUI.getInstance();
		StartingWindow.setIniciandoLabel("43");
		nuevoMenu = fact.generaMenu();
		StartingWindow.setIniciandoLabel("45");
		// nuevoMenu.getMenuBar().addKeyListener(new Keyboard());
		// fact = FactoriaGUI.getInstance();
		nuevoExplorador = fact.generaExplorador();
		StartingWindow.setIniciandoLabel("47");
		// nuevoExplorador.addKeyListener(new Keyboard());
		// fact = FactoriaGUI.getInstance();
		cEditor = fact.generaCreadorEditor();
		StartingWindow.setIniciandoLabel("50");
		// Genera nueva pestaña del editor
		// fact = FactoriaGUI.getInstance();
		nuevaSalida = fact.generaSalida();
		StartingWindow.setIniciandoLabel("52");
		// STATUSBAR DE PRUEBA
		statusBar = fact.buildStatusBar();
		StartingWindow.setIniciandoLabel("55");
		contentPane.add(statusBar.getSBar(), BorderLayout.SOUTH);
		StartingWindow.setIniciandoLabel("57");
		// //////////////////////////////////////////////////

		// nuevaSalida.addKeyListener(new Keyboard());
		// fact = FactoriaGUI.getInstance();
		// nuevoIcono = fact.generaIconos();
		/*
		 * barraHerrFija = Iconos.generaToolBarFija();
		 * 
		 * /////////////////////////////////////////////////// // METER
		 * COMPROBACION DE QUE HAY ICONOS EN LA LISTA EDITABLE // CARGAR LA
		 * ULTIMA LISTA USADA QUE DEBERIA ESTAR EN EL ARCHIVO DE PROPIEDADES
		 * //////////////////////////////////////////////////// try {
		 * ListaIconosEditables.limpiaListas();
		 * ListaIconosEditables.cargaLista(".\\configuration\\toolbar\\listaBH.BHcfg"); }
		 * catch (Exception e) { JOptionPane.showMessageDialog(null,
		 * labels.getString("s127"), labels.getString("s128"),
		 * JOptionPane.ERROR_MESSAGE); } barraHerrFija =
		 * Iconos.generaToolBarEditable();
		 */
		cargaBHinicial();
		StartingWindow.setIniciandoLabel("60");
		FactoriaOperaciones facto = FactoriaOperaciones.getInstance();
		StartingWindow.setIniciandoLabel("62");
		nuevoproy = facto.generaProyectoConfig();
		StartingWindow.setIniciandoLabel("65");
		// Establece menu
		this.setJMenuBar(nuevoMenu.getMenuBar());
		StartingWindow.setIniciandoLabel("67");
		// Contenido del frame
		// contentPane.add(nuevoIcono, BorderLayout.NORTH);
		/*
		 * JPanel bhPanel = new JPanel(); bhPanel.setLayout(new FlowLayout());
		 * bhPanel.add(barraHerrFija); bhPanel.add(barraHerrEditable);
		 */
		contentPane.add(barraHerrFija, BorderLayout.NORTH);
		StartingWindow.setIniciandoLabel("70");
		/*
		 * contentPane.add(barraHerrEditable, BorderLayout.NORTH);
		 * contentPane.add(bhPanel, BorderLayout.NORTH);
		 */
		splitPaneV = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
				nuevoExplorador, cEditor.dameEditores());
		StartingWindow.setIniciandoLabel("72");
		splitPaneV.setResizeWeight(0.05);
		StartingWindow.setIniciandoLabel("75");
		splitPaneV.setContinuousLayout(true);
		StartingWindow.setIniciandoLabel("77");
		splitPaneH = new JSplitPane(JSplitPane.VERTICAL_SPLIT, splitPaneV,
				nuevaSalida);
		StartingWindow.setIniciandoLabel("80");
		splitPaneH.setResizeWeight(0.9);
		StartingWindow.setIniciandoLabel("82");
		splitPaneH.setContinuousLayout(true);
		StartingWindow.setIniciandoLabel("85");
		contentPane.add(splitPaneH, BorderLayout.CENTER);
		StartingWindow.setIniciandoLabel("87");
		// Cierra ventanas automaticamente
		this.setDefaultCloseOperation(EXIT_ON_CLOSE);
		StartingWindow.setIniciandoLabel("90");
		// this.setLocation(Component.CENTER_ALIGNMENT,CENTER_ALIGNMENT);
		this.setLocationRelativeTo(null);
		StartingWindow.setIniciandoLabel("95");
		//this.setVisible(true);
		this.addWindowListener(new Win());
		logger.info(labels.getString("s66"));
		StartingWindow.closeStartingWindow();
		
		
	}

	public Editor getnuevoEditor() {
		return (Editor) cEditor.dameEditorI(0);
	}

	public Salida getnuevaSalida() {
		return nuevaSalida;
	}

	// Metodo que bloquea la Ventana principal
	public void deshabilitaVentana() {
		this.setEnabled(false);
	}

	/**
	 * Metodo que obtiene el menu
	 * 
	 * @return
	 */
	public Menu getnuevoMenu() {
		return nuevoMenu;
	}

	public CreadorEditor getCreadorEditor() {
		return cEditor;
	}

	public ProyectoConfig getProyecto() {
		return nuevoproy;
	}

	/**
	 * Metodo que estable los oyentes del menu
	 * 
	 * 
	 */
	public void estableceMenuOyentes() {
		nuevoMenu.estableceOyentesMenu();
	}

	/*
	 * public void actualizaBH() { Idioma i = Idioma.getInstance();
	 * 
	 * try { i.seleccionIdioma(Integer.parseInt(almacenPropiedades
	 * .getPropiedad("idioma"))); } catch (Exception e) { e.printStackTrace(); }
	 * ResourceBundle labels = i.getLabels(); barraHerrFija =
	 * Iconos.generaToolBarFija();
	 * 
	 * /////////////////////////////////////////////////// // METER COMPROBACION
	 * DE QUE HAY ICONOS EN LA LISTA EDITABLE // CARGAR LA ULTIMA LISTA USADA
	 * QUE DEBERIA ESTAR EN EL ARCHIVO DE PROPIEDADES
	 * ////////////////////////////////////////////////////
	 * 
	 * try { ListaIconosEditables.limpiaListas();
	 * ListaIconosEditables.cargaLista(".\\configuration\\toolbar\\ultimaConfigBH.BHcfg");
	 * ListaIconosEditables.añadirIcono(new IconoEditable()); } catch (Exception
	 * e) { JOptionPane.showMessageDialog(null, labels.getString("s127"),
	 * labels.getString("s128"), JOptionPane.ERROR_MESSAGE); } barraHerrFija =
	 * Iconos.generaToolBarEditable(); }
	 */

	public void cargaBHinicial() {
		Idioma i = Idioma.getInstance();

		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		barraHerrFija = Iconos.generaToolBarFija();

		// /////////////////////////////////////////////////
		// METER COMPROBACION DE QUE HAY ICONOS EN LA LISTA EDITABLE
		// CARGAR LA ULTIMA LISTA USADA QUE DEBERIA ESTAR EN EL ARCHIVO DE
		// PROPIEDADES
		// //////////////////////////////////////////////////
		
		String currentTBCfg = null;
		try {
			ListaIconosEditables.limpiaListas();
			currentTBCfg = almacenPropiedades.getPropiedad("currentTBCfg");
			ListaIconosEditables.cargaLista(currentTBCfg);
			almacenPropiedades.setPropiedad("currentTBCfg",currentTBCfg);
		}
		catch (Exception e) {
			//sacar ruta relativa
			String currentTBCfg2;
			int index = currentTBCfg.lastIndexOf("\\");
			if (index == -1) index = currentTBCfg.lastIndexOf("/");
			currentTBCfg2 = ".\\configuration\\toolbar\\"+currentTBCfg.substring(index + 1,currentTBCfg.length());
			try {
				ListaIconosEditables.cargaLista(currentTBCfg2);
				JOptionPane.showMessageDialog(null, labels.getString("s958")+currentTBCfg+labels.getString("s957")+currentTBCfg2);
				almacenPropiedades.setPropiedad("currentTBCfg",currentTBCfg2);
			} 
			catch (Exception e1) {
				logger.error(labels.getString("s127"));
				try {
					ListaIconosEditables.cargaLista(".//configuration/toolbar/default.BHcfg");
					JOptionPane.showMessageDialog(null, labels.getString("s958")+currentTBCfg+labels.getString("s959"));
					almacenPropiedades.setPropiedad("currentTBCfg",".//configuration/toolbar/default.BHcfg");
				} catch (HeadlessException e2) {
					// TODO Auto-generated catch block
					e2.printStackTrace();
				} catch (Exception e2) {
					// TODO Auto-generated catch block
					e2.printStackTrace();
				}
			}
			logger.error(labels.getString("s127"));
		}
		barraHerrFija = Iconos.generaToolBarEditable();
		Iconos.generaToolBarFija();	
		//Ventana.getInstance().validate();
		//Ventana.getInstance().repaint();
	}

	
	
	public void closeDefaultPrj(){
		
		try {
			String file=almacenPropiedades.getPropiedad("DefaultAcidePrj");
			
		if (file.equals(".//configuration/Default.acidePrj")){
		FactoriaES fact=new FactoriaES();
		Fichero f=fact.generaFichero();
		getProyecto().borraFich();
		
		getProyecto().setNumFich(Integer.toString(getCreadorEditor().dameNumEditores()));
		
		for (int i = 0; i < getCreadorEditor().dameNumEditores(); i++) {
			Fich fic=new Fich();
			fic.setPath(getCreadorEditor().dameEditorI(i).getPath());
			fic.setSetFile(getCreadorEditor().dameEditorI(i).isCompilerFile());
			fic.setMainFile(getCreadorEditor().dameEditorI(i).isMainFile());
			fic.setName(getCreadorEditor().dameEditorI(i).getName());
			fic.setDirectory(false);
			getProyecto().setfich(fic);
			Ventana v = Ventana.getInstance();
			/*for(int j=0; j<v.getProyecto().dameNumFich(); j++){
				if(v.getProyecto().getfich(j).getPath().equals(getCreadorEditor().dameEditorI(i).getPath()))
					fic.setSetFile(v.getProyecto().getfich(j).isSetFile());
					fic.setMainFile(v.getProyecto().getfich(j).isMainFile());
			}*/
				
		}
		getProyecto().setLenguaje(almacenPropiedades.getPropiedad("idioma"));
		getProyecto().setnombreProy("");
		String cad = getProyecto().salvarProy();
		f.salvar(".//configuration/Default.acidePrj", cad);
		almacenPropiedades.setPropiedad("DefaultAcidePrj", ".//configuration/Default.acidePrj");
		}
		
		
		
	} catch (Exception e) {
		e.printStackTrace();
	  }
	}
	
	public Explorador getNuevoExplorador() {
		return nuevoExplorador;
	}

	public void setNuevoExplorador(Explorador nuevoExplorador) {
		this.nuevoExplorador = nuevoExplorador;
	}

	public ProyectoGUI getProyGUI() {
		return proyGUI;
	}

	public void setProyGUI(ProyectoGUI proyGUI) {
		this.proyGUI = proyGUI;
	}

	public StatusBar getStatusBar() {
		return statusBar;
	}

	public JSplitPane getSplitPaneH() {
		return splitPaneH;
	}

	public void setSplitPaneH(JSplitPane splitPaneH) {
		this.splitPaneH = splitPaneH;
	}
	
	public void setTamExplorador(int i) {
		nuevoExplorador.setTam_explorer(i);
	}
	
	public JSplitPane getSplitPaneV() {
		return splitPaneV;
	}

	public void setSplitPaneV(JSplitPane splitPaneV) {
		this.splitPaneV = splitPaneV;
	}

	
	public class Win implements WindowListener {
		

		public void windowActivated(WindowEvent arg0) {
			// TODO Auto-generated method stub
			
		}

		public void windowClosed(WindowEvent arg0) {
			// TODO Auto-generated method stub
	
		}

		public void windowClosing(WindowEvent arg0) {
			Idioma i = Idioma.getInstance();
			try {
				i.seleccionIdioma(Integer.parseInt(almacenPropiedades
						.getPropiedad("idioma")));
			}
			catch (Exception e) {
				e.printStackTrace();
			}
			final ResourceBundle labels = i.getLabels();
			
			//mig
			Ventana v=Ventana.getInstance();
			boolean c=false;
			
			if(v.getProyecto().isModified()){
				
				int res=JOptionPane.showConfirmDialog(null, labels.getString("s657"), labels.getString("s953"), JOptionPane.YES_NO_OPTION);
				
				if (res==JOptionPane.OK_OPTION){
					v.getnuevoMenu().getGuardarProyecto().setEnabled(true);
					v.getnuevoMenu().getGuardarProyecto().doClick();
					//v.getnuevoMenu().getGuardarProyecto().setEnabled(false);
				}
			
			}
			int eS=v.getCreadorEditor().getEditorSeleccionado();
			int editor=v.getCreadorEditor().dameNumEditores();
			v.getCreadorEditor().setEditorSeleccionado(editor-1);		
			for (int z=editor-1;z>=0;z--){
				v.getCreadorEditor().setEditorSeleccionado(z);
			  	if (v.getCreadorEditor().isRedButton()==true){
			  		int opt=JOptionPane.showConfirmDialog(null,labels.getString("s643"),labels.getString("s953"),JOptionPane.YES_NO_OPTION);
					
					if(opt==JOptionPane.OK_OPTION){
						v.getnuevoMenu().save_o_saveAS();
					}
			  	}
			}
			v.getCreadorEditor().setEditorSeleccionado(eS);
			if(!c){
			
			getnuevaSalida().ejecutaComandoSalida();
			try {
				String currentMenu = almacenPropiedades.getPropiedad("currentMenuCfg");
				if((currentMenu.endsWith("lastModified.menuCfg")) || (currentMenu.endsWith("newMenu.menuCfg"))) {
					String previous = almacenPropiedades.getPropiedad("previousMenuCfg");
					almacenPropiedades.setPropiedad("currentMenuCfg",previous);
				}
				String currentTB = almacenPropiedades.getPropiedad("currentTBCfg");
				if((currentTB.endsWith("lastModified.BHcfg")) || currentTB.endsWith("newTB.BHcfg")) {
					String previous = almacenPropiedades.getPropiedad("previousTBCfg");
					almacenPropiedades.setPropiedad("currentTBCfg",previous);
				}
				String currentGrammar = almacenPropiedades.getPropiedad("currentGrammar");
				if((currentGrammar.endsWith("lastModified.jar")) || (currentGrammar.endsWith("newGrammar.jar"))) {
					String previous = almacenPropiedades.getPropiedad("previousGrammar");
					almacenPropiedades.setPropiedad("currentGrammar",previous);
				}
			}
			catch (Exception e) {
				JOptionPane.showMessageDialog(null, e.getMessage(), labels
						.getString("s294"), JOptionPane.ERROR_MESSAGE);
			}
          ////////////SAVE DEFAULT CONFIGURATION//////
		//FactoriaOperaciones facto=FactoriaOperaciones.getInstance();
		//DefaultConfiguration dc=facto.buildDefaultConfiguration();
		//dc.saveDefaultConf();
        closeDefaultPrj();
		/////////////////
		v.getProyecto().salvarProy2();
		
			}
		}

		public void windowDeactivated(WindowEvent arg0) {
			// TODO Auto-generated method stub
			
		}

		public void windowDeiconified(WindowEvent arg0) {
			// TODO Auto-generated method stub
			
		}

		public void windowIconified(WindowEvent arg0) {
			// TODO Auto-generated method stub
			
		}

		public void windowOpened(WindowEvent arg0) {
			// TODO Auto-generated method stub
			
		}
	}


}// class
