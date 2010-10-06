package gui.parametrizacion;

import idioma.Idioma;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import operaciones.configuracion.MenuConfig;
import operaciones.log.Log;
import org.apache.log4j.Logger;
import principal.almacenPropiedades;
import es.texto.Fichero;
import es.texto.FiltroFicheros;
import gui.Ventana;

/**
 * Clase que implementa la ventana que incluye los campos necesarios para
 * realizar la parametrizacion de los menús. 
 */

public class MenuGUI {
	
	private static final long serialVersionUID = 1L;

	private JFrame frame;
	
	private static boolean changesSaved;
	
	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();
	
	public MenuGUI(boolean modify) {
		changesSaved = true;
		if (modify) modifyMenuGUI();
		else newMenuGUI();		
	}

	public void newMenuGUI() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		logger.info(labels.getString("s531"));
		frame = new JFrame();
		frame.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		frame.setTitle(labels.getString("s298"));     
		// Creacion de todos los paneles de la ventana
		JPanel archivoPanel = new JPanel();
		archivoPanel.setLayout(new GridLayout(0,3));
		archivoPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s500")));
		JPanel edicionPanel = new JPanel();
		edicionPanel.setLayout(new GridLayout(0,3));
		edicionPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s501")));
		JPanel proyectoPanel = new JPanel();
		proyectoPanel.setLayout(new GridLayout(0,3));
		proyectoPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s502")));
		JPanel viewPanel = new JPanel();
		viewPanel.setLayout(new GridLayout(0,3));
		viewPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s503")));
		JPanel configuracionPanel = new JPanel();
		configuracionPanel.setLayout(new GridLayout(0,5));
		configuracionPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s504")));
		JPanel idiomaPanel = new JPanel();
		idiomaPanel.setLayout(new GridLayout(0,3));
		idiomaPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s505")));
		JPanel ayudaPanel = new JPanel();
		ayudaPanel.setLayout(new GridLayout(0,3));
		ayudaPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s506")));
		JPanel botonesPanel = new JPanel();
		botonesPanel.setLayout(new GridBagLayout());
		// Creacion de todas las componentes de la ventana		
		// Menu Archivo
		final JCheckBox nuevoFichCB = new JCheckBox(labels.getString("s8"));
		final JCheckBox abrirFichCB = new JCheckBox(labels.getString("s9"));
		final JCheckBox guardarFichCB = new JCheckBox(labels.getString("s10"));
		final JCheckBox saveCB = new JCheckBox(labels.getString("s617"));
		final JCheckBox saveAllCB = new JCheckBox(labels.getString("s217"));
		final JCheckBox closeFileCB = new JCheckBox(labels.getString("s238"));
		final JCheckBox closeAllCB = new JCheckBox(labels.getString("s239"));
		//mig
		final JCheckBox setFile2CB = new JCheckBox(labels.getString("s254"));
		final JCheckBox unsetFile2CB = new JCheckBox(labels.getString("s255"));
		final JCheckBox setMain2CB = new JCheckBox(labels.getString("s256"));
		final JCheckBox unsetMain2CB = new JCheckBox(labels.getString("s952"));
		final JCheckBox printCB = new JCheckBox(labels.getString("s624"));
		final JCheckBox salirFichCB = new JCheckBox(labels.getString("s13"));
		//Menu Edicion
		final JCheckBox deshacerCB = new JCheckBox(labels.getString("s21"));
		final JCheckBox repetirCB = new JCheckBox(labels.getString("s22"));
		final JCheckBox copiarCB = new JCheckBox(labels.getString("s23"));
		final JCheckBox pegarCB = new JCheckBox(labels.getString("s25"));
		final JCheckBox cortarCB = new JCheckBox(labels.getString("s24"));
		final JCheckBox selectAllCB = new JCheckBox(labels.getString("s190"));
		final JCheckBox goToCB = new JCheckBox(labels.getString("s222"));
		final JCheckBox buscarCB = new JCheckBox(labels.getString("s26"));
		final JCheckBox reemplazarCB = new JCheckBox(labels.getString("s27"));
		// Menu Proyecto
		final JCheckBox nuevoProyCB = new JCheckBox(labels.getString("s14"));
		final JCheckBox abrirProyCB = new JCheckBox(labels.getString("s15"));
		final JCheckBox guardarProyCB = new JCheckBox(labels.getString("s16"));
		final JCheckBox closeProjectCB = new JCheckBox(labels.getString("s228"));
		//mig
		final JCheckBox newProjectFileCB = new JCheckBox(labels.getString("s947"));
		final JCheckBox añadirFichProyCB = new JCheckBox(labels.getString("s17"));
		final JCheckBox removeFileCB = new JCheckBox(labels.getString("s218"));
		//mig
		final JCheckBox deleteFileCB = new JCheckBox(labels.getString("s950"));
		final JCheckBox addFolderCB = new JCheckBox(labels.getString("s219"));
		final JCheckBox removeFolderCB = new JCheckBox(labels.getString("s220"));
		final JCheckBox compilarCB = new JCheckBox(labels.getString("s18"));
		final JCheckBox ejecutarCB = new JCheckBox(labels.getString("s19"));
		final JCheckBox setFileCB = new JCheckBox(labels.getString("s254"));
		final JCheckBox unsetFileCB = new JCheckBox(labels.getString("s255"));
		final JCheckBox setMainCB = new JCheckBox(labels.getString("s256"));
		//mig
		final JCheckBox unsetMainCB = new JCheckBox(labels.getString("s952"));
		final JCheckBox saveAsProjectCB = new JCheckBox(labels.getString("s926"));
		// Menu Ver		
		final JCheckBox mostrarLogCB = new JCheckBox(labels.getString("s28"));
		final JCheckBox showBrowserCB = new JCheckBox(labels.getString("s221"));
		final JCheckBox showShellCB = new JCheckBox(labels.getString("s223"));
		// Menu Configuracion
		// Submenu lexica
		final JCheckBox newLexicalCB = new JCheckBox(labels.getString("s224") + " - " + labels.getString("s249"));
		final JCheckBox cargarParamCB = new JCheckBox(labels.getString("s224") + " - " + labels.getString("s35"));
		final JCheckBox lexicaCB = new JCheckBox(labels.getString("s224") + " - " + labels.getString("s29"));
		final JCheckBox saveLexicalCB = new JCheckBox(labels.getString("s224") + " - " + labels.getString("s250"));
		final JCheckBox saveAsLexicalCB = new JCheckBox(labels.getString("s224") + " - " + labels.getString("s285"));
		// Submenu sintactica
		final JCheckBox sintacticaCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s30"));
		final JCheckBox loadSyntaxCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s226"));
		final JCheckBox modifySyntaxCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s227"));
		final JCheckBox saveGrammarCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s251"));
		final JCheckBox saveAsGrammarCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s286"));
		final JCheckBox setPathsCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s912"));
		final JCheckBox autoSyntaxAnalysisCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s911"));
		// Submenu consola
		final JCheckBox configurarCB = new JCheckBox(labels.getString("s332") + " - " + labels.getString("s333"));
		final JCheckBox comandoExternoCB = new JCheckBox(labels.getString("s332") + " - " + labels.getString("s341"));
		// Submenu idioma
		final JCheckBox españolCB = new JCheckBox(labels.getString("s6") + " - " + labels.getString("s11"));
		final JCheckBox inglesCB = new JCheckBox(labels.getString("s6") + " - " + labels.getString("s12"));
		// Submenu menu
		final JCheckBox newMenuCB = new JCheckBox(labels.getString("s34") + " - " + labels.getString("s275"));
		final JCheckBox loadMenuCB = new JCheckBox(labels.getString("s34") + " - " + labels.getString("s276"));
		final JCheckBox modifyMenuCB = new JCheckBox(labels.getString("s34") + " - " + labels.getString("s277"));
		final JCheckBox saveMenuCB = new JCheckBox(labels.getString("s34") + " - " + labels.getString("s278"));
		final JCheckBox saveAsMenuCB = new JCheckBox(labels.getString("s34") + " - " + labels.getString("s279"));
		// Submenu toolbar
		final JCheckBox newTBCB = new JCheckBox(labels.getString("s169") + " - " + labels.getString("s280"));
		final JCheckBox loadTBCB = new JCheckBox(labels.getString("s169") + " - " + labels.getString("s281"));
		final JCheckBox modifyTBCB = new JCheckBox(labels.getString("s169") + " - " + labels.getString("s282"));
		final JCheckBox saveTBCB = new JCheckBox(labels.getString("s169") + " - " + labels.getString("s283"));
		final JCheckBox saveAsTBCB = new JCheckBox(labels.getString("s169") + " - " + labels.getString("s284"));
		// Elementos sin submenu
		final JCheckBox compilerCB = new JCheckBox(labels.getString("s240"));
		// Menu Ayuda
		final JCheckBox mostrarAyudaCB = new JCheckBox(labels.getString("s38"));
		final JCheckBox acercadeCB = new JCheckBox(labels.getString("s39"));
		// Vacios
		final JCheckBox menuCB = new JCheckBox();
		final JCheckBox edicionIconosCB = new JCheckBox();
		// Botones
		JButton aceptarBoton = new JButton(labels.getString("s507"));
		aceptarBoton.setToolTipText(labels.getString("s508"));
		JButton cancelarBoton = new JButton(labels.getString("s509"));
		cancelarBoton.setToolTipText(labels.getString("s510"));
		JButton cargarBoton = new JButton(labels.getString("s511"));
		cargarBoton.setToolTipText(labels.getString("s512"));
		JButton guardarBoton = new JButton(labels.getString("s513"));
		guardarBoton.setToolTipText(labels.getString("s514"));
		JButton todosBoton = new JButton(labels.getString("s515"));
		todosBoton.setToolTipText(labels.getString("s516"));
		JButton ningunoBoton = new JButton(labels.getString("s517"));
		ningunoBoton.setToolTipText(labels.getString("s518"));
		/* 
		 * Ponemos el checkbox de configuracion - menu siempre activo y no modificable 
		 * porque si se deselecciona no habria forma de volver a hacer cambios en 
		 * el menu 
		 */
		//menuCB.setSelected(true);
		//menuCB.setEnabled(false);
		modifyMenuCB.setSelected(true);
		modifyMenuCB.setEnabled(false);
		// Oyentes de los botones
		aceptarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				boolean[] valores = new boolean[73];
				valores[0] = nuevoFichCB.isSelected();
				valores[1] = abrirFichCB.isSelected();
				valores[2] = guardarFichCB.isSelected();
				valores[3] = saveCB.isSelected();
				valores[4] = saveAllCB.isSelected();
				valores[5] = printCB.isSelected();
				valores[6] = salirFichCB.isSelected();
				valores[7] = deshacerCB.isSelected();
				valores[8] = repetirCB.isSelected();
				valores[9] = copiarCB.isSelected();
				valores[10] = pegarCB.isSelected();
				valores[11] = cortarCB.isSelected();
				valores[12] = selectAllCB.isSelected();
				valores[13] = goToCB.isSelected();
				valores[14] = buscarCB.isSelected();
				valores[15] = reemplazarCB.isSelected();
				valores[16] = nuevoProyCB.isSelected();
				valores[17] = abrirProyCB.isSelected();
				valores[18] = guardarProyCB.isSelected();
				valores[19] = añadirFichProyCB.isSelected();
				valores[20] = removeFileCB.isSelected();
				valores[21] = addFolderCB.isSelected();
				valores[22] = removeFolderCB.isSelected();
				valores[23] = compilarCB.isSelected();
				valores[24] = ejecutarCB.isSelected();
				valores[25] = mostrarLogCB.isSelected();
				valores[26] = showBrowserCB.isSelected();
				valores[27] = showShellCB.isSelected();
				valores[28] = cargarParamCB.isSelected();
				valores[29] = lexicaCB.isSelected();
				valores[30] = sintacticaCB.isSelected();
				valores[31] = loadSyntaxCB.isSelected();
				valores[32] = modifySyntaxCB.isSelected();
				valores[33] = configurarCB.isSelected();
				valores[34] = comandoExternoCB.isSelected();
				valores[35] = españolCB.isSelected();
				valores[36] = inglesCB.isSelected();
				// opcion de menu siempre activada
				valores[37] = true;
				valores[38] = edicionIconosCB.isSelected();				
				valores[39] = mostrarAyudaCB.isSelected();
				valores[40] = acercadeCB.isSelected();
				valores[41] = closeProjectCB.isSelected();
				valores[42] = closeFileCB.isSelected();
				valores[43] = closeAllCB.isSelected();
				valores[44] = compilerCB.isSelected();
				valores[45] = newLexicalCB.isSelected();
				valores[46] = saveLexicalCB.isSelected();
				valores[47] = saveGrammarCB.isSelected();
				valores[48] = setFileCB.isSelected();
				valores[49] = unsetFileCB.isSelected();
				valores[50] = setMainCB.isSelected();
				valores[51] = newMenuCB.isSelected();
				valores[52] = loadMenuCB.isSelected();
				// modifyMenuCB option always on
				valores[53] = true;
				valores[54] = saveMenuCB.isSelected();
				valores[55] = saveAsMenuCB.isSelected();
				valores[56] = newTBCB.isSelected();
				valores[57] = loadTBCB.isSelected();
				valores[58] = modifyTBCB.isSelected();
				valores[59] = saveTBCB.isSelected();
				valores[60] = saveAsTBCB.isSelected();
				valores[61] = saveAsGrammarCB.isSelected();
				valores[62] = saveAsLexicalCB.isSelected();
				valores[63] = setPathsCB.isSelected();
				valores[64] = autoSyntaxAnalysisCB.isSelected();
				valores[65] = saveAsProjectCB.isSelected();
				//mig
				valores[66] = newProjectFileCB.isSelected();
				valores[67] = deleteFileCB.isSelected();
				valores[68] = unsetMainCB.isSelected();
				valores[69] = setFile2CB.isSelected();
				valores[70] = unsetFile2CB.isSelected();
				valores[71] = setMain2CB.isSelected();
				valores[72] = unsetMain2CB.isSelected();

				MenuConfig.setAll(valores);
				String newName = ".//configuration/menu/newMenu.menuCfg";
				MenuConfig.guardarMenuCfgFich(newName,valores);
				try {
					String previousMenu = almacenPropiedades.getPropiedad("currentMenuCfg");
					if(changesSaved)
						almacenPropiedades.setPropiedad("previousMenuCfg",previousMenu);
					almacenPropiedades.setPropiedad("currentMenuCfg",newName);
					Ventana v = Ventana.getInstance();
					v.getnuevoMenu().getSaveMenu().setEnabled(false);
					v.getnuevoMenu().setMenuConfig();
					v.validate();
					v.repaint();
					changesSaved = false;
					frame.dispose();
					logger.info(labels.getString("s519"));
					//mig
					String prj=null;
					try {
						prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					if (!(prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
						Ventana.getInstance().getProyecto().setModified(true);
					}
				
				}
				catch (Exception e1) {
					JOptionPane.showMessageDialog(null,e1.getMessage(),labels.getString("s292"),JOptionPane.ERROR_MESSAGE);
				}				
			}
		});
		cancelarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				logger.info(labels.getString("s520"));
				frame.dispose();
			}
		});
		cargarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero f = new Fichero();
				FiltroFicheros filtro = new FiltroFicheros(labels.getString("s172"));
				filtro.addExtension("menuCfg");
				String ruta = f.leer(filtro);
				boolean[] valores;
				try {
					valores = MenuConfig.cargarMenuCfgFich(ruta);
					almacenPropiedades.setPropiedad("currentMenuCfg",ruta);
					nuevoFichCB.setSelected(valores[0]);
					abrirFichCB.setSelected(valores[1]);
					guardarFichCB.setSelected(valores[2]);
					saveCB.setSelected(valores[3]);
					saveAllCB.setSelected(valores[4]);
					printCB.setSelected(valores[5]);
					salirFichCB.setSelected(valores[6]);
					deshacerCB.setSelected(valores[7]);
					repetirCB.setSelected(valores[8]);
					copiarCB.setSelected(valores[9]);
					pegarCB.setSelected(valores[10]);
					cortarCB.setSelected(valores[11]);
					selectAllCB.setSelected(valores[12]);
					goToCB.setSelected(valores[13]);
					buscarCB.setSelected(valores[14]);
					reemplazarCB.setSelected(valores[15]);
					nuevoProyCB.setSelected(valores[16]);
					abrirProyCB.setSelected(valores[17]);
					guardarProyCB.setSelected(valores[18]);
					añadirFichProyCB.setSelected(valores[19]);
					removeFileCB.setSelected(valores[20]);
					addFolderCB.setSelected(valores[21]);
					removeFolderCB.setSelected(valores[22]);
					compilarCB.setSelected(valores[23]);
					ejecutarCB.setSelected(valores[24]);
					mostrarLogCB.setSelected(valores[25]);
					showBrowserCB.setSelected(valores[26]);
					showShellCB.setSelected(valores[27]);
					cargarParamCB.setSelected(valores[28]);
					lexicaCB.setSelected(valores[29]);
					sintacticaCB.setSelected(valores[30]);
					loadSyntaxCB.setSelected(valores[31]);
					modifySyntaxCB.setSelected(valores[32]);
					configurarCB.setSelected(valores[33]);
					comandoExternoCB.setSelected(valores[34]);
					españolCB.setSelected(valores[35]);
					inglesCB.setSelected(valores[36]);
					menuCB.setSelected(true);
					edicionIconosCB.setSelected(valores[38]);					
					mostrarAyudaCB.setSelected(valores[39]);
					acercadeCB.setSelected(valores[40]);
					closeProjectCB.setSelected(valores[41]);
					closeFileCB.setSelected(valores[42]);
					closeAllCB.setSelected(valores[43]);
					compilerCB.setSelected(valores[44]);
					newLexicalCB.setSelected(valores[45]);
					saveLexicalCB.setSelected(valores[46]);
					saveGrammarCB.setSelected(valores[47]);
					setFileCB.setSelected(valores[48]);
					unsetFileCB.setSelected(valores[49]);
					setMainCB.setSelected(valores[50]);
					newMenuCB.setSelected(valores[51]);
					loadMenuCB.setSelected(valores[52]);
					// modifyMenuCB option always on
					modifyMenuCB.setSelected(true);
					saveMenuCB.setSelected(valores[54]);
					saveAsMenuCB.setSelected(valores[55]);
					newTBCB.setSelected(valores[56]);
					loadTBCB.setSelected(valores[57]);
					modifyTBCB.setSelected(valores[58]);
					saveTBCB.setSelected(valores[59]);
					saveAsTBCB.setSelected(valores[60]);
					saveAsGrammarCB.setSelected(valores[61]);
					saveAsLexicalCB.setSelected(valores[62]);
					setPathsCB.setSelected(valores[63]);
					autoSyntaxAnalysisCB.setSelected(valores[64]);
					saveAsProjectCB.setSelected(valores[65]);
					//mig
					newProjectFileCB.setSelected(valores[66]);
					deleteFileCB.setSelected(valores[67]);
					unsetMainCB.setSelected(valores[68]);
					setFile2CB.setSelected(valores[69]);
					unsetFile2CB.setSelected(valores[70]);
					setMain2CB.setSelected(valores[71]);
					unsetMain2CB.setSelected(valores[72]);
					
					changesSaved = true;
					logger.info(labels.getString("s522") + ruta + labels.getString("s523"));
				}
				catch (Exception e1) {
					logger.info(labels.getString("s521") + ruta + " " + e1.getMessage());
				}				
			}
		});
		guardarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				boolean[] valores = new boolean[73];
				valores[0] = nuevoFichCB.isSelected();
				valores[1] = abrirFichCB.isSelected();
				valores[2] = guardarFichCB.isSelected();
				valores[3] = saveCB.isSelected();
				valores[4] = saveAllCB.isSelected();
				valores[5] = printCB.isSelected();
				valores[6] = salirFichCB.isSelected();
				valores[7] = deshacerCB.isSelected();
				valores[8] = repetirCB.isSelected();
				valores[9] = copiarCB.isSelected();
				valores[10] = pegarCB.isSelected();
				valores[11] = cortarCB.isSelected();
				valores[12] = selectAllCB.isSelected();
				valores[13] = goToCB.isSelected();
				valores[14] = buscarCB.isSelected();
				valores[15] = reemplazarCB.isSelected();
				valores[16] = nuevoProyCB.isSelected();
				valores[17] = abrirProyCB.isSelected();
				valores[18] = guardarProyCB.isSelected();
				valores[19] = añadirFichProyCB.isSelected();
				valores[20] = removeFileCB.isSelected();
				valores[21] = addFolderCB.isSelected();
				valores[22] = removeFolderCB.isSelected();
				valores[23] = compilarCB.isSelected();
				valores[24] = ejecutarCB.isSelected();
				valores[25] = mostrarLogCB.isSelected();
				valores[26] = showBrowserCB.isSelected();
				valores[27] = showShellCB.isSelected();
				valores[28] = cargarParamCB.isSelected();
				valores[29] = lexicaCB.isSelected();
				valores[30] = sintacticaCB.isSelected();
				valores[31] = loadSyntaxCB.isSelected();
				valores[32] = modifySyntaxCB.isSelected();
				valores[33] = configurarCB.isSelected();
				valores[34] = comandoExternoCB.isSelected();
				valores[35] = españolCB.isSelected();
				valores[36] = inglesCB.isSelected();
				// opcion de menu siempre activada
				valores[37] = true;
				valores[38] = edicionIconosCB.isSelected();			
				valores[39] = mostrarAyudaCB.isSelected();
				valores[40] = acercadeCB.isSelected();
				valores[41] = closeProjectCB.isSelected();
				valores[42] = closeFileCB.isSelected();
				valores[43] = closeAllCB.isSelected();
				valores[44] = compilerCB.isSelected();
				valores[45] = newLexicalCB.isSelected();
				valores[46] = saveLexicalCB.isSelected();
				valores[47] = saveGrammarCB.isSelected();
				valores[48] = setFileCB.isSelected();
				valores[49] = unsetFileCB.isSelected();
				valores[50] = setMainCB.isSelected();
				valores[51] = newMenuCB.isSelected();
				valores[52] = loadMenuCB.isSelected();
				// modifyMenu option always on
				valores[53] = true;
				valores[54] = saveMenuCB.isSelected();
				valores[55] = saveAsMenuCB.isSelected();
				valores[56] = newTBCB.isSelected();
				valores[57] = loadTBCB.isSelected();
				valores[58] = modifyTBCB.isSelected();
				valores[59] = saveTBCB.isSelected();
				valores[60] = saveAsTBCB.isSelected();
				valores[61] = saveAsGrammarCB.isSelected();
				valores[62] = saveAsLexicalCB.isSelected();
				valores[63] = setPathsCB.isSelected();
				valores[64] = autoSyntaxAnalysisCB.isSelected();
				valores[65] = saveAsProjectCB.isSelected();
				//mig
				valores[66] = newProjectFileCB.isSelected();
				valores[67] = deleteFileCB.isSelected();
				valores[68] = unsetMainCB.isSelected();
				valores[69] = setFile2CB.isSelected();
				valores[70] = unsetFile2CB.isSelected();
				valores[71] = setMain2CB.isSelected();
				valores[72] = unsetMain2CB.isSelected();
				

				JFileChooser selector = new JFileChooser();
				FiltroFicheros filtro = new FiltroFicheros(labels.getString("s126"));
				filtro.addExtension("menuCfg");
				selector.setFileFilter(filtro);
				String nombreFichero = "";
				int valor = selector.showSaveDialog(selector);
				if (valor == JFileChooser.APPROVE_OPTION) {
					nombreFichero = selector.getSelectedFile().getAbsolutePath();
					if(!nombreFichero.endsWith(".menuCfg")) nombreFichero += ".menuCfg";
					MenuConfig.guardarMenuCfgFich(nombreFichero,valores);
					almacenPropiedades.setPropiedad("currentMenuCfg",nombreFichero);
					changesSaved = true;
					// Muestra operacion en el log
					logger.info(labels.getString("s528") + nombreFichero + labels.getString("s529"));				
				} 
				else if (valor == JFileChooser.CANCEL_OPTION) {
					selector.cancelSelection();
					// Muestra operacion en el log
					logger.info(labels.getString("s527"));
				}				
			}
		});
		todosBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				nuevoFichCB.setSelected(true);
				abrirFichCB.setSelected(true);
				guardarFichCB.setSelected(true);
				saveCB.setSelected(true);
				saveAllCB.setSelected(true);
				printCB.setSelected(true);
				salirFichCB.setSelected(true);
				deshacerCB.setSelected(true);
				repetirCB.setSelected(true);
				copiarCB.setSelected(true);
				pegarCB.setSelected(true);
				cortarCB.setSelected(true);
				selectAllCB.setSelected(true);
				goToCB.setSelected(true);
				buscarCB.setSelected(true);
				reemplazarCB.setSelected(true);
				nuevoProyCB.setSelected(true);
				abrirProyCB.setSelected(true);
				guardarProyCB.setSelected(true);
				añadirFichProyCB.setSelected(true);
				removeFileCB.setSelected(true);
				addFolderCB.setSelected(true);
				removeFolderCB.setSelected(true);
				compilarCB.setSelected(true);
				ejecutarCB.setSelected(true);
				mostrarLogCB.setSelected(true);
				showBrowserCB.setSelected(true);
				showShellCB.setSelected(true);
				cargarParamCB.setSelected(true);
				lexicaCB.setSelected(true);
				sintacticaCB.setSelected(true);
				loadSyntaxCB.setSelected(true);
				modifySyntaxCB.setSelected(true);
				configurarCB.setSelected(true);
				comandoExternoCB.setSelected(true);
				españolCB.setSelected(true);
				inglesCB.setSelected(true);
				menuCB.setSelected(true);
				edicionIconosCB.setSelected(true);				
				mostrarAyudaCB.setSelected(true);
				acercadeCB.setSelected(true);
				closeProjectCB.setSelected(true);
				closeFileCB.setSelected(true);
				closeAllCB.setSelected(true);
				compilerCB.setSelected(true);
				newLexicalCB.setSelected(true);
				saveLexicalCB.setSelected(true);
				saveGrammarCB.setSelected(true);
				setFileCB.setSelected(true);
				unsetFileCB.setSelected(true);
				setMainCB.setSelected(true);
				newMenuCB.setSelected(true);
				loadMenuCB.setSelected(true);
				modifyMenuCB.setSelected(true);
				saveMenuCB.setSelected(true);
				saveAsMenuCB.setSelected(true);
				newTBCB.setSelected(true);
				loadTBCB.setSelected(true);
				modifyTBCB.setSelected(true);
				saveTBCB.setSelected(true);
				saveAsTBCB.setSelected(true);
				saveAsGrammarCB.setSelected(true);
				saveAsLexicalCB.setSelected(true);
				setPathsCB.setSelected(true);
				autoSyntaxAnalysisCB.setSelected(true);
				saveAsProjectCB.setSelected(true);
				//mig
				newProjectFileCB.setSelected(true);
				deleteFileCB.setSelected(true);
				unsetMainCB.setSelected(true);
				setFile2CB.setSelected(true);
				unsetFile2CB.setSelected(true);
				setMain2CB.setSelected(true);
				unsetMain2CB.setSelected(true);
			}
		});
		ningunoBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				nuevoFichCB.setSelected(false);
				abrirFichCB.setSelected(false);
				guardarFichCB.setSelected(false);
				saveCB.setSelected(false);
				saveAllCB.setSelected(false);
				printCB.setSelected(false);
				salirFichCB.setSelected(false);
				deshacerCB.setSelected(false);
				repetirCB.setSelected(false);
				copiarCB.setSelected(false);
				pegarCB.setSelected(false);
				cortarCB.setSelected(false);
				selectAllCB.setSelected(false);
				goToCB.setSelected(false);
				buscarCB.setSelected(false);
				reemplazarCB.setSelected(false);
				nuevoProyCB.setSelected(false);
				abrirProyCB.setSelected(false);
				guardarProyCB.setSelected(false);
				añadirFichProyCB.setSelected(false);
				removeFileCB.setSelected(false);
				addFolderCB.setSelected(false);
				removeFolderCB.setSelected(false);
				compilarCB.setSelected(false);
				ejecutarCB.setSelected(false);
				mostrarLogCB.setSelected(false);
				showBrowserCB.setSelected(false);
				showShellCB.setSelected(false);
				cargarParamCB.setSelected(false);
				lexicaCB.setSelected(false);
				sintacticaCB.setSelected(false);
				loadSyntaxCB.setSelected(false);
				modifySyntaxCB.setSelected(false);
				configurarCB.setSelected(false);
				comandoExternoCB.setSelected(false);
				españolCB.setSelected(false);
				inglesCB.setSelected(false);
				menuCB.setSelected(true);
				edicionIconosCB.setSelected(false);				
				mostrarAyudaCB.setSelected(false);
				acercadeCB.setSelected(false);
				closeProjectCB.setSelected(false);
				closeFileCB.setSelected(false);
				closeAllCB.setSelected(false);
				compilerCB.setSelected(false);
				newLexicalCB.setSelected(false);
				saveLexicalCB.setSelected(false);
				saveGrammarCB.setSelected(false);
				setFileCB.setSelected(false);
				unsetFileCB.setSelected(false);
				setMainCB.setSelected(false);
				newMenuCB.setSelected(false);
				loadMenuCB.setSelected(false);
				modifyMenuCB.setSelected(true);
				saveMenuCB.setSelected(false);
				saveAsMenuCB.setSelected(false);
				newTBCB.setSelected(false);
				loadTBCB.setSelected(false);
				modifyTBCB.setSelected(false);
				saveTBCB.setSelected(false);
				saveAsTBCB.setSelected(false);
				saveAsGrammarCB.setSelected(false);
				saveAsLexicalCB.setSelected(false);
				setPathsCB.setSelected(false);
				autoSyntaxAnalysisCB.setSelected(false);
				saveAsProjectCB.setSelected(false);
				//mig
				newProjectFileCB.setSelected(false);
				deleteFileCB.setSelected(false);
				unsetMainCB.setSelected(false);
				setFile2CB.setSelected(false);
				unsetFile2CB.setSelected(false);
				setMain2CB.setSelected(false);
				unsetMain2CB.setSelected(false);
			}
		});
		ActionListener escPressed = new ActionListener()	{
			public void actionPerformed(ActionEvent e) {
				frame.dispose();
			}
		};
		cancelarBoton.registerKeyboardAction(escPressed,"EscapeKey",KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE,0,true),JComponent.WHEN_IN_FOCUSED_WINDOW);
		// Menu Archivo
		archivoPanel.add(nuevoFichCB);
		archivoPanel.add(abrirFichCB);
		archivoPanel.add(guardarFichCB);
		archivoPanel.add(saveCB);
		archivoPanel.add(saveAllCB);
		archivoPanel.add(closeFileCB);
		archivoPanel.add(closeAllCB);
		//mig
		//archivoPanel.add(setFile2CB);
		//archivoPanel.add(unsetFile2CB);
		//archivoPanel.add(setMain2CB);
		//archivoPanel.add(unsetMain2CB);
		archivoPanel.add(printCB);
		archivoPanel.add(salirFichCB);
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(0,0,0,0);
		c.gridwidth = 6;
		c.gridx = 0;
		c.gridy = 0;
		c.ipadx = 0;
		frame.add(archivoPanel,c);
		// Menu Edicion
		edicionPanel.add(deshacerCB);
		edicionPanel.add(repetirCB);
		edicionPanel.add(copiarCB);
		edicionPanel.add(pegarCB);
		edicionPanel.add(cortarCB);
		edicionPanel.add(selectAllCB);
		edicionPanel.add(goToCB);
		edicionPanel.add(buscarCB);
		edicionPanel.add(reemplazarCB);
		c.gridy = 1;
		frame.add(edicionPanel,c);
		// Menu Proyecto
		proyectoPanel.add(nuevoProyCB);
		proyectoPanel.add(abrirProyCB);
		proyectoPanel.add(closeProjectCB);
		proyectoPanel.add(guardarProyCB);		
		proyectoPanel.add(saveAsProjectCB);
		//mig
		proyectoPanel.add(newProjectFileCB);
		proyectoPanel.add(añadirFichProyCB);
		proyectoPanel.add(removeFileCB);
		proyectoPanel.add(deleteFileCB);
		proyectoPanel.add(addFolderCB);
		proyectoPanel.add(removeFolderCB);
		proyectoPanel.add(compilarCB);
		proyectoPanel.add(ejecutarCB);
		proyectoPanel.add(setFileCB);
		proyectoPanel.add(unsetFileCB);
		proyectoPanel.add(setMainCB);
		//mig
		proyectoPanel.add(unsetMainCB);
		c.gridy = 2;
		frame.add(proyectoPanel,c);
		// Menu Ver
		viewPanel.add(mostrarLogCB);
		viewPanel.add(showBrowserCB);
		viewPanel.add(showShellCB);
		c.gridy = 3;
		frame.add(viewPanel,c);
		// Menu Configuracion
		//configuracionPanel.add(menuCB);
		configuracionPanel.add(compilerCB);
		configuracionPanel.add(newLexicalCB);
		configuracionPanel.add(sintacticaCB);
		configuracionPanel.add(newMenuCB);
		configuracionPanel.add(newTBCB);
		configuracionPanel.add(configurarCB);
		configuracionPanel.add(cargarParamCB);		
		configuracionPanel.add(loadSyntaxCB);
		configuracionPanel.add(loadMenuCB);
		configuracionPanel.add(loadTBCB);
		configuracionPanel.add(comandoExternoCB);
		configuracionPanel.add(lexicaCB);
		configuracionPanel.add(modifySyntaxCB);
		configuracionPanel.add(modifyMenuCB);
		configuracionPanel.add(modifyTBCB);
		configuracionPanel.add(españolCB);
		configuracionPanel.add(saveLexicalCB);
		configuracionPanel.add(saveGrammarCB);
		configuracionPanel.add(saveMenuCB);
		configuracionPanel.add(saveTBCB);
		configuracionPanel.add(inglesCB);
		configuracionPanel.add(saveAsLexicalCB);
		configuracionPanel.add(saveAsGrammarCB);
		configuracionPanel.add(saveAsMenuCB);
		configuracionPanel.add(saveAsTBCB);
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(setPathsCB);
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(autoSyntaxAnalysisCB);
		c.gridy = 4;
		frame.add(configuracionPanel,c);
		// Menu Ayuda
		ayudaPanel.add(mostrarAyudaCB);
		ayudaPanel.add(acercadeCB);
		c.gridy = 5;
		frame.add(ayudaPanel,c);
		// Botones
		c.gridwidth = 1;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(5,5,5,5);
		botonesPanel.add(todosBoton,c);
		c.gridx = 1; 
		botonesPanel.add(ningunoBoton,c);
		c.gridx = 2;
		//botonesPanel.add(guardarBoton,c);
		c.gridx = 3;
		//botonesPanel.add(cargarBoton,c);
		c.gridx = 4;
		botonesPanel.add(aceptarBoton,c);
		c.gridx = 5;
		botonesPanel.add(cancelarBoton,c);
		c.gridy = 6;
		c.insets = new Insets(0,0,0,0);
		frame.add(botonesPanel,c);
		// Se muestra el frame
		frame.setVisible(true);
		frame.setResizable(false);
		frame.pack();
		frame.setLocationRelativeTo(null);
		logger.info(labels.getString("s530"));
	}
	
	public void modifyMenuGUI() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = i.getLabels();
		logger.info(labels.getString("s531"));
		frame = new JFrame();
		frame.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		String s = null;
		try {
			s = almacenPropiedades.getPropiedad("currentMenuCfg");
			int index = s.lastIndexOf("\\");
			if (index == -1) index = s.lastIndexOf("/");
			s = s.substring(index + 1,s.length() - 8);
		}
		catch (Exception e2) {
			JOptionPane.showMessageDialog(null,e2.getMessage(),labels.getString("s295"),JOptionPane.ERROR_MESSAGE);
		}
		frame.setTitle(labels.getString("s532") + " - " + s);
		// Creacion de todos los paneles de la ventana
		JPanel archivoPanel = new JPanel();
		archivoPanel.setLayout(new GridLayout(0,3));
		archivoPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s500")));
		JPanel edicionPanel = new JPanel();
		edicionPanel.setLayout(new GridLayout(0,3));
		edicionPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s501")));
		JPanel proyectoPanel = new JPanel();
		proyectoPanel.setLayout(new GridLayout(0,3));
		proyectoPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s502")));
		JPanel viewPanel = new JPanel();
		viewPanel.setLayout(new GridLayout(0,3));
		viewPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s503")));
		JPanel configuracionPanel = new JPanel();
		configuracionPanel.setLayout(new GridLayout(0,5));
		configuracionPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s504")));
		JPanel idiomaPanel = new JPanel();
		idiomaPanel.setLayout(new GridLayout(0,3));
		idiomaPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s505")));
		JPanel ayudaPanel = new JPanel();
		ayudaPanel.setLayout(new GridLayout(0,3));
		ayudaPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s506")));
		JPanel botonesPanel = new JPanel();
		botonesPanel.setLayout(new GridBagLayout());
		// Creacion de todas las componentes de la ventana		
		// Menu Archivo
		final JCheckBox nuevoFichCB = new JCheckBox(labels.getString("s8"),MenuConfig.getNuevoFich());
		final JCheckBox abrirFichCB = new JCheckBox(labels.getString("s9"),MenuConfig.getAbrirFich());
		final JCheckBox guardarFichCB = new JCheckBox(labels.getString("s10"),MenuConfig.getSalvarFich());
		final JCheckBox saveCB = new JCheckBox(labels.getString("s617"),MenuConfig.getSave());
		final JCheckBox saveAllCB = new JCheckBox(labels.getString("s217"),MenuConfig.getSaveAll());
		final JCheckBox closeFileCB = new JCheckBox(labels.getString("s238"),MenuConfig.getCloseFile());
		final JCheckBox closeAllCB = new JCheckBox(labels.getString("s239"),MenuConfig.getCloseAll());
		//mig
		final JCheckBox setFile2CB = new JCheckBox(labels.getString("s254"),MenuConfig.getSetFile2());
		final JCheckBox unsetFile2CB = new JCheckBox(labels.getString("s255"),MenuConfig.getUnsetFile2());
		final JCheckBox setMain2CB = new JCheckBox(labels.getString("s256"),MenuConfig.getSetMain2());
		final JCheckBox unsetMain2CB = new JCheckBox(labels.getString("s952"),MenuConfig.getUnsetMain2());
		final JCheckBox printCB = new JCheckBox(labels.getString("s624"),MenuConfig.getPrint());
		final JCheckBox salirFichCB = new JCheckBox(labels.getString("s13"),MenuConfig.getSalir());		
		//Menu Edicion
		final JCheckBox deshacerCB = new JCheckBox(labels.getString("s21"),MenuConfig.getDeshacer());
		final JCheckBox repetirCB = new JCheckBox(labels.getString("s22"),MenuConfig.getRepetir());
		final JCheckBox copiarCB = new JCheckBox(labels.getString("s23"),MenuConfig.getCopiar());
		final JCheckBox pegarCB = new JCheckBox(labels.getString("s25"),MenuConfig.getPegar());
		final JCheckBox cortarCB = new JCheckBox(labels.getString("s24"),MenuConfig.getCortar());
		final JCheckBox selectAllCB = new JCheckBox(labels.getString("s190"),MenuConfig.getSelectAll());
		final JCheckBox goToCB = new JCheckBox(labels.getString("s222"),MenuConfig.getGoTo());
		final JCheckBox buscarCB = new JCheckBox(labels.getString("s26"),MenuConfig.getBuscar());
		final JCheckBox reemplazarCB = new JCheckBox(labels.getString("s27"),MenuConfig.getReemplazar());
		// Menu Proyecto
		final JCheckBox nuevoProyCB = new JCheckBox(labels.getString("s14"),MenuConfig.getNuevoProyecto());
		final JCheckBox abrirProyCB = new JCheckBox(labels.getString("s15"),MenuConfig.getAbrirProyecto());
		final JCheckBox guardarProyCB = new JCheckBox(labels.getString("s16"),MenuConfig.getGuardarProyecto());
		final JCheckBox closeProjectCB = new JCheckBox(labels.getString("s228"),MenuConfig.getCloseProject());
		//mig
		final JCheckBox newProjectFileCB = new JCheckBox(labels.getString("s947"),MenuConfig.getNewProjectFile());
		final JCheckBox añadirFichProyCB = new JCheckBox(labels.getString("s17"),MenuConfig.getAñadirFichero());
		final JCheckBox removeFileCB = new JCheckBox(labels.getString("s218"),MenuConfig.getRemoveFile());
		//mig
		final JCheckBox deleteFileCB = new JCheckBox(labels.getString("s950"),MenuConfig.getDeleteFile());
		final JCheckBox addFolderCB = new JCheckBox(labels.getString("s219"),MenuConfig.getAddFolder());
		final JCheckBox removeFolderCB = new JCheckBox(labels.getString("s220"),MenuConfig.getRemoveFolder());
		final JCheckBox compilarCB = new JCheckBox(labels.getString("s18"),MenuConfig.getCompilar());
		final JCheckBox ejecutarCB = new JCheckBox(labels.getString("s19"),MenuConfig.getEjecutar());
		final JCheckBox setFileCB = new JCheckBox(labels.getString("s254"),MenuConfig.getSetFile());
		final JCheckBox unsetFileCB = new JCheckBox(labels.getString("s255"),MenuConfig.getUnsetFile());
		final JCheckBox setMainCB = new JCheckBox(labels.getString("s256"),MenuConfig.getSetMain());
		//mig
		final JCheckBox unsetMainCB = new JCheckBox(labels.getString("s952"),MenuConfig.getUnsetMain());
		final JCheckBox saveAsProjectCB = new JCheckBox(labels.getString("s926"),MenuConfig.isSaveAsProject());
		// Menu Ver		
		final JCheckBox mostrarLogCB = new JCheckBox(labels.getString("s28"),MenuConfig.getMostrarLog());
		final JCheckBox showBrowserCB = new JCheckBox(labels.getString("s221"),MenuConfig.getShowBrowserCBox());
		final JCheckBox showShellCB = new JCheckBox(labels.getString("s223"),MenuConfig.getShowShellWindowCBox());
		// Menu Configuracion
		// Submenu lexica
		final JCheckBox newLexicalCB = new JCheckBox(labels.getString("s224") + " - " + labels.getString("s249"),MenuConfig.getNewLexical());
		final JCheckBox cargarParamCB = new JCheckBox(labels.getString("s224") + " - " + labels.getString("s35"),MenuConfig.getCargarParam());
		final JCheckBox lexicaCB = new JCheckBox(labels.getString("s224") + " - " + labels.getString("s29"),MenuConfig.getLexica());
		final JCheckBox saveLexicalCB = new JCheckBox(labels.getString("s224") + " - " + labels.getString("s250"),MenuConfig.getSaveLexical());
		final JCheckBox saveAsLexicalCB = new JCheckBox(labels.getString("s224") + " - " + labels.getString("s285"),MenuConfig.isSaveAsLexical());
		// Submenu sintactica
		final JCheckBox sintacticaCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s30"),MenuConfig.getSintactica());
		final JCheckBox loadSyntaxCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s226"),MenuConfig.getLoadSyntax());
		final JCheckBox modifySyntaxCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s227"),MenuConfig.getModifySyntax());
		final JCheckBox saveGrammarCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s251"),MenuConfig.getSaveGrammar());
		final JCheckBox saveAsGrammarCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s286"),MenuConfig.isSaveAsGrammar());
		final JCheckBox setPathsCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s912"),MenuConfig.isSetPaths());
		final JCheckBox autoSyntaxAnalysisCB = new JCheckBox(labels.getString("s225") + " - " + labels.getString("s911"),MenuConfig.isAutoSyntaxAnalysis());
		// Submenu consola
		final JCheckBox configurarCB = new JCheckBox(labels.getString("s332") + " - " + labels.getString("s333"),MenuConfig.getConfigurar());
		final JCheckBox comandoExternoCB = new JCheckBox(labels.getString("s332") + " - " + labels.getString("s341"),MenuConfig.getComandoExterno());
		// Submenu idioma
		final JCheckBox españolCB = new JCheckBox(labels.getString("s6") + " - " + labels.getString("s11"),MenuConfig.getEspañol());
		final JCheckBox inglesCB = new JCheckBox(labels.getString("s6") + " - " + labels.getString("s12"),MenuConfig.getEnglish());
		// Submenu menu
		final JCheckBox newMenuCB = new JCheckBox(labels.getString("s34") + " - " + labels.getString("s275"),MenuConfig.isNewMenu());
		final JCheckBox loadMenuCB = new JCheckBox(labels.getString("s34") + " - " + labels.getString("s276"),MenuConfig.isLoadMenu());
		final JCheckBox modifyMenuCB = new JCheckBox(labels.getString("s34") + " - " + labels.getString("s277"),MenuConfig.isModifyMenu());
		final JCheckBox saveMenuCB = new JCheckBox(labels.getString("s34") + " - " + labels.getString("s278"),MenuConfig.isSaveMenu());
		final JCheckBox saveAsMenuCB = new JCheckBox(labels.getString("s34") + " - " + labels.getString("s279"),MenuConfig.isSaveAsMenu());
		// Submenu toolbar
		final JCheckBox newTBCB = new JCheckBox(labels.getString("s169") + " - " + labels.getString("s280"),MenuConfig.isNewTB());
		final JCheckBox loadTBCB = new JCheckBox(labels.getString("s169") + " - " + labels.getString("s281"),MenuConfig.isLoadTB());
		final JCheckBox modifyTBCB = new JCheckBox(labels.getString("s169") + " - " + labels.getString("s282"),MenuConfig.isModifyTB());
		final JCheckBox saveTBCB = new JCheckBox(labels.getString("s169") + " - " + labels.getString("s283"),MenuConfig.isSaveTB());
		final JCheckBox saveAsTBCB = new JCheckBox(labels.getString("s169") + " - " + labels.getString("s284"),MenuConfig.isSaveAsTB());
		// Elementos sin submenu
		final JCheckBox compilerCB = new JCheckBox(labels.getString("s240"),MenuConfig.getCompiler());
		// Menu Ayuda
		final JCheckBox mostrarAyudaCB = new JCheckBox(labels.getString("s38"),MenuConfig.getMostrarAyuda());
		final JCheckBox acercadeCB = new JCheckBox(labels.getString("s39"),MenuConfig.getAcercade());
		// Vacios
		final JCheckBox menuCB = new JCheckBox();
		final JCheckBox edicionIconosCB = new JCheckBox();
		// Botones
		JButton aceptarBoton = new JButton(labels.getString("s507"));
		aceptarBoton.setToolTipText(labels.getString("s508"));
		JButton cancelarBoton = new JButton(labels.getString("s509"));
		cancelarBoton.setToolTipText(labels.getString("s510"));
		JButton cargarBoton = new JButton(labels.getString("s511"));
		cargarBoton.setToolTipText(labels.getString("s512"));
		JButton guardarBoton = new JButton(labels.getString("s513"));
		guardarBoton.setToolTipText(labels.getString("s514"));
		JButton todosBoton = new JButton(labels.getString("s515"));
		todosBoton.setToolTipText(labels.getString("s516"));
		JButton ningunoBoton = new JButton(labels.getString("s517"));
		ningunoBoton.setToolTipText(labels.getString("s518"));
		/* 
		 * Ponemos el checkbox de configuracion - menu - modificar siempre activo y no modificable 
		 * porque si se deselecciona no habria forma de volver a hacer cambios en 
		 * el menu 
		 */
		//menuCB.setSelected(true);
		//menuCB.setEnabled(false);
		modifyMenuCB.setSelected(true);
		modifyMenuCB.setEnabled(false);
		// Oyentes de los botones
		aceptarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				boolean[] valores = new boolean[73];
				valores[0] = nuevoFichCB.isSelected();
				valores[1] = abrirFichCB.isSelected();
				valores[2] = guardarFichCB.isSelected();
				valores[3] = saveCB.isSelected();
				valores[4] = saveAllCB.isSelected();
				valores[5] = printCB.isSelected();
				valores[6] = salirFichCB.isSelected();
				valores[7] = deshacerCB.isSelected();
				valores[8] = repetirCB.isSelected();
				valores[9] = copiarCB.isSelected();
				valores[10] = pegarCB.isSelected();
				valores[11] = cortarCB.isSelected();
				valores[12] = selectAllCB.isSelected();
				valores[13] = goToCB.isSelected();
				valores[14] = buscarCB.isSelected();
				valores[15] = reemplazarCB.isSelected();
				valores[16] = nuevoProyCB.isSelected();
				valores[17] = abrirProyCB.isSelected();
				valores[18] = guardarProyCB.isSelected();
				valores[19] = añadirFichProyCB.isSelected();
				valores[20] = removeFileCB.isSelected();
				valores[21] = addFolderCB.isSelected();
				valores[22] = removeFolderCB.isSelected();
				valores[23] = compilarCB.isSelected();
				valores[24] = ejecutarCB.isSelected();
				valores[25] = mostrarLogCB.isSelected();
				valores[26] = showBrowserCB.isSelected();
				valores[27] = showShellCB.isSelected();
				valores[28] = cargarParamCB.isSelected();
				valores[29] = lexicaCB.isSelected();
				valores[30] = sintacticaCB.isSelected();
				valores[31] = loadSyntaxCB.isSelected();
				valores[32] = modifySyntaxCB.isSelected();
				valores[33] = configurarCB.isSelected();
				valores[34] = comandoExternoCB.isSelected();
				valores[35] = españolCB.isSelected();
				valores[36] = inglesCB.isSelected();
				// opcion de menu siempre activada
				valores[37] = true;
				valores[38] = edicionIconosCB.isSelected();				
				valores[39] = mostrarAyudaCB.isSelected();
				valores[40] = acercadeCB.isSelected();
				valores[41] = closeProjectCB.isSelected();
				valores[42] = closeFileCB.isSelected();
				valores[43] = closeAllCB.isSelected();
				valores[44] = compilerCB.isSelected();
				valores[45] = newLexicalCB.isSelected();
				valores[46] = saveLexicalCB.isSelected();
				valores[47] = saveGrammarCB.isSelected();
				valores[48] = setFileCB.isSelected();
				valores[49] = unsetFileCB.isSelected();
				valores[50] = setMainCB.isSelected();
				valores[51] = newMenuCB.isSelected();
				valores[52] = loadMenuCB.isSelected();
				// modifyMenuCB option always on
				valores[53] = true;
				valores[54] = saveMenuCB.isSelected();
				valores[55] = saveAsMenuCB.isSelected();
				valores[56] = newTBCB.isSelected();
				valores[57] = loadTBCB.isSelected();
				valores[58] = modifyTBCB.isSelected();
				valores[59] = saveTBCB.isSelected();
				valores[60] = saveAsTBCB.isSelected();
				valores[61] = saveAsGrammarCB.isSelected();
				valores[62] = saveAsLexicalCB.isSelected();
				valores[63] = setPathsCB.isSelected();
				valores[64] = autoSyntaxAnalysisCB.isSelected();
				valores[65] = saveAsProjectCB.isSelected();
				//mig
				valores[66] = newProjectFileCB.isSelected();
				valores[67] = deleteFileCB.isSelected();
				valores[68] = unsetMainCB.isSelected();
				valores[69] = setFile2CB.isSelected();
				valores[70] = unsetFile2CB.isSelected();
				valores[71] = setMain2CB.isSelected();
				valores[72] = unsetMain2CB.isSelected();
				
				
				MenuConfig.setAll(valores);
				String newName = ".//configuration/menu/lastModified.menuCfg";
				MenuConfig.guardarMenuCfgFich(newName,valores);
				try {
					String previousMenu = almacenPropiedades.getPropiedad("currentMenuCfg");
					if(!previousMenu.endsWith("lastModified.menuCfg"))
						almacenPropiedades.setPropiedad("previousMenuCfg",previousMenu);
					almacenPropiedades.setPropiedad("currentMenuCfg",newName);
					Ventana v = Ventana.getInstance();
					v.getnuevoMenu().setMenuConfig();
					v.getnuevoMenu().getSaveMenu().setEnabled(true);
					v.validate();
					v.repaint();
					changesSaved = false;
					frame.dispose();
					logger.info(labels.getString("s519"));
				}
				catch (Exception e1) {
					JOptionPane.showMessageDialog(null,e1.getMessage(),labels.getString("s292"),JOptionPane.ERROR_MESSAGE);
				}				
			}
		});
		cancelarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				logger.info(labels.getString("s520"));
				frame.dispose();
			}
		});
		cargarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero f = new Fichero();
				FiltroFicheros filtro = new FiltroFicheros(labels.getString("s172"));
				filtro.addExtension("menuCfg");
				String ruta = f.leer(filtro);
				boolean[] valores;
				try {
					valores = MenuConfig.cargarMenuCfgFich(ruta);
					almacenPropiedades.setPropiedad("currentMenuCfg",ruta);
					nuevoFichCB.setSelected(valores[0]);
					abrirFichCB.setSelected(valores[1]);
					guardarFichCB.setSelected(valores[2]);
					saveCB.setSelected(valores[3]);
					saveAllCB.setSelected(valores[4]);
					printCB.setSelected(valores[5]);
					salirFichCB.setSelected(valores[6]);
					deshacerCB.setSelected(valores[7]);
					repetirCB.setSelected(valores[8]);
					copiarCB.setSelected(valores[9]);
					pegarCB.setSelected(valores[10]);
					cortarCB.setSelected(valores[11]);
					selectAllCB.setSelected(valores[12]);
					goToCB.setSelected(valores[13]);
					buscarCB.setSelected(valores[14]);
					reemplazarCB.setSelected(valores[15]);
					nuevoProyCB.setSelected(valores[16]);
					abrirProyCB.setSelected(valores[17]);
					guardarProyCB.setSelected(valores[18]);
					añadirFichProyCB.setSelected(valores[19]);
					removeFileCB.setSelected(valores[20]);
					addFolderCB.setSelected(valores[21]);
					removeFolderCB.setSelected(valores[22]);
					compilarCB.setSelected(valores[23]);
					ejecutarCB.setSelected(valores[24]);
					mostrarLogCB.setSelected(valores[25]);
					showBrowserCB.setSelected(valores[26]);
					showShellCB.setSelected(valores[27]);
					cargarParamCB.setSelected(valores[28]);
					lexicaCB.setSelected(valores[29]);
					sintacticaCB.setSelected(valores[30]);
					loadSyntaxCB.setSelected(valores[31]);
					modifySyntaxCB.setSelected(valores[32]);
					configurarCB.setSelected(valores[33]);
					comandoExternoCB.setSelected(valores[34]);
					españolCB.setSelected(valores[35]);
					inglesCB.setSelected(valores[36]);
					menuCB.setSelected(true);
					edicionIconosCB.setSelected(valores[38]);					
					mostrarAyudaCB.setSelected(valores[39]);
					acercadeCB.setSelected(valores[40]);
					closeProjectCB.setSelected(valores[41]);
					closeFileCB.setSelected(valores[42]);
					closeAllCB.setSelected(valores[43]);
					compilerCB.setSelected(valores[44]);
					newLexicalCB.setSelected(valores[45]);
					saveLexicalCB.setSelected(valores[46]);
					saveGrammarCB.setSelected(valores[47]);
					setFileCB.setSelected(valores[48]);
					unsetFileCB.setSelected(valores[49]);
					setMainCB.setSelected(valores[50]);
					newMenuCB.setSelected(valores[51]);
					loadMenuCB.setSelected(valores[52]);
					// modifyMenuCB option always on
					modifyMenuCB.setSelected(true);
					saveMenuCB.setSelected(valores[54]);
					saveAsMenuCB.setSelected(valores[55]);
					newTBCB.setSelected(valores[56]);
					loadTBCB.setSelected(valores[57]);
					modifyTBCB.setSelected(valores[58]);
					saveTBCB.setSelected(valores[59]);
					saveAsTBCB.setSelected(valores[60]);
					saveAsGrammarCB.setSelected(valores[61]);
					saveAsLexicalCB.setSelected(valores[62]);
					setPathsCB.setSelected(valores[63]);
					autoSyntaxAnalysisCB.setSelected(valores[64]);
					saveAsProjectCB.setSelected(valores[65]);
					//mig
					newProjectFileCB.setSelected(valores[66]);
					deleteFileCB.setSelected(valores[67]);
					unsetMainCB.setSelected(valores[68]);
					setFile2CB.setSelected(valores[69]);
					unsetFile2CB.setSelected(valores[70]);
					setMain2CB.setSelected(valores[71]);
					unsetMain2CB.setSelected(valores[72]);
					
					changesSaved = true;
					logger.info(labels.getString("s522") + ruta + labels.getString("s523"));
				}
				catch (Exception e1) {
					logger.info(labels.getString("s521") + ruta + " " + e1.getMessage());
				}								
			}
		});
		guardarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				boolean[] valores = new boolean[73];
				valores[0] = nuevoFichCB.isSelected();
				valores[1] = abrirFichCB.isSelected();
				valores[2] = guardarFichCB.isSelected();
				valores[3] = saveCB.isSelected();
				valores[4] = saveAllCB.isSelected();
				valores[5] = printCB.isSelected();
				valores[6] = salirFichCB.isSelected();
				valores[7] = deshacerCB.isSelected();
				valores[8] = repetirCB.isSelected();
				valores[9] = copiarCB.isSelected();
				valores[10] = pegarCB.isSelected();
				valores[11] = cortarCB.isSelected();
				valores[12] = selectAllCB.isSelected();
				valores[13] = goToCB.isSelected();
				valores[14] = buscarCB.isSelected();
				valores[15] = reemplazarCB.isSelected();
				valores[16] = nuevoProyCB.isSelected();
				valores[17] = abrirProyCB.isSelected();
				valores[18] = guardarProyCB.isSelected();
				valores[19] = añadirFichProyCB.isSelected();
				valores[20] = removeFileCB.isSelected();
				valores[21] = addFolderCB.isSelected();
				valores[22] = removeFolderCB.isSelected();
				valores[23] = compilarCB.isSelected();
				valores[24] = ejecutarCB.isSelected();
				valores[25] = mostrarLogCB.isSelected();
				valores[26] = showBrowserCB.isSelected();
				valores[27] = showShellCB.isSelected();
				valores[28] = cargarParamCB.isSelected();
				valores[29] = lexicaCB.isSelected();
				valores[30] = sintacticaCB.isSelected();
				valores[31] = loadSyntaxCB.isSelected();
				valores[32] = modifySyntaxCB.isSelected();
				valores[33] = configurarCB.isSelected();
				valores[34] = comandoExternoCB.isSelected();
				valores[35] = españolCB.isSelected();
				valores[36] = inglesCB.isSelected();
				// opcion de menu siempre activada
				valores[37] = true;
				valores[38] = edicionIconosCB.isSelected();			
				valores[39] = mostrarAyudaCB.isSelected();
				valores[40] = acercadeCB.isSelected();
				valores[41] = closeProjectCB.isSelected();
				valores[42] = closeFileCB.isSelected();
				valores[43] = closeAllCB.isSelected();
				valores[44] = compilerCB.isSelected();
				valores[45] = newLexicalCB.isSelected();
				valores[46] = saveLexicalCB.isSelected();
				valores[47] = saveGrammarCB.isSelected();
				valores[48] = setFileCB.isSelected();
				valores[49] = unsetFileCB.isSelected();
				valores[50] = setMainCB.isSelected();
				valores[51] = newMenuCB.isSelected();
				valores[52] = loadMenuCB.isSelected();
				// modifyMenu option always on
				valores[53] = true;
				valores[54] = saveMenuCB.isSelected();
				valores[55] = saveAsMenuCB.isSelected();
				valores[56] = newTBCB.isSelected();
				valores[57] = loadTBCB.isSelected();
				valores[58] = modifyTBCB.isSelected();
				valores[59] = saveTBCB.isSelected();
				valores[60] = saveAsTBCB.isSelected();
				valores[61] = saveAsGrammarCB.isSelected();
				valores[62] = saveAsLexicalCB.isSelected();
				valores[63] = setPathsCB.isSelected();
				valores[64] = autoSyntaxAnalysisCB.isSelected();
				valores[65] = saveAsProjectCB.isSelected();
				//mig
				valores[66] = newProjectFileCB.isSelected();
				valores[67] = deleteFileCB.isSelected();
				valores[68] = unsetMainCB.isSelected();
				valores[69] = setFile2CB.isSelected();
				valores[70] = unsetFile2CB.isSelected();
				valores[71] = setMain2CB.isSelected();
				valores[72] = unsetMain2CB.isSelected();

				JFileChooser selector = new JFileChooser();
				FiltroFicheros filtro = new FiltroFicheros(labels.getString("s126"));
				filtro.addExtension("menuCfg");
				selector.setFileFilter(filtro);
				String nombreFichero = "";
				int valor = selector.showSaveDialog(selector);
				if (valor == JFileChooser.APPROVE_OPTION) {
					nombreFichero = selector.getSelectedFile().getAbsolutePath();
					if(!nombreFichero.endsWith(".menuCfg")) nombreFichero += ".menuCfg";
					MenuConfig.guardarMenuCfgFich(nombreFichero,valores);
					almacenPropiedades.setPropiedad("currentMenuCfg",nombreFichero);
					changesSaved = true;
					// Muestra operacion en el log
					logger.info(labels.getString("s528") + nombreFichero + labels.getString("s529"));				
				} 
				else if (valor == JFileChooser.CANCEL_OPTION) {
					selector.cancelSelection();
					// Muestra operacion en el log
					logger.info(labels.getString("s527"));
				}		
			}
		});
		todosBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				nuevoFichCB.setSelected(true);
				abrirFichCB.setSelected(true);
				guardarFichCB.setSelected(true);
				saveCB.setSelected(true);
				saveAllCB.setSelected(true);
				printCB.setSelected(true);
				salirFichCB.setSelected(true);
				deshacerCB.setSelected(true);
				repetirCB.setSelected(true);
				copiarCB.setSelected(true);
				pegarCB.setSelected(true);
				cortarCB.setSelected(true);
				selectAllCB.setSelected(true);
				goToCB.setSelected(true);
				buscarCB.setSelected(true);
				reemplazarCB.setSelected(true);
				nuevoProyCB.setSelected(true);
				abrirProyCB.setSelected(true);
				guardarProyCB.setSelected(true);
				añadirFichProyCB.setSelected(true);
				removeFileCB.setSelected(true);
				addFolderCB.setSelected(true);
				removeFolderCB.setSelected(true);
				compilarCB.setSelected(true);
				ejecutarCB.setSelected(true);
				mostrarLogCB.setSelected(true);
				showBrowserCB.setSelected(true);
				showShellCB.setSelected(true);
				cargarParamCB.setSelected(true);
				lexicaCB.setSelected(true);
				sintacticaCB.setSelected(true);
				loadSyntaxCB.setSelected(true);
				modifySyntaxCB.setSelected(true);
				configurarCB.setSelected(true);
				comandoExternoCB.setSelected(true);
				españolCB.setSelected(true);
				inglesCB.setSelected(true);
				menuCB.setSelected(true);
				edicionIconosCB.setSelected(true);				
				mostrarAyudaCB.setSelected(true);
				acercadeCB.setSelected(true);
				closeProjectCB.setSelected(true);
				closeFileCB.setSelected(true);
				closeAllCB.setSelected(true);
				compilerCB.setSelected(true);
				newLexicalCB.setSelected(true);
				saveLexicalCB.setSelected(true);
				saveGrammarCB.setSelected(true);
				setFileCB.setSelected(true);
				unsetFileCB.setSelected(true);
				setMainCB.setSelected(true);
				newMenuCB.setSelected(true);
				loadMenuCB.setSelected(true);
				modifyMenuCB.setSelected(true);
				saveMenuCB.setSelected(true);
				saveAsMenuCB.setSelected(true);
				newTBCB.setSelected(true);
				loadTBCB.setSelected(true);
				modifyTBCB.setSelected(true);
				saveTBCB.setSelected(true);
				saveAsTBCB.setSelected(true);
				saveAsGrammarCB.setSelected(true);
				saveAsLexicalCB.setSelected(true);
				setPathsCB.setSelected(true);
				autoSyntaxAnalysisCB.setSelected(true);
				saveAsProjectCB.setSelected(true);
				//mig
				newProjectFileCB.setSelected(true);
				deleteFileCB.setSelected(true);
				unsetMainCB.setSelected(true);
				setFile2CB.setSelected(true);
				unsetFile2CB.setSelected(true);
				setMain2CB.setSelected(true);
				unsetMain2CB.setSelected(true);
			}
		});
		ningunoBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				nuevoFichCB.setSelected(false);
				abrirFichCB.setSelected(false);
				guardarFichCB.setSelected(false);
				saveCB.setSelected(false);
				saveAllCB.setSelected(false);
				printCB.setSelected(false);
				salirFichCB.setSelected(false);
				deshacerCB.setSelected(false);
				repetirCB.setSelected(false);
				copiarCB.setSelected(false);
				pegarCB.setSelected(false);
				cortarCB.setSelected(false);
				selectAllCB.setSelected(false);
				goToCB.setSelected(false);
				buscarCB.setSelected(false);
				reemplazarCB.setSelected(false);
				nuevoProyCB.setSelected(false);
				abrirProyCB.setSelected(false);
				guardarProyCB.setSelected(false);
				añadirFichProyCB.setSelected(false);
				removeFileCB.setSelected(false);
				addFolderCB.setSelected(false);
				removeFolderCB.setSelected(false);
				compilarCB.setSelected(false);
				ejecutarCB.setSelected(false);
				mostrarLogCB.setSelected(false);
				showBrowserCB.setSelected(false);
				showShellCB.setSelected(false);
				cargarParamCB.setSelected(false);
				lexicaCB.setSelected(false);
				sintacticaCB.setSelected(false);
				loadSyntaxCB.setSelected(false);
				modifySyntaxCB.setSelected(false);
				configurarCB.setSelected(false);
				comandoExternoCB.setSelected(false);
				españolCB.setSelected(false);
				inglesCB.setSelected(false);
				menuCB.setSelected(true);
				edicionIconosCB.setSelected(false);				
				mostrarAyudaCB.setSelected(false);
				acercadeCB.setSelected(false);
				closeProjectCB.setSelected(false);
				closeFileCB.setSelected(false);
				closeAllCB.setSelected(false);
				compilerCB.setSelected(false);
				newLexicalCB.setSelected(false);
				saveLexicalCB.setSelected(false);
				saveGrammarCB.setSelected(false);
				setFileCB.setSelected(false);
				unsetFileCB.setSelected(false);
				setMainCB.setSelected(false);
				newMenuCB.setSelected(false);
				loadMenuCB.setSelected(false);
				modifyMenuCB.setSelected(true);
				saveMenuCB.setSelected(false);
				saveAsMenuCB.setSelected(false);
				newTBCB.setSelected(false);
				loadTBCB.setSelected(false);
				modifyTBCB.setSelected(false);
				saveTBCB.setSelected(false);
				saveAsTBCB.setSelected(false);
				saveAsGrammarCB.setSelected(false);
				saveAsLexicalCB.setSelected(false);
				setPathsCB.setSelected(false);
				autoSyntaxAnalysisCB.setSelected(false);
				saveAsProjectCB.setSelected(false);
				//mig
				newProjectFileCB.setSelected(false);
				deleteFileCB.setSelected(false);
				unsetMainCB.setSelected(false);
				setFile2CB.setSelected(false);
				unsetFile2CB.setSelected(false);
				setMain2CB.setSelected(false);
				unsetMain2CB.setSelected(false);
			}
		});
		ActionListener escPressed = new ActionListener()	{
			public void actionPerformed(ActionEvent e) {
				frame.dispose();
			}
		};
		cancelarBoton.registerKeyboardAction(escPressed,"EscapeKey",KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE,0,true),JComponent.WHEN_IN_FOCUSED_WINDOW);
		// Menu Archivo
		archivoPanel.add(nuevoFichCB);
		archivoPanel.add(abrirFichCB);
		archivoPanel.add(guardarFichCB);
		archivoPanel.add(saveCB);
		archivoPanel.add(saveAllCB);
		archivoPanel.add(closeFileCB);
		archivoPanel.add(closeAllCB);
		//mig
		//archivoPanel.add(setFile2CB);
		//archivoPanel.add(unsetFile2CB);
		//archivoPanel.add(setMain2CB);
		//archivoPanel.add(unsetMain2CB);
		archivoPanel.add(printCB);
		archivoPanel.add(salirFichCB);
		c.fill = GridBagConstraints.BOTH;
		c.gridwidth = 6;
		c.gridx = 0;
		c.gridy = 0;
		frame.add(archivoPanel,c);
		// Menu Edicion
		edicionPanel.add(deshacerCB);
		edicionPanel.add(repetirCB);
		edicionPanel.add(copiarCB);
		edicionPanel.add(pegarCB);
		edicionPanel.add(cortarCB);
		edicionPanel.add(selectAllCB);
		edicionPanel.add(goToCB);
		edicionPanel.add(buscarCB);
		edicionPanel.add(reemplazarCB);
		c.gridy = 1;
		frame.add(edicionPanel,c);
		// Menu Proyecto
		proyectoPanel.add(nuevoProyCB);
		proyectoPanel.add(abrirProyCB);
		proyectoPanel.add(closeProjectCB);
		proyectoPanel.add(guardarProyCB);		
		proyectoPanel.add(saveAsProjectCB);
		//mig
		proyectoPanel.add(newProjectFileCB);
		proyectoPanel.add(añadirFichProyCB);
		proyectoPanel.add(removeFileCB);
		proyectoPanel.add(deleteFileCB);
		proyectoPanel.add(addFolderCB);
		proyectoPanel.add(removeFolderCB);
		proyectoPanel.add(compilarCB);
		proyectoPanel.add(ejecutarCB);
		proyectoPanel.add(setFileCB);
		proyectoPanel.add(unsetFileCB);
		proyectoPanel.add(setMainCB);
		//mig
		proyectoPanel.add(unsetMainCB);
		c.gridy = 2;
		frame.add(proyectoPanel,c);
		// Menu Ver
		viewPanel.add(mostrarLogCB);
		viewPanel.add(showBrowserCB);
		viewPanel.add(showShellCB);
		c.gridy = 3;
		frame.add(viewPanel,c);
		// Menu Configuracion
		//configuracionPanel.add(menuCB);
		configuracionPanel.add(compilerCB);
		configuracionPanel.add(newLexicalCB);
		configuracionPanel.add(sintacticaCB);
		configuracionPanel.add(newMenuCB);
		configuracionPanel.add(newTBCB);
		configuracionPanel.add(configurarCB);
		configuracionPanel.add(cargarParamCB);		
		configuracionPanel.add(loadSyntaxCB);
		configuracionPanel.add(loadMenuCB);
		configuracionPanel.add(loadTBCB);
		configuracionPanel.add(comandoExternoCB);
		configuracionPanel.add(lexicaCB);
		configuracionPanel.add(modifySyntaxCB);
		configuracionPanel.add(modifyMenuCB);
		configuracionPanel.add(modifyTBCB);
		configuracionPanel.add(españolCB);
		configuracionPanel.add(saveLexicalCB);
		configuracionPanel.add(saveGrammarCB);
		configuracionPanel.add(saveMenuCB);
		configuracionPanel.add(saveTBCB);
		configuracionPanel.add(inglesCB);
		configuracionPanel.add(saveAsLexicalCB);
		configuracionPanel.add(saveAsGrammarCB);
		configuracionPanel.add(saveAsMenuCB);
		configuracionPanel.add(saveAsTBCB);
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(setPathsCB);
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(new JLabel(""));
		configuracionPanel.add(autoSyntaxAnalysisCB);
		c.gridy = 4;
		frame.add(configuracionPanel,c);
		// Menu Ayuda
		ayudaPanel.add(mostrarAyudaCB);
		ayudaPanel.add(acercadeCB);
		c.gridy = 5;
		frame.add(ayudaPanel,c);
		// Botones
		c.gridwidth = 1;
		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(5,5,5,5);
		botonesPanel.add(todosBoton,c);
		c.gridx = 1; 
		botonesPanel.add(ningunoBoton,c);
		c.gridx = 2;
		//botonesPanel.add(guardarBoton,c);
		c.gridx = 3;
		//botonesPanel.add(cargarBoton,c);
		c.gridx = 4;
		botonesPanel.add(aceptarBoton,c);
		c.gridx = 5;
		botonesPanel.add(cancelarBoton,c);
		c.gridy = 6;
		c.insets = new Insets(0,0,0,0);
		frame.add(botonesPanel,c);
		// Se muestra el frame
		frame.setVisible(true);
		frame.setResizable(false);
		frame.pack();
		frame.setLocationRelativeTo(null);
		logger.info(labels.getString("s530"));
	}

	public static boolean isChangesSaved() {
		return changesSaved;
	}

	public static void setChangesSaved(boolean changes) {
		MenuGUI.changesSaved = changes;
	}
}
