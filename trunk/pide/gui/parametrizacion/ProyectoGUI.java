package gui.parametrizacion;

import idioma.Idioma;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.plaf.TabbedPaneUI;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.JTree;

import operaciones.configuracion.Fich;
import operaciones.fabrica.FactoriaES;
import operaciones.fabrica.FactoriaGUI;
import operaciones.genericas.Key;
import operaciones.genericas.Windows;
import operaciones.log.Log;

import org.apache.log4j.Logger;

//import principal.FaltaPropiedadException;
import principal.almacenPropiedades;

import es.configuracion.lenguajeprog.Lenguaje;
import es.texto.ExtFilter;
import es.texto.Fichero;
import es.texto.FiltroFicheros;
import gui.Ventana;
import gui.explorador.Explorador;
import gui.menus.SearchGUI;


/**
 * Clase que implementa el interfaz gráfico para crear una confifuración nueva de un proyecto o modificar una existente
 */
public class ProyectoGUI extends JFrame {
	
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	
	private JFrame frame;

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();
	
	private ResourceBundle labels;
	
	private String nombreConfLexico;
	
	private JLabel nombreConfLabelLexico;
	
	private String nombreConfGram;
	
	private JLabel nombreConfLabelGram;
	
	private JTextField nombreTextField;
	
	private JButton aceptarBoton;
	
	//mig
	private String workspacePath;
	private JTextField workspaceTextField;
	private boolean b1;
	private boolean b2;
	private boolean expl;
	private boolean shell;
	private int width;
	private int height;
	private int posx;
	private int posy;
	private int width1;
	private int height1;
	
	public ProyectoGUI() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		//mig
		b1=false;
		b2=false;
		
		labels = i.getLabels();
        Ventana v=Ventana.getInstance();
        v.setEnabled(false);
        logger.info(labels.getString("s587"));
		frame = new JFrame();
		frame.setTitle(labels.getString("s588"));
		frame.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		// Creacion de todos los paneles de la ventana
		JPanel generalPanel = new JPanel();
		generalPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s589")));
		generalPanel.setLayout(new GridBagLayout());
		JPanel confLenguajePanel = new JPanel();
		confLenguajePanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s590")));
		confLenguajePanel.setLayout(new GridBagLayout());
		JPanel compiladorPanel = new JPanel();
		compiladorPanel.setBorder(BorderFactory.createTitledBorder(labels.getString("s591")));
		compiladorPanel.setLayout(new GridBagLayout());
		JPanel botonesPanel = new JPanel();
		botonesPanel.setLayout(new GridBagLayout());
		// Creacion de todos los elementos de los paneles
		JLabel nombreLabel = new JLabel(labels.getString("s592"));
		nombreTextField = new JTextField();
		nombreTextField.setToolTipText(labels.getString("s593"));
		
		nombreConfLexico =labels.getString("s598");
		nombreConfLabelLexico = new JLabel(labels.getString("s599") +" "+ nombreConfLexico);
		
		nombreConfGram = labels.getString("s598");
		nombreConfLabelGram =new JLabel(labels.getString("s642") +" "+ nombreConfGram);
		JButton crearBotonLexico = new JButton(labels.getString("s600"));
		crearBotonLexico.setHorizontalAlignment(JButton.CENTER);
		crearBotonLexico.setToolTipText(labels.getString("s601"));
		
		JButton cargarBotonLexico = new JButton(labels.getString("s602"));
		cargarBotonLexico.setHorizontalAlignment(JButton.CENTER);
		cargarBotonLexico.setToolTipText(labels.getString("s603"));
		
		JButton crearBotonGrammar = new JButton(labels.getString("s600"));
		crearBotonGrammar.setHorizontalAlignment(JButton.CENTER);
		crearBotonGrammar.setToolTipText(labels.getString("s601"));
		JButton cargarBotonGrammar = new JButton(labels.getString("s602"));
		cargarBotonGrammar.setHorizontalAlignment(JButton.CENTER);
		cargarBotonGrammar.setToolTipText(labels.getString("s603"));
		
		
		JButton interpreter= new JButton(labels.getString("s637"));
		interpreter.setHorizontalAlignment(JButton.CENTER);
		interpreter.setToolTipText(labels.getString("s637"));
		
		JButton compiler= new JButton(labels.getString("s636"));
		compiler.setHorizontalAlignment(JButton.CENTER);
		compiler.setToolTipText(labels.getString("s636"));
		
		////mig////////////////////////////////////////////////////////////////////
		JButton workspaceButton = new JButton(labels.getString("s948"));
		workspaceButton.setHorizontalAlignment(JButton.RIGHT);
		
		workspaceTextField = new JTextField("");
		
		JLabel workspaceLabel = new JLabel(labels.getString("s949"));
		///////////////////////////////////////////////////////////////////////////
		
		aceptarBoton = new JButton(labels.getString("s154"));
		aceptarBoton.setHorizontalAlignment(JButton.CENTER);
		aceptarBoton.setToolTipText(labels.getString("s611"));
		JButton cancelarBoton = new JButton(labels.getString("s162"));
		cancelarBoton.setHorizontalAlignment(JButton.CENTER);
		cancelarBoton.setToolTipText(labels.getString("s612"));
		
		crearBotonLexico.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
			Ventana v=Ventana.getInstance();
			v.getnuevoMenu().getNuevaConfLenguaje().doClick();
			
			}
		});
		cargarBotonLexico.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
		    Ventana v=Ventana.getInstance();
				v.getnuevoMenu().getCargarParam().doClick();
				nombreConfLexico = v.getProyecto().getLenguajeConfig();
				String nombreConfLex = Lenguaje.getInstance().getNombre();
				nombreConfLabelLexico.setText(labels.getString("s599") +" "+ nombreConfLex);
				
				}
		});
		crearBotonGrammar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v=Ventana.getInstance();
				v.getnuevoMenu().getSintactica().doClick();
				int index = v.getProyecto().getGrammarConfig().lastIndexOf("\\");
				if (index == -1) index = v.getProyecto().getGrammarConfig().lastIndexOf("/");
				String grammarName = v.getProyecto().getGrammarConfig().substring(index + 1,v.getProyecto().getGrammarConfig().length() - 4);
				nombreConfLabelGram.setText(labels.getString("s642") +" "+ grammarName);
			 nombreConfLabelGram.validate();
			 nombreConfLabelGram.repaint();

			}
			});
		cargarBotonGrammar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v=Ventana.getInstance();
				v.getnuevoMenu().getLoadSyntax().doClick();
				int index = v.getProyecto().getGrammarConfig().lastIndexOf("\\");
				if (index == -1) index = v.getProyecto().getGrammarConfig().lastIndexOf("/");
				String grammarName = v.getProyecto().getGrammarConfig().substring(index + 1,v.getProyecto().getGrammarConfig().length() - 4);
				nombreConfLabelGram.setText(labels.getString("s642") +" "+ grammarName);
			 nombreConfLabelGram.validate();
			 nombreConfLabelGram.repaint();
			}
			});
		///mig//////////////////////////////////////////////////
		workspaceButton.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				JFileChooser fc = new JFileChooser();
				fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				int option = fc.showOpenDialog(null);
				if(option == JFileChooser.APPROVE_OPTION) {
					workspacePath = fc.getSelectedFile().getAbsolutePath();
				}
				workspaceTextField.setText(workspacePath);
				workspaceTextField.validate();
				workspaceTextField.repaint();
			}
		});
		////////////////////////////////////////////////////////
		aceptarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v=Ventana.getInstance();
				if (!nombreTextField.getText().equals("")){
					//mig
					boolean a=true;
					String file2 = workspaceTextField.getText()+"\\"+nombreTextField.getText();
					if(!file2.contains(".acidePrj"))file2 = file2+".acidePrj";
					File ff=new File(file2);
					if (ff.exists()){
						int res = JOptionPane.showConfirmDialog(null,labels.getString("s955"),labels.getString("s953"),JOptionPane.YES_NO_OPTION);
						if(res == JOptionPane.YES_OPTION){a = true;}
						if(res == JOptionPane.NO_OPTION){a = false;}
					}
					else a=true;
					
					if(a){
						
					//mig
					if(!b1){
						v.getProyecto().setpathEjecu(null);
						v.getProyecto().setarg(null);
					}else{
						if(v.getProyecto().getpathEjecu().equals("")) v.getProyecto().setpathEjecu(null);
						if(v.getProyecto().getarg().equals("")) v.getProyecto().setarg(null);
					}
					//v.getProyecto().setExtensionFile(null);
					//v.getProyecto().setSeparatorFile(null);
					if(!b2){
						v.getProyecto().setShellPath(null);
						v.getProyecto().setShellDir(null);
					}else{
						if(v.getProyecto().getShellPath().equals("")) v.getProyecto().setShellPath(null);
						if(v.getProyecto().getShellDir().equals("")) v.getProyecto().setShellDir(null);
					}
					
					v.getProyecto().borraFich();
					v.getProyecto().setnombreProy(nombreTextField.getText());
					if(nombreConfLexico.equals(labels.getString("s598"))) {try {
						nombreConfLexico = almacenPropiedades.getPropiedad("pathLenguaje");
					} catch (Exception e1) {
						//e1.printStackTrace();
					}}
					v.getProyecto().setLenguajeConfig(nombreConfLexico);
					v.getProyecto().setFirstSave(false);
					logger.info(labels.getString("s615") + nombreTextField.getText());
					v.getNuevoExplorador().getRaiz().removeAllChildren();
					Fich fic=new Fich();
					fic.setPath(nombreTextField.getText());
					fic.setName(nombreTextField.getText());
			    	fic.setPadre(null);
			    	fic.setDirectory(true);
			    	DefaultMutableTreeNode d = new DefaultMutableTreeNode(fic);
			    	d.setAllowsChildren(true);
				
				///mig////////////////////////////////////////////////////////////
				//File directorio = new File(workspaceTextField.getText()+"\\"+nombreTextField.getText());
				
				//directorio.mkdir();
				//String p = directorio.getAbsolutePath();
				//almacenPropiedades.setPropiedad("DefaultPath",p);
				
				
				////
				try {
					FactoriaES fact = FactoriaES.getInstance();
				
					ResourceBundle labels = Idioma.getInstance().getLabels();
					Fichero f = fact.generaFichero();
					
					if (!v.getProyecto().getnombreProy().equals("")) {
						// Selecciona la extension del proyecto
						String[] ExtPide = new String[] { "acidePrj" };
						f.getFileChooser().addChoosableFileFilter(
								new ExtFilter(ExtPide, labels.getString("s328")));
						
						String file = workspaceTextField.getText()+"\\"+nombreTextField.getText();
						
						String currentMenu=almacenPropiedades.getPropiedad("currentMenuCfg");
						String currentTB=almacenPropiedades.getPropiedad("currentTBCfg");
						
						v.getProyecto().setCurrentMenu(currentMenu);
						v.getProyecto().setCurrentTB(currentTB);
						
						if(!file.contains(".acidePrj"))file = file+".acidePrj";
						
						v.getProyecto().setpathProy(file);
						String cad = v.getProyecto().salvarProy();
						f.salvar(v.getProyecto().getpathProy(), cad);		
						v.getProyecto().setFirstSave(true);
						almacenPropiedades.setPropiedad("DefaultAcidePrj", file);
						almacenPropiedades.setPropiedad("DefaultPath",file);
						
						v.getProyecto().setModified(false);
					}
				} catch (Exception ex) {
					ex.printStackTrace();
				}
				
				//////////////////////////////////////////////////////////////////
				
			    v.getNuevoExplorador().getRaiz().add(d);
				v.setTitle(labels.getString("s425")+" - "+nombreTextField.getText());
			    
				//ADD ALL EDITORS 
		           v.getProyecto().setNumFich(
						Integer.toString(v.getCreadorEditor()
								.dameNumEditores()));
				//Posicion del elemento en el árbol
		           	for (int i = 0; i < v.getCreadorEditor().dameNumEditores(); i++){ 
				    fic=new Fich();
				     fic.setMainFile(v.getCreadorEditor().dameEditorI(i).isMainFile());
				     fic.setSetFile(v.getCreadorEditor().dameEditorI(i).isCompilerFile());
		        	 fic.setPath(v.getCreadorEditor().dameEditorI(i).getPath());
					 fic.setPadre(d.toString());
		               	   v.getProyecto().setfich(fic);
					String file=v.getCreadorEditor().dameEditorI(i).getPath();
					int in = file.lastIndexOf("\\");
					String fich="";
					if (in!=-1){
					in++;
					fich = file.substring(in, file.length());
					String ruta = file.substring(0, in);
					}else{
						in =file.lastIndexOf("/");
						in++;
						fich = file.substring(in, file.length());
						String ruta = file.substring(0, in);
					}
						
					fic.setName(fich);
					DefaultMutableTreeNode de = new DefaultMutableTreeNode(fic);
				  	de.setAllowsChildren(false);
					d.add(de);
									    
		           }
		           	
		        //mig
		        v.getProyecto().setExplorer(true);
		        v.getProyecto().setShell(true);
		        v.getProyecto().setWidthWindow(v.getWidth());
		        v.getProyecto().setHeightWindow(v.getHeight());
		        v.getProyecto().setPosx(v.getX());
		        v.getProyecto().setPosy(v.getY());
		        v.getProyecto().setWidth1(v.getNuevoExplorador().getWidth());
		        v.getProyecto().setHeight1(v.getnuevaSalida().getHeight());
		           	
				v.validate();
	    		v.repaint();
				v.getNuevoExplorador().setEnabledAddFile();
				v.getNuevoExplorador().setEnabledSaveProj();
				v.getNuevoExplorador().getTreeModel().reload();
				v.getNuevoExplorador().expandTree();
				v.setEnabled(true);
				frame.dispose();
				//mig
				if(!v.getnuevoMenu().getShowBrowserCBox().isSelected()) v.getNuevoExplorador().showExplorer();
				v.getnuevoMenu().getShowBrowserCBox().setSelected(true);
				v.getnuevoMenu().getShowShellWindowCBox().setSelected(true);
				v.getnuevoMenu().habilitaMenuProyecto();
				}
				else{}
			}
			}
		});
		cancelarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				logger.info(labels.getString("s614"));
				Ventana v=Ventana.getInstance();
				v.setEnabled(true);
				frame.dispose();
			}
		});
		
		interpreter.addActionListener(new ActionListener(){
			FactoriaGUI fact = FactoriaGUI.getInstance();
			
			public void actionPerformed(ActionEvent e){
				fact.generaConfigurarConsolaGUI();
			
			}
		});
		compiler.addActionListener(new ActionListener(){
			FactoriaGUI fact = FactoriaGUI.getInstance();
			
			public void actionPerformed(ActionEvent e){
				fact.buildCompilerGUI();
			
			}
		});
		
		// Valores para nombreLabel
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(5,5,5,5);
		c.gridx = 0;
		c.gridy = 0;
		c.ipady = 10;
		generalPanel.add(nombreLabel,c);
		// Valores para nombreTextField
		c.gridx = 1;
		c.ipadx = 140;
		c.ipady = 0;
		generalPanel.add(nombreTextField,c);
		///mig////////////////////////////////////////////////////////////
		// Valores para workspaceLabel
		c.gridx = 0;
		c.gridy = 1;
		c.ipadx = 0;
		c.ipady = 0;
		generalPanel.add(workspaceLabel,c);
		// Valores para workspaceTextField
		c.gridx = 1;
		c.gridy = 1;
		c.ipadx = 0;
		c.ipady = 0;
		generalPanel.add(workspaceTextField,c);
		// Valores para workspaceButton
		c.gridx = 2;
		c.gridy = 1;
		c.ipadx = 0;
		c.ipady = 0;
		generalPanel.add(workspaceButton,c);
		//////////////////////////////////////////////////////////////////
		c.insets = new Insets(0,0,0,0);
		c.gridx = 0;
		c.gridy = 0;
		frame.add(generalPanel,c);
		// Valores para nombreConfLabel
		c.insets = new Insets(5,5,5,5);
		c.gridwidth = 2;
		confLenguajePanel.add(nombreConfLabelLexico,c);
        // Valores para crearBoton
		c.gridwidth = 1;
		c.gridx = 0;
		c.gridy = 1;
		c.ipadx = 40;
		confLenguajePanel.add(crearBotonLexico,c);
		// Valores para cargarBoton
		c.gridx = 1;
		c.ipadx = 0;
		confLenguajePanel.add(cargarBotonLexico,c);
	    
		c.gridx=0;
		c.gridy=2;
		c.insets = new Insets(5,5,5,5);
		c.gridwidth = 2;
		confLenguajePanel.add(nombreConfLabelGram,c);
		 //Valores para crearBoton
		c.gridy = 3;
		c.gridwidth = 1;
		c.gridx = 0;
		c.ipadx = 40;
		confLenguajePanel.add(crearBotonGrammar,c);
        //Valores para cargarBoton
		c.gridx = 1;
		c.ipadx = 0;
		confLenguajePanel.add(cargarBotonGrammar,c);
		c.gridx=0;
		c.gridy=1;
		frame.add(confLenguajePanel,c);
	    
		c.gridx = 0; 
		c.gridy = 0;
		c.ipady = 10;
					
		compiladorPanel.add(compiler,c);
		c.gridx = 1; 
		compiladorPanel.add(interpreter,c);
		
		c.gridx=0;
		c.gridy=2;
	
		frame.add(compiladorPanel,c);
		// Valores para aceptarBoton
		c.insets = new Insets(5,5,5,5);
		c.gridy = 0;
		botonesPanel.add(aceptarBoton,c);
		// Valores para cancelarBoton
		c.gridx = 1;
		c.insets = new Insets(5,70,5,5);
		botonesPanel.add(cancelarBoton,c);
		// Valores para botonesPanel
		c.insets = new Insets(0,0,0,0);
		c.gridx = 0;
		c.gridy = 3;
		frame.add(botonesPanel,c);
		frame.setVisible(true);
		frame.setResizable(false);
		frame.pack();
		frame.setLocationRelativeTo(null);
		logger.info(labels.getString("s613"));
		Windows window=new Windows();
		frame.addWindowListener(window);
	}
	public JLabel getNombreConfLabelLexico() {
		return nombreConfLabelLexico;
	}
	public void setNombreConfLabelLexico(String nombreConfLabel) {
		this.nombreConfLabelLexico.setText(nombreConfLabel);
	}
	public String getNombreConfLexico() {
		return nombreConfLexico;
	}
	public void setNombreConfLexico(String nombreConfLexico) {
		this.nombreConfLexico = nombreConfLexico;
	}

	public JButton getAceptarBoton() {
		return aceptarBoton;
	}
	public String getNombreConfGram() {
		return nombreConfGram;
	}
	public void setNombreConfGram(String nombreConfGram) {
		this.nombreConfGram = nombreConfGram;
	}
	public JLabel getNombreConfLabelGram() {
		return nombreConfLabelGram;
	}
	public void setNombreConfLabelGram(JLabel nombreConfLabelGram) {
		this.nombreConfLabelGram = nombreConfLabelGram;
	}
	public String getWorkspacePath() {
		return workspacePath;
	}
	public void setWorkspacePath(String workspacePath) {
		this.workspacePath = workspacePath;
	}
	public void setB2(boolean b) {
		b2 = b;
	}
	public void setB1(boolean b) {
		b1 = b;
	}
	
}
