package gui.editor;

import gui.Ventana;
import gui.editor.CreadorEditor.TestPlaf.TestPlafLayout.CloseButton;
import idioma.Idioma;

import javax.swing.*;
import javax.swing.plaf.basic.*;
import javax.swing.tree.TreePath;

import operaciones.configuracion.Fich;
import operaciones.fabrica.FactoriaES;
import operaciones.log.Log;
import org.apache.log4j.Logger;

import principal.almacenPropiedades;

import es.texto.Fichero;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ResourceBundle;
import principal.*;
 

public class CreadorEditor {

	
	/**
	 * Nos permite crear editores
	 */
	//JTabbedPane pane; 
	DnDTabbedPane pane;
	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	TestPlaf tp;
	
	static java.util.ArrayList closeButtons = new java.util.ArrayList();

	/**
	 * Instancia un CreadorEditor
	 */
	public CreadorEditor(){
		ResourceBundle labels = Idioma.getInstance().getLabels();
		try {
			Mouse mouse = new Mouse();
			pane = new DnDTabbedPane();
			tp = new TestPlaf();
			pane.setUI(tp);
			pane.addMouseListener(mouse);
		} catch (RuntimeException e) {
			logger.info(labels.getString("s315"));
			e.printStackTrace();
		}
		
	}
	
	/**
	 * Crea un nuevo editor y lo añade al panel externo
	 * 
	 * @param nombre
	 * @param tooltip
	 */
	public void nuevoEditor(String nombre, String tooltip, int tipo) {
		Editor e = new Editor();
		
		switch(tipo){
			case 0: {pane.addTab(nombre,null,e,tooltip);
					e.setIcon(null);
					break;}
			case 1: {pane.addTab(nombre,new ImageIcon("./Iconos/main.PNG"),e,tooltip);
					e.setIcon(new ImageIcon("./Iconos/main.PNG"));
					break;}
			case 2: {pane.addTab(nombre,new ImageIcon("./Iconos/compilable.PNG"),e,tooltip);
					new ImageIcon("./Iconos/compilable.PNG");
					break;}
		}
		pane.revalidate();
		pane.repaint();
		//System.out.println(closeButtons.size());
	}
	
	/**
	 * Devuelve el pane con todos los editores
	 * 
	 * @return
	 */
	public JTabbedPane dameEditores(){
		return pane;
	}
	
	/**
	 * Devuleve el editor que ocupa la posición pos
	 * 
	 * @param pos
	 * @return
	 */
	public Editor dameEditorI(int pos){
		if((pos < pane.getComponentCount())&&(pos >=0)){
			return (Editor) pane.getComponentAt(pos);
		}
		else{
			return null;
		}
	}
	
	
	/**
	 * Devuleve el numero de editores que contiene el panel
	 * 
	 * @return
	 */
	public int dameNumEditores(){
		return pane.getTabCount();
	}
	
	/**
	 * Crea una nueva pesta;a con los atributos que se le pasan como parametro
	 * 
	 * @param nombre
	 * @param tipText
	 * @param texto
	 * @param editable
	 */
	public void nuevaPestaña(String nombre, String tipText, String texto, boolean editable, int tipo) {
		
		boolean encontrado = false;
		int pos = 0;
		for(int i=0;i<dameNumEditores();i++){
			if(dameEditorI(i).getPath() == tipText){
				encontrado = true;
				pos = i;
			}
		}
		if (!encontrado){
			File fich = new File(tipText);
			int index = nombre.lastIndexOf("\\");
			String subnombre = nombre.substring(index+1);
			nuevoEditor(subnombre, tipText, tipo);
			int n = dameNumEditores() -1;		
			dameEditorI(n).cargaTexto(texto);
			dameEditorI(n).setEditable(editable);
			dameEditores().setSelectedIndex(n);
			dameEditorI(n).setPath(tipText);
			dameEditorI(n).setUltimoCambio(fich.lastModified());
			dameEditorI(n).setUltimoTam(fich.length());
		}
		else{
			setEditorSeleccionado(pos);
		}
		
	}
	
	/**
	 * Selecciona un editor concreto
	 * 
	 * @param n
	 */
	public void setEditorSeleccionado(int n){
		dameEditores().setSelectedIndex(n);
	}
	
	/**
	 * Devuelve el editor seleccionado
	 * 
	 * @return
	 */
	public int getEditorSeleccionado(){
		return dameEditores().getSelectedIndex();
	}
	
	public Editor EditorSeleccionado(){
		return dameEditorI(dameEditores().getSelectedIndex());
	}
	
	//mig
	/**
	 * Devuelve el editor principal
	 * 
	 * @return
	 */
	public Editor EditorPrincipal(){
		
		for(int i=0; i<dameNumEditores(); i++){
			
			if(dameEditorI(i).isMainFile()) return dameEditorI(i);
		}
		
		return null;
	}
	
	/**
	 * Clase encargada de gestionar los editores abiertos en un tabeddpane
	 */
	static class TestPlaf extends BasicTabbedPaneUI
	 
	{
 
		TestPlafLayout t;
		
		/* (non-Javadoc)
		 * @see javax.swing.plaf.basic.BasicTabbedPaneUI#createLayoutManager()
		 */
		protected LayoutManager createLayoutManager()
 
		{
 
			t = new TestPlafLayout();
			return t;
 
		}
		
		public CloseButton getCloseButtoni(int pos){
			return t.getclosebuttoni(pos);
		}
 
 
 
		
 
		/* (non-Javadoc)
		 * @see javax.swing.plaf.basic.BasicTabbedPaneUI#getTabInsets(int, int)
		 */
		protected Insets getTabInsets(int tabPlacement,int tabIndex)
 
		{
 
			 
			Insets defaultInsets = (Insets)super.getTabInsets(tabPlacement,tabIndex).clone();
 
			defaultInsets.right += 40;
 
			defaultInsets.top += 4;
 
			defaultInsets.bottom += 4;
 
			return defaultInsets;
 
		}
 
 
 
		/**
		 * Clase que extiende algunas propiedades del layout que contendra al tabbedpane
		 */
		class TestPlafLayout extends TabbedPaneLayout
 
		{
 
			//lista de botones de cerrar
 
			
			public CloseButton getclosebuttoni(int pos){
				return (CloseButton) closeButtons.get(pos);
			}
 
			/* (non-Javadoc)
			 * @see java.awt.LayoutManager#layoutContainer(java.awt.Container)
			 */
			public void layoutContainer(Container parent)
 
			{
 
				super.layoutContainer(parent);
 
				//nos aseguramos de que hay tantos botones de cerrar como tabs
 
				while(tabPane.getTabCount() > closeButtons.size())
 
				{
 
					closeButtons.add(new CloseButton(closeButtons.size()));

 
				}
				
				Rectangle rect = new Rectangle();
 
				int i;
 
				for(i = 0; i < tabPane.getTabCount();i++)
 
				{
 
					rect = getTabBounds(i,rect);
 
					JButton closeButton = (JButton)closeButtons.get(i);
 
					//desplaza el boton de cerrar 
					closeButton.setLocation(rect.x+rect.width-20,rect.y+5);
 
					closeButton.setSize(15,15);
 
					tabPane.add(closeButton);
 
				}
 
 
 
				for(;i < closeButtons.size();i++)
 
				{
 
					//comprueba y elimina los botones de mas
					
					
					tabPane.remove((JButton)closeButtons.get(i));
					closeButtons.remove(i);

				}
 
			}
 
 

 
			/**
			 * Implementa UIResource usado cuando añadimos el boton al tabbedpane 
			 */
			class CloseButton extends JButton implements javax.swing.plaf.UIResource
 
			{
 
				/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

				public CloseButton(int index) {
 
					super(new CloseButtonAction(index));
					//setBackground(new Color(0,200,0));
					setForeground(new Color(0,200,0));
					setFont(new Font("Arial", Font.BOLD, 12));
					ResourceBundle labels = Idioma.getInstance().getLabels();
                                        setToolTipText(labels.getString("s316"));

					 
					setMargin(new Insets(0,0,0,0));
 
                    addMouseListener(new MouseAdapter()
 
				         {
 
					       public void mouseEntered(MouseEvent e)
 
					       {
 
						     //setForeground(new Color(0,0,0));
 
					        }
 
					        public void mouseExited(MouseEvent e)
 
					        {
 
						     //setForeground(new Color(0,0,0));
 
					         }
 
				         });
 
				}
				
				public void redbutton(){
					setForeground(new Color(255,0,0));
					//setBackground(new Color(255,0,0));
				}
				
				public void greenbutton(){
					setForeground(new Color(0,200,0));
					//setBackground(new Color(0,200,0));
				}
				
				public boolean isRedButton(){
					if (getForeground().equals(new Color(255,0,0)))
			              return true;
					return false;
				}
				
			}
 
 
 
			/**
			 * Implementa la accion del boton cerrar
			 */
			class CloseButtonAction extends AbstractAction
 
			{
 
				/**
				 * 
				 */
				private static final long serialVersionUID = 1L;
				int index;
 
				public CloseButtonAction(int index)
 
				{
 
					super("x");
 
					this.index = index;
 
				}
 
 
 
				public void actionPerformed(ActionEvent e)
 
				{
                      Ventana v=Ventana.getInstance();
                      //////////mig
                      Idioma i = Idioma.getInstance();
              		try {
              			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
              					.getPropiedad("idioma")));
              		} catch (Exception ee) {
              			ee.printStackTrace();
              		}
                    final ResourceBundle labels = i.getLabels();
      				//int editor=v.getCreadorEditor().getEditorSeleccionado();
      				if (v.getCreadorEditor().isRedButton(index)==true){
      					int opt=JOptionPane.showConfirmDialog(null,labels.getString("s643"));
      					
      					if(opt==JOptionPane.OK_OPTION){
      						
      						if (v.getCreadorEditor().dameEditorI(index).getPath().equals(labels.getString("s79"))==true){
      							
      							FactoriaES fac=FactoriaES.getInstance();
      							Fichero f=fac.generaFichero();
      							String archivo = " ";
      							archivo = f.escribir();
      							if (archivo.equals(" ")) {
      								//logger.info(labels.getString("s92"));
      							} else {
      								boolean resultado = f.salvar(archivo, v.getCreadorEditor().dameEditorI(index).getTexto());
      								// Muestra en el log
      								if (resultado) {
      									//logger.info(labels.getString("s93")+archivo+labels.getString("s94"));
      									Ventana.getInstance().getCreadorEditor().greenButton(index);
      									v.getCreadorEditor().dameEditorI(index).setPath(archivo);
      									v.getCreadorEditor().dameEditorI(index).setToolTipText(archivo); 
      									int in = archivo.lastIndexOf("\\");
      									in++;
      									String file = archivo.substring(in, archivo.length());
      									v.getCreadorEditor().dameEditorI(index).setName(file);
      									v.getStatusBar().setMessage("");
      								} else {
      									//logger.info(labels.getString("s95") + archivo);
      								}
      							}

      						}
      						else {
      							FactoriaES fact = FactoriaES.getInstance();
      							Fichero f = fact.generaFichero();
      							//String archivo = " ";	
      								if (v.getCreadorEditor().dameEditorI(index).getPath().equals(labels.getString("s79"))==false){
      								boolean resultado = f.salvar(v.getCreadorEditor()
      										.dameEditorI(index).getPath(), v.getCreadorEditor()
      										.dameEditorI(index).getTexto());
      								// Muestra en el log
      								if (resultado) {
      									 //logger.info(labels.getString("s93") + archivo+ labels.getString("s94"));
      									Ventana.getInstance().getCreadorEditor().greenButton(index);
      								} else {
      									//logger.info(labels.getString("s95") + archivo);
      								}
      							}else{
      								String archivo = " ";
      								if (v.getCreadorEditor().dameNumEditores() == 0) {
      									//logger.info(labels.getString("s89"));
      								} else {
      									archivo = f.escribir();
      									if (archivo.equals(" ")) {
      										//logger.info(labels.getString("s92"));
      									} else {
      										boolean resultado = f.salvar(archivo, v.getCreadorEditor()
      												.dameEditorI(index).getTexto());
      										// Muestra en el log
      										if (resultado) {
      											//logger.info(labels.getString("s93") + archivo+ labels.getString("s94"));
      											 Ventana.getInstance().getCreadorEditor().greenButton(index);
      											 int in = archivo.lastIndexOf("\\");
      												in++;
      												String file = archivo.substring(in, archivo.length());
      											 v.getCreadorEditor().getPane().setTitleAt(index,file);
      										     v.getCreadorEditor().dameEditorI(index).setPath(archivo);
      							                 v.getCreadorEditor().getPane().setToolTipText(archivo);
      							                 v.getStatusBar().setMessage("");
      							               
      										} else {
      											//logger.info(labels.getString("s95") + archivo);
      										}
      									}
      								}
      							}

      						}
      						//mig
      						for(int i2=0; i2<v.getProyecto().getFichSize(); i2++){
      							if(v.getProyecto().getfich(i2).getPath().equals(v.getCreadorEditor().dameEditorI(index).getPath())){
      								v.getProyecto().getfich(i2).setOpened(false);
      							}
      						}
      						String prj=null;
      						try {
      							prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
      						} catch (Exception e1) {
      							// TODO Auto-generated catch block
      							e1.printStackTrace();
      						}
      						if (!(prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
      							v.getProyecto().setModified(true);
      						}
      						v.getCreadorEditor().getPane().remove(index);
      						
      					}
      					else if (opt==JOptionPane.NO_OPTION) {
      						v.getCreadorEditor().getPane().remove(index);
      						v.getStatusBar().setMessage("");
      					}
      				}
      				else {
      					//mig
      					for(int i2=0; i2<v.getProyecto().getFichSize(); i2++){
      						if(v.getProyecto().getfich(i2).getPath().equals(v.getCreadorEditor().dameEditorI(index).getPath())){
      							v.getProyecto().getfich(i2).setOpened(false);
      						}
      					}
      					String prj=null;
      					try {
      						prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
      					} catch (Exception e1) {
      						// TODO Auto-generated catch block
      						e1.printStackTrace();
      					}
      					if (!(prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
      						v.getProyecto().setModified(true);
      					}
      					v.getCreadorEditor().getPane().remove(index);
      					v.getStatusBar().setMessage("");
      				}
      				if(v.getCreadorEditor().getPane().getTabCount()==0){
      					v.getnuevoMenu().deshabilitaMenuArchivo();
      					v.getnuevoMenu().deshabilitaMenuEdicion();
      				}
                      ///////////
                      //v.getnuevoMenu().getCloseFile().doClick();
					//tabPane.remove(index);
					for (int pos = index; pos < closeButtons.size()-1;pos++){
						CloseButton c1 = (CloseButton) closeButtons.get(pos);
						CloseButton c2 = (CloseButton) closeButtons.get(pos+1);
						//c1.setBackground(c2.getBackground());
						c1.setForeground(c2.getForeground());
						closeButtons.set(pos,c1);
					}
				}
 
			}



			public java.util.ArrayList getCloseButtons() {
				return closeButtons;
			}	
 
		}	
 
	}	
	
		
	class Mouse implements MouseListener{

		public void mouseClicked(MouseEvent arg0) {
			if(pane.getComponentCount()!=0){
			String s = EditorSeleccionado().getPath();
			if (s != null){
				File f = new File(s);
				//System.out.print(f.length());
				//System.out.println(EditorSeleccionado().getUltimoTam());
				if((f.lastModified() != EditorSeleccionado().getUltimoCambio()) || (f.length() != EditorSeleccionado().getUltimoTam())){
					Idioma i = Idioma.getInstance();

					try {
						i.seleccionIdioma(Integer.parseInt(almacenPropiedades
								.getPropiedad("idioma")));
					} catch (Exception e) {
						e.printStackTrace();
					}
					ResourceBundle labels = i.getLabels();
					int j = JOptionPane.showConfirmDialog(null,labels.getString("s65"));
					if (j == 0){
						Fichero nuevo = new Fichero();
						EditorSeleccionado().cargaTexto(nuevo.cargar(s));
						EditorSeleccionado().setUltimoCambio(f.lastModified());
						EditorSeleccionado().setUltimoTam(f.length());
					}
					else{
						EditorSeleccionado().setUltimoCambio(f.lastModified());
						EditorSeleccionado().setUltimoTam(f.length());
					}
				}
			}
			}

		//Incluye mensaje en STATUS BAR
		if(EditorSeleccionado()!= null){
			Ventana v = Ventana.getInstance();
			
			v.getStatusBar().setMessage(v.getCreadorEditor().EditorSeleccionado().getPath());
			for (int i=0;i<v.getProyecto().dameNumFich();i++){
			if(v.getProyecto().getfich(i).getPath().equals(v.getCreadorEditor().EditorSeleccionado().getPath()))
				if(v.getProyecto().getfich(i).isSetFile())
					if(v.getProyecto().getfich(i).isMainFile()) v.getStatusBar().setMessage(v.getCreadorEditor().EditorSeleccionado().getPath()+" <MAIN>");
					else v.getStatusBar().setMessage(v.getCreadorEditor().EditorSeleccionado().getPath()+" <COMPILABLE>");
				else v.getStatusBar().setMessage(v.getCreadorEditor().EditorSeleccionado().getPath());	
			}
			
			//Ventana v = Ventana.getInstance();
			String pr=null;
			try {
				pr=almacenPropiedades.getPropiedad("DefaultAcidePrj");
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			
			if ((pr.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
			
				//status
				if(v.getCreadorEditor().EditorSeleccionado().isCompilerFile())
					if(v.getCreadorEditor().EditorSeleccionado().isMainFile()) v.getStatusBar().setMessage(v.getCreadorEditor().EditorSeleccionado().getPath()+" <MAIN>");
					else v.getStatusBar().setMessage(v.getCreadorEditor().EditorSeleccionado().getPath()+" <COMPILABLE>");
				else v.getStatusBar().setMessage(v.getCreadorEditor().EditorSeleccionado().getPath());
			}
		
		}
		
		//poner el archivo seleccionado en el editor
		//si hay proyecto
		Ventana v = Ventana.getInstance();
		String prj=null;
		try {
			prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		
		if (!(prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
		
		if(EditorSeleccionado()!= null){
			Fich f=new Fich();
			int y=-1;
			int editor = v.getCreadorEditor().getEditorSeleccionado();
			for(int j=0; j<v.getProyecto().dameNumFich(); j++){
		          
				if(v.getProyecto().getfich(j).getPath().equals(v.getCreadorEditor().EditorSeleccionado().getPath())){
					f = v.getProyecto().getfich(j);
					for(int m=0; m<v.getProyecto().dameNumFich()+1; m++){				
						if(v.getNuevoExplorador().getArbol().getPathForRow(m).getLastPathComponent().toString().equals(f.getLastPathComponent())){
							
							y=m;
						}
					}
				}
			}
			
			TreePath currentSelection = v.getNuevoExplorador().getArbol().getPathForRow(y);
			v.getNuevoExplorador().getArbol().setSelectionPath(currentSelection);
		}
		}
		
		if(v.getCreadorEditor().dameNumEditores()>0){
		int index = Ventana.getInstance().getCreadorEditor().getEditorSeleccionado();
		ImageIcon ic = (ImageIcon) pane.getIconAt(index);
		Ventana.getInstance().getCreadorEditor().getPane().setIcon(ic);}
		}

		public void mousePressed(MouseEvent arg0) {
			if(Ventana.getInstance().getCreadorEditor().dameNumEditores()>0){
			int index = Ventana.getInstance().getCreadorEditor().getEditorSeleccionado();
			ImageIcon ic = (ImageIcon) pane.getIconAt(index);
			Ventana.getInstance().getCreadorEditor().getPane().setIcon(ic);}
		}

		public void mouseReleased(MouseEvent arg0) {
			// TODO Auto-generated method stub
		}

		public void mouseEntered(MouseEvent arg0) {
			// TODO Auto-generated method stub
		}

		public void mouseExited(MouseEvent arg0) {
			// TODO Auto-generated method stub
		}	
	}


	public DnDTabbedPane getPane() {
		return pane;
	}

	public TestPlaf getTp() {
		return tp;
	}

	public void greenButton(){
		Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(Ventana.getInstance().
				getCreadorEditor().getEditorSeleccionado()).greenbutton();
	}

	public boolean isRedButton(){
		return Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(Ventana.getInstance().
				getCreadorEditor().getEditorSeleccionado()).isRedButton();
	}
	
	public void greenButton(int i){
		Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(i).greenbutton();
	}

	public boolean isRedButton(int i){
		return Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(i).isRedButton();
	}
}
