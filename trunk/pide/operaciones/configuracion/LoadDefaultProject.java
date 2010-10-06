package operaciones.configuracion;

import es.configuracion.lenguajeprog.Lenguaje;
import es.texto.Fichero;
import gui.Ventana;
import gui.iconos.Iconos;
import gui.parametrizacion.EdicionIconosGUI;
import gui.parametrizacion.MenuGUI;

import idioma.Idioma;

import java.awt.Cursor;
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Document;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.undo.UndoableEdit;

import operaciones.fabrica.FactoriaES;
import operaciones.fabrica.FactoriaOperaciones;
import principal.almacenPropiedades;

public class LoadDefaultProject {
	Ventana v = Ventana.getInstance();
	Idioma i = Idioma.getInstance();
	
	private DefaultMutableTreeNode search_listdir(ArrayList list, String f){
		int i=0;
		boolean encontrado=false;
		while (i<list.size()&&!encontrado){
			DefaultMutableTreeNode temp=(DefaultMutableTreeNode)list.get(i);
			Fich fich=(Fich)temp.getUserObject();
			if (fich.getName().equals(f)){
				encontrado=true;
				return (DefaultMutableTreeNode) list.get(i);
			}else
				i++;
		}
			return null;
	 }
	/*public  void loadDefault(){

    // OPTION PROJECT////
			v.getProyecto().borraFich();
			Fichero f = new Fichero();
			String texto=null;
			try {
				texto = f.cargar(almacenPropiedades.getPropiedad("DefaultAcidePrj"));
			} catch (Exception e) {
				e.printStackTrace();
			}*/
	
	public String preloadDefault(){
		
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		
		v.getProyecto().borraFich();
		Fichero f = new Fichero();
		String texto=null;
		String ruta = null;

		try {
			ruta = almacenPropiedades.getPropiedad("DefaultAcidePrj");
			texto = f.cargar(ruta);
			
		if(texto == null){
			texto = f.cargar(".//configuration/Default.acidePrj");
			almacenPropiedades.setPropiedad("DefaultAcidePrj", ".//configuration/Default.acidePrj");
			JOptionPane.showMessageDialog(null, labels.getString("s960")+ruta+labels.getString("s959"));
		}
		} catch (Exception e) {
			e.printStackTrace();
			texto = f.cargar(".//configuration/Default.acidePrj");
			JOptionPane.showMessageDialog(null, labels.getString("s960")+ruta+labels.getString("s959"));
		}
		return texto;
	}
	
	public  void loadDefault(String texto){
			
			//poner cursor de espera
			Cursor cu = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
			v.setCursor(cu);
		
			//Load project
			v.getProyecto().cargaProy(texto);
			
			//antiguo rundefaultconf
			Ventana v=Ventana.getInstance();
			if (!v.getProyecto().isExplorer())
				v.getnuevoMenu().getShowBrowserCBox().doClick();
			if (!v.getProyecto().isShell())
				v.getnuevoMenu().getShowShellWindowCBox().doClick();
			v.setSize(v.getProyecto().getWidthWindow(),v.getProyecto().getHeightWindow());
			v.setLocation(v.getProyecto().getPosx(), v.getProyecto().getPosy());
			//mig actualizar tamaño de los paneles
			v.getSplitPaneV().setDividerLocation(v.getProyecto().getWidth1());
			v.getSplitPaneH().setDividerLocation(v.getProyecto().getHeight1());
			
			v.validate();
			v.repaint();
			v.setVisible(true);
			
			//mig
			String prj=null;
			try {
				prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			boolean def = true;
			if (!(prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
				v.getnuevoMenu().habilitaMenuProyecto();
				def = false;
			}
			
			//abrir archivos
			for (int j = 0; j < v.getProyecto().dameNumFich(); j++) {
				
				if(!v.getProyecto().getfich(j).isDirectory()){
				
				FactoriaES fact=FactoriaES.getInstance();
				Fichero ff = fact.generaFichero();
				String text = null;
				text = ff.cargar(v.getProyecto().getfich(j).getPath());
				String fich = null;
				String file = v.getProyecto().getfich(j).getPath();
				if (file != null) {
					int in = file.lastIndexOf("/");
					in++;
					fich = file.substring(in, file.length());
					String ruta = file.substring(0, in);
				}
				if (def || v.getProyecto().getfich(j).isOpened()) {
					//mig
					v.getnuevoMenu().habilitaMenuArchivo();
					v.getnuevoMenu().habilitaMenuEdicion();
					
					//status
					v.getStatusBar().setMessage(v.getProyecto().getfich(j).getPath());
					//se mira el tipo
					int t = 0;
					if(v.getProyecto().getfich(j).isSetFile()) {
						t = 2;
						//status
						v.getStatusBar().setMessage(v.getProyecto().getfich(j).getPath()+" <COMPILABLE>");
					}
					if(v.getProyecto().getfich(j).isMainFile()) {
						t = 1;
						//status
						v.getStatusBar().setMessage(v.getProyecto().getfich(j).getPath()+" <MAIN>");
					}				
					
					v.getCreadorEditor().nuevaPestaña(fich, file, text, true, t);
					
					//poner marcados para proyecto vacio
					for(int i=0; i<v.getCreadorEditor().dameNumEditores(); i++){
						if(v.getCreadorEditor().dameEditorI(i).getPath().equals(v.getProyecto().getfich(j).getPath())){
							if(v.getProyecto().getfich(j).isSetFile()) v.getCreadorEditor().dameEditorI(i).setCompilerFile(true);
							if(v.getProyecto().getfich(j).isMainFile())v.getCreadorEditor().dameEditorI(i).setMainFile(true);;
						}
					}
					
					// UNDO REDO
					//v.getnuevoMenu().habilita_salvarFich();
					int numeditor = v.getCreadorEditor().getEditorSeleccionado();
					DefaultStyledDocument doc = v.getCreadorEditor().EditorSeleccionado().getDoc();
		
					doc.addUndoableEditListener(new UndoableEditListener() {
						public void undoableEditHappened(UndoableEditEvent evt) {
							Ventana v = Ventana.getInstance();
							UndoableEdit edit = evt.getEdit();
							if (!((edit instanceof DefaultDocumentEvent) &&
									(((DefaultDocumentEvent)edit).getType() == 
										DefaultDocumentEvent.EventType.CHANGE))) {
								v.getnuevoMenu().getUndo().addEdit(evt.getEdit());
							}
						}	
					});
					// CURSOR EN LA PRIMERA POSICION DEL EDITOR DE TEXTO
					numeditor = v.getCreadorEditor().getEditorSeleccionado();
					v.getCreadorEditor().dameEditorI(numeditor).getEditor().setCaretPosition(0);
				}
				}
			}
			  
			///
			
			//Load lexical
            //if(!v.getProyecto().getLenguajeConfig().contains("\\")) almacenPropiedades.setPropiedad("pathLenguaje",".//configuration/Lexical/"+v.getProyecto().getLenguajeConfig()+".xml");
            	//else 
            almacenPropiedades.setPropiedad("pathLenguaje",v.getProyecto().getLenguajeConfig());
            
            Lenguaje leng = Lenguaje.getInstance();
    		try {
    			leng.cargar(almacenPropiedades.getPropiedad("pathLenguaje"));
    		} catch (Exception e) {
    			e.printStackTrace();
    		}
    			
            //Load idioma
			String idioma = v.getProyecto().getLenguaje();
			if (idioma.equals("0")){
				v.getnuevoMenu().getEspañol().doClick();
			}else{
				v.getnuevoMenu().getEnglish().doClick();
			}
			try {
				i.seleccionIdioma(Integer.parseInt(almacenPropiedades
						.getPropiedad("idioma")));
			} catch (NumberFormatException e) {
				e.printStackTrace();
			} catch (Exception e) {
				e.printStackTrace();
			}
			
		ResourceBundle labels = i.getLabels();
		v.getStatusBar().setMessageLexical(labels.getString("s449")+" "+leng.getNombre());
		
            //Load Menu config
		
			/*boolean[] valores = null;
			String menuFile = null;
			try {
				menuFile = almacenPropiedades.getPropiedad("currentMenuCfg");
				valores = MenuConfig.cargarMenuCfgFich(menuFile);
			}
			catch (Exception e3) {
				menuFile = ".//configuration/menu/default_allOn.menuCfg";
				try {
					valores = MenuConfig.cargarMenuCfgFich(menuFile);
				} catch (Exception e) {
					e.printStackTrace();
				}
				almacenPropiedades.setPropiedad("currentMenuCfg",menuFile);
				almacenPropiedades.setPropiedad("previousMenuCfg",menuFile);
			}
				//valores = MenuConfig.cargarMenuCfgFich(almacenPropiedades.getPropiedad("currentMenuCfg"));
				MenuConfig.setAll(valores);
				v.getnuevoMenu().setMenuConfig();
				v.validate();
				v.repaint();
				v.getnuevoMenu().getSaveMenu().setEnabled(true);
				MenuGUI.setChangesSaved(true);*/
		    
		    //Load TB config
		
				/*try {
					ListaIconosEditables.cargaLista(almacenPropiedades.getPropiedad("currentTBCfg"));
					ListaIconosEditables.cargaListaAux(almacenPropiedades.getPropiedad("currentTBCfg"));
				}
				catch (Exception e4) {
					try {
						ListaIconosEditables.cargaLista(".//configuration/toolbar/default.BHcfg");
					} catch (Exception e) {
						e.printStackTrace();
					}
					try {
						ListaIconosEditables.cargaListaAux(".//configuration/toolbar/default.BHcfg");
					} catch (Exception e) {
						e.printStackTrace();
					}
					almacenPropiedades.setPropiedad("currentTBCfg",".//configuration/toolbar/default.BHcfg");
					almacenPropiedades.setPropiedad("previousTBCfg",".//configuration/toolbar/default.BHcfg");
				}
				Iconos.generaToolBarFija();				
				Iconos.generaToolBarEditable();
				v.validate();
				v.repaint();
				v.getnuevoMenu().getSaveTB().setEnabled(true);
				EdicionIconosGUI.setChangesSaved(true);*/
			
			//Load shell
				almacenPropiedades.setPropiedad("pathEjecutable", v.getProyecto().getShellDir());
				almacenPropiedades.setPropiedad("ejecutable", v.getProyecto().getShellPath());
				//almacenPropiedades.setPropiedad("echoCommand", v.getProyecto().getEchoCommand());
				//almacenPropiedades.setPropiedad("exitCommand", v.getProyecto().getExitCommand());
				v.getnuevaSalida().resetSalida();
		
				
		try {
			if (almacenPropiedades
					.getPropiedad("DefaultAcidePrj").equals(".//configuration/Default.acidePrj")) {
				v.setTitle(v.getTitle()+" - <empty>");
			} else {
			   String file= v.getProyecto().getnombreProy();
			    Fich fi=new Fich();
			    fi.setPath(file);
			    fi.setName(file);
			    fi.setPadre(null);
			    fi.setDirectory(true);
			    v.getNuevoExplorador().getAddFile().setEnabled(true);
			    v.getNuevoExplorador().getRemoveFile().setEnabled(true);
				v.setTitle(labels.getString("s425") + " - "
						+ v.getProyecto().getnombreProy());
				v.getNuevoExplorador().getRaiz().removeAllChildren();
				DefaultMutableTreeNode d = new DefaultMutableTreeNode(fi);
				v.getNuevoExplorador().getRaiz().add(d);
				ArrayList listdir=new ArrayList();
				
				for (int i=0; i<Integer.parseInt(v.getProyecto().getNumFich()); i++) {
					DefaultMutableTreeNode h=new DefaultMutableTreeNode(v.getProyecto().getfich(i));
					if (v.getProyecto().getfich(i).isDirectory()){
						//System.out.println("dir");
							h.setAllowsChildren(true); 
							listdir.add(h);
						}
						else h.setAllowsChildren(false);		
					
					if(v.getProyecto().getfich(i).getPadre().equals(v.getProyecto().getnombreProy())){
						d.add(h);
					}
					else{
						DefaultMutableTreeNode fh=search_listdir(listdir,v.getProyecto().getfich(i).getPadre());
						fh.add(h);
					}
				}
				v.getNuevoExplorador().getTreeModel().reload();
				v.getNuevoExplorador().expandTree();
				v.getNuevoExplorador().setEnabledAddFile();
				v.getNuevoExplorador().setEnabledSaveProj();
			    if(v.getProyecto().dameNumFich()>0) v.getNuevoExplorador().setEnabledRemoveFile();
			    	else v.getNuevoExplorador().getRemoveFile().setEnabled(false);
			    //mig
			    v.getProyecto().setFirstSave(true);
			    v.getProyecto().setpathProy(almacenPropiedades.getPropiedad("DefaultAcidePrj"));
			}
		} catch (NumberFormatException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		//poner cursor estandar
		cu = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
		v.setCursor(cu);
		
	}
}
