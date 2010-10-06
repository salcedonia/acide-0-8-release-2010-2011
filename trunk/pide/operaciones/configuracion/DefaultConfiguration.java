package operaciones.configuracion;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.io.File;
import java.util.ArrayList;

import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Document;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.undo.UndoableEdit;

import operaciones.fabrica.FactoriaES;
import principal.almacenPropiedades;
import es.texto.Fichero;
import gui.Ventana;

/** Class that implements Default Configuration
 * 
 *
 */
public class DefaultConfiguration {

	
	private boolean explorer;
	
	private boolean shell;
	
	private int widthWindow;
	
	private int heightWindow;
	
	//Position x window
	private int posx;
	
	//position y windows
	private int posy;
	
	private int numEditor;
	
	private ArrayList<String> editores;
	
	//paneles
	private int width1;
	private int height1;
	
	
	FactoriaES fact=FactoriaES.getInstance();
	public void loadDefaultConf(){
	 
		Fichero f=fact.generaFichero();
	    String text="";
        try{
			text = f.cargar(almacenPropiedades
					.getPropiedad("DefaultConfiguration"));
		} catch (Exception e) {
			e.printStackTrace();
		}
		int cont=0;
		int contf=0;
		   
		  //Explorer
		   contf=text.indexOf("\n",cont);
		   String cond=text.substring(cont,contf);
		   if (cond.equals("true")) explorer=true;
		   else explorer=false;
		   cont=contf+1;
		   
		  //Shell
		   contf=text.indexOf("\n",cont);
		    cond=text.substring(cont,contf);
		   if (cond.equals("true")) shell=true;
		   else shell=false;
		   cont=contf+1;	
		   
		   //Width
		   contf=text.indexOf("\n",cont);
           cond=text.substring(cont,contf);
		   widthWindow=Integer.parseInt(cond);
	       cont=contf+1;
          //Height
	       
		   contf=text.indexOf("\n",cont);
           cond=text.substring(cont,contf);
		   heightWindow=Integer.parseInt(cond);
	       cont=contf+1;
		   
	       //Position x Window
		   contf=text.indexOf("\n",cont);
           cond=text.substring(cont,contf);
		   posx=Integer.parseInt(cond);
	       cont=contf+1;
	       
          //Position y Window
		   contf=text.indexOf("\n",cont);
           cond=text.substring(cont,contf);
		   posy=Integer.parseInt(cond);
	       cont=contf+1;
		   
	       //Width1
		   contf=text.indexOf("\n",cont);
           cond=text.substring(cont,contf);
		   width1=Integer.parseInt(cond);
	       cont=contf+1;
          //Height1
		   contf=text.indexOf("\n",cont);
           cond=text.substring(cont,contf);
		   height1=Integer.parseInt(cond);
	       cont=contf+1;
	       
	       //Number of editors
		   contf=text.indexOf("\n",cont);
           cond=text.substring(cont,contf);
		   numEditor=Integer.parseInt(cond);
	       cont=contf+1;
		   //Editors
		   editores=new ArrayList<String>(numEditor);
		   for(int i=0;i<numEditor;i++){
		    contf=text.indexOf("\n",cont);
			      cond=text.substring(cont,contf);
			      cont=contf+1;
				 editores.add(cond);
				 }
	}
	public void saveDefaultConf(){
		Ventana v=Ventana.getInstance();
		String texto="";
		if (v.getnuevoMenu().getShowBrowserCBox().isSelected()) explorer=true;
		else explorer=false;
		texto=texto+Boolean.toString(explorer)+"\n";
		if (v.getnuevoMenu().getShowShellWindowCBox().isSelected()) shell=true;
		else shell=false;
		texto=texto+Boolean.toString(shell)+"\n";
		texto=texto+Integer.toString(v.getWidth())+"\n";
		texto=texto+Integer.toString(v.getHeight())+"\n";
		texto=texto+Integer.toString(v.getX())+"\n";
		texto=texto+Integer.toString(v.getY())+"\n";
		//mig
		texto=texto+Integer.toString(v.getNuevoExplorador().getWidth())+"\n"; //width1
		texto=texto+Integer.toString(v.getnuevaSalida().getHeight())+"\n"; //height1
		//texto=texto+Integer.toString(v.getCreadorEditor().getPane().getWidth())+"\n"; //width1
		//texto=texto+Integer.toString(v.getCreadorEditor().getPane().getHeight())+"\n"; //height1
		//texto=texto+Integer.toString(v.getCreadorEditor().dameNumEditores())+"\n";
		Fichero f=fact.generaFichero();
		
		String prj=null;
		try {
			prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		/*if (!(prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
			//si hay proyecto
			texto=texto+Integer.toString(v.getProyecto().dameNumFich())+"\n";
			for (int i = 0; i < v.getProyecto().dameNumFich(); i++){
				//texto=texto+v.getCreadorEditor().dameEditorI(i).getPath()+"\n";
				if(v.getProyecto().getfich(i).isOpened())
					texto=texto+v.getProyecto().getfich(i).getPath()+"\n";
			}
		}
		else{*/
		//si no hay proyecto
		texto=texto+Integer.toString(v.getCreadorEditor().dameNumEditores())+"\n";
		for (int i = 0; i < v.getCreadorEditor().dameNumEditores(); i++){
			texto=texto+v.getCreadorEditor().dameEditorI(i).getPath()+"\n";
		}
		//}
		f.salvar(".//configuration/Defaultconf.acide", texto);
		
}

public void runDefaultConf(){
	Ventana v=Ventana.getInstance();
	if (!explorer)
		v.getnuevoMenu().getShowBrowserCBox().doClick();
	
	if (!shell)
		v.getnuevoMenu().getShowShellWindowCBox().doClick();
	
	v.setSize(widthWindow,heightWindow);
	v.setLocation(posx, posy);
	//mig actualizar tamaño de los paneles

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
	if (!(prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy()==null)){
		v.getnuevoMenu().habilitaMenuProyecto();
	}
	for (int j = 0; j < numEditor; j++) {
		//mig
		v.getnuevoMenu().habilitaMenuArchivo();
		v.getnuevoMenu().habilitaMenuEdicion();
		Fichero f=fact.generaFichero();
		String texto = null;
		texto = f.cargar(editores.get(j));
		String fich = null;
		String file= editores.get(j);
		if (file != null) {
			int in = file.lastIndexOf("/");
			in++;
			fich = file.substring(in, file.length());
			String ruta = file.substring(0, in);
		}
		
		//se mira el tipo
		int t = 0;
		
		v.getCreadorEditor().nuevaPestaña(fich, file, texto, true, t);
		
//		 UNDO REDO
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
		// CURSOS EN LA PRIMERA POSICION DEL EDITOR DE TEXTO
		numeditor = v.getCreadorEditor().getEditorSeleccionado();
		v.getCreadorEditor().dameEditorI(numeditor).getEditor()
				.setCaretPosition(0);

	}
}
public int getHeightWindow() {
	return heightWindow;
}
public void setHeightWindow(int heightWindow) {
	this.heightWindow = heightWindow;
}
public int getNumEditor() {
	return numEditor;
}
public void setNumEditor(int numEditor) {
	this.numEditor = numEditor;
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
public int getWidthWindow() {
	return widthWindow;
}
public void setWidthWindow(int widthWindow) {
	this.widthWindow = widthWindow;
}
}