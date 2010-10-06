package operaciones.genericas;

import gui.Ventana;
import gui.menus.SearchGUI;
import gui.menus.ReplaceGUI;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JTextField;

public class Mouse implements MouseListener {

   private static String c;
   
   private static boolean opc;
	
   public void mouseClicked(MouseEvent arg0) {
	   SearchGUI bu = SearchGUI.getInstance();
		ReplaceGUI re = ReplaceGUI.getInstance();
		// BUSCAR
		bu.setPosActual(-2);
		bu.setCiclo(false);
		bu.setFin(false);
		bu.getB().setPosTemp(-2);
		bu.getB().setCiclo(false);
		bu.setCiclo(false);
		bu.setTextSelec(null);
		SearchGUI.setPrimera(true);
		// REEMPLAZAR
		re.setPosActual(-2);
		re.setCiclo(false);
		re.setFin(false);
		re.getB().setPosTemp(-2);
		re.getB().setCiclo(false);
		re.setCiclo(false);
		re.setTextSelec(null);
		ReplaceGUI.setPrimera(true);
		ReplaceGUI.setPrimerReem(true);
    	
	}

	public void mousePressed(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	public void mouseReleased(MouseEvent arg0) {
		// TODO Auto-generated method stub
		SearchGUI bu = SearchGUI.getInstance();
		ReplaceGUI re = ReplaceGUI.getInstance();
	Ventana v=Ventana.getInstance();
	String t=null;
	int numeditor;
	numeditor = v.getCreadorEditor().getEditorSeleccionado();
	t=v.getCreadorEditor().dameEditorI(numeditor).getEditor().getSelectedText();
	if (t!=null){
		bu.getSeleccionado().setSelected(true);
		bu.getRadioTodo().setEnabled(false);
	}
	else{
		bu.getDocActual().setSelected(true);
		bu.getRadioTodo().setEnabled(true);
	}
	if (t!=null){
		re.getSeleccionado().setSelected(true);
		re.getRadioTodo().setEnabled(false);
	}
	else{
		re.getDocActual().setSelected(true);
		re.getRadioTodo().setEnabled(true);
	}	
//	 BUSCAR
	bu.setPosActual(-2);
	bu.setCiclo(false);
	bu.setFin(false);
	bu.getB().setPosTemp(-2);
	bu.getB().setCiclo(false);
	bu.setCiclo(false);
	SearchGUI.setPrimera(true);
	bu.setTextSelec(null);
	// REEMPLAZAR
	re.setPosActual(-2);
	re.setCiclo(false);
	re.setFin(false);
	re.getB().setPosTemp(-2);
	re.getB().setCiclo(false);
	re.setCiclo(false);
	re.setTextSelec(null);
	ReplaceGUI.setPrimera(true);	
	ReplaceGUI.setPrimerReem(true);
	}

	public void mouseEntered(MouseEvent arg0) {
		 
	}

	public void mouseExited(MouseEvent arg0) {
		// TODO Auto-generated method stub

	}


}
