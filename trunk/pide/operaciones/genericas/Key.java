package operaciones.genericas;

import gui.Ventana;
import gui.menus.SearchGUI;
import gui.menus.ReplaceGUI;

import idioma.Idioma;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.Locale;

import javax.swing.KeyStroke;

import operaciones.genericas.Mouse;
/**
 * Clase Listener que corresponden a los oyentes de teclado y Mouse (Genéricos)
 * 
 * 
 */
public class Key extends KeyAdapter {

	public void keyPressed(KeyEvent evt) {
		
		SearchGUI bu = SearchGUI.getInstance();
		ReplaceGUI re = ReplaceGUI.getInstance();
		Ventana v = Ventana.getInstance();
		Idioma i = Idioma.getInstance();
		Toolkit toolkit=Toolkit.getDefaultToolkit();
		String t=null;
		int numeditor;
		numeditor = v.getCreadorEditor().getEditorSeleccionado();
		
		if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
			//System.out.println("esc");
			if (bu.isFocused()==true)
				bu.dispose();
			if (re.isFocused()==true)
				re.dispose();
		}
		
		if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
			//System.out.println("enter");
			if (bu.isFocused()==true)
			   bu.getBuscar().doClick();
			if (re.isFocused()==true) 
			   re.getBuscar().doClick();
		}

		if (evt.getKeyCode() == KeyEvent.VK_F3) {
			//System.out.println("f3");
			t=v.getCreadorEditor().dameEditorI(numeditor).getEditor().getSelectedText();
			    if (t!=null){
			    	bu.setT1(t);
			    	bu.setDocActual(true);
			    	bu.setRadioAdelante(true);
			    	bu.getBuscar().doClick();
			    }
		}
		 
		// BUSCAR
		bu.setPosActual(-2);
		bu.setCiclo(false);
		bu.setFin(false);
		bu.getB().setPosTemp(-2);
		bu.getB().setCiclo(false);
		bu.setCiclo(false);
		bu.setTextSelec(null);
		SearchGUI.setPrimera(true);
		if (t!=null){
			bu.getSeleccionado().setSelected(true);
			bu.getRadioTodo().setEnabled(false);
		}
		else{
			bu.getDocActual().setSelected(true);
			bu.getRadioTodo().setEnabled(true);
		}
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
		if (t!=null){
			re.getSeleccionado().setSelected(true);
			re.getRadioTodo().setEnabled(false);
		}
		else{
			re.getDocActual().setSelected(false);
			re.getRadioTodo().setEnabled(true);
		}
			
		if (((i.getCurrentLocale().equals(new Locale("en","EN")))&&(evt.isControlDown() && evt.getKeyCode()==KeyEvent.VK_F))||
				((i.getCurrentLocale().equals(new Locale("es","ES")))&&(evt.isControlDown() && evt.getKeyCode()==KeyEvent.VK_B))) {
			//System.out.println("buscar");
			t=v.getCreadorEditor().dameEditorI(numeditor).getEditor().getSelectedText();
			if (bu.isVisible()) bu.dispose();
			if (t!=null){
				//	BUSCAR
				bu.getSeleccionado().setSelected(true);
				bu.getRadioTodo().setEnabled(false);
				bu.setT1("");
			
			} 
			else{
				bu.getDocActual().setSelected(false);
				bu.getRadioTodo().setEnabled(true);
				bu.setT1("");
		
			}
		}
		
		if (((i.getCurrentLocale().equals(new Locale("en","EN")))&&(evt.isControlDown() && evt.getKeyCode()==KeyEvent.VK_L))||
				((i.getCurrentLocale().equals(new Locale("es","ES")))&&(evt.isControlDown() && evt.getKeyCode()==KeyEvent.VK_R))){
			//System.out.println("reemplazar");
            if (re.isVisible()) re.dispose();
			t=null;
			 numeditor = v.getCreadorEditor().getEditorSeleccionado();
			      t=v.getCreadorEditor().dameEditorI(numeditor).getEditor().getSelectedText();
			if (t!=null){
				// REEMPLAZAR
				re.getSeleccionado().setSelected(true);
				re.getRadioTodo().setEnabled(false);
				re.setT1("");
			} else{
				// REEMPLAZAR
				re.getDocActual().setSelected(true);
				re.getRadioTodo().setEnabled(true);
				re.setT1("");
			}
		
		}
		
		if (evt.getKeyCode()==KeyEvent.VK_CAPS_LOCK){
			boolean state = toolkit.getLockingKeyState(evt.getKeyCode());
			if(state)
				v.getStatusBar().setCapsLock("CAPS");
			else
				v.getStatusBar().setCapsLock("");
			
			v.validate();
			v.repaint();	
	
		}
		
		if (evt.getKeyCode()==KeyEvent.VK_NUM_LOCK){
	  	  	boolean state = toolkit.getLockingKeyState(evt.getKeyCode());
	  	  	if(state)
	  	  		v.getStatusBar().setNumLock("NUM");
	  	  	else
	  	  		v.getStatusBar().setNumLock("");

	  	  	v.validate();
	  	  	v.repaint();	

		}
		
		if (evt.getKeyCode()==KeyEvent.VK_SCROLL_LOCK){
			boolean state = toolkit.getLockingKeyState(evt.getKeyCode());
			if(state)
				v.getStatusBar().setScrollLock("SCROLL");
			else
				v.getStatusBar().setScrollLock("");

			v.validate();
			v.repaint();
			
		}
		
	}
}