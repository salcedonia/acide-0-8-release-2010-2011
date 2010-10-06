package operaciones.genericas;

import gui.Ventana;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

public class Key2 extends KeyAdapter {

	public void keyPressed(KeyEvent evt) {
		
		Ventana v=Ventana.getInstance();
		
		v.getnuevoMenu().getSalvarFich().setEnabled(true);
		v.getnuevoMenu().getSave().setEnabled(true);
		v.getnuevoMenu().getSaveAll().setEnabled(true);
		
		v.getnuevoMenu().getDeshacer().setEnabled(true);
		v.getnuevoMenu().getRepetir().setEnabled(true);
		v.getnuevoMenu().getCopiar().setEnabled(true);
		v.getnuevoMenu().getPegar().setEnabled(true);
		v.getnuevoMenu().getCortar().setEnabled(true);
		
		v.getnuevoMenu().getGuardarProyecto().setEnabled(true);
		//v.getnuevoMenu().getSaveAsProject().setEnabled(true);
		v.getnuevoMenu().getRemoveFile().setEnabled(true);
		v.getnuevoMenu().getDeleteFile().setEnabled(true);
		v.getnuevoMenu().getRemoveFolder().setEnabled(true);
		v.getnuevoMenu().getSetMain().setEnabled(true);
		v.getnuevoMenu().getUnsetMain().setEnabled(true);
		v.getnuevoMenu().getSetCompilable().setEnabled(true);
		v.getnuevoMenu().getUnsetCompilable().setEnabled(true);
	}
	
}
