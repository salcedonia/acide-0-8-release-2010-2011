package operaciones.genericas;

import gui.Ventana;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/**Clase que implementa Listener de Windows
 * 
 *
 */
public class Windows extends WindowAdapter {



	public void windowClosing(WindowEvent arg0) {
		// TODO Auto-generated method stub
    Ventana v=Ventana.getInstance();
   v.setEnabled(true);
   v.setAlwaysOnTop(true);
   v.setAlwaysOnTop(false);
	}

	public void windowClosed(WindowEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	
  
	
}
