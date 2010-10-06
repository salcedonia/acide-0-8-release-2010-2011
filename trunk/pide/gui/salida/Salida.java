package gui.salida;

import gui.Ventana;

import idioma.Idioma;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.ResourceBundle;
import operaciones.genericas.*;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.JTextComponent;
import operaciones.log.Log;
import operaciones.salida.ProcessThread;
import org.apache.log4j.Logger;
import principal.almacenPropiedades;

/**
 * 
 */
public class Salida extends JPanel {

	/**
	 * Atributo: serialVersionUID: clase serializable
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	/**
	 * Atributo: salida: Establece una interfaz de salida
	 * 
	 */
	private JTextComponent salida;
	
	private int maxPos;
	
	private ProcessThread p;
	
	private String echoCommand;
	
	private int longCom; 
	
	//mig
	private JPopupMenu jpopup;
	
	private JMenuItem copy;
	
	private JMenuItem cut;
	
	private JMenuItem paste;
	
	private ArrayList<String> hist;
	
	private int index;
	
	private int maxIndex;
	
	private DefaultStyledDocument text; 
	/**
	 * Instancia una salida
	 * 
	 */
	public Salida(boolean editable) {
		super();
		ResourceBundle labels = Idioma.getInstance().getLabels();
		try {
			logger.info(labels.getString("s422"));
			salida = creaSalida();
			salida.setEditable(editable);
			/*salida.addCaretListener(new CaretListener() {
		        public void caretUpdate(CaretEvent e) {
		            if (salida.getCaretPosition()<salida.getText().length()-longCom){
		        		salida.setCaretPosition(salida.getText().length());
		        	}
		        }
		    });*/
			echoCommand = "";
			maxPos = 0;
			longCom = 0;
			salida.addKeyListener(new EnterKey());
			//mig
			salida.addKeyListener(new Key2());
			salida.addMouseListener(new MouseAdapter(){
				public void mouseReleased(MouseEvent arg0) {
					String s = salida.getSelectedText();
					boolean b = s == null;
					if(salida.getCaretPosition() < maxPos && b) salida.setCaretPosition(maxPos);
				}
			});
			Font fuente=new Font("Monospaced", Font.PLAIN, 12);
			salida.setFont(fuente);
			setLayout(new BorderLayout());
			JScrollPane pScroll = new JScrollPane(salida);
			add(pScroll);
			p = new ProcessThread();
			logger.info(labels.getString("s423"));
			//mig
			jpopup=new JPopupMenu();
		} catch (Exception e) {
			logger.info(labels.getString("s424"));
			e.printStackTrace();
		}
		//mig
		hist = new ArrayList<String>();
		index = 0;
		maxIndex = 0;
		salida.addMouseListener(new MouseAdapter(){
			
			public void mousePressed(MouseEvent arg0) {
	     		if(arg0.isPopupTrigger()) {	
			        jpopup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
				}
			}
			public void mouseReleased(MouseEvent arg0) {	
				if (arg0.isPopupTrigger()) {
					
					if(salida.getSelectedText() == null){
						copy.setEnabled(false);
						cut.setEnabled(false);
					}else{
						copy.setEnabled(true);
						if(salida.getSelectionStart() < maxPos) cut.setEnabled(false);
		     				else{cut.setEnabled(true);}
					}
					if(salida.getSelectionStart() < maxPos) paste.setEnabled(false);
		     			else paste.setEnabled(true);
		     		
			        jpopup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
				}
			}
		}		
		);
		copy = new JMenuItem(labels.getString("s187"));
		copy.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				salida.copy();
			}
		});
		jpopup.add(copy);
		cut = new JMenuItem(labels.getString("s188"));
		cut.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				if(salida.getSelectionStart() >= maxPos) salida.cut();
			}
		});
		jpopup.add(cut);
		paste = new JMenuItem(labels.getString("s189"));
		paste.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				if(salida.getSelectionStart() >= maxPos) salida.paste();
			}
		});
		jpopup.add(paste);
	}

	/**
	 * Clase que devuelve un area de texto
	 * 
	 * @return
	 */
	protected JTextComponent creaSalida() {
		
		text = new DefaultStyledDocument(); 
		JTextPane areaTexto = new JTextPane(text) {

			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			public void setSize(Dimension d) {
				if (d.width < getParent().getSize().width)
					d.width = getParent().getSize().width;

				super.setSize(d);
			}

			public boolean getScrollableTracksViewportWidth() {
				return false;
			}
		};
		return areaTexto;

	}

	
	/**
	 * Obtiene el interfaz Salida
	 * 
	 * @return La salida
	 */
	public JTextComponent getSalida() {
		return salida;
	}

	/**
	 * Carga un texto que se le pasa como parametro en la salida
	 * 
	 * @param texto
	 */
	public void cargaTexto(String texto) {
	//	salida.setText(texto);
	}
	
	public void añadeTexto(String texto){
		try {
			if (Boolean.parseBoolean(almacenPropiedades.getPropiedad("echoCommand"))&&
					(echoCommand.length() <= texto.length()) ){
				if (texto.substring(0,echoCommand.length()).equals(echoCommand)){
					texto = texto.substring(echoCommand.length(),texto.length());
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		String textoAux = salida.getText();
		textoAux = textoAux + texto;
		salida.setText(textoAux);
		
		/*try {
			text.insertString(text.getLength(),texto, null);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		maxPos = text.getLength();
		salida.setCaretPosition(text.getLength());*/
		
		//salida.getText().concat(texto);
		maxPos = salida.getText().length();
		salida.setCaretPosition(salida.getText().length());
	}

	/**
	 * Devuelve en un String el contenido de la salida
	 * 
	 * @return
	 */
	public String getTexto() {
		return salida.getText();
	}

	/**
	 * Ejecuta el comand
	 */
	public void ejecutaCMD() {
		String args[] = null;
		p.main(args);
	}
	
	public void resetSalida(){
		salida.setText("");
		ejecutaCMD();
	}
	
	public void ejecutaComandoSalida(){
		try {
			String s = almacenPropiedades.getPropiedad("exitCommand");
			echoCommand = s;
			if(ProcessThread.getWriter()!=null){
				ProcessThread.getWriter().write(s + '\n');
				ProcessThread.getWriter().flush();}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	//ejecutar comando de la barra de herramientas
	public void ejecutaComando(String com){
		try {
			if (Ventana.getInstance().getCreadorEditor().dameNumEditores()>0){
				com = com.replace("$activeFile$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getPath());
				//mig
				com = com.replace("$activeFilePath$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFilePath());
				com = com.replace("$activeFileExt$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFileExt());
				com = com.replace("$activeFileName$",Ventana.getInstance().getCreadorEditor().EditorSeleccionado().getFileName());
			}
			
			//mig
			Ventana v=Ventana.getInstance();
			String prj=null;
			try {
				prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			if ((prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
				//no hay proyecto
				if(Ventana.getInstance().getCreadorEditor().EditorPrincipal()!=null){
					com = com.replace("$mainFile$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getPath());
					com = com.replace("$mainFilePath$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFilePath());
					com = com.replace("$mainFileExt$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFileExt());
					com = com.replace("$mainFileName$",Ventana.getInstance().getCreadorEditor().EditorPrincipal().getFileName());
				}
				else{
					//no main file
				}
			}
			else{//hay proyecto
				int j=-1;
				for (int i=0;i<v.getProyecto().dameNumFich();i++){
					if(v.getProyecto().getfich(i).isMainFile()) j=i;
				}
				if(j!=-1){
					com = com.replace("$mainFile$",v.getProyecto().getfich(j).getPath());
					com = com.replace("$mainFilePath$",v.getProyecto().getfich(j).getFilePath());
					com = com.replace("$mainFileExt$",v.getProyecto().getfich(j).getFileExt());
					com = com.replace("$mainFileName$",v.getProyecto().getfich(j).getFileName());
				}
				else{
					//no main file
				}
			}
			//System.out.println(com);
			
			añadeTexto(com + '\n');
			echoCommand = com;
			ProcessThread.getWriter().write(com + '\n');
			ProcessThread.getWriter().flush();
			
			//Process p = Runtime.getRuntime().exec (com + '\n');
			//InputStream is = p.getInputStream();
            
			//BufferedReader br = new BufferedReader (new InputStreamReader (is));
			
			//System.out.println("m"+br.readLine());
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public int getMaxPos() {
		return maxPos;
	}
	
	class EnterKey implements KeyListener {

		public void keyTyped(KeyEvent arg0) {
			if (salida.getCaretPosition() < maxPos){
					arg0.consume();
			}
			else{
				longCom++;
			}
		}
		int y=0;
		public void keyPressed(KeyEvent arg0) {
			if ((salida.getCaretPosition() == maxPos) && ((arg0.getKeyCode() == KeyEvent.VK_LEFT) || (arg0.getKeyCode() == 8)||(arg0.getKeyCode() == KeyEvent.VK_UP))){
					arg0.consume();
			}	
			if (salida.getSelectionStart() < maxPos){
					arg0.consume();
			}
			
			if(arg0.getKeyChar() == '\n'){
				try {
					//arg0.consume();
					longCom = 0;
					//String s = (String) salida.getText().subSequence(maxPos,salida.getText().length()); 
					String s = (String) salida.getText().subSequence(maxPos,text.getLength());
					echoCommand = s;
					ProcessThread.getWriter().write(s + arg0.getKeyChar());
					ProcessThread.getWriter().flush();
					
					//Process p = Runtime.getRuntime().exec (s + arg0.getKeyChar());
		            //String line;
					//BufferedReader br = new BufferedReader (new InputStreamReader (p.getInputStream()));
					
					
					maxPos = maxPos + s.length();
					salida.setCaretPosition(maxPos);
					if(hist.contains(s)){
						maxIndex--;
						hist.remove(s);
					}
					hist.add(s);
					maxIndex++;
					index = maxIndex;
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			//mig
			if(arg0.getKeyCode() == KeyEvent.VK_UP){
				arg0.consume();
				//historial
				if(index > -1){
					if(index == 0){index = maxIndex-1;}
					//if(index != maxIndex){index--;}
						else{index--;}
					String s = (String)salida.getText().subSequence(maxPos,salida.getText().length());
					try {
						text.remove(maxPos, s.length());
						if(index>-1)
							text.insertString(maxPos,hist.get(index), null);
					} catch (BadLocationException e) {
						e.printStackTrace();
					}
				}
			}
			if(arg0.getKeyCode() == KeyEvent.VK_DOWN){
				//historial
				if(index > -1){
					if(index >= maxIndex-1){index = 0;}
					else{index++;}
					String s = (String)salida.getText().subSequence(maxPos,salida.getText().length());
					try {
						text.remove(maxPos, s.length());
						text.insertString(maxPos,hist.get(index), null);
					} catch (BadLocationException e) {
						e.printStackTrace();
					}
				}
			}
			if(arg0.getKeyCode() == KeyEvent.VK_ESCAPE){
				String s = (String)salida.getText().subSequence(maxPos,salida.getText().length());
				try {
					text.remove(maxPos, s.length());
				} catch (BadLocationException e) {
					e.printStackTrace();
				}
			}
			if(arg0.getKeyCode() == KeyEvent.VK_HOME){
				arg0.consume();
				salida.setCaretPosition(maxPos);
			}
			if(arg0.getKeyCode() == KeyEvent.VK_END){
				arg0.consume();
				String s = (String)salida.getText().subSequence(maxPos,salida.getText().length());
				salida.setCaretPosition(maxPos + s.length());
			}
			if(arg0.isControlDown() && arg0.getKeyCode() == KeyEvent.VK_C){
				salida.copy();
				/*try {
					ProcessThread.getWriter().write(arg0.getKeyCode());
					ProcessThread.getWriter().flush();
				} catch (IOException e) {
					e.printStackTrace();
				}*/
			}
		}

		public void keyReleased(KeyEvent arg0) {	
			if (salida.getCaretPosition() < maxPos){
					arg0.consume();
			}
		}
		
	}

}
