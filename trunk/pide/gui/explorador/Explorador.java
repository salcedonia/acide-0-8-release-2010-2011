package gui.explorador;

import idioma.Idioma;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.Document;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;
import javax.swing.tree.*;
import javax.swing.undo.UndoableEdit;

import operaciones.configuracion.Fich;
import operaciones.genericas.Key;
import operaciones.genericas.Key2;

import operaciones.log.Log;
import org.apache.log4j.Logger;

import principal.almacenPropiedades;

import es.texto.ExtensionesValidas;
import es.texto.Fichero;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.*;
import java.util.ResourceBundle;

import gui.Ventana;
import gui.editor.CreadorEditor;
import java.awt.event.KeyListener;
/**
 * Clase que implementa el explorador de directorios
 * 
 * 
 */
public class Explorador extends JPanel {

	/**
	 * Atributo: serialVersionUID: clase serializable
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Atributo: arbol: Representa la estructura de directorios desde el nodo
	 * raiz
	 * 
	 */
	private JTree arbol;

	/**
	 * Atributo: raiz: Nodo raiz del arbol de directorios
	 * 
	 */
	private DefaultMutableTreeNode raiz;

	/**
	 * Atributo:
	 */
	private DefaultTreeModel treeModel;

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	/**
	 * Atributo
	 * 
	 */
	private JPopupMenu popup;

	/**
	 * Atributo
	 * 
	 */
	private JMenuItem saveProj;

	/**
	 * Atributo
	 * 
	 */
	private JMenuItem addFile;

	/**
	 * Atributo
	 * 
	 */
	private JMenuItem removeFile;
	
	private JMenuItem newFile;

	private JMenuItem addFolder;
   
	private JMenuItem removeFolder;

	private Ventana v;
  
	private int tam_explorer;

	//mig
	private JMenuItem deleteFile;
	
	private JMenuItem setMain;
	
	private JMenuItem setCompilable;
	
	private JMenuItem unsetMain;
	
	private JMenuItem unsetCompilable;
	//
	
	public int getTam_explorer() {
	return tam_explorer;
}

public void setTam_explorer(int tam_explorer) {
	this.tam_explorer = tam_explorer;
}

	public Explorador() {
		final ResourceBundle labels = Idioma.getInstance().getLabels();
		try {
			logger.info(labels.getString("s324"));
            init_Popup();
			raiz = new DefaultMutableTreeNode("raiz");
			treeModel = new DefaultTreeModel(raiz);
			treeModel.setAsksAllowsChildren(true);
			//generaArbol(raiz, new File("./"));
			setLayout(new BorderLayout());
			arbol = new JTree(treeModel);
			arbol.setRootVisible(false);
			arbol.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
            arbol.setShowsRootHandles(true);
            arbol.setCellRenderer(new ExtendedTreeCellRenderer());
            //arbol.getCellRenderer().getTreeCellRendererComponent(arbol, null, false, false, false, 0, false);
			
            dobleClick d = new dobleClick();
			EnterAction e = new EnterAction();
			arbol.addMouseListener(d);
			arbol.addKeyListener(e);
			arbol.addKeyListener(new Key());
			arbol.addKeyListener(new Key2());
			try {
				Cursor cu = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
				arbol.setCursor(cu);
			  
		} catch (HeadlessException e1) {
			e1.printStackTrace();
		} 
		} catch (Exception e) {
			logger.info(labels.getString("s325"));
			e.printStackTrace();
		}
		logger.info(labels.getString("s326"));
		this.add(new JScrollPane((JTree) arbol), "Center");
	    
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.Component#getPreferredSize()
	 */
	public Dimension getPreferredSize() {
		return new Dimension(200, 120);
	}

public void init_Popup(){
	final ResourceBundle labels = Idioma.getInstance().getLabels();
			logger.info(labels.getString("s324"));
		popup = new JPopupMenu();
        JMenuItem newProject = new JMenuItem(labels.getString("s14"));
		newProject.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent arg0) {
				Ventana v = Ventana.getInstance();
				v.getnuevoMenu().getNuevoProyecto().doClick();
			}

		});
		popup.add(newProject);

		JMenuItem openProj = new JMenuItem(labels.getString("s15"));
		openProj.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				Ventana v = Ventana.getInstance();
				v.getnuevoMenu().getAbrirProyecto().doClick();
			}
		});
		popup.add(openProj);
		saveProj = new JMenuItem(labels.getString("s16"));
		saveProj.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				Ventana v = Ventana.getInstance();
				v.getnuevoMenu().getGuardarProyecto().setEnabled(true);
				v.getnuevoMenu().getGuardarProyecto().doClick();
				//v.getnuevoMenu().getGuardarProyecto().setEnabled(false);
			}
		});
		//saveProj.setEnabled(false);
		popup.add(saveProj);
		popup.addSeparator();
		
		//mig//////////////////////////////////////////////////////////////////////////////////
		newFile = new JMenuItem(labels.getString("s947"));
		newFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
               Ventana v=Ventana.getInstance();
               v.getnuevoMenu().getNewProjectFile().setEnabled(true);
               v.getnuevoMenu().getNewProjectFile().doClick();
               //v.getnuevoMenu().getNewProjectFile().setEnabled(false);
		}
		});
		popup.add(newFile);
		///////////////////////////////////////////////////////////////////////////////////////
		
		addFile = new JMenuItem(labels.getString("s17"));
		addFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				Ventana v = Ventana.getInstance();
				v.getnuevoMenu().getAnadirFichero().setEnabled(true);
				v.getnuevoMenu().getAnadirFichero().doClick();
				//v.getnuevoMenu().getAnadirFichero().setEnabled(false);
			}
		});
		//addFile.setEnabled(false);
		popup.add(addFile);

		removeFile = new JMenuItem(labels.getString("s618"));
		removeFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
               Ventana v=Ventana.getInstance();
               v.getnuevoMenu().getRemoveFile().setEnabled(true);
               v.getnuevoMenu().getRemoveFile().doClick();
               //v.getnuevoMenu().getRemoveFile().setEnabled(false);
		}
		});
		popup.add(removeFile);
		
		//mig
		deleteFile = new JMenuItem(labels.getString("s950"));
		deleteFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
               Ventana v=Ventana.getInstance();
               v.getnuevoMenu().getDeleteFile().setEnabled(true);
               v.getnuevoMenu().getDeleteFile().doClick();
               //v.getnuevoMenu().getDeleteFile().setEnabled(false);
		}
		});
		popup.add(deleteFile);
		
		popup.addSeparator();
		setCompilable = new JMenuItem(labels.getString("s254"));
		setCompilable.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
               Ventana v=Ventana.getInstance();
               v.getnuevoMenu().getSetCompilable().setEnabled(true);
               v.getnuevoMenu().getSetCompilable().doClick();
               //v.getnuevoMenu().getSetCompilable().setEnabled(false);
		}
		});
		popup.add(setCompilable);
		
		unsetCompilable = new JMenuItem(labels.getString("s255"));
		unsetCompilable.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
               Ventana v=Ventana.getInstance();
               v.getnuevoMenu().getUnsetCompilable().setEnabled(true);
               v.getnuevoMenu().getUnsetCompilable().doClick();
               //v.getnuevoMenu().getUnsetCompilable().setEnabled(false);
		}
		});
		popup.add(unsetCompilable);
		
		setMain = new JMenuItem(labels.getString("s256"));
		setMain.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
               Ventana v=Ventana.getInstance();
               v.getnuevoMenu().getSetMain().setEnabled(true);
               v.getnuevoMenu().getSetMain().doClick();
               //v.getnuevoMenu().getSetMain().setEnabled(false);
		}
		});
		popup.add(setMain);
		
		unsetMain = new JMenuItem(labels.getString("s952"));
		unsetMain.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
               Ventana v=Ventana.getInstance();
               v.getnuevoMenu().getUnsetMain().setEnabled(true);
               v.getnuevoMenu().getUnsetMain().doClick();
               //v.getnuevoMenu().getUnsetMain().setEnabled(false);
		}
		});
		popup.add(unsetMain);
		//
		
		popup.addSeparator();
		removeFolder = new JMenuItem(labels.getString("s220"));
		removeFolder.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
               Ventana v=Ventana.getInstance();
               v.getnuevoMenu().getRemoveFolder().setEnabled(true);
               v.getnuevoMenu().getRemoveFolder().doClick();
               //v.getnuevoMenu().getRemoveFolder().setEnabled(false);
		}
		});
		
		addFolder = new JMenuItem(labels.getString("s219"));
		addFolder.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
               Ventana v=Ventana.getInstance();
               v.getnuevoMenu().getAddFolder().setEnabled(true);
               v.getnuevoMenu().getAddFolder().doClick();
               //v.getnuevoMenu().getAddFolder().setEnabled(false);
		}
		});		
		popup.add(addFolder);
		popup.add(removeFolder);
				
		
}
	/**
	 * Clase que genera todo el arbol de directorios
	 * 
	 * @param nodo
	 * @param f
	 */
	public void generaArbol(DefaultMutableTreeNode nodo, File f) {
		ExtensionesValidas ev = ExtensionesValidas.getInstance();

		if (!f.isDirectory()) {
			if (ev.extensionValida(f.getName())) {
				// System.out.println("ARCHIVO - " + f.getName());
				DefaultMutableTreeNode hijo = new DefaultMutableTreeNode(f);
				nodo.add(hijo);
			}
		} else {
			// System.out.println("DIRECTORIO - " + f.getName());
			DefaultMutableTreeNode hijo = new DefaultMutableTreeNode(f);
			nodo.add(hijo);
			File fList[] = f.listFiles();
			for (int i = 0; i < fList.length; i++)
				generaArbol(hijo, fList[i]);
		}
	}

	/**
	 * Implementamos la tipica accion del doble click sobre un icono para abrir
	 * un archivo
	 * 
	 */
	class dobleClick implements MouseListener {
		public void mouseClicked(MouseEvent evt) {
			Ventana v=Ventana.getInstance();
			TreePath path = arbol.getPathForLocation(evt.getX(), evt.getY());
			String filePath;
	   if (path != null) {
		   v.getStatusBar().setMessage(path.getLastPathComponent().toString());
		   //status
		   DefaultMutableTreeNode d =  (DefaultMutableTreeNode) path.getLastPathComponent();
		   Object nodo=d.getUserObject();
		   Fich fc= (Fich) nodo;
		   fc.getPath();
		   
		   for (int i=0;i<v.getProyecto().dameNumFich();i++){
			   
			   if(v.getProyecto().getfich(i).getPath().equals(fc.getPath()))
				 if(!v.getProyecto().getfich(i).isDirectory()){
				   if(v.getProyecto().getfich(i).isSetFile())
						if(v.getProyecto().getfich(i).isMainFile()) v.getStatusBar().setMessage(v.getProyecto().getfich(i).getName()+" <MAIN>");
						else v.getStatusBar().setMessage(v.getProyecto().getfich(i).getName()+" <COMPILABLE>");
					else v.getStatusBar().setMessage(v.getProyecto().getfich(i).getName());
			   }
			   else {
				   filePath = path.getLastPathComponent().toString();
				   v.getStatusBar().setMessage(filePath);
			   }
		   }
		   //
		   
		   for(int i=0; i<v.getCreadorEditor().dameNumEditores(); i++){
			   if(v.getCreadorEditor().dameEditorI(i).getPath().equals(fc.getPath())){
				   v.getCreadorEditor().setEditorSeleccionado(i);
			   }
		   }
		   
		//filePath = path.getLastPathComponent().toString();
		  //v.getStatusBar().setMessage(filePath);		
			v.validate();
			v.repaint();
   		}
			if (evt.getClickCount() > 1) {
				path = arbol
						.getPathForLocation(evt.getX(), evt.getY());
				
				if (path != null) {
					DefaultMutableTreeNode d =  (DefaultMutableTreeNode) path.getLastPathComponent();
					Object nodo=d.getUserObject();
					Fich fc= (Fich) nodo;
					
					boolean b = false;
					int s = -1;
					for(int j=0; j<v.getCreadorEditor().dameNumEditores(); j++){
						if(v.getCreadorEditor().dameEditorI(j).getPath().equals(fc.getPath())){
							b = true;
							s = j;
						}
					}
					if(!b){//si no esta abierto ya
					if (!fc.isDirectory()) {
						Fichero fd = new Fichero();
						CreadorEditor ce = v.getCreadorEditor();
						String s2 = null;
						s2 = fd.cargar(fc.getPath());
						
						int j = -1;
						for (int z = 0; z < v.getProyecto().dameNumFich(); z++) {
							if(v.getProyecto().getfich(z).getPath().equals(fc.getPath())) j =z;
						}
						
						//se mira el tipo
						int t = 0;
						if(v.getProyecto().getfich(j).isSetFile()) t = 2;
						if(v.getProyecto().getfich(j).isMainFile()) t = 1;
						
						ce.nuevaPestaña(fc.getPath(), fc.getPath(), s2, true, t);
						// UNDO REDO
						//v.getnuevoMenu().habilita_salvarFich();
						v.getnuevoMenu().habilitaMenuArchivo();
						v.getnuevoMenu().habilitaMenuEdicion();
						int numeditor = v.getCreadorEditor()
								.getEditorSeleccionado();
						Document doc = v.getCreadorEditor().dameEditorI(
								numeditor).getEditor().getDocument();
						doc.addUndoableEditListener(new UndoableEditListener() {
							public void undoableEditHappened(UndoableEditEvent evt) {
								Ventana v = Ventana.getInstance();
								UndoableEdit edit = evt.getEdit();
								if (edit instanceof DefaultDocumentEvent &&
							            ((DefaultDocumentEvent)edit).getType() == 
							                DefaultDocumentEvent.EventType.CHANGE) {
									return;	
							        }else{
							        	v.getnuevoMenu().getUndo().addEdit(evt.getEdit());	
							        }
							        }
						});
						// CURSOR EN LA PRIMERA POSICION DEL EDITOR DE TEXTO
						numeditor = v.getCreadorEditor()
								.getEditorSeleccionado();
						v.getCreadorEditor().dameEditorI(numeditor).getEditor()
								.setCaretPosition(0);
					
						//mig
						for(int z=0; z<v.getProyecto().getFichSize(); z++){
							if(v.getProyecto().getfich(z).getPath().equals(fc.getPath())){
								v.getProyecto().getfich(z).setOpened(true);
							}
						}
						v.getProyecto().setModified(true);
					}
					
					
				}else{
					v.getCreadorEditor().setEditorSeleccionado(s);
				}
				}
			
			}

		}

		public void mouseEntered(MouseEvent arg0) {
			// TODO Auto-generated method stub

		}

		public void mouseExited(MouseEvent arg0) {
			// TODO Auto-generated method stub

		}

		public void mousePressed(MouseEvent arg0) {
			// TODO Auto-generated method stub
			if (arg0.isPopupTrigger()) {
				popup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
			}
		}

		public void mouseReleased(MouseEvent arg0) {
			if (arg0.isPopupTrigger()) {
				
				//mig
				Ventana v=Ventana.getInstance();
				String prj=null;
				try {
					prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
					saveProj.setEnabled(false);
					newFile.setEnabled(false);
					addFile.setEnabled(false);
					removeFile.setEnabled(false);
					deleteFile.setEnabled(false);
					setMain.setEnabled(false);
					unsetMain.setEnabled(false);
					setCompilable.setEnabled(false);
					unsetCompilable.setEnabled(false);
					addFolder.setEnabled(false);
					removeFolder.setEnabled(false);
				}else{
					saveProj.setEnabled(false);
					newFile.setEnabled(true);
					addFile.setEnabled(true);
					removeFile.setEnabled(false);
					deleteFile.setEnabled(false);
					setMain.setEnabled(false);
					unsetMain.setEnabled(false);
					setCompilable.setEnabled(false);
					unsetCompilable.setEnabled(false);
					addFolder.setEnabled(true);
					removeFolder.setEnabled(false);
					
					if(v.getProyecto().isModified()){
						saveProj.setEnabled(true);
					}
					
					TreePath path = v.getNuevoExplorador().getArbol().getSelectionPath();
					DefaultMutableTreeNode filePath;
					Fich fc;
					if(path!=null){
						
						filePath = (DefaultMutableTreeNode) path.getLastPathComponent();
						fc=(Fich)filePath.getUserObject();
						
						if(!fc.isDirectory()){
							removeFile.setEnabled(true);
							deleteFile.setEnabled(true);
							if(!fc.isMainFile()) setMain.setEnabled(true);
							if(fc.isMainFile()) unsetMain.setEnabled(true);
							if(!fc.isSetFile()||(fc.isSetFile()&&fc.isMainFile())) setCompilable.setEnabled(true);
							if(fc.isSetFile()&&!fc.isMainFile()) unsetCompilable.setEnabled(true);
						}
						else{
							removeFolder.setEnabled(true);
						} 
					}
				}
				popup.show(arg0.getComponent(), arg0.getX(), arg0.getY());
			}
		}
	}

	/**
	 * Clase que implementa la accion del teclado en el Explorador
	 * 
	 * 
	 */
	class EnterAction extends KeyAdapter {
		public void keyPressed(KeyEvent evt) {
			if (evt.getKeyCode() == KeyEvent.VK_ENTER) {
				TreePath path = arbol.getSelectionPath();
				Ventana v = Ventana.getInstance();
				if (path != null) {
					String filePath = path.getLastPathComponent().toString();
					 v.getStatusBar().setMessage(filePath);		
					 DefaultMutableTreeNode d =  (DefaultMutableTreeNode) path.getLastPathComponent();
						Object nodo=d.getUserObject();
						Fich fc= (Fich) nodo;
							
					if (!fc.isDirectory()) {
						Fichero fd = new Fichero();
						CreadorEditor ce = v.getCreadorEditor();
						String s = null;
						s = fd.cargar(fc.getPath());
						
						int j = -1;
						for (int z = 0; z < v.getProyecto().dameNumFich(); z++) {
							if(v.getProyecto().getfich(z).getPath().equals(fc.getPath())) j =z;
						}
						
						//se mira el tipo
						int t = 0;
						if(v.getProyecto().getfich(j).isSetFile()) {
							t = 2;
							//status
							v.getStatusBar().setMessage(filePath+" <COMPILABLE>");
						}
						if(v.getProyecto().getfich(j).isMainFile()) {
							t = 1;
							//status
							v.getStatusBar().setMessage(filePath+" <MAIN>");
						}
    					
    						
						ce.nuevaPestaña(fc.getPath(), fc.getPath(), s, true, t);
						// UNDO REDO
						v.getnuevoMenu().habilita_salvarFich();
						int numeditor = v.getCreadorEditor()
								.getEditorSeleccionado();
						Document doc = v.getCreadorEditor().dameEditorI(
								numeditor).getEditor().getDocument();
						doc.addUndoableEditListener(new UndoableEditListener() {
							public void undoableEditHappened(UndoableEditEvent evt) {
								Ventana v = Ventana.getInstance();
								UndoableEdit edit = evt.getEdit();
								if (edit instanceof DefaultDocumentEvent &&
							            ((DefaultDocumentEvent)edit).getType() == 
							                DefaultDocumentEvent.EventType.CHANGE) {
									return;	
							        }else{
							        	v.getnuevoMenu().getUndo().addEdit(evt.getEdit());	
							        }
							        }
						});
						// CURSOS EN LA PRIMERA POSICION DEL EDITOR DE TEXTO
						numeditor = v.getCreadorEditor()
								.getEditorSeleccionado();
						v.getCreadorEditor().dameEditorI(numeditor).getEditor()
								.setCaretPosition(0);
					}

				}
				}
			
		}
	}

	public DefaultMutableTreeNode getRaiz() {
		return raiz;
	}

	public JTree getArbol() {
		return arbol;
	}

	public void setArbol(JTree arbol) {
		this.arbol = arbol;
	}

	public void setRaiz(DefaultMutableTreeNode raiz) {
		this.raiz = raiz;
	}

	public DefaultTreeModel getTreeModel() {
		return treeModel;
	}

	public void setTreeModel(DefaultMutableTreeNode treeModel) {
		this.treeModel = new DefaultTreeModel(treeModel);
	}

	public JMenuItem getAddFile() {
		return addFile;
	}

	public void setEnabledAddFile() {
		this.addFile.setEnabled(true);
	}

	public JMenuItem getSaveProj() {
		return saveProj;
	}

	public void setEnabledSaveProj() {
		this.saveProj.setEnabled(true);
	}

	public void setEnabledRemoveFile() {
		this.removeFile.setEnabled(true);
	}
	
	public void setEnabledDeleteFile() {
		this.deleteFile.setEnabled(true);
	}
	
 public void disposeExplorer(){

   Ventana v =Ventana.getInstance();
   this.tam_explorer=v.getSplitPaneV().getDividerLocation();
   v.getSplitPaneV().setDividerLocation(0);
   v.getSplitPaneV().getLeftComponent().setVisible(false);
 
 }
 public void showExplorer(){
	
	Ventana v=Ventana.getInstance();
	
     v.getSplitPaneV().setDividerLocation(this.tam_explorer);
     v.getSplitPaneV().getLeftComponent().setVisible(true);
 }

 public void expandTree(){
	 int row = 0;
	    while (row < getArbol().getRowCount()) {
	    	getArbol().expandRow(row);
	        row++;
	    }
 }
public JMenuItem getRemoveFile() {
	return removeFile;
}

public JComponent getDeleteFile() {
	return deleteFile;
}

}

//class SimpleCellRenderer extends JLabel implements TreeCellRenderer {
class ExtendedTreeCellRenderer extends DefaultTreeCellRenderer {
    static Icon ICON_COMP = new ImageIcon("./Iconos/compilable.png");
    static Icon ICON_MAIN = new ImageIcon("./Iconos/main.png");
    static Icon ICON_FOLDER = new ImageIcon("./Iconos/folder.png");
    static Icon ICON_DEF = new ImageIcon("./Iconos/default.png");
    
    /**
     * El metodo que dibuja cada uno de los elementos del JTree.
     * @param tree El JTree a dibujar
     * @param value El valor del objeto a dibujar
     * @param sel Si esta seleccionado o no
     * @param expanded Si esta expandido o no
     * @param leaf Si es nodo hoja (no tiene hijos)
     * @param row El numero de fila
     * @param hasFocus Si esta seleccionado
     * @return El componente a dibujar
     */
    public Component getTreeCellRendererComponent(JTree tree,
            Object value, boolean sel, boolean expanded, boolean leaf,
            int row, boolean hasFocus) {
    	
    	super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
    	
        DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
        Fich fc;
        //if(row>-1) {
        	try {
				fc = (Fich)node.getUserObject();
				
				setText(fc.getName());
	        	setIcon(ICON_DEF);
	        	if(fc.isDirectory()) setIcon(ICON_FOLDER);
	        	if(fc.isSetFile()) setIcon(ICON_COMP);
	        	if(fc.isMainFile()) setIcon(ICON_MAIN);
	        	
        	} catch (RuntimeException e) {
				//e.printStackTrace();
			}
        //}
        return this;
     }
}

