package gui.editor;

import gui.Ventana;
import idioma.Idioma;
import java.awt.Adjustable;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ResourceBundle;

import javax.swing.Icon;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextPane;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.basic.BasicArrowButton;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.tree.TreePath;

import operaciones.configuracion.Fich;
import operaciones.genericas.*;
import operaciones.log.Log;
import org.apache.log4j.Logger;
import principal.almacenPropiedades;


/**
 * Clase que implementa el area de edicion
 * 
 * 
 */
public class Editor extends JPanel {

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	/**
	 * Atributo: serialVersionUID: clase serializable
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Atributo: editor: Establece una interfaz de editor
	 * 
	 */
	private JTextPane editor;
	
	private JTextPane editor2;
	
	private JScrollPane pScroll;
	
	private JScrollPane p2Scroll;
	
	private int verticalvalue;

	private int verticalvalue2;
	
	private int horizontalvalue;

	private int horizontalvalue2;
	
	private String path;
	
	private long ultimoCambio;
	
	private long ultimoTam;
	
	private JPopupMenu popup;
	
	private JPopupMenu popup2;
	
	private SyntaxisDoc doc;
	
	private MyDocumentListener editor1Listener;
	
	private MyDocumentListener2 editor2Listener;
	
	private MyCaretListener1 caret1Listener;
	
	private MyCaretListener2 caret2Listener;
	
	private MyAdjustmentListener1 listener1;
	
	private MyAdjustmentListener2 listener2;
	
	private dobleClick dc;
	
	private Click1 click1;
	
	private Click2 click2;
	
	private bClick1 bclick1;
	
	private bClick2 bclick2;
	
	private int activo;
	
	private LineNumber lineNumber;
	
	private LineNumber lineNumber2;
	
	private JSplitPane splitPaneH;
	
	private JTextPane areaTexto;
	
	//indica si es archivo principal
	private boolean mainFile;
	
	//indica si está marcado para compilación
	private boolean compilerFile;
	
	private static int brace1;
	
	private static int brace2;
	
	private JMenuItem addFile;
	
	private JMenuItem removeFile;
	
	private JMenuItem addFile2;
	
	private JMenuItem removeFile2;
	
	private JMenuItem copy;
	
	private JMenuItem copy2;
	
	private JMenuItem paste;
	
	private JMenuItem paste2;
	
	private JMenuItem cut;
	
	private JMenuItem cut2;
	
	private JMenuItem setCompilable;
	
	private JMenuItem setCompilable2;
	
	private JMenuItem unsetCompilable;
	
	private JMenuItem unsetCompilable2;
	
	private JMenuItem setMain;
	
	private JMenuItem setMain2;
	
	private JMenuItem unsetMain;
	
	private JMenuItem unsetMain2;
	
	private Icon icon;
	/**
	 * Clase principal que instancia un editor de texto
	 * 
	 */
	public Editor() {
		super();
		ResourceBundle labels = Idioma.getInstance().getLabels();
		try {	
			brace1 = -1;
			brace2 = -1;
			verticalvalue = 0;
			verticalvalue2 = 0;
			horizontalvalue = 0;
			horizontalvalue2 = 0;
			activo = 1;
			logger.info(labels.getString("s317"));
			doc = new SyntaxisDoc();
			editor = creaEditor();
			editor.setCaretPosition(0);
			editor2 = creaEditor();
			editor2.setCaretPosition(0);
			editor.addKeyListener(new KeyListener(){
				public void keyTyped(KeyEvent e) {
					
				}

				public void keyPressed(KeyEvent e) {
					if (brace1 != -1) {
						doc.elimBrace(brace1);
						brace1 = -1;
					}
					if (brace2 != -1) {
						doc.elimBrace(brace2);
						brace2 = -1;
					}
					
				}

				public void keyReleased(KeyEvent e) {
									
				};
			});
			editor2.addKeyListener(new KeyListener(){
				public void keyTyped(KeyEvent e) {
					
				}

				public void keyPressed(KeyEvent e) {
					if (brace1 != -1) {
						doc.elimBrace(brace1);
						brace1 = -1;
					}
					if (brace2 != -1) {
						doc.elimBrace(brace2);
						brace2 = -1;
					}	
				}

				public void keyReleased(KeyEvent e) {
									
				};
			});
			popup = new JPopupMenu();
			copy = new JMenuItem(labels.getString("s187"));
			copy.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					//JOptionPane.showMessageDialog(null,"Copiar","Copy",JOptionPane.INFORMATION_MESSAGE);
					Ventana v = Ventana.getInstance();
					Idioma i = Idioma.getInstance();
					try {
						i.seleccionIdioma(Integer.parseInt(almacenPropiedades
								.getPropiedad("idioma")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = i.getLabels();
					logger.info(labels.getString("s99"));
					v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor().copy();
					v.getnuevaSalida().cargaTexto(labels.getString("s99"));
				}
			});
			popup.add(copy);
			cut = new JMenuItem(labels.getString("s188"));
			cut.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					//JOptionPane.showMessageDialog(null,"Cortar","Cut",JOptionPane.INFORMATION_MESSAGE);
					Ventana v = Ventana.getInstance();
					Idioma i = Idioma.getInstance();
					try {
						i.seleccionIdioma(Integer.parseInt(almacenPropiedades
								.getPropiedad("idioma")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = i.getLabels();
					logger.info(labels.getString("s97"));
					v.getnuevaSalida().cargaTexto(labels.getString("s97"));
					v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor().cut();
				}
			});
			popup.add(cut);
			paste = new JMenuItem(labels.getString("s189"));
			paste.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					//JOptionPane.showMessageDialog(null,"Pegar","Paste",JOptionPane.INFORMATION_MESSAGE);
					Ventana v = Ventana.getInstance();
					Idioma i = Idioma.getInstance();
					try {
						i.seleccionIdioma(Integer.parseInt(almacenPropiedades
								.getPropiedad("idioma")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = i.getLabels();
					logger.info(labels.getString("s98"));
					v.getnuevaSalida().cargaTexto(labels.getString("s98"));
					v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor()
							.paste();
				}
			});
			popup.add(paste);
			JMenuItem selectAll = new JMenuItem(labels.getString("s191"));
			selectAll.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					Ventana v = Ventana.getInstance();
					v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor().setCaretPosition(0);
					int length = v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor().getText().length();
					v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor().setSelectionEnd(length);
				}
			});
			popup.add(selectAll);
			popup.addSeparator();
			//mig
			addFile = new JMenuItem(labels.getString("s17"));
			addFile.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getAnadirFichero2().doClick();
			}
			});
			removeFile = new JMenuItem(labels.getString("s618"));
			removeFile.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getRemoveFile2().doClick();
			}
			});
			popup.add(removeFile);
			JMenuItem deleteFile = new JMenuItem(labels.getString("s950"));
			deleteFile.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getDeleteFile2().doClick();
			}
			});
			popup.add(deleteFile);
			
			popup.addSeparator();
			//mig
			setCompilable = new JMenuItem(labels.getString("s254"));
			setCompilable.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getSetCompilable2().setEnabled(true);
	               v.getnuevoMenu().getSetCompilable2().doClick();
	               //v.getnuevoMenu().getSetCompilable2().setEnabled(false);
			}
			});
			popup.add(setCompilable);
			
			unsetCompilable = new JMenuItem(labels.getString("s255"));
			unsetCompilable.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getUnsetCompilable2().setEnabled(true);
	               v.getnuevoMenu().getUnsetCompilable2().doClick();
	               //v.getnuevoMenu().getUnsetCompilable2().setEnabled(false);
			}
			});
			popup.add(unsetCompilable);
			
			setMain = new JMenuItem(labels.getString("s256"));
			setMain.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getSetMain2().setEnabled(true);
	               v.getnuevoMenu().getSetMain2().doClick();
	               //v.getnuevoMenu().getSetMain2().setEnabled(false);
			}
			});
			popup.add(setMain);
			
			unsetMain = new JMenuItem(labels.getString("s952"));
			unsetMain.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getUnsetMain2().setEnabled(true);
	               v.getnuevoMenu().getUnsetMain2().doClick();
	               //v.getnuevoMenu().getUnsetMain2().setEnabled(false);
			}
			});
			popup.add(unsetMain);
			popup.addSeparator();
			//Añadido JMenuItem Print
			JMenuItem print =new JMenuItem(labels.getString("s624"));
			print.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					Ventana v = Ventana.getInstance();
					v.getnuevoMenu().getPrint().setEnabled(true);
					v.getnuevoMenu().getPrint().doClick();
					//v.getnuevoMenu().getPrint().setEnabled(false);		
				}
			});
			popup.add(print);
			popup2 = new JPopupMenu();
			copy2 = new JMenuItem(labels.getString("s187"));
			copy2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					//JOptionPane.showMessageDialog(null,"Copiar","Copy",JOptionPane.INFORMATION_MESSAGE);
					Ventana v = Ventana.getInstance();
					Idioma i = Idioma.getInstance();
					try {
						i.seleccionIdioma(Integer.parseInt(almacenPropiedades
								.getPropiedad("idioma")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = i.getLabels();
					logger.info(labels.getString("s99"));
					v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor().copy();
					v.getnuevaSalida().cargaTexto(labels.getString("s99"));
				}
			});
			popup2.add(copy2);
			cut2 = new JMenuItem(labels.getString("s188"));
			cut2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					//JOptionPane.showMessageDialog(null,"Cortar","Cut",JOptionPane.INFORMATION_MESSAGE);
					Ventana v = Ventana.getInstance();
					Idioma i = Idioma.getInstance();
					try {
						i.seleccionIdioma(Integer.parseInt(almacenPropiedades
								.getPropiedad("idioma")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = i.getLabels();
					logger.info(labels.getString("s97"));
					v.getnuevaSalida().cargaTexto(labels.getString("s97"));
					v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor().cut();
				}
			});
			popup2.add(cut2);
			paste2 = new JMenuItem(labels.getString("s189"));
			paste2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					//JOptionPane.showMessageDialog(null,"Pegar","Paste",JOptionPane.INFORMATION_MESSAGE);
					Ventana v = Ventana.getInstance();
					Idioma i = Idioma.getInstance();
					try {
						i.seleccionIdioma(Integer.parseInt(almacenPropiedades
								.getPropiedad("idioma")));
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					ResourceBundle labels = i.getLabels();
					logger.info(labels.getString("s98"));
					v.getnuevaSalida().cargaTexto(labels.getString("s98"));
					v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor()
							.paste();
				}
			});
			popup2.add(paste2);
			JMenuItem selectAll2 = new JMenuItem(labels.getString("s191"));
			selectAll2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					Ventana v = Ventana.getInstance();
					v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor().setCaretPosition(0);
					int length = v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor().getText().length();
					v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado()).getEditor().setSelectionEnd(length);
				}
			});
			popup2.add(selectAll2);
			popup2.addSeparator();
			//mig
			addFile2 = new JMenuItem(labels.getString("s17"));
			addFile2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getAnadirFichero2().doClick();
			}
			});
			popup2.add(addFile2);
			removeFile2 = new JMenuItem(labels.getString("s618"));
			removeFile2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getRemoveFile2().doClick();
			}
			});
			popup2.add(removeFile2);
			JMenuItem deleteFile2 = new JMenuItem(labels.getString("s950"));
			deleteFile2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getDeleteFile2().doClick();
			}
			});
			popup2.add(deleteFile2);

			popup2.addSeparator();
			//mig
			setCompilable2 = new JMenuItem(labels.getString("s254"));
			setCompilable2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getSetCompilable2().setEnabled(true);
	               v.getnuevoMenu().getSetCompilable2().doClick();
	               //v.getnuevoMenu().getSetCompilable2().setEnabled(false);
			}
			});
			popup2.add(setCompilable2);
			
			unsetCompilable2 = new JMenuItem(labels.getString("s255"));
			unsetCompilable2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getUnsetCompilable2().setEnabled(true);
	               v.getnuevoMenu().getUnsetCompilable2().doClick();
	               //v.getnuevoMenu().getUnsetCompilable2().setEnabled(false);
			}
			});
			popup2.add(unsetCompilable2);
			
			setMain2 = new JMenuItem(labels.getString("s256"));
			setMain2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getSetMain2().setEnabled(true);
	               v.getnuevoMenu().getSetMain2().doClick();
	               //v.getnuevoMenu().getSetMain2().setEnabled(false);
			}
			});
			popup2.add(setMain2);
			
			unsetMain2 = new JMenuItem(labels.getString("s952"));
			unsetMain2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
	               Ventana v=Ventana.getInstance();
	               v.getnuevoMenu().getUnsetMain2().setEnabled(true);
	               v.getnuevoMenu().getUnsetMain2().doClick();
	               //v.getnuevoMenu().getUnsetMain2().setEnabled(false);
			}
			});
			popup2.add(unsetMain2);
			popup2.addSeparator();
			//Añadido JMenuItem Print
			JMenuItem print2 =new JMenuItem(labels.getString("s624"));
			print2.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					Ventana v = Ventana.getInstance();
					v.getnuevoMenu().getPrint().setEnabled(true);
					v.getnuevoMenu().getPrint().doClick();
					//v.getnuevoMenu().getPrint().setEnabled(false);
				}
			});
			dc = new dobleClick(); 
			popup2.add(print2);
			editor.addMouseListener(new PopupListener());
			editor2.addMouseListener(new PopupListener2());
			editor.addMouseListener(dc);
			editor2.addMouseListener(dc);
			setLayout(new BorderLayout());
			pScroll = new JScrollPane(editor);
			p2Scroll = new JScrollPane(editor2);
			lineNumber = new LineNumber( editor );
			lineNumber2 = new LineNumber( editor2 );
			pScroll.setRowHeaderView( lineNumber );
			p2Scroll.setRowHeaderView( lineNumber2 );
			click1 = new Click1();
			click2 = new Click2();
			pScroll.getVerticalScrollBar().addMouseListener(click1);
			pScroll.getHorizontalScrollBar().addMouseListener(click1);
			p2Scroll.getVerticalScrollBar().addMouseListener(click2);
			p2Scroll.getHorizontalScrollBar().addMouseListener(click2);
			pScroll.addMouseListener(click1);
			p2Scroll.addMouseListener(click2);
			
			bclick1 = new bClick1();
			bclick2 = new bClick2();
	        Component comp[] = pScroll.getVerticalScrollBar().getComponents();
	        for (int i=0;i<comp.length;i++)
	        {
	            if (comp[i] instanceof BasicArrowButton)
	            {
	                comp[i].addMouseListener(bclick1);
	            }
	        }

	        Component comph[] = pScroll.getHorizontalScrollBar().getComponents();
	        for (int i=0;i<comph.length;i++)
	        {
	            if (comph[i] instanceof BasicArrowButton)
	            {
	                comph[i].addMouseListener(bclick1);
	            }
	        }
	        
	        Component comp2[] = p2Scroll.getVerticalScrollBar().getComponents();
	        for (int i=0;i<comp2.length;i++)
	        {
	            if (comp2[i] instanceof BasicArrowButton)
	            {
	                comp2[i].addMouseListener(bclick2);
	            }
	        }
	        
	        Component comph2[] = pScroll.getHorizontalScrollBar().getComponents();
	        for (int i=0;i<comph2.length;i++)
	        {
	            if (comph2[i] instanceof BasicArrowButton)
	            {
	                comph2[i].addMouseListener(bclick2);
	            }
	        }
	        
			editor1Listener = new MyDocumentListener();
			editor.getDocument().addDocumentListener(editor1Listener);
			editor2Listener = new MyDocumentListener2();
			editor2.getDocument().addDocumentListener(editor2Listener);
			caret1Listener = new MyCaretListener1();
			editor.addCaretListener(caret1Listener );
			caret2Listener = new MyCaretListener2();
			editor2.addCaretListener(caret2Listener );
			pScroll.setMinimumSize(new Dimension(0,0));
			p2Scroll.setMinimumSize(new Dimension(0,0));
			splitPaneH = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
					pScroll, p2Scroll);
			//splitPaneH.setResizeWeight(0.5);
			splitPaneH.setDividerLocation(0);
			splitPaneH.setContinuousLayout(true);
			listener1 = new MyAdjustmentListener1();
			listener2 = new MyAdjustmentListener2();
			pScroll.getVerticalScrollBar().addAdjustmentListener(listener1);
			p2Scroll.getVerticalScrollBar().addAdjustmentListener(listener2);
			add(splitPaneH);
			logger.info(labels.getString("s318"));
			path = null;
			ultimoCambio = 0;
			ultimoTam = 0;
	     	//Se ha añadido un listener
			Key teclado=new Key();
		    editor.addKeyListener(teclado);
		    editor2.addKeyListener(teclado);
		    Mouse mouse=new Mouse();
		    editor.addMouseListener(mouse);
		    editor2.addMouseListener(mouse);
		    //mig
		    Key2 teclado2=new Key2();
		    editor.addKeyListener(teclado2);
		    editor2.addKeyListener(teclado2);

		} catch (Exception e) {
			logger.info(labels.getString("s319"));
			e.printStackTrace();
		}
	
	}
	
	/**
	 * Clase que devuelve un area de texto
	 * 
	 * @return
	 */
	protected JTextPane creaEditor() {
		areaTexto = new JTextPane(doc) {
			
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
		
		areaTexto.setFont(new Font("monospaced", Font.PLAIN, 12));	
			
		return areaTexto;
	}

	/**
	 * Obtiene el interfaz Editor
	 * 
	 * @return El editor
	 */
	public static JTextComponent getEditor() {
		
		int pos = Ventana.getInstance().getCreadorEditor().getEditorSeleccionado();
		int activo = Ventana.getInstance().getCreadorEditor().dameEditorI(pos).activo;
		if (activo == 2)	return Ventana.getInstance().getCreadorEditor().dameEditorI(pos).editor2;
		else  return Ventana.getInstance().getCreadorEditor().dameEditorI(pos).editor;
		
	}

	/**
	 * Carga un texto que se le pasa como parametro en el editor
	 * 
	 * @param texto
	 */
	public void cargaTexto(String texto) {
		editor2.getDocument().removeDocumentListener(editor2Listener);
		editor.getDocument().removeDocumentListener(editor1Listener);
		
		editor.setText(texto);
		//editor2.setText(texto);
		String sold = Ventana.getInstance().getCreadorEditor().pane.getTitleAt(Ventana.getInstance().getCreadorEditor().getEditorSeleccionado());
		sold = sold.replace(" *","");
		Ventana.getInstance().getCreadorEditor().pane.setTitleAt(Ventana.getInstance().getCreadorEditor().getEditorSeleccionado(), sold);
		editor.setCaretPosition(0);
		editor2.setCaretPosition(0);
		editor2.getDocument().addDocumentListener(editor2Listener);
		editor.getDocument().addDocumentListener(editor1Listener);
		this.revalidate();
		
	}

	/**
	 * Selecciona una parte del texto del editor
	 * 
	 * @param iniPos
	 * @param lon
	 */
	public void seleccionaTexto(int iniPos,int lon){
		editor.setSelectionStart(iniPos);
		editor.setSelectionEnd(iniPos + lon);
		editor2.setSelectionStart(iniPos);
		editor2.setSelectionEnd(iniPos + lon);
		if (activo == 1) editor.requestFocus();
		else editor2.requestFocus();
	}
	
	
	/**
	 * Devuelve en un String el contenido del editor
	 * 
	 * @return
	 */
	public String getTexto() {
		return editor.getText();
	}
	
	/**
	 * Instancia el path del archivo
	 * 
	 * @param ruta
	 */
	public void setPath(String ruta){
		path = ruta;
	}
	
	/**
	 * Devuelve el path del archivo incluyendo el nombre del archivo
	 * 
	 * @return
	 */
	public String getPath(){
		return path;
	}
	
	/**
	 * Devuelve el path del archivo sin nombre del archivo
	 * 
	 * @return
	 */
	//mig
	public String getFilePath() {
		String filePath;
		
		filePath = path.substring(0,path.lastIndexOf("\\"));
		
		return filePath;
	}
	
	/**
	 * Devuelve la extension del archivo
	 * 
	 * @return
	 */
	public String getFileExt() {
		String fileExt;
		
		fileExt = path.substring(path.lastIndexOf(".")+1);
		
		return fileExt;
	}
	
	/**
	 * Devuelve el nombre del archivo sin extension
	 * 
	 * @return
	 */
	public CharSequence getFileName() {
		String fileExt;
		
		fileExt = path.substring(path.lastIndexOf("\\")+1,path.lastIndexOf("."));
		
		return fileExt;
	}
	
	/**
	 * Devuelve el ultimo cambio
	 * 
	 * @return
	 */
	public long getUltimoCambio() {
		return ultimoCambio;
	}

	/**
	 * Instancia el ultimo cambio
	 * 
	 * @param ultimoCambio
	 */
	public void setUltimoCambio(long ultimoCambio) {
		this.ultimoCambio = ultimoCambio;
	}

	/**
	 * Devuelve el ultimo tamaño
	 * 
	 * @return
	 */
	public long getUltimoTam() {
		return ultimoTam;
	}
	
	public void setEditable(boolean b){
		editor.setEditable(b);
		editor2.setEditable(b);
	}

	/**
	 * Instancia el ultimo tamaño
	 * 
	 * @param ultimoTam
	 */
	public void setUltimoTam(long ultimoTam) {
		this.ultimoTam = ultimoTam;
	}
	
	class PopupListener extends MouseAdapter {
		
		public void mousePressed(MouseEvent e) {
	        maybeShowPopup(e);
	    }

	    public void mouseReleased(MouseEvent e) {
	    	maybeShowPopup(e);
	    }

	    private void maybeShowPopup(MouseEvent e) {
	        if (e.isPopupTrigger()) {
	        	//menu dinamico
	        	Ventana v=Ventana.getInstance();
	        	String prj=null;
				try {
					prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if ((prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
					v.getnuevoEditor().getAddFile().setEnabled(false);
					v.getnuevoEditor().getRemoveFile().setEnabled(false);
				}else{
					String file = v.getCreadorEditor().dameEditorI(v.getCreadorEditor().getEditorSeleccionado()).getPath();
					boolean a = false;
					for(int i=0; i<v.getProyecto().dameNumFich(); i++){
						if(v.getProyecto().getfich(i).getPath().equals(file)) {
							a = true;
						}
					}
					if(a){
						v.getnuevoEditor().getRemoveFile().setEnabled(true);
						v.getnuevoEditor().getAddFile().setEnabled(false);}
					else{
						v.getnuevoEditor().getRemoveFile().setEnabled(false);
						v.getnuevoEditor().getAddFile().setEnabled(true);}
				}
	            popup.show(e.getComponent(), e.getX(), e.getY());
	        }
	    }
	}
	
	class PopupListener2 extends MouseAdapter {
		
		public void mousePressed(MouseEvent e) {
	        maybeShowPopup(e);
	    }

	    public void mouseReleased(MouseEvent e) {
	    	maybeShowPopup(e);
	    }

	    private void maybeShowPopup(MouseEvent e) {
	        if (e.isPopupTrigger()) {
	        	//menu dinamico
	        	Ventana v=Ventana.getInstance();
	        	Editor edit = v.getCreadorEditor().EditorSeleccionado();
	        	edit.getCopy2().setEnabled(false);
				edit.getCut2().setEnabled(false);
				edit.getPaste2().setEnabled(false);
				edit.getAddFile2().setEnabled(false);
				edit.getRemoveFile2().setEnabled(false);
				edit.getSetCompilable2().setEnabled(false);
				edit.getUnsetCompilable2().setEnabled(false);
				edit.getSetMain2().setEnabled(false);
				edit.getUnsetMain2().setEnabled(false);
	        	
	        	String prj2=null;
	        	
				try {
					prj2=almacenPropiedades.getPropiedad("DefaultAcidePrj");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if(Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null)!=null){
					edit.getPaste2().setEnabled(true);
				}
				if(edit.getEditor().getSelectedText() != null){
					edit.getCopy2().setEnabled(true);
					edit.getCut2().setEnabled(true);
				}
				if ((prj2.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
					edit.getAddFile2().setEnabled(false);
					edit.getRemoveFile2().setEnabled(false);
				}else{
					String file = v.getCreadorEditor().dameEditorI(v.getCreadorEditor().getEditorSeleccionado()).getPath();
					boolean a = false;
					for(int i=0; i<v.getProyecto().dameNumFich(); i++){
						if(v.getProyecto().getfich(i).getPath().equals(file)) {
							a = true;
						}
					}
					if(a){
						edit.getRemoveFile2().setEnabled(true);
						edit.getAddFile2().setEnabled(false);}
					else{
						edit.getRemoveFile2().setEnabled(false);
						edit.getAddFile2().setEnabled(true);}
				}
				//marcado
				//si no hay proyecto
				if ((prj2.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
					if(!edit.isMainFile()) edit.getSetMain2().setEnabled(true);
					if(edit.isMainFile()) edit.getUnsetMain2().setEnabled(true);
					if(!edit.isCompilerFile()||(edit.isCompilerFile()&&edit.isMainFile())) edit.getSetCompilable2().setEnabled(true);
					if(edit.isCompilerFile()&&!edit.isMainFile()) edit.getUnsetCompilable2().setEnabled(true);
				}else{// si hay proyecto
					boolean a = false;
					for(int i=0; i<v.getProyecto().dameNumFich(); i++){
						if(v.getProyecto().getfich(i).getPath().equals(edit.getPath()))
								a = true;
					}
					if(a){//si el archivo pertence al proyecto
						if(!edit.isMainFile()) edit.getSetMain2().setEnabled(true);
						if(edit.isMainFile()) edit.getUnsetMain2().setEnabled(true);
						if(!edit.isCompilerFile()||(edit.isCompilerFile()&&edit.isMainFile())) edit.getSetCompilable2().setEnabled(true);
						if(edit.isCompilerFile()&&!edit.isMainFile()) edit.getUnsetCompilable2().setEnabled(true);
					}
				}

	            popup2.show(e.getComponent(), e.getX(), e.getY());
	        }
	    }
	}

	public SyntaxisDoc getDoc() {
		return doc;
	}

	public JMenuItem getCut() {
		return cut;
	}

	public JMenuItem getCut2() {
		return cut2;
	}
	
	public JMenuItem getCopy() {
		return copy;
	}
	
	public JMenuItem getCopy2() {
		return copy2;
	}

	public JMenuItem getPaste() {
		return paste;
	}
	
	public JMenuItem getPaste2() {
		return paste2;
	}
	
	public JMenuItem getAddFile() {
		return addFile;
	}
	
	public JMenuItem getAddFile2() {
		return addFile2;
	}

	public JMenuItem getRemoveFile() {
		return removeFile;
	}

	public JMenuItem getRemoveFile2() {
		return removeFile2;
	}

	public JMenuItem getSetCompilable() {
		return setCompilable;
	}
	
	public JMenuItem getSetCompilable2() {
		return setCompilable2;
	}

	public JMenuItem getUnsetCompilable() {
		return unsetCompilable;
	}
	
	public JMenuItem getUnsetCompilable2() {
		return unsetCompilable2;
	}
	
	public JMenuItem getSetMain() {
		return setMain;
	}
	
	public JMenuItem getSetMain2() {
		return setMain2;
	}
	
	public JMenuItem getUnsetMain2() {
		return unsetMain2;
	}
	
	public JMenuItem getUnsetMain() {
		return unsetMain;
	}
	
	public void resetDoc() {
		String s = "";
		try {
			s = doc.getText(0,doc.getLength());
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		SyntaxisDoc sy = new SyntaxisDoc();
		editor.setStyledDocument(sy);
		editor.setText(s);
		editor2.setStyledDocument(sy);
		editor.setCaretPosition(0);
		editor2.setCaretPosition(0);
		editor.getDocument().addDocumentListener(editor1Listener);
		editor2.getDocument().addDocumentListener(editor2Listener);
	}
	
	/*
	**  Position the caret at the start of a line.
	*/
	public void gotoLine(int line)
	{
		JTextComponent component = getEditor();
		Element root = component.getDocument().getDefaultRootElement();
		line = Math.max(line, 1);
		line = Math.min(line, root.getElementCount());
		component.setCaretPosition( root.getElement( line - 1 ).getStartOffset() );

	}
 
	class MyDocumentListener implements DocumentListener
	{
		String asterisco = " *";
	    public void insertUpdate(DocumentEvent e)
	    {
			Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(Ventana.getInstance().
					getCreadorEditor().getEditorSeleccionado()).redbutton();
			
			Ventana.getInstance().getnuevoMenu().getSalvarFich().setEnabled(true);
			/*if (brace1 != -1) {
				doc.elimBrace(brace1);
				brace1 = -1;
			}
			if (brace2 != -1) {
				doc.elimBrace(brace2);
				brace2 = -1;
			}*/
	    }
	    public void removeUpdate(DocumentEvent e)
	    {
			Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(Ventana.getInstance().
					getCreadorEditor().getEditorSeleccionado()).redbutton();
			Ventana.getInstance().getnuevoMenu().getSalvarFich().setEnabled(true);
			/*if (brace1 != -1) {
				doc.elimBrace(brace1);
				brace1 = -1;
			}
			if (brace2 != -1) {
				doc.elimBrace(brace2);
				brace2 = -1;
			}*/
	    }
	    public void changedUpdate(DocumentEvent e)
	    {
			Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(Ventana.getInstance().
					getCreadorEditor().getEditorSeleccionado()).redbutton();
			Ventana.getInstance().getnuevoMenu().getSalvarFich().setEnabled(true);
			/*if (brace1 != -1) {
				doc.elimBrace(brace1);
				brace1 = -1;
			}
			if (brace2 != -1) {
				doc.elimBrace(brace2);
				brace2 = -1;
			}*/
	    }
	    public void textChanged(String action)
	    {
			Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(Ventana.getInstance().
					getCreadorEditor().getEditorSeleccionado()).redbutton();
			Ventana.getInstance().getnuevoMenu().getSalvarFich().setEnabled(true);
			/*if (brace1 != -1) {
				doc.elimBrace(brace1);
				brace1 = -1;
			}
			if (brace2 != -1) {
				doc.elimBrace(brace2);
				brace2 = -1;
			}*/
	    }
	}

	class MyDocumentListener2 implements DocumentListener
	{
		String asterisco = " *";
	    public void insertUpdate(DocumentEvent e)
	    {
			Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(Ventana.getInstance().
					getCreadorEditor().getEditorSeleccionado()).redbutton();
			Ventana.getInstance().getnuevoMenu().getSalvarFich().setEnabled(true);
			/*if (brace1 != -1) {
				doc.elimBrace(brace1);
				brace1 = -1;
			}
			if (brace2 != -1) {
				doc.elimBrace(brace2);
				brace2 = -1;
			}*/
	    }
	    public void removeUpdate(DocumentEvent e)
	    {
			Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(Ventana.getInstance().
					getCreadorEditor().getEditorSeleccionado()).redbutton();
			Ventana.getInstance().getnuevoMenu().getSalvarFich().setEnabled(true);
			/*if (brace1 != -1) {
				doc.elimBrace(brace1);
				brace1 = -1;
			}
			if (brace2 != -1) {
				doc.elimBrace(brace2);
				brace2 = -1;
			}*/
	    }
	    public void changedUpdate(DocumentEvent e)
	    {
			Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(Ventana.getInstance().
					getCreadorEditor().getEditorSeleccionado()).redbutton();
			Ventana.getInstance().getnuevoMenu().getSalvarFich().setEnabled(true);
			/*if (brace1 != -1) {
				doc.elimBrace(brace1);
				brace1 = -1;
			}
			if (brace2 != -1) {
				doc.elimBrace(brace2);
				brace2 = -1;
			}*/
	    }
	    public void textChanged(String action)
	    {
			Ventana.getInstance().getCreadorEditor().getTp().getCloseButtoni(Ventana.getInstance().
					getCreadorEditor().getEditorSeleccionado()).redbutton();
			Ventana.getInstance().getnuevoMenu().getSalvarFich().setEnabled(true);
			/*if (brace1 != -1) {
				doc.elimBrace(brace1);
				brace1 = -1;
			}
			if (brace2 != -1) {
				doc.elimBrace(brace2);
				brace2 = -1;
			}*/
	    }
	}
	
	class MyCaretListener1 implements CaretListener{

		public void caretUpdate( CaretEvent e ){			
			if (editor.isFocusOwner()){
				activo = 1;
			}
			Element root = doc.getDefaultRootElement();
            int dot = e.getDot();
            int line = root.getElementIndex( dot );
            int col = dot - root.getElement( line ).getStartOffset();
            Ventana.getInstance().getStatusBar().getMessageLineCol().setText((line+1) + ":" + (col+1) );
            
            try {
            	//System.out.println(brace1);
            	//System.out.println(brace2);
            	//System.out.println();
            	if (brace1 != -1) {
					doc.elimBrace(brace1);
					brace1 = -1;
				}
				if (brace2 != -1) {
					doc.elimBrace(brace2);
					brace2 = -1;
				}
				int ini = getEditor().getCaretPosition();
				int n = matchingbraces.findMatchingBracket(getEditor().getDocument(),ini-1);
				if (((ini>0) && (ini <= doc.getLength())) && ((n>=0) && (n <= doc.getLength()))){
				if (n > -1){
					if (n > ini) {
						brace1 = ini-1;
						brace2 = n;
						doc.setBrace(brace1);
						doc.setBrace(brace2);
					}
					else if (n < ini){
						brace1 = n;
						brace2 = ini-1;
						doc.setBrace(brace1);
						doc.setBrace(brace2);
					}
				}
				}
			} catch (BadLocationException ex) {
				//ex.printStackTrace();
			}
          }
	}
	
	class MyCaretListener2 implements CaretListener{

        public void caretUpdate( CaretEvent e ){
        	if (editor2.isFocusOwner()){
        		activo = 2;
        	}
			Element root = doc.getDefaultRootElement();
            int dot = e.getDot();
            int line = root.getElementIndex( dot );
            int col = dot - root.getElement( line ).getStartOffset();
            Ventana.getInstance().getStatusBar().getMessageLineCol().setText((line+1) + ":" + (col+1) );
            try {
            	//System.out.println(brace1);
            	//System.out.println(brace2);
            	//System.out.println();
            	if (brace1 != -1) {
					doc.elimBrace(brace1);
					brace1 = -1;
				}
				if (brace2 != -1) {
					doc.elimBrace(brace2);
					brace2 = -1;
				}
				int ini = getEditor().getCaretPosition();
				int n = matchingbraces.findMatchingBracket(getEditor().getDocument(),ini-1);
				if (((ini>0) && (ini <= doc.getLength())) && ((n>=0) && (n <= doc.getLength()))){
				if (n > -1){
					if (n > ini) {
						brace1 = ini-1;
						brace2 = n;
						doc.setBrace(brace1);
						doc.setBrace(brace2);
					}
					else if (n < ini){
						brace1 = n;
						brace2 = ini-1;
						doc.setBrace(brace1);
						doc.setBrace(brace2);
					}
				}
				}
			} catch (BadLocationException ex) {
				//ex.printStackTrace();
			}
          }
	}
	
	class MyAdjustmentListener1 implements AdjustmentListener {
        public void adjustmentValueChanged(AdjustmentEvent evt) {
        	Adjustable source = evt.getAdjustable();
            if (evt.getValueIsAdjusting()) {
            	verticalvalue = pScroll.getVerticalScrollBar().getValue();
            	horizontalvalue = pScroll.getHorizontalScrollBar().getValue();
            	verticalvalue2 = p2Scroll.getVerticalScrollBar().getValue();
            	horizontalvalue2 = p2Scroll.getHorizontalScrollBar().getValue();
                return;
            }
            int orient = source.getOrientation();
            if (orient == Adjustable.HORIZONTAL) {
            } else {
            }
            int type = evt.getAdjustmentType();
            switch (type) {
              case AdjustmentEvent.UNIT_INCREMENT:
                  break;
              case AdjustmentEvent.UNIT_DECREMENT:
                  break;
              case AdjustmentEvent.BLOCK_INCREMENT:
                  break;
              case AdjustmentEvent.BLOCK_DECREMENT:
                  break;
              case AdjustmentEvent.TRACK:
                  break;
            }
            int value = evt.getValue();       
            if (editor.isFocusOwner() || pScroll.getVerticalScrollBar().isFocusOwner() || pScroll.getHorizontalScrollBar().isFocusOwner()){
            	p2Scroll.getVerticalScrollBar().setValue(verticalvalue2);
            	p2Scroll.getHorizontalScrollBar().setValue(horizontalvalue2);
            	verticalvalue = pScroll.getVerticalScrollBar().getValue();
        		horizontalvalue = pScroll.getHorizontalScrollBar().getValue();
            }
            if (editor2.isFocusOwner() || p2Scroll.getVerticalScrollBar().isFocusOwner() || p2Scroll.getHorizontalScrollBar().isFocusOwner()){
                pScroll.getVerticalScrollBar().setValue(verticalvalue);
                pScroll.getHorizontalScrollBar().setValue(horizontalvalue);
                verticalvalue2 = p2Scroll.getVerticalScrollBar().getValue();
        		horizontalvalue2 = p2Scroll.getHorizontalScrollBar().getValue();
            }
        }
	}
        class MyAdjustmentListener2 implements AdjustmentListener {
            public void adjustmentValueChanged(AdjustmentEvent evt) {
            	Adjustable source = evt.getAdjustable();
                if (evt.getValueIsAdjusting()) {
                	verticalvalue2 = p2Scroll.getVerticalScrollBar().getValue();
                	horizontalvalue2 = p2Scroll.getHorizontalScrollBar().getValue();
                	verticalvalue = pScroll.getVerticalScrollBar().getValue();
                	horizontalvalue = pScroll.getHorizontalScrollBar().getValue();
                    return;
                }
                int orient = source.getOrientation();
                if (orient == Adjustable.HORIZONTAL) {
                } else {
                }
                int type = evt.getAdjustmentType();
                switch (type) {
                  case AdjustmentEvent.UNIT_INCREMENT:
                      break;
                  case AdjustmentEvent.UNIT_DECREMENT:
                      break;
                  case AdjustmentEvent.BLOCK_INCREMENT:
                      break;
                  case AdjustmentEvent.BLOCK_DECREMENT:
                      break;
                  case AdjustmentEvent.TRACK:
                      break;
                }
                int value = evt.getValue();       
                if (editor.isFocusOwner() || pScroll.getVerticalScrollBar().isFocusOwner() || pScroll.getHorizontalScrollBar().isFocusOwner()){
                	p2Scroll.getVerticalScrollBar().setValue(verticalvalue2);
                	p2Scroll.getHorizontalScrollBar().setValue(horizontalvalue2);
                	verticalvalue = pScroll.getVerticalScrollBar().getValue();
            		horizontalvalue = pScroll.getHorizontalScrollBar().getValue();
                }
                if (editor2.isFocusOwner() || p2Scroll.getVerticalScrollBar().isFocusOwner() || p2Scroll.getHorizontalScrollBar().isFocusOwner()){
                    pScroll.getVerticalScrollBar().setValue(verticalvalue);
                    pScroll.getHorizontalScrollBar().setValue(horizontalvalue);
                    verticalvalue2 = p2Scroll.getVerticalScrollBar().getValue();
            		horizontalvalue2 = p2Scroll.getHorizontalScrollBar().getValue();
                }
            }
        }
        
        class Click1 implements MouseListener {

			public void mouseClicked(MouseEvent arg0) {
				editor.requestFocus();
			}

			public void mousePressed(MouseEvent arg0) {
				editor.requestFocus();		
			}

			public void mouseReleased(MouseEvent arg0) {
			}

			public void mouseEntered(MouseEvent arg0) {
			}

			public void mouseExited(MouseEvent arg0) {
			}
        	
        }
        
        class Click2 implements MouseListener {

			public void mouseClicked(MouseEvent arg0) {
				editor2.requestFocus();
			}

			public void mousePressed(MouseEvent arg0) {
				editor2.requestFocus();
			}

			public void mouseReleased(MouseEvent arg0) {
			}

			public void mouseEntered(MouseEvent arg0) {
			}

			public void mouseExited(MouseEvent arg0) {
			}
        	
        }
        
        class bClick1 implements MouseListener {

			public void mouseClicked(MouseEvent arg0) {
				editor.requestFocus();
			}

			public void mousePressed(MouseEvent arg0) {
				editor.requestFocus();
			}

			public void mouseReleased(MouseEvent arg0) {	
			}

			public void mouseEntered(MouseEvent arg0) {	
			}

			public void mouseExited(MouseEvent arg0) {	
			}
        } 
        
        class bClick2 implements MouseListener {

			public void mouseClicked(MouseEvent arg0) {
				editor2.requestFocus();
			}

			public void mousePressed(MouseEvent arg0) {
				editor2.requestFocus();
			}

			public void mouseReleased(MouseEvent arg0) {
			}

			public void mouseEntered(MouseEvent arg0) {
			}

			public void mouseExited(MouseEvent arg0) {
			}
        	
        } 
        
        class dobleClick implements MouseListener {
    		public void mouseClicked(MouseEvent evt) {
    			
    			if (evt.getClickCount() > 1) {
    				try {
    					int ini = getEditor().getCaretPosition();
						int n = matchingbraces.findMatchingBracket(getEditor().getDocument(),ini-1);
						if (n > -1){
							if (n > ini) seleccionaTexto(ini-1,n-ini+2);
							if (n < ini) seleccionaTexto(n,ini-n);
						}
					} catch (BadLocationException e) {
						e.printStackTrace();
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

			public void mousePressed(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void mouseReleased(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void mouseEntered(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void mouseExited(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}
        }

		public boolean isCompilerFile() {
			return compilerFile;
		}

		public void setCompilerFile(boolean compilerFile) {
			this.compilerFile = compilerFile;
		}

		public boolean isMainFile() {
			return mainFile;
		}

		public void setMainFile(boolean mainFile) {
			this.mainFile = mainFile;
		}
		
		public Icon getIcon() {
			return icon;
		}
		public void setIcon(Icon i) {
			icon = i;
		}

		
		
		
}