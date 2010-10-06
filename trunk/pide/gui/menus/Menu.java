package gui.menus;

import idioma.Idioma;
import javax.swing.*;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import javax.swing.undo.*;

import org.apache.log4j.Logger;

import operaciones.configuracion.DefaultConfiguration;
import operaciones.configuracion.Fich;
import operaciones.configuracion.ListaIconosEditables;
import operaciones.configuracion.LoadDefaultProject;
import operaciones.configuracion.MenuConfig;
import operaciones.fabrica.FactoriaES;
import operaciones.fabrica.FactoriaGUI;
import operaciones.fabrica.FactoriaOperaciones;
import operaciones.lexicas.Comments;
import operaciones.lexicas.DividerList;
import operaciones.lexicas.ListaTiposToken;
import operaciones.log.Log;
import operaciones.sintacticas.GrammarGenerator;
import principal.Acide;
import principal.almacenPropiedades;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Locale;
import java.util.ResourceBundle;
import gui.PleaseWaitWindow;
import gui.Ventana;
import gui.editor.CreadorEditor;
import gui.editor.Editor;
import gui.editor.SyntaxisDoc;
import gui.iconos.Iconos;
import gui.parametrizacion.EdicionIconosGUI;
import gui.parametrizacion.MenuGUI;
import gui.parametrizacion.SetPathsGUI;
import gui.parametrizacion.SintacticaGUI;
import es.texto.ExtFilter;
import es.bytes.ByteFile;
import es.configuracion.lenguajeprog.Lenguaje;
import es.texto.ExtensionesValidas;
import es.texto.Fichero;
import es.texto.FiltroFicheros;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.AbstractDocument.DefaultDocumentEvent;

import java.awt.dnd.DragSource;
import java.awt.dnd.DragGestureRecognizer;
import java.awt.dnd.DragGestureListener;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DragSourceListener;

/**
 * Clase que construye el interfaz de los menus
 * 
 */

public class Menu {

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	/**
	 * Atributo: menuBar: Establece una interfaz de menu
	 * 
	 */
	private JMenuBar menuBar;

	/**
	 * Resto de atributos del menu
	 * 
	 */

	private JMenuItem nuevoFich;

	private JMenuItem abrirFich;

	/**
	 * Salva Fichero Como
	 * 
	 */
	private JMenuItem salvarFich;

	/**
	 * Salva fichero Actual
	 * 
	 */
	private JMenuItem save;

	private JMenuItem salir;

	private JMenuItem lexmodify;

	private JMenuItem sintactica;

	private JMenu menu;

	private JMenuItem deshacer;

	private JMenuItem repetir;

	private JMenuItem buscar;

	private JMenuItem pegar;

	private JMenuItem copiar;

	private JMenuItem cortar;

	private JMenuItem reemplazar;

	private JMenuItem mostrarLog;

	private JMenuItem cargarParam;

	// private JMenuItem salvarParam;

	private JMenuItem compilador;

	private JMenuItem interprete;

	private JMenuItem parser;

	private JMenuItem español;

	private JMenuItem english;

	private JMenuItem nuevaConfLenguaje;

	private JMenuItem nuevoProyecto;

	private JMenu archivo;

	private JMenu edicion;

	private JMenu proyecto;

	private JMenu view;

	private JMenu parametrizacion;

	private JMenu idioma;

	private JMenu ayuda;

	private JMenu consola;

	private JMenuItem configurar;

	private JMenuItem comandoExterno;

	private JMenuItem compilar;

	private JMenuItem ejecutar;

	private JMenuItem depurar;

	private JMenuItem abrirProyecto;

	private JMenuItem guardarProyecto;

	// mig
	private JMenuItem newProjectFile;

	private JMenuItem anadirFichero;

	private JMenuItem mostrarAyuda;

	private JMenuItem acerca_de;

	private JMenu edicionIconos;

	private JMenuItem selectAll;

	private JMenuItem print;

	private JMenuItem saveAll;

	private JMenuItem removeFile;

	// mig
	private JMenuItem deleteFile;

	private JMenuItem addFolder;

	private JMenuItem removeFolder;

	private JCheckBoxMenuItem showBrowserCBox;

	private JMenuItem goTo;

	private JCheckBoxMenuItem showShellWindowCBox;

	private JMenu lexicSubMenu;

	private JMenu syntaxSubMenu;

	private JMenuItem loadSyntax;

	private JMenuItem modifySyntax;

	private JMenuItem closeProject;

	private JMenuItem closeFile;

	private JMenuItem closeAll;

	private JMenuItem compiler;

	private JMenuItem newLexical;

	private JMenuItem saveLexical;

	private JMenuItem saveGrammar;
	// mig
	private JMenuItem setCompilable;

	private JMenuItem unsetCompilable;

	private JMenuItem setMain;

	private JMenuItem unsetMain;
	// para el menu Archivo
	private JMenuItem setCompilable2;

	private JMenuItem unsetCompilable2;

	private JMenuItem setMain2;

	private JMenuItem unsetMain2;

	private JMenuItem removeFile2;

	private JMenuItem deleteFile2;

	private JMenuItem anadirFichero2;
	//
	private JMenuItem newMenu;

	private JMenuItem loadMenu;

	private JMenuItem modifyMenu;

	private JMenuItem saveMenu;

	private JMenuItem saveAsMenu;

	private JMenuItem newTB;

	private JMenuItem loadTB;

	private JMenuItem modifyTB;

	private JMenuItem saveTB;

	private JMenuItem saveAsTB;

	private JMenuItem saveAsGrammar;

	private JMenuItem saveAsLexical;

	private JMenuItem setPaths;

	// private JMenuItem javacPath;
	//	
	// private JMenuItem javaPath;
	//	
	// private JMenuItem jarPath;

	private JCheckBoxMenuItem autoSyntaxAnalysisCBox;

	private JMenuItem saveAsProject;

	// size shell
	private int tam_shell;

	// private JMenu menuSubMenu;

	private boolean shellIsFocus;

	private boolean isNPF;

	/**
	 * Atributo privado Usado para Deshacer /Rehacer
	 * 
	 */
	private UndoManager undo = new UndoManager();

	public Menu() {
		
		Idioma i = Idioma.getInstance();
		
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}

		final ResourceBundle labels = i.getLabels();
		logger.info(labels.getString("s68"));
		menuBar = new JMenuBar();
		// Menus
		archivo = new JMenu();
		edicion = new JMenu();
		proyecto = new JMenu();
		view = new JMenu();
		parametrizacion = new JMenu();
		idioma = new JMenu();
		ayuda = new JMenu();
		consola = new JMenu();
		// MenuItem
		nuevoFich = new JMenuItem();
		abrirFich = new JMenuItem();
		salvarFich = new JMenuItem();
		save = new JMenuItem();
		salir = new JMenuItem();
		compilar = new JMenuItem();
		ejecutar = new JMenuItem();
		depurar = new JMenuItem();
		lexmodify = new JMenuItem();
		sintactica = new JMenuItem();
		sintactica.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new SintacticaGUI(false);
			}
		});
		compilador = new JMenuItem();
		interprete = new JMenuItem();
		parser = new JMenuItem();
		menu = new JMenu();
		cargarParam = new JMenuItem();
		// salvarParam = new JMenuItem();
		nuevaConfLenguaje = new JMenuItem();
		nuevoProyecto = new JMenuItem();
		abrirProyecto = new JMenuItem();
		guardarProyecto = new JMenuItem();
		// mig
		newProjectFile = new JMenuItem();
		anadirFichero = new JMenuItem();
		deshacer = new JMenuItem();
		repetir = new JMenuItem();
		copiar = new JMenuItem();
		pegar = new JMenuItem();
		cortar = new JMenuItem();
		buscar = new JMenuItem();
		reemplazar = new JMenuItem();
		mostrarLog = new JMenuItem();
		español = new JMenuItem();
		english = new JMenuItem();
		mostrarAyuda = new JMenuItem();
		configurar = new JMenuItem();
		comandoExterno = new JMenuItem();
		acerca_de = new JMenuItem();
		print = new JMenuItem();
		edicionIconos = new JMenu();
		selectAll = new JMenuItem();
		saveAll = new JMenuItem();
		saveAll.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				Ventana v = Ventana.getInstance();
				// mig
				int eS = v.getCreadorEditor().getEditorSeleccionado();
				int editor = v.getCreadorEditor().dameNumEditores();
				for (int i = 0; i < editor; i++) {
					v.getCreadorEditor().setEditorSeleccionado(i);
					save_o_saveAS();
				}
				v.getCreadorEditor().setEditorSeleccionado(eS);
			}
		});
		removeFile = new JMenuItem();
		deleteFile = new JMenuItem();
		removeFile2 = new JMenuItem();
		deleteFile2 = new JMenuItem();
		anadirFichero2 = new JMenuItem();

		addFolder = new JMenuItem();
		addFolder.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				String file = "";
				String cad = JOptionPane.showInputDialog(null, labels
						.getString("s656"));
				Ventana v = Ventana.getInstance();
				if (cad != null) {
					TreePath path = v.getNuevoExplorador().getArbol()
							.getSelectionPath();
					DefaultMutableTreeNode filePath;
					Fich fd;
					// mig
					if (path != null) {// hay carpeta seleccionada
						filePath = (DefaultMutableTreeNode) path
								.getLastPathComponent();
						fd = (Fich) filePath.getUserObject();

						if (!fd.isDirectory()) {// hay archivo seleccionado
							filePath = v.getNuevoExplorador().getRaiz()
									.getNextNode();
							fd = (Fich) filePath.getUserObject();
						}

					} else {// no hay carpeta seleccionada
						filePath = v.getNuevoExplorador().getRaiz()
								.getNextNode();
						fd = (Fich) filePath.getUserObject();
					}

					// if (fd.isDirectory()){
					Fich f = new Fich();
					f.setPath(cad);
					f.setName(cad);
					f.setPadre(fd.getName());
					f.setDirectory(true);
					v.getProyecto().setfich(f);
					DefaultMutableTreeNode def = new DefaultMutableTreeNode(f);
					def.setAllowsChildren(true);
					filePath.add(def);
					filePath.setAllowsChildren(true);
					v.getNuevoExplorador().getTreeModel().reload();
					v.getNuevoExplorador().expandTree();
					v.getProyecto().setModified(true);
					// }
				}
			}
		});
		removeFolder = new JMenuItem();
		removeFolder.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				Idioma i = Idioma.getInstance();
				try {
					i.seleccionIdioma(Integer.parseInt(almacenPropiedades
							.getPropiedad("idioma")));
				} catch (Exception e) {
					e.printStackTrace();
				}
				ResourceBundle labels = i.getLabels();
				Ventana v = Ventana.getInstance();
				int opcion = JOptionPane.showConfirmDialog(null, labels
						.getString("s654"));
				if (opcion == JOptionPane.OK_OPTION) {
					// mig
					v.getProyecto().setModified(true);
					// logger.info(labels.getString("s77"));
					Toolkit toolkit = Toolkit.getDefaultToolkit();
					TreePath currentSelection = v.getNuevoExplorador()
							.getArbol().getSelectionPath();
					if (currentSelection != null) {
						DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
								.getLastPathComponent());
						Fich p = (Fich) currentNode.getUserObject();
						if (p.isDirectory()) {
							MutableTreeNode parent = (MutableTreeNode) (currentNode
									.getParent());
							if (parent != null) {
								v.getNuevoExplorador().getTreeModel()
										.removeNodeFromParent(currentNode);
								toolkit.beep();
								ArrayList<String> contRemove = new ArrayList<String>();
								String fileRemove = "";
								if ((currentNode.getDepth() <= 2)
										&& (p.getName().equals(v.getProyecto()
												.getnombreProy()))) {
									v.getNuevoExplorador().getAddFile()
											.setEnabled(false);
									v.getNuevoExplorador().getSaveProj()
											.setEnabled(false);
									v.getNuevoExplorador().getRemoveFile()
											.setEnabled(false);
									v.getNuevoExplorador().getDeleteFile()
											.setEnabled(false);
									v.setTitle(labels.getString("s425")
											+ " - <empty>");
									Fichero f = new Fichero();

									f.salvar(".//configuration/file_acidePrj",
											"<EMPTY>");
									v.validate();
									v.repaint();
									v.getProyecto().setnombreProy("");
									almacenPropiedades
											.setPropiedad("DefaultAcidePrj",
													".//configuration/Default.acidePrj");
								}

								int cont = -1;
								for (int j = 0; j < v.getProyecto()
										.dameNumFich(); j++) {
									if (!p.getName().equals(
											v.getProyecto().getnombreProy())) {
										if (v.getProyecto().getfich(j)
												.getName().equals(p.getName())) {
											cont = j;

										} else if (v.getProyecto().getfich(j)
												.getPadre().equals(p.getName())) {
											if (v.getProyecto().getfich(j)
													.isDirectory() != true) {
												contRemove.add(v.getProyecto()
														.getfich(j).getPath());
												if (v.getProyecto()
														.dameNumFich() != 1)
													v.getProyecto().removeFich(
															j);
												else
													v.getProyecto().removeFich(
															0);
											} else {
												String dir = v.getProyecto()
														.getfich(j).getName();
												for (int k = j + 1; k < v
														.getProyecto()
														.dameNumFich(); k++) {
													if (v.getProyecto()
															.getfich(j)
															.getPadre().equals(
																	dir)) {
														contRemove.add(v
																.getProyecto()
																.getfich(k)
																.getPath());

														if (v.getProyecto()
																.dameNumFich() != 1)
															v
																	.getProyecto()
																	.removeFich(
																			k);
														else
															v
																	.getProyecto()
																	.removeFich(
																			0);

													}
												}
											}

										}
									}
								}
								if (cont != -1)
									if (v.getProyecto().dameNumFich() != 1)
										v.getProyecto().removeFich(cont);
									else
										v.getProyecto().removeFich(0);
								int op = JOptionPane.showConfirmDialog(null,
										labels.getString("s655"));
								if (op == JOptionPane.OK_OPTION) {

									for (int j = 0; j < contRemove.size(); j++) {
										File fi = new File(contRemove.get(j));
										if (fi.isFile())
											fi.delete();
									}

								} else
									v.getStatusBar()
											.setMessage("Option cancel");

								return;

							}
							toolkit.beep();
						}

					} else if ((opcion == JOptionPane.NO_OPTION)
							|| (opcion == JOptionPane.CANCEL_OPTION)) {
						// Muestra resultado operacion en el log
						// logger.info(labels.getString("s78"));

					}
					//System.out.println(v.getProyecto().dameNumFich());
					v.getProyecto().setNumFich(
							Integer.toString(v.getProyecto().dameNumFich()));
					if (v.getProyecto().dameNumFich() > 0) {
						v.getNuevoExplorador().setEnabledRemoveFile();
						v.getNuevoExplorador().setEnabledDeleteFile();
					} else {
						v.getNuevoExplorador().getRemoveFile()
								.setEnabled(false);
						v.getNuevoExplorador().getDeleteFile()
								.setEnabled(false);
					}
				}
			}
		});
		showBrowserCBox = new JCheckBoxMenuItem(labels.getString("s221"));
		showBrowserCBox.setSelected(true);
		showBrowserCBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				Ventana v = Ventana.getInstance();
				if (showBrowserCBox.isSelected()) {
					v.getNuevoExplorador().showExplorer();
				} else
					v.getNuevoExplorador().disposeExplorer();
				String prj = null;
				try {
					prj = almacenPropiedades.getPropiedad("DefaultAcidePrj");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if (!(prj.equals(".//configuration/Default.acidePrj") && v
						.getProyecto().getnombreProy().equals(""))) {
					v.getProyecto().setModified(true);
				}
			}
		});
		showShellWindowCBox = new JCheckBoxMenuItem(labels.getString("s223"));
		showShellWindowCBox.setSelected(true);
		showShellWindowCBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				Ventana v = Ventana.getInstance();
				if (showShellWindowCBox.isSelected()) {
					v.getSplitPaneH().setDividerLocation(tam_shell);
					v.getSplitPaneH().getBottomComponent().setVisible(true);
				} else {
					tam_shell = v.getSplitPaneH().getDividerLocation();
					v.getSplitPaneH().setDividerLocation(0);
					v.getSplitPaneH().getBottomComponent().setVisible(false);
				}
				String prj = null;
				try {
					prj = almacenPropiedades.getPropiedad("DefaultAcidePrj");
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if (!(prj.equals(".//configuration/Default.acidePrj") && v
						.getProyecto().getnombreProy().equals(""))) {
					v.getProyecto().setModified(true);
				}

			}
		});

		goTo = new JMenuItem();
		lexicSubMenu = new JMenu();
		syntaxSubMenu = new JMenu();
		loadSyntax = new JMenuItem();
		loadSyntax.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				SintacticaGUI.loadGrammarGUI();
			}
		});
		modifySyntax = new JMenuItem();
		modifySyntax.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new SintacticaGUI(true);
			}
		});
		closeProject = new JMenuItem();
		closeFile = new JMenuItem();
		closeFile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();
				int editor = v.getCreadorEditor().getEditorSeleccionado();
				if (v.getCreadorEditor().isRedButton() == true) {
					int opt = JOptionPane.showConfirmDialog(null, labels
							.getString("s643"));

					if (opt == JOptionPane.OK_OPTION) {

						if (v.getCreadorEditor().EditorSeleccionado().getPath()
								.equals(labels.getString("s79")) == true) {

							FactoriaES fac = FactoriaES.getInstance();
							Fichero f = fac.generaFichero();
							String archivo = " ";
							archivo = f.escribir();
							if (archivo.equals(" ")) {
								logger.info(labels.getString("s92"));
							} else {
								boolean resultado = f.salvar(archivo, v
										.getCreadorEditor()
										.EditorSeleccionado().getTexto());
								// Muestra en el log
								if (resultado) {
									logger
											.info(labels.getString("s93")
													+ archivo
													+ labels.getString("s94"));
									Ventana.getInstance().getCreadorEditor()
											.greenButton();
									v.getCreadorEditor().dameEditorI(editor)
											.setPath(archivo);
									v.getCreadorEditor().dameEditorI(editor)
											.setToolTipText(archivo);
									int in = archivo.lastIndexOf("\\");
									in++;
									String file = archivo.substring(in, archivo
											.length());
									v.getCreadorEditor().dameEditorI(editor)
											.setName(file);
									// mig
									File fich = new File(v.getCreadorEditor()
											.EditorSeleccionado().getPath());
									Ventana.getInstance().getCreadorEditor()
											.EditorSeleccionado()
											.setUltimoCambio(
													fich.lastModified());
									Ventana.getInstance().getCreadorEditor()
											.EditorSeleccionado().setUltimoTam(
													fich.length());
									v.getStatusBar().setMessage("");
								} else {
									logger.info(labels.getString("s95")
											+ archivo);
								}
							}

						}
						else {
							save.setEnabled(true);
							save.doClick();
							v.getStatusBar().setMessage("");
						}

						// mig
						for (int i = 0; i < v.getProyecto().getFichSize(); i++) {
							if (v.getProyecto().getfich(i).getPath().equals(
									v.getCreadorEditor().dameEditorI(editor)
											.getPath())) {
								v.getProyecto().getfich(i).setOpened(false);
							}
						}
						String prj = null;
						try {
							prj = almacenPropiedades
									.getPropiedad("DefaultAcidePrj");
						} catch (Exception e1) {
							// TODO Auto-generated catch block
							e1.printStackTrace();
						}
						if (!(prj.equals(".//configuration/Default.acidePrj") && v
								.getProyecto().getnombreProy().equals(""))) {
							v.getProyecto().setModified(true);
						}
						v.getCreadorEditor().getPane().remove(editor);

					} else if (opt == JOptionPane.NO_OPTION){
						v.getCreadorEditor().getPane().remove(editor);
						v.getStatusBar().setMessage("");}
				} else {
					// mig
					for (int i = 0; i < v.getProyecto().getFichSize(); i++) {
						if (v.getProyecto().getfich(i).getPath().equals(
								v.getCreadorEditor().dameEditorI(editor)
										.getPath())) {
							v.getProyecto().getfich(i).setOpened(false);
						}
					}
					String prj = null;
					try {
						prj = almacenPropiedades
								.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
							e1.printStackTrace();
					}
					if (!(prj.equals(".//configuration/Default.acidePrj") && v
							.getProyecto().getnombreProy().equals(""))) {
						v.getProyecto().setModified(true);
					}
					v.getCreadorEditor().getPane().remove(editor);
					v.getStatusBar().setMessage("");
				}
				if (v.getCreadorEditor().getPane().getTabCount() == 0) {
					deshabilitaMenuArchivo();
					deshabilitaMenuEdicion();
				}
			}
		});

		closeAll = new JMenuItem();
		closeAll.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();
				int editor = v.getCreadorEditor().dameNumEditores();
				v.getCreadorEditor().setEditorSeleccionado(editor - 1);
				for (int i = editor - 1; i >= 0; i--) {
					v.getCreadorEditor().setEditorSeleccionado(i);
					if (v.getCreadorEditor().isRedButton() == true) {
						int opt = JOptionPane.showConfirmDialog(null, labels
								.getString("s643"));

						if (opt == JOptionPane.OK_OPTION) {

							if (v.getCreadorEditor().EditorSeleccionado()
									.getPath().equals(labels.getString("s79")) == true) {

								FactoriaES fac = FactoriaES.getInstance();
								Fichero f = fac.generaFichero();
								String archivo = " ";
								archivo = f.escribir();
								if (archivo.equals(" ")) {
									logger.info(labels.getString("s92"));
								} else {
									boolean resultado = f.salvar(archivo, v
											.getCreadorEditor()
											.EditorSeleccionado().getTexto());
									// Muestra en el log
									if (resultado) {
										logger.info(labels.getString("s93")
												+ archivo
												+ labels.getString("s94"));
										Ventana.getInstance()
												.getCreadorEditor()
												.greenButton();
										v.getCreadorEditor().dameEditorI(i)
												.setPath(archivo);
										v.getCreadorEditor().dameEditorI(i)
												.setToolTipText(archivo);
										int in = archivo.lastIndexOf("\\");
										in++;
										String file = archivo.substring(in,
												archivo.length());
										v.getCreadorEditor().dameEditorI(i)
												.setName(file);
										// mig
										File fich = new File(v
												.getCreadorEditor()
												.EditorSeleccionado().getPath());
										Ventana.getInstance()
												.getCreadorEditor()
												.EditorSeleccionado()
												.setUltimoCambio(
														fich.lastModified());
										Ventana.getInstance()
												.getCreadorEditor()
												.EditorSeleccionado()
												.setUltimoTam(fich.length());

									} else {
										logger.info(labels.getString("s95")
												+ archivo);
									}
								}

							} else {
								save.setEnabled(true);
								save.doClick();
							}

							String prj = null;
							try {
								prj = almacenPropiedades
										.getPropiedad("DefaultAcidePrj");
							} catch (Exception e1) {
								// TODO Auto-generated catch block
								e1.printStackTrace();
							}
							if (!(prj
									.equals(".//configuration/Default.acidePrj") && v
									.getProyecto().getnombreProy().equals(""))) {
								v.getProyecto().setModified(true);
							}

							// mig
							for (int z = 0; z < v.getProyecto().getFichSize(); z++) {
								if (v.getProyecto().getfich(z).getPath()
										.equals(
												v.getCreadorEditor()
														.dameEditorI(i)
														.getPath())) {
									v.getProyecto().getfich(z).setOpened(false);
								}
							}
						} else {
							if (opt == JOptionPane.CANCEL_OPTION) {
								return;
							}
						}
					}
					// mig
					for (int z = 0; z < v.getProyecto().getFichSize(); z++) {
						if (v.getProyecto().getfich(z).getPath().equals(
								v.getCreadorEditor().dameEditorI(i).getPath())) {
							v.getProyecto().getfich(z).setOpened(false);
						}
					}

				}
				for (int i = 0; i < editor; i++) {
					v.getCreadorEditor().setEditorSeleccionado(0);
					v.getCreadorEditor().getPane().remove(0);
					v.getCreadorEditor().getPane().validate();
				}

				deshabilitaMenuArchivo();
				deshabilitaMenuEdicion();
			}
		});
		compiler = new JMenuItem();
		compiler.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				FactoriaGUI factGUI = FactoriaGUI.getInstance();

				factGUI.buildCompilerGUI();

			}
		});
		newLexical = new JMenuItem();
		saveLexical = new JMenuItem();
		saveGrammar = new JMenuItem();
		saveGrammar.setEnabled(false);
		saveGrammar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				SintacticaGUI.saveGrammarGUI();
			}
		});
		// mig
		// para archivo
		setCompilable2 = new JMenuItem();
		setCompilable2.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();

				if (!v.getCreadorEditor().EditorSeleccionado().isCompilerFile()
						|| (v.getCreadorEditor().EditorSeleccionado()
								.isCompilerFile() && v.getCreadorEditor()
								.EditorSeleccionado().isMainFile())) {

					String prj = null;
					try {
						prj = almacenPropiedades.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if ((prj.equals(".//configuration/Default.acidePrj") && v
							.getProyecto().getnombreProy().equals(""))) {
						// no hay proyecto
						v.getCreadorEditor().EditorSeleccionado()
								.setCompilerFile(true);
						if (v.getCreadorEditor().EditorSeleccionado()
								.isMainFile())
							v.getCreadorEditor().EditorSeleccionado()
									.setMainFile(false);
						for (int i = 0; i < v.getProyecto().dameNumFich(); i++) {
							if (v.getProyecto().getfich(i).getPath().equals(
									v.getCreadorEditor().EditorSeleccionado()
											.getPath()))
								v.getProyecto().getfich(i).setSetFile(true);
							if (v.getProyecto().getfich(i).isMainFile())
								v.getProyecto().getfich(i).setMainFile(false);
						}
						// poner icono en tab
						v.getCreadorEditor().getPane().setIconAt(
								v.getCreadorEditor().getEditorSeleccionado(),
								new ImageIcon("./Iconos/compilable.PNG"));
						// status bar
						v.getStatusBar().setMessage(
								v.getCreadorEditor().EditorSeleccionado()
										.getPath()
										+ " <COMPILABLE>");
					} else {// hay proyecto

						v.getCreadorEditor().EditorSeleccionado().setCompilerFile(true);
						if (v.getCreadorEditor().EditorSeleccionado().isMainFile())
							v.getCreadorEditor().EditorSeleccionado().setMainFile(false);
						v.getProyecto().setModified(true);
						// status
						v.getStatusBar().setMessage(
								v.getCreadorEditor().EditorSeleccionado()
										.getPath()
										+ " <COMPILABLE>");

						for (int i = 0; i < v.getProyecto().dameNumFich(); i++) {
							if (v.getProyecto().getfich(i).getPath().equals(
									v.getCreadorEditor().EditorSeleccionado()
											.getPath())) {
								v.getProyecto().getfich(i).setSetFile(true);
								if (v.getProyecto().getfich(i).isMainFile())
									v.getProyecto().getfich(i).setMainFile(
											false);
								// poner icono en tab
								v.getCreadorEditor().getPane().setIconAt(
												v.getCreadorEditor().getEditorSeleccionado(),
												new ImageIcon("./Iconos/compilable.PNG"));
							}
						}
					}
				}
			}
		});
		// para archivo
		unsetCompilable2 = new JMenuItem();
		unsetCompilable2.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();

				if (v.getCreadorEditor().EditorSeleccionado().isCompilerFile()
						&& !v.getCreadorEditor().EditorSeleccionado()
								.isMainFile()) {

					String prj = null;
					try {
						prj = almacenPropiedades
								.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if ((prj.equals(".//configuration/Default.acidePrj") && v
							.getProyecto().getnombreProy().equals(""))) {
						// no hay proyecto
						v.getCreadorEditor().EditorSeleccionado()
								.setCompilerFile(false);
						// quitar status
						v.getStatusBar().setMessage(
								v.getCreadorEditor().EditorSeleccionado()
										.getPath());
						// quitar icono en tab
						v.getCreadorEditor().getPane().setIconAt(
								v.getCreadorEditor().getEditorSeleccionado(),
								null);

						for (int i = 0; i < v.getProyecto().dameNumFich(); i++) {
							if (v.getProyecto().getfich(i).getPath().equals(
									v.getCreadorEditor().EditorSeleccionado()
											.getPath()))
								v.getProyecto().getfich(i).setSetFile(false);
						}
					} else {// hay proyecto

						v.getCreadorEditor().EditorSeleccionado().setCompilerFile(false);
						v.getProyecto().setModified(true);
						// quitar status
						v.getStatusBar().setMessage(
								v.getCreadorEditor().EditorSeleccionado()
										.getPath());
						// quitar icono en tab
						v.getCreadorEditor().getPane().setIconAt(
								v.getCreadorEditor().getEditorSeleccionado(),
								null);
						for (int i = 0; i < v.getProyecto().dameNumFich(); i++) {
							if (v.getProyecto().getfich(i).getPath().equals(
									v.getCreadorEditor().EditorSeleccionado()
											.getPath()))
								v.getProyecto().getfich(i).setSetFile(false);
						}
					}
				}
			}
		});
		// mig
		// para proyecto
		setMain = new JMenuItem();
		setMain.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();

				TreePath path = v.getNuevoExplorador().getArbol()
						.getSelectionPath();
				DefaultMutableTreeNode filePath;
				Fich fc;
				// mig

				if (path != null) {// hay carpeta seleccionada

					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					fc = (Fich) filePath.getUserObject();

					if (!fc.isMainFile()) {// se puede aplicar

						if (!fc.isDirectory()) {// hay archivo seleccionado

							for (int i = 0; i < v.getProyecto().getFichSize(); i++) {
								// quitar main anterior
								if (v.getProyecto().getfich(i).isMainFile()) {
									v.getProyecto().getfich(i).setMainFile(false);
									v.getProyecto().getfich(i).setSetFile(false);
									for (int j = 0; j < v.getCreadorEditor()
											.dameNumEditores(); j++) {
										if (v.getCreadorEditor().dameEditorI(j)
												.getPath().equals(
														v.getProyecto()
																.getfich(i)
																.getPath()))
											v.getCreadorEditor().getPane()
													.setIconAt(j, null);
									}
								}
							}

							fc.setMainFile(true);
							fc.setSetFile(true);
							v.getProyecto().setModified(true);

							v.getStatusBar().setMessage(fc.getPath() + " <MAIN>");

							// poner icono en tab
							for (int j = 0; j < v.getCreadorEditor()
									.dameNumEditores(); j++) {
								if (v.getCreadorEditor().dameEditorI(j)
										.getPath().equals(fc.getPath())) {
									v.getCreadorEditor().getPane().setIconAt(j,
											new ImageIcon("./Iconos/main.PNG"));
								}
							}
						}
					}
				} else {// no hay nada seleccionado
					// si no hay proyecto
					String prj = null;
					try {
						prj = almacenPropiedades
								.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if ((prj.equals(".//configuration/Default.acidePrj") && v
							.getProyecto().getnombreProy().equals(""))) {
						int editor = v.getCreadorEditor().dameNumEditores();
						if (editor > 0) {
							setMain2.doClick();
						}
					}
				}

			}
		});
		// mig
		// para proyecto
		unsetMain = new JMenuItem();
		unsetMain.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();

				TreePath path = v.getNuevoExplorador().getArbol()
						.getSelectionPath();
				DefaultMutableTreeNode filePath;
				Fich fc;
				// mig
				if (path != null) {// hay carpeta seleccionada

					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					fc = (Fich) filePath.getUserObject();

					if (fc.isMainFile()) {// si se puede aplicar

						if (!fc.isDirectory()) {// hay archivo seleccionado

							fc.setMainFile(false);
							fc.setSetFile(false);
							v.getProyecto().setModified(true);
							// quitar status
							v.getStatusBar().setMessage(fc.getPath());
							// quitar icono en tab
							for (int j = 0; j < v.getCreadorEditor()
									.dameNumEditores(); j++) {
								if (v.getCreadorEditor().dameEditorI(j)
										.getPath().equals(fc.getPath())) {
									v.getCreadorEditor().getPane().setIconAt(j,
											null);
								}
							}
						}
					}
				} else {// no hay nada seleccionado
					// si no hay proyecto
					String prj = null;
					try {
						prj = almacenPropiedades
								.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if ((prj.equals(".//configuration/Default.acidePrj") && v
							.getProyecto().getnombreProy().equals(""))) {
						int editor = v.getCreadorEditor().dameNumEditores();
						if (editor > 0) {
							unsetMain2.doClick();
						}
					}
				}

			}
		});
		// mig
		// para proyecto
		setCompilable = new JMenuItem();
		setCompilable.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();

				TreePath path = v.getNuevoExplorador().getArbol()
						.getSelectionPath();
				DefaultMutableTreeNode filePath;
				Fich fc;
				// mig
				if (path != null) {// hay carpeta seleccionada

					filePath = (DefaultMutableTreeNode) path.getLastPathComponent();
					fc = (Fich) filePath.getUserObject();

					if (!fc.isSetFile() || (fc.isSetFile() && fc.isMainFile())) {// si
																					// se
																					// puede
																					// aplicar

						if (!fc.isDirectory()) {// hay archivo seleccionado

							if (fc.isMainFile()) fc.setMainFile(false);

							fc.setSetFile(true);
							v.getProyecto().setModified(true);

							// poner icono en tab
							for (int j = 0; j < v.getCreadorEditor()
									.dameNumEditores(); j++) {
								if (v.getCreadorEditor().dameEditorI(j)
										.getPath().equals(fc.getPath())) {
									v.getCreadorEditor().getPane().setIconAt(
											j,new ImageIcon("./Iconos/compilable.PNG"));
							// status
							v.getStatusBar().setMessage(v.getCreadorEditor().dameEditorI(j)
													.getPath()+ " <COMPILABLE>");
								}
							}
						}
					}
				} else {// no hay nada seleccionado
					// si no hay proyecto
					String prj = null;
					try {
						prj = almacenPropiedades
								.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if ((prj.equals(".//configuration/Default.acidePrj") && v
							.getProyecto().getnombreProy().equals(""))) {
						int editor = v.getCreadorEditor().dameNumEditores();
						if (editor > 0) {
							setCompilable2.doClick();
						}
					}
				}
			}
		});
		// para proyecto
		unsetCompilable = new JMenuItem();
		unsetCompilable.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();

				TreePath path = v.getNuevoExplorador().getArbol()
						.getSelectionPath();
				DefaultMutableTreeNode filePath;
				Fich fc;
				// mig
				if (path != null) {// hay carpeta seleccionada

					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					fc = (Fich) filePath.getUserObject();

					if (fc.isSetFile() && !fc.isMainFile()) {// se puede
																// aplicar

						if (!fc.isDirectory()) {// hay archivo seleccionado

							fc.setSetFile(false);
							v.getProyecto().setModified(true);
							// quitar status
							v.getStatusBar().setMessage(fc.getPath());
							// quitar icono en tab
							for (int j = 0; j < v.getCreadorEditor()
									.dameNumEditores(); j++) {
								if (v.getCreadorEditor().dameEditorI(j)
										.getPath().equals(fc.getPath())) {
									v.getCreadorEditor().getPane().setIconAt(j,
											null);
								}
							}
						}
					}
				} else {// no hay nada seleccionado
					// si no hay proyecto
					String prj = null;
					try {
						prj = almacenPropiedades
								.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if ((prj.equals(".//configuration/Default.acidePrj") && v
							.getProyecto().getnombreProy().equals(""))) {
						int editor = v.getCreadorEditor().dameNumEditores();
						if (editor > 0) {
							unsetCompilable2.doClick();
						}
					}
				}
			}
		});
		// para archivo suelto
		setMain2 = new JMenuItem();
		setMain2.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();

				if (!v.getCreadorEditor().EditorSeleccionado().isMainFile()) {

					String prj = null;
					try {
						prj = almacenPropiedades.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if ((prj.equals(".//configuration/Default.acidePrj") && v
							.getProyecto().getnombreProy().equals(""))) {
						// no hay proyecto
						// quitar main anterior
						for (int i = 0; i < v.getCreadorEditor().dameNumEditores(); i++) {
							if (v.getCreadorEditor().dameEditorI(i).isMainFile()) {
								v.getCreadorEditor().dameEditorI(i)
										.setMainFile(false);
								v.getCreadorEditor().dameEditorI(i)
										.setCompilerFile(false);
								v.getStatusBar().setMessage(
										v.getCreadorEditor().dameEditorI(i)
												.getPath());
								v.getCreadorEditor().getPane().setIconAt(i,
										null);
							}
						}

						v.getCreadorEditor().EditorSeleccionado().setMainFile(
								true);
						v.getCreadorEditor().EditorSeleccionado()
								.setCompilerFile(true);
						// status
						v.getStatusBar().setMessage(
								v.getCreadorEditor().EditorSeleccionado()
										.getPath()
										+ " <MAIN>");
						// poner icono en tab
						v.getCreadorEditor().getPane().setIconAt(
								v.getCreadorEditor().getEditorSeleccionado(),
								new ImageIcon("./Iconos/main.PNG"));
					} else {// hay proyecto
						// quitar main anterior
						for (int i = 0; i < v.getCreadorEditor()
								.dameNumEditores(); i++) {
							if (v.getCreadorEditor().dameEditorI(i)
									.isMainFile()) {
								v.getCreadorEditor().dameEditorI(i)
										.setMainFile(false);
								v.getCreadorEditor().dameEditorI(i)
										.setCompilerFile(false);
								v.getStatusBar().setMessage(
										v.getCreadorEditor().dameEditorI(i)
												.getPath());
								v.getCreadorEditor().getPane().setIconAt(i,
										null);
							}
						}

						v.getCreadorEditor().EditorSeleccionado().setMainFile(
								true);
						v.getCreadorEditor().EditorSeleccionado()
								.setCompilerFile(true);
						v.getStatusBar().setMessage(
								v.getCreadorEditor().EditorSeleccionado()
										.getPath()
										+ "  <MAIN>");

						for (int i = 0; i < v.getProyecto().dameNumFich(); i++) {
							if (v.getProyecto().getfich(i).getPath().equals(
									v.getCreadorEditor().EditorSeleccionado()
											.getPath())) {

								for (int j = 0; j < v.getProyecto()
										.getFichSize(); j++) {

									if (v.getProyecto().getfich(j).isMainFile()) {
										v.getProyecto().getfich(j).setMainFile(
												false);
										v.getProyecto().getfich(j).setSetFile(
												false);
										for (int z = 0; z < v
												.getCreadorEditor()
												.dameNumEditores(); z++) {
											if (v.getCreadorEditor()
													.dameEditorI(z).getPath()
													.equals(
															v.getProyecto()
																	.getfich(j)
																	.getPath()))
												v.getCreadorEditor().getPane()
														.setIconAt(z, null);
										}
									}
								}

								v.getProyecto().getfich(i).setMainFile(true);
								v.getProyecto().getfich(i).setSetFile(true);
								v.getProyecto().setModified(true);

								// poner icono en el tab
								v.getCreadorEditor().getPane().setIconAt(
										v.getCreadorEditor()
												.getEditorSeleccionado(),
										new ImageIcon("./Iconos/main.PNG"));

							}
						}

					}
				}
			}
		});
		// para archivo suelto
		unsetMain2 = new JMenuItem();
		unsetMain2.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Ventana v = Ventana.getInstance();

				if (v.getCreadorEditor().EditorSeleccionado().isMainFile()) {// si
																				// se
																				// puede
																				// aplicar

					String prj = null;
					try {
						prj = almacenPropiedades
								.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if ((prj.equals(".//configuration/Default.acidePrj") && v
							.getProyecto().getnombreProy().equals(""))) {
						// no hay proyecto
						v.getCreadorEditor().EditorSeleccionado().setMainFile(
								false);
						// quitar status
						v.getStatusBar().setMessage(
								v.getCreadorEditor().EditorSeleccionado()
										.getPath());
						// quitar icono en tab
						v.getCreadorEditor().getPane().setIconAt(
								v.getCreadorEditor().getEditorSeleccionado(),
								null);
					} else {// hay proyecto

						v.getCreadorEditor().EditorSeleccionado().setMainFile(false);
						v.getProyecto().setModified(true);
						// quitar status
						v.getStatusBar().setMessage(
								v.getCreadorEditor().EditorSeleccionado()
										.getPath());
						// quitar icono en tab
						v.getCreadorEditor().getPane().setIconAt(
								v.getCreadorEditor().getEditorSeleccionado(),
								null);
						for (int i = 0; i < v.getProyecto().dameNumFich(); i++) {
							if (v.getProyecto().getfich(i).getPath().equals(
									v.getCreadorEditor().EditorSeleccionado()
											.getPath()))
								v.getProyecto().getfich(i).setMainFile(false);
						}
					}
				}
			}
		});
		newMenu = new JMenuItem();
		newMenu.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new MenuGUI(false);
			}
		});

		loadMenu = new JMenuItem();
		loadMenu.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser chooser = new JFileChooser();
				FiltroFicheros filtro = new FiltroFicheros(labels
						.getString("s287"));
				filtro.addExtension("menuCfg");
				chooser.setFileFilter(filtro);
				chooser.setCurrentDirectory(new File(".//configuration/menu/"));
				int option = chooser.showOpenDialog(null);
				if (option == JFileChooser.APPROVE_OPTION) {
					String menuFile = chooser.getSelectedFile()
							.getAbsolutePath();
					boolean[] valores = null;
					try {
						valores = MenuConfig.cargarMenuCfgFich(menuFile);
						almacenPropiedades.setPropiedad("currentMenuCfg",
								menuFile);
						MenuConfig.setAll(valores);
						Ventana v = Ventana.getInstance();
						v.getnuevoMenu().setMenuConfig();
						v.validate();
						v.repaint();
						saveMenu.setEnabled(false);
						logger.info(labels.getString("s289"));
						MenuGUI.setChangesSaved(true);
						// mig
						String prj = null;
						try {
							prj = almacenPropiedades
									.getPropiedad("DefaultAcidePrj");
						} catch (Exception e1) {
							// TODO Auto-generated catch block
							e1.printStackTrace();
						}
						if (!(prj.equals(".//configuration/Default.acidePrj") && v
								.getProyecto().getnombreProy().equals(""))) {
							Ventana.getInstance().getProyecto().setModified(
									true);
						}

					} catch (Exception e1) {
						JOptionPane.showMessageDialog(null, labels
								.getString("s288")
								+ " " + menuFile, labels.getString("289"),
								JOptionPane.ERROR_MESSAGE);
						logger.error(labels.getString("s288") + " " + menuFile);
					}
				} else if (option == JFileChooser.CANCEL_OPTION) {
					logger.info(labels.getString("s290"));
				}
			}
		});
		modifyMenu = new JMenuItem();
		modifyMenu.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new MenuGUI(true);
			}
		});
		saveMenu = new JMenuItem();
		saveMenu.setEnabled(false);
		saveMenu.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					String previous = almacenPropiedades
							.getPropiedad("previousMenuCfg");
					String current = almacenPropiedades
							.getPropiedad("currentMenuCfg");
					ByteFile.copy(current, previous);
					almacenPropiedades.setPropiedad("currentMenuCfg", previous);
					saveMenu.setEnabled(false);
					MenuGUI.setChangesSaved(true);
				} catch (Exception e1) {
					JOptionPane.showMessageDialog(null, e1.getMessage(), labels
							.getString("s293"), JOptionPane.ERROR_MESSAGE);
					logger.error(e1.getMessage());
				}
			}
		});
		saveAsMenu = new JMenuItem();
		saveAsMenu.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					String currentMenu = almacenPropiedades
							.getPropiedad("currentMenuCfg");
					JFileChooser selector = new JFileChooser();
					FiltroFicheros filtro = new FiltroFicheros(labels
							.getString("s126"));
					filtro.addExtension("menuCfg");
					selector.setFileFilter(filtro);
					selector.setCurrentDirectory(new File(
							".//configuration/menu/"));
					String nombreFichero = "";
					int valor = selector.showSaveDialog(selector);
					if (valor == JFileChooser.APPROVE_OPTION) {
						nombreFichero = selector.getSelectedFile()
								.getAbsolutePath();
						if (!nombreFichero.endsWith(".menuCfg"))
							nombreFichero += ".menuCfg";
						ByteFile.copy(currentMenu, nombreFichero);
						almacenPropiedades.setPropiedad("currentMenuCfg",
								nombreFichero);
						saveMenu.setEnabled(false);
						MenuGUI.setChangesSaved(true);
						// Muestra operacion en el log
						logger.info(labels.getString("s528") + nombreFichero
								+ labels.getString("s529"));
					} else if (valor == JFileChooser.CANCEL_OPTION) {
						selector.cancelSelection();
						// Muestra operacion en el log
						logger.info(labels.getString("s527"));
					}
				} catch (Exception e1) {
					JOptionPane.showMessageDialog(null, e1.getMessage(), labels
							.getString("s291"), JOptionPane.ERROR_MESSAGE);
					logger.error(e1.getMessage());
				}
			}
		});
		newTB = new JMenuItem();
		newTB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new EdicionIconosGUI(false);
			}
		});
		loadTB = new JMenuItem();
		loadTB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser chooser = new JFileChooser();
				FiltroFicheros filtro = new FiltroFicheros(labels
						.getString("s904"));
				filtro.addExtension("BHcfg");
				chooser.setFileFilter(filtro);
				chooser.setCurrentDirectory(new File(
						".//configuration/toolbar/"));
				int option = chooser.showOpenDialog(null);
				if (option == JFileChooser.APPROVE_OPTION) {
					String TBFile = chooser.getSelectedFile().getAbsolutePath();
					// boolean[] valores = null;
					try {
						// valores = MenuConfig.cargarMenuCfgFich(menuFile);
						ListaIconosEditables.cargaLista(TBFile);
						ListaIconosEditables.cargaListaAux(TBFile);
						Iconos.generaToolBarFija();
						Iconos.generaToolBarEditable();
						almacenPropiedades.setPropiedad("currentTBCfg", TBFile);
						// MenuConfig.setAll(valores);
						Ventana v = Ventana.getInstance();
						// v.getnuevoMenu().setMenuConfig();
						v.validate();
						v.repaint();
						saveTB.setEnabled(false);
						logger.info(labels.getString("s905") + TBFile);
						EdicionIconosGUI.setChangesSaved(true);
						// mig
						String prj = null;
						try {
							prj = almacenPropiedades
									.getPropiedad("DefaultAcidePrj");
						} catch (Exception e1) {
							// TODO Auto-generated catch block
							e1.printStackTrace();
						}
						if (!(prj.equals(".//configuration/Default.acidePrj") && v
								.getProyecto().getnombreProy().equals(""))) {
							Ventana.getInstance().getProyecto().setModified(
									true);
						}

					} catch (Exception e1) {
						JOptionPane.showMessageDialog(null, labels
								.getString("s906")
								+ TBFile, labels.getString("s907"),
								JOptionPane.ERROR_MESSAGE);
						logger.error(labels.getString("s906") + TBFile);
					}
				} else if (option == JFileChooser.CANCEL_OPTION) {
					logger.info(labels.getString("s908"));
				}
			}
		});
		modifyTB = new JMenuItem();
		modifyTB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new EdicionIconosGUI(true);
			}
		});
		saveTB = new JMenuItem();
		saveTB.setEnabled(false);
		saveTB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					String previous = almacenPropiedades
							.getPropiedad("previousTBCfg");
					String current = almacenPropiedades
							.getPropiedad("currentTBCfg");
					ByteFile.copy(current, previous);
					almacenPropiedades.setPropiedad("currentTBCfg", previous);
					saveTB.setEnabled(false);
					EdicionIconosGUI.setChangesSaved(true);
				} catch (Exception e1) {
					JOptionPane.showMessageDialog(null, e1.getMessage(), labels
							.getString("s299"), JOptionPane.ERROR_MESSAGE);
				}
			}
		});
		saveAsTB = new JMenuItem();
		saveAsTB.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					String current = almacenPropiedades
							.getPropiedad("currentTBCfg");
					JFileChooser selector = new JFileChooser();
					FiltroFicheros filtro = new FiltroFicheros(labels
							.getString("s158"));
					filtro.addExtension("BHcfg");
					selector.setFileFilter(filtro);
					selector.setCurrentDirectory(new File(
							".//configuration/toolbar/"));
					String nombreFichero = "";
					int valor = selector.showSaveDialog(selector);
					if (valor == JFileChooser.APPROVE_OPTION) {
						nombreFichero = selector.getSelectedFile()
								.getAbsolutePath();
						if (!nombreFichero.endsWith(".BHcfg"))
							nombreFichero += ".BHcfg";
						ByteFile.copy(current, nombreFichero);
						almacenPropiedades.setPropiedad("currentTBCfg",
								nombreFichero);
						saveTB.setEnabled(false);
						EdicionIconosGUI.setChangesSaved(true);
						// Muestra operacion en el log
						logger.info(labels.getString("s900") + nombreFichero
								+ labels.getString("s901"));
					} else if (valor == JFileChooser.CANCEL_OPTION) {
						selector.cancelSelection();
						// Muestra operacion en el log
						logger.info(labels.getString("s902"));
					}
				} catch (Exception e1) {
					JOptionPane.showMessageDialog(null, e1.getMessage(), labels
							.getString("s903"), JOptionPane.ERROR_MESSAGE);
					logger.error(e1.getMessage());
				}
			}
		});
		saveAsGrammar = new JMenuItem();
		saveAsGrammar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				SintacticaGUI.saveAsGrammarGUI();
			}
		});
		saveAsLexical = new JMenuItem();
		setPaths = new JMenuItem();
		setPaths.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				new SetPathsGUI();
			}
		});
		// javacPath = new JMenuItem();
		// javaPath = new JMenuItem();
		// jarPath = new JMenuItem();
		autoSyntaxAnalysisCBox = new JCheckBoxMenuItem(labels.getString("s911"));
		autoSyntaxAnalysisCBox.setSelected(false);
		autoSyntaxAnalysisCBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				PleaseWaitWindow.showPleaseWaitWindow();
			}
		});
		saveAsProject = new JMenuItem();

		saveAsProject.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {

				try {
					FactoriaES fact = FactoriaES.getInstance();

					Ventana v = Ventana.getInstance();

					ResourceBundle labels = Idioma.getInstance().getLabels();
					Fichero f = fact.generaFichero();

					if (!v.getProyecto().getnombreProy().equals("")) {
						// Selecciona la extension del proyecto
						String[] ExtPide = new String[] { "acidePrj" };
						f.getFileChooser()
								.addChoosableFileFilter(
										new ExtFilter(ExtPide, labels
												.getString("s328")));

						String file = f.escribir();
						String len = almacenPropiedades.getPropiedad("idioma");
						String currentMenu = almacenPropiedades
								.getPropiedad("currentMenuCfg");
						String currentTB = almacenPropiedades
								.getPropiedad("currentTBCfg");
						v.getProyecto().setLenguaje(len);
						v.getProyecto().setCurrentMenu(currentMenu);
						v.getProyecto().setCurrentTB(currentTB);
						// mig
						if (!file.contains(".acidePrj"))
							file = file + ".acidePrj";
						v.getProyecto().setpathProy(file);
						String cad = v.getProyecto().salvarProy();
						f.salvar(v.getProyecto().getpathProy(), cad);
						v.getProyecto().setFirstSave(true);
						almacenPropiedades
								.setPropiedad("DefaultAcidePrj", file);
						almacenPropiedades.setPropiedad("DefaultPath", file);
						// mig
						v.getProyecto().setModified(false);
					}
				} catch (Exception ex) {
					ex.printStackTrace();
				}
			}
		});

		// mig
		proyecto.addMouseListener(new Click());
		archivo.addMouseListener(new Click());
		edicion.addMouseListener(new Click());

		// menuSubMenu = new JMenu();
		setMenu();
		setMenuConfig();
		logger.info(labels.getString("s69"));

	}

	/**
	 * Constructora
	 * 
	 * 
	 */
	public void setMenu() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		archivo.setText(labels.getString("s1"));
		edicion.setText(labels.getString("s2"));
		proyecto.setText(labels.getString("s3"));
		view.setText(labels.getString("s4"));
		parametrizacion.setText(labels.getString("s5"));
		idioma.setText(labels.getString("s6"));
		ayuda.setText(labels.getString("s7"));
		consola.setText(labels.getString("s332"));
		nuevoFich.setText(labels.getString("s8"));
		nuevoFich.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.CTRL_MASK));
		abrirFich.setText(labels.getString("s9"));

		// internacionalizacion mig
		if (i.getCurrentLocale().equals(new Locale("en", "EN")))
			abrirFich.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
					ActionEvent.CTRL_MASK));
		else
			abrirFich.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,
					ActionEvent.CTRL_MASK));

		salvarFich.setText(labels.getString("s10"));

		save.setText(labels.getString("s617"));
		// internacionalizacion mig
		if (i.getCurrentLocale().equals(new Locale("en", "EN")))
			save.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
					ActionEvent.CTRL_MASK));

		else
			save.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
					ActionEvent.CTRL_MASK));

		español.setText(labels.getString("s11"));
		español.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
				ActionEvent.ALT_MASK));
		english.setText(labels.getString("s12"));
		english.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,
				ActionEvent.ALT_MASK));
		// Deshabilitada opcion guardar fichero
		// salvarFich.setEnabled(false);

		closeFile.setEnabled(false);
		closeAll.setEnabled(false);
		salvarFich.setEnabled(false);
		save.setEnabled(false);
		saveAll.setEnabled(false);
		setMain2.setEnabled(false);
		unsetMain2.setEnabled(false);
		setCompilable2.setEnabled(false);
		unsetCompilable2.setEnabled(false);
		print.setEnabled(false);

		closeProject.setEnabled(false);
		guardarProyecto.setEnabled(false);
		saveAsProject.setEnabled(false);
		newProjectFile.setEnabled(false);
		anadirFichero.setEnabled(false);
		removeFile.setEnabled(false);
		deleteFile.setEnabled(false);
		addFolder.setEnabled(false);
		removeFolder.setEnabled(false);
		compilar.setEnabled(false);
		ejecutar.setEnabled(false);
		setMain.setEnabled(false);
		unsetMain.setEnabled(false);
		setCompilable.setEnabled(false);
		unsetCompilable.setEnabled(false);

		deshacer.setEnabled(false);
		repetir.setEnabled(false);
		copiar.setEnabled(false);
		pegar.setEnabled(false);
		cortar.setEnabled(false);
		selectAll.setEnabled(false);
		goTo.setEnabled(false);
		buscar.setEnabled(false);
		reemplazar.setEnabled(false);

		salir.setText(labels.getString("s13"));
		salir.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
				ActionEvent.ALT_MASK));
		nuevoProyecto.setText(labels.getString("s14"));
		nuevoProyecto.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));
		abrirProyecto.setText(labels.getString("s15"));
		abrirProyecto.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));
		guardarProyecto.setText(labels.getString("s16"));
		guardarProyecto.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));
		newProjectFile.setText(labels.getString("s947"));
		anadirFichero.setText(labels.getString("s17"));
		anadirFichero.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A,
				ActionEvent.ALT_MASK + ActionEvent.SHIFT_MASK));
		compilar.setText(labels.getString("s18"));
		compilar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.ALT_MASK));
		ejecutar.setText(labels.getString("s19"));
		ejecutar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,
				ActionEvent.ALT_MASK));
		depurar.setText(labels.getString("s20"));
		depurar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D,
				ActionEvent.ALT_MASK));
		deshacer.setText(labels.getString("s21"));
		deshacer.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z,
				ActionEvent.CTRL_MASK));
		repetir.setText(labels.getString("s22"));
		repetir.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Y,
				ActionEvent.CTRL_MASK));
		copiar.setText(labels.getString("s23"));
		copiar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.CTRL_MASK));
		cortar.setText(labels.getString("s24"));
		cortar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
				ActionEvent.CTRL_MASK));
		pegar.setText(labels.getString("s25"));
		pegar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V,
				ActionEvent.CTRL_MASK));
		buscar.setText(labels.getString("s26"));
		// internacionalizacion mig
		if (i.getCurrentLocale().equals(new Locale("en", "EN")))
			buscar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F,
					ActionEvent.CTRL_MASK));
		else
			buscar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B,
					ActionEvent.CTRL_MASK));
		reemplazar.setText(labels.getString("s27"));
		// internacionalizacion mig
		if (i.getCurrentLocale().equals(new Locale("en", "EN")))
			reemplazar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_R,
					ActionEvent.CTRL_MASK));
		else
			reemplazar.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L,
					ActionEvent.CTRL_MASK));
		mostrarLog.setText(labels.getString("s28"));
		mostrarLog.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		lexmodify.setText(labels.getString("s29"));
		lexmodify.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		sintactica.setText(labels.getString("s30"));
		sintactica.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_T,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		compilador.setText(labels.getString("s31"));
		compilador.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		interprete.setText(labels.getString("s32"));
		interprete.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		parser.setText(labels.getString("s33"));
		parser.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		menu.setText(labels.getString("s34"));
		// menu.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_M,
		// ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		cargarParam.setText(labels.getString("s35"));
		cargarParam.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		// salvarParam.setText(labels.getString("s36"));
		nuevaConfLenguaje.setText(labels.getString("s37"));
		nuevaConfLenguaje.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
				ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		mostrarAyuda.setText(labels.getString("s38"));
		mostrarAyuda.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,
				ActionEvent.CTRL_MASK));
		configurar.setText(labels.getString("s333"));
		comandoExterno.setText(labels.getString("s341"));
		acerca_de.setText(labels.getString("s39"));
		print.setText(labels.getString("s624"));
		print.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
				ActionEvent.CTRL_MASK));
		edicionIconos.setText(labels.getString("s169"));
		selectAll.setText(labels.getString("s190"));
		selectAll.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E,
				ActionEvent.CTRL_MASK));
		saveAll.setText(labels.getString("s217"));
		// internacionalizacion mig
		if (i.getCurrentLocale().equals(new Locale("en", "EN")))
			saveAll.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
					ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));
		else
			saveAll.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G,
					ActionEvent.CTRL_MASK + ActionEvent.SHIFT_MASK));

		removeFile.setText(labels.getString("s218"));
		deleteFile.setText(labels.getString("s950"));

		addFolder.setText(labels.getString("s219"));
		removeFolder.setText(labels.getString("s220"));
		showBrowserCBox.setText(labels.getString("s221"));
		goTo.setText(labels.getString("s222"));
		showShellWindowCBox.setText(labels.getString("s223"));
		lexicSubMenu.setText(labels.getString("s224"));
		syntaxSubMenu.setText(labels.getString("s225"));
		loadSyntax.setText(labels.getString("s226"));
		modifySyntax.setText(labels.getString("s227"));
		closeProject.setText(labels.getString("s228"));
		closeFile.setText(labels.getString("s238"));
		closeAll.setText(labels.getString("s239"));
		compiler.setText(labels.getString("s240"));
		newLexical.setText(labels.getString("s249"));
		saveLexical.setText(labels.getString("s250"));
		saveGrammar.setText(labels.getString("s251"));
		setCompilable.setText(labels.getString("s254"));
		unsetCompilable.setText(labels.getString("s255"));
		setMain.setText(labels.getString("s256"));
		unsetMain.setText(labels.getString("s952"));
		setCompilable2.setText(labels.getString("s254"));
		unsetCompilable2.setText(labels.getString("s255"));
		setMain2.setText(labels.getString("s256"));
		unsetMain2.setText(labels.getString("s952"));
		// menuSubMenu.setText(labels.getString("s34"));
		newMenu.setText(labels.getString("s275"));
		loadMenu.setText(labels.getString("s276"));
		modifyMenu.setText(labels.getString("s277"));
		saveMenu.setText(labels.getString("s278"));
		saveAsMenu.setText(labels.getString("s279"));
		newTB.setText(labels.getString("s280"));
		loadTB.setText(labels.getString("s281"));
		modifyTB.setText(labels.getString("s282"));
		saveTB.setText(labels.getString("s283"));
		saveAsTB.setText(labels.getString("s284"));
		saveAsGrammar.setText(labels.getString("s285"));
		saveAsLexical.setText(labels.getString("s286"));
		setPaths.setText(labels.getString("s912"));
		autoSyntaxAnalysisCBox.setText(labels.getString("s911"));
		saveAsProject.setText(labels.getString("s926"));

	}

	public void setMenuConfig() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		String currentMenu = null;
		try {
			currentMenu = almacenPropiedades.getPropiedad("currentMenuCfg");
			boolean[] valores = MenuConfig.cargarMenuCfgFich(currentMenu);
			MenuConfig.setAll(valores);
			almacenPropiedades.setPropiedad("currentMenuCfg", currentMenu);
			logger.info(labels.getString("s70") + " " + currentMenu);
		} catch (Exception e) {
			logger.info(labels.getString("s71") + e.getMessage());
			// saca ruta relativa
			String currentMenu2;
			int index = currentMenu.lastIndexOf("\\");
			if (index == -1)
				index = currentMenu.lastIndexOf("/");
			currentMenu2 = ".\\configuration\\menu\\"
					+ currentMenu.substring(index + 1, currentMenu.length());
			boolean[] valores;
			try {
				valores = MenuConfig.cargarMenuCfgFich(currentMenu2);
				MenuConfig.setAll(valores);
				almacenPropiedades.setPropiedad("currentMenuCfg", currentMenu2);
				logger.info(labels.getString("s70") + " " + currentMenu2);
				JOptionPane
						.showMessageDialog(null, labels.getString("s956")
								+ currentMenu + labels.getString("s957")
								+ currentMenu2);
			} catch (Exception e1) {
				// e1.printStackTrace();
				try {
					valores = MenuConfig
							.cargarMenuCfgFich(".//configuration/menu/default_allOn.menuCfg");
					MenuConfig.setAll(valores);
					almacenPropiedades.setPropiedad("currentMenuCfg",
							".//configuration/menu/default_allOn.menuCfg");
					JOptionPane.showMessageDialog(null, labels
							.getString("s956")
							+ currentMenu + labels.getString("s959"));
				} catch (HeadlessException e2) {
					// TODO Auto-generated catch block
					e2.printStackTrace();
				} catch (Exception e2) {
					// TODO Auto-generated catch block
					e2.printStackTrace();
				}
			}

			/*
			 * MenuConfig.todosActivados(); boolean[] valores = new boolean[73];
			 * for (int j = 0; j < valores.length; j++) valores[j] = true;
			 * String defaultMenu =
			 * ".//configuration/menu/default_allOn.menuCfg";
			 * MenuConfig.guardarMenuCfgFich(defaultMenu,valores);
			 */

			// almacenPropiedades.setPropiedad("currentMenuCfg",currentMenu2);
			// almacenPropiedades.setPropiedad("previousMenuCfg",currentMenu2);
			// v.getnuevoMenu().getSaveMenu().setEnabled(true);
			// MenuGUI.setChangesSaved(true);
		}
		archivo.removeAll();
		edicion.removeAll();
		proyecto.removeAll();
		view.removeAll();
		parametrizacion.removeAll();
		idioma.removeAll();
		ayuda.removeAll();
		consola.removeAll();
		lexicSubMenu.removeAll();
		syntaxSubMenu.removeAll();
		menu.removeAll();
		edicionIconos.removeAll();
		menuBar.removeAll();
		if (MenuConfig.getNuevoFich())
			archivo.add(nuevoFich);
		if (MenuConfig.getAbrirFich())
			archivo.add(abrirFich);
		if (MenuConfig.getCloseFile())
			archivo.add(closeFile);
		if (MenuConfig.getCloseAll())
			archivo.add(closeAll);
		if ((MenuConfig.getNuevoFich() || MenuConfig.getAbrirFich()
				|| MenuConfig.getCloseFile() || MenuConfig.getCloseAll())
				&& (MenuConfig.getSave() || MenuConfig.getSalvarFich() || MenuConfig
						.getSaveAll()))
			archivo.addSeparator();
		if (MenuConfig.getSave())
			archivo.add(save);
		if (MenuConfig.getSalvarFich())
			archivo.add(salvarFich);
		if (MenuConfig.getSaveAll())
			archivo.add(saveAll);
		if ((MenuConfig.getNuevoFich() || MenuConfig.getAbrirFich()
				|| MenuConfig.getCloseFile() || MenuConfig.getCloseAll()
				|| MenuConfig.getSave() || MenuConfig.getSalvarFich() || MenuConfig
				.getSaveAll())
				&& (MenuConfig.getPrint()))
			archivo.addSeparator();
		// mig
		if (MenuConfig.getSetFile2())
			// archivo.add(setCompilable2);
			if (MenuConfig.getUnsetFile2())
				// archivo.add(unsetCompilable2);
				if (MenuConfig.getSetMain2())
					// archivo.add(setMain2);
					if (MenuConfig.getUnsetMain2())
						// archivo.add(unsetMain2);
						// archivo.addSeparator();
						if (MenuConfig.getPrint())
							archivo.add(print);
		if ((MenuConfig.getNuevoFich() || MenuConfig.getAbrirFich()
				|| MenuConfig.getSalvarFich() || MenuConfig.getSave()
				|| MenuConfig.getSaveAll() || MenuConfig.getPrint()
				|| MenuConfig.getCloseFile() || MenuConfig.getCloseAll())
				&& MenuConfig.getSalir())
			archivo.addSeparator();
		if (MenuConfig.getSalir())
			archivo.add(salir);
		if (MenuConfig.getDeshacer())
			edicion.add(deshacer);
		if (MenuConfig.getRepetir())
			edicion.add(repetir);
		if ((MenuConfig.getDeshacer() || MenuConfig.getRepetir())
				&& (MenuConfig.getCopiar() || MenuConfig.getPegar()
						|| MenuConfig.getCortar() || MenuConfig.getSelectAll()))
			edicion.addSeparator();
		if (MenuConfig.getCopiar())
			edicion.add(copiar);
		if (MenuConfig.getPegar())
			edicion.add(pegar);
		if (MenuConfig.getCortar())
			edicion.add(cortar);
		if (MenuConfig.getSelectAll())
			edicion.add(selectAll);
		if ((MenuConfig.getDeshacer() || MenuConfig.getRepetir()
				|| MenuConfig.getCopiar() || MenuConfig.getPegar()
				|| MenuConfig.getCortar() || MenuConfig.getSelectAll())
				&& MenuConfig.getGoTo())
			edicion.addSeparator();
		if (MenuConfig.getGoTo())
			edicion.add(goTo);
		if ((MenuConfig.getDeshacer() || MenuConfig.getRepetir()
				|| MenuConfig.getCopiar() || MenuConfig.getPegar()
				|| MenuConfig.getCortar() || MenuConfig.getSelectAll() || MenuConfig
				.getGoTo())
				&& (MenuConfig.getBuscar() || MenuConfig.getReemplazar()))
			edicion.addSeparator();
		if (MenuConfig.getBuscar())
			edicion.add(buscar);
		if (MenuConfig.getReemplazar())
			edicion.add(reemplazar);
		if (MenuConfig.getNuevoProyecto())
			proyecto.add(nuevoProyecto);
		if (MenuConfig.getAbrirProyecto())
			proyecto.add(abrirProyecto);
		if (MenuConfig.getCloseProject())
			proyecto.add(closeProject);
		if ((MenuConfig.getAbrirProyecto() || MenuConfig.getNuevoProyecto() || MenuConfig
				.getCloseProject())
				&& (MenuConfig.getGuardarProyecto() || MenuConfig
						.isSaveAsProject()))
			proyecto.addSeparator();
		if (MenuConfig.getGuardarProyecto())
			proyecto.add(guardarProyecto);
		if (MenuConfig.isSaveAsProject())
			proyecto.add(saveAsProject);
		if ((MenuConfig.getAbrirProyecto() || MenuConfig.getNuevoProyecto()
				|| MenuConfig.getGuardarProyecto()
				|| MenuConfig.getCloseProject() || MenuConfig.isSaveAsProject())
				&& (MenuConfig.getAñadirFichero() || MenuConfig.getRemoveFile()
						|| MenuConfig.getAddFolder()
						|| MenuConfig.getRemoveFolder()
						/* mig */|| MenuConfig.getDeleteFile() || MenuConfig
						.getNewProjectFile()))
			proyecto.addSeparator();
		if (MenuConfig.getNewProjectFile())
			proyecto.add(newProjectFile);
		if (MenuConfig.getAñadirFichero())
			proyecto.add(anadirFichero);
		if (MenuConfig.getRemoveFile())
			proyecto.add(removeFile);
		if (MenuConfig.getDeleteFile())
			proyecto.add(deleteFile);
		if (MenuConfig.getAddFolder())
			proyecto.add(addFolder);
		if (MenuConfig.getRemoveFolder())
			proyecto.add(removeFolder);
		if ((MenuConfig.getAbrirProyecto() || MenuConfig.getNuevoProyecto()
				|| MenuConfig.getGuardarProyecto()
				|| MenuConfig.isSaveAsProject()
				|| MenuConfig.getAñadirFichero() || MenuConfig.getRemoveFile()
				|| MenuConfig.getAddFolder() || MenuConfig.getRemoveFolder()
				/* mig */|| MenuConfig.getDeleteFile() || MenuConfig
				.getNewProjectFile())
				&& (MenuConfig.getCompilar() || MenuConfig.getEjecutar()
						|| MenuConfig.getSetFile() || MenuConfig.getUnsetFile() || MenuConfig
						.getSetMain()))
			proyecto.addSeparator();
		if (MenuConfig.getCompilar())
			proyecto.add(compilar);
		if (MenuConfig.getEjecutar())
			proyecto.add(ejecutar);
		if ((MenuConfig.getAbrirProyecto() || MenuConfig.getNuevoProyecto()
				|| MenuConfig.getGuardarProyecto()
				|| MenuConfig.isSaveAsProject()
				|| MenuConfig.getAñadirFichero() || MenuConfig.getRemoveFile()
				|| MenuConfig.getAddFolder() || MenuConfig.getRemoveFolder()
				/* mig */|| MenuConfig.getDeleteFile()
				|| MenuConfig.getNewProjectFile() || MenuConfig.getCompilar() || MenuConfig
				.getEjecutar())
				&& (MenuConfig.getSetFile() || MenuConfig.getUnsetFile() || MenuConfig
						.getSetMain()))
			proyecto.addSeparator();
		if (MenuConfig.getSetFile())
			proyecto.add(setCompilable);
		if (MenuConfig.getUnsetFile())
			proyecto.add(unsetCompilable);
		if (MenuConfig.getSetMain())
			proyecto.add(setMain);
		if (MenuConfig.getUnsetMain())
			proyecto.add(unsetMain);
		if (MenuConfig.getMostrarLog())
			view.add(mostrarLog);
		if (MenuConfig.getShowBrowserCBox())
			view.add(showBrowserCBox);
		if (MenuConfig.getShowShellWindowCBox())
			view.add(showShellWindowCBox);
		if (MenuConfig.getNewLexical())
			lexicSubMenu.add(newLexical);
		if (MenuConfig.getCargarParam())
			lexicSubMenu.add(cargarParam);
		if (MenuConfig.getLexica())
			lexicSubMenu.add(lexmodify);
		if (MenuConfig.getSaveLexical())
			lexicSubMenu.add(saveLexical);
		if (MenuConfig.isSaveAsLexical())
			lexicSubMenu.add(saveAsLexical);
		if (MenuConfig.getCargarParam() || MenuConfig.getLexica()
				|| MenuConfig.getNewLexical() || MenuConfig.getSaveLexical()
				|| MenuConfig.isSaveAsLexical())
			parametrizacion.add(lexicSubMenu);
		if (MenuConfig.getSintactica())
			syntaxSubMenu.add(sintactica);
		if (MenuConfig.getLoadSyntax())
			syntaxSubMenu.add(loadSyntax);
		if (MenuConfig.getModifySyntax())
			syntaxSubMenu.add(modifySyntax);
		if (MenuConfig.getSaveGrammar())
			syntaxSubMenu.add(saveGrammar);
		if (MenuConfig.isSaveAsGrammar())
			syntaxSubMenu.add(saveAsGrammar);
		if ((MenuConfig.getSintactica() || MenuConfig.getLoadSyntax()
				|| MenuConfig.getModifySyntax() || MenuConfig.getSaveGrammar() || MenuConfig
				.isSaveAsGrammar())
				&& (MenuConfig.isSetPaths()))
			syntaxSubMenu.addSeparator();
		if (MenuConfig.isSetPaths())
			syntaxSubMenu.add(setPaths);
		if ((MenuConfig.getSintactica() || MenuConfig.getLoadSyntax()
				|| MenuConfig.getModifySyntax() || MenuConfig.getSaveGrammar()
				|| MenuConfig.isSaveAsGrammar() || MenuConfig.isSetPaths())
				&& (MenuConfig.isAutoSyntaxAnalysis()))
			syntaxSubMenu.addSeparator();
		if (MenuConfig.isAutoSyntaxAnalysis())
			syntaxSubMenu.add(autoSyntaxAnalysisCBox);
		if (MenuConfig.getSintactica() || MenuConfig.getLoadSyntax()
				|| MenuConfig.getModifySyntax() || MenuConfig.getSaveGrammar()
				|| MenuConfig.isSaveAsGrammar()) {
			parametrizacion.add(syntaxSubMenu);
			// syntaxSubMenu.setEnabled(false);
		}
		if (MenuConfig.getCompiler())
			parametrizacion.add(compiler);
		if ((MenuConfig.getCargarParam() || MenuConfig.getLexica()
				|| MenuConfig.getSintactica() || MenuConfig.getLoadSyntax()
				|| MenuConfig.getModifySyntax() || MenuConfig.getCompiler()
				|| MenuConfig.getNewLexical() || MenuConfig.getSaveLexical() || MenuConfig
				.getSaveGrammar())
				&& (MenuConfig.getConfigurar() || MenuConfig
						.getComandoExterno()))
			parametrizacion.addSeparator();
		if (MenuConfig.getConfigurar())
			consola.add(configurar);
		if (MenuConfig.getComandoExterno())
			consola.add(comandoExterno);
		if (MenuConfig.getConfigurar() || MenuConfig.getComandoExterno())
			parametrizacion.add(consola);
		if ((MenuConfig.getCargarParam() || MenuConfig.getLexica()
				|| MenuConfig.getSintactica() || MenuConfig.getLoadSyntax()
				|| MenuConfig.getModifySyntax() || MenuConfig.getCompiler()
				|| MenuConfig.getConfigurar() || MenuConfig.getComandoExterno()
				|| MenuConfig.getNewLexical() || MenuConfig.getSaveLexical() || MenuConfig
				.getSaveGrammar())
				&& (MenuConfig.getEspañol() || MenuConfig.getEnglish()
						|| MenuConfig.getMenu() || MenuConfig
						.getEdicionIconos()))
			parametrizacion.addSeparator();
		if (MenuConfig.getEspañol())
			idioma.add(español);
		if (MenuConfig.getEnglish())
			idioma.add(english);
		if (MenuConfig.getEspañol() || MenuConfig.getEnglish())
			parametrizacion.add(idioma);
		// ***************************************************************
		if (MenuConfig.isNewMenu())
			menu.add(newMenu);
		if (MenuConfig.isLoadMenu())
			menu.add(loadMenu);
		if (MenuConfig.isModifyMenu())
			menu.add(modifyMenu);
		if (MenuConfig.isSaveMenu())
			menu.add(saveMenu);
		if (MenuConfig.isSaveAsMenu())
			menu.add(saveAsMenu);
		if (MenuConfig.isNewMenu() || MenuConfig.isLoadMenu()
				|| MenuConfig.isModifyMenu() || MenuConfig.isSaveMenu()
				|| MenuConfig.isSaveAsMenu())
			parametrizacion.add(menu);
		// ***************************************************************
		// ***************************************************************
		if (MenuConfig.isNewTB())
			edicionIconos.add(newTB);
		if (MenuConfig.isLoadTB())
			edicionIconos.add(loadTB);
		if (MenuConfig.isModifyTB())
			edicionIconos.add(modifyTB);
		if (MenuConfig.isSaveTB())
			edicionIconos.add(saveTB);
		if (MenuConfig.isSaveAsTB())
			edicionIconos.add(saveAsTB);
		if (MenuConfig.isNewTB() || MenuConfig.isLoadTB()
				|| MenuConfig.isModifyTB() || MenuConfig.isSaveTB()
				|| MenuConfig.isSaveAsTB())
			parametrizacion.add(edicionIconos);
		// ***************************************************************
		if (MenuConfig.getMostrarAyuda())
			ayuda.add(mostrarAyuda);
		if (MenuConfig.getMostrarAyuda() && MenuConfig.getAcercade())
			ayuda.addSeparator();
		if (MenuConfig.getAcercade())
			ayuda.add(acerca_de);
		if (MenuConfig.getNuevoFich() || MenuConfig.getAbrirFich()
				|| MenuConfig.getSalvarFich() || MenuConfig.getSave()
				|| MenuConfig.getSaveAll() || MenuConfig.getPrint()
				|| MenuConfig.getSalir())
			menuBar.add(archivo);
		if (MenuConfig.getDeshacer() || MenuConfig.getRepetir()
				|| MenuConfig.getCopiar() || MenuConfig.getPegar()
				|| MenuConfig.getCortar() || MenuConfig.getSelectAll()
				|| MenuConfig.getGoTo() || MenuConfig.getBuscar()
				|| MenuConfig.getReemplazar())
			menuBar.add(edicion);
		if (MenuConfig.getNuevoProyecto() || MenuConfig.getAbrirProyecto()
				|| MenuConfig.getGuardarProyecto()
				|| MenuConfig.getCloseProject()
				|| MenuConfig.getAñadirFichero() || MenuConfig.getRemoveFile()
				/* mig */|| MenuConfig.getDeleteFile()
				|| MenuConfig.getNewProjectFile() || MenuConfig.getAddFolder()
				|| MenuConfig.getRemoveFolder() || MenuConfig.getCompilar()
				|| MenuConfig.getEjecutar())
			menuBar.add(proyecto);
		if (MenuConfig.getMostrarLog() || MenuConfig.getShowBrowserCBox()
				|| MenuConfig.getShowShellWindowCBox())
			menuBar.add(view);
		// parametrizacion siempre va a tener activo menu asi que no lo
		// comprobamos
		menuBar.add(parametrizacion);
		if (MenuConfig.getMostrarAyuda() || MenuConfig.getAcercade())
			menuBar.add(ayuda);
		menuBar.setVisible(true);

	}

	/**
	 * Obtiene el interfaz Menu
	 * 
	 * @return El menuBar
	 */
	public JMenuBar getMenuBar() {
		return menuBar;
	}

	public UndoManager getUndo() {
		return undo;
	}

	/**
	 * Metodo que establece los oyentes de los menus(Se añadiran más)
	 * 
	 */
	public void estableceOyentesMenu() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		salir.addActionListener(new Menu_salir_Adaptador());
		nuevoFich.addActionListener(new Menu_nuevo_Adaptador());
		lexmodify.addActionListener(new Menu_lexico_Adaptador());
		abrirFich.addActionListener(new Menu_abrirFich_Adaptador());
		salvarFich.addActionListener(new Menu_salvarFich_Adaptador());
		save.addActionListener(new Menu_save_Adaptador());
		deshacer.addActionListener(new Menu_deshacer_Adaptador());
		repetir.addActionListener(new Menu_repetir_Adaptador());
		buscar.addActionListener(new Menu_buscar_Adaptador());
		reemplazar.addActionListener(new Menu_reemplazar_Adaptador());
		cortar.addActionListener(new Menu_cortar_Adaptador());
		pegar.addActionListener(new Menu_pegar_Adaptador());
		copiar.addActionListener(new Menu_copiar_Adaptador());
		// TODO Cambiar este oyente para la opcion del submenu que corresponda
		menu.addActionListener(new Menu_menu_Adaptor());
		// sintactica.addActionListener(new Menu_sintactica_Adaptor());
		mostrarLog.addActionListener(new Menu_mostrarLog_Adaptor());
		cargarParam.addActionListener(new Menu_cargarLexParam_Adaptor());
		// salvarParam.addActionListener(new Menu_salvarParam_Adaptor());
		compilador.addActionListener(new Menu_compilador_Adaptor());
		interprete.addActionListener(new Menu_interprete_Adaptor());
		parser.addActionListener(new Menu_parser_Adaptor());
		español.addActionListener(new Menu_español_Adaptor());
		english.addActionListener(new Menu_english_Adaptor());
		nuevaConfLenguaje.addActionListener(new Menu_nuevaConfLeng_Adaptor());
		nuevoProyecto.addActionListener(new Menu_nuevoProyecto_Adaptor());
		guardarProyecto.addActionListener(new Menu_salvarProyecto_Adaptor());
		abrirProyecto.addActionListener(new Menu_abrirProyecto_Adaptor());
		configurar.addActionListener(new Menu_configurarConsola_Adaptor());
		comandoExterno.addActionListener(new Menu_comandoExterno_Adaptor());
		edicionIconos.addActionListener(new Menu_edicionIconos_Adaptor());
		anadirFichero.addActionListener(new Menu_anadirFich_Adaptor());
		selectAll.addActionListener(new Menu_selectAll_Adaptor());
		acerca_de.addActionListener(new Menu_About_Adaptor());
		print.addActionListener(new Menu_Print_Adapter());
		ejecutar.addActionListener(new Menu_Execution_Adapter());
		compilar.addActionListener(new Menu_Compiler_Adapter());
		logger.info(labels.getString("s72"));
		goTo.addActionListener(new Menu_GoTo_Adapter());
		removeFile.addActionListener(new Menu_RemoveFile_Adapter());
		closeProject.addActionListener(new Menu_CloseProj_Adapter());
		// modifySyntax.addActionListener(new Menu_ModifySyntax_Adapter());
		saveLexical.addActionListener(new Menu_SaveLexical_Adapter());
		saveAsLexical.addActionListener(new Menu_SaveAsLexical_Adapter());
		newLexical.addActionListener(new Menu_NewLexical_Adapter());
		// mig
		deleteFile.addActionListener(new Menu_DeleteFile_Adapter());
		newProjectFile.addActionListener(new Menu_newProjectFile_Adaptor());
		removeFile2.addActionListener(new Menu_RemoveFile2_Adapter());
		deleteFile2.addActionListener(new Menu_DeleteFile2_Adapter());
		anadirFichero2.addActionListener(new Menu_anadirFich2_Adaptor());
	}

	class Menu_nuevaConfLeng_Adaptor implements ActionListener {
		FactoriaGUI fact = FactoriaGUI.getInstance();

		public void actionPerformed(ActionEvent e) {
			ListaTiposToken.getInstance().reset();
			DividerList.getInstance().reset();
			Comments.getInstance().reset();
			fact.generaLenguajeGUI();
		}
	}

	public int save_o_saveAS() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Ventana v = Ventana.getInstance();
		int editor = v.getCreadorEditor().getEditorSeleccionado();
		if (v.getCreadorEditor().isRedButton() == true) {
			if (v.getCreadorEditor().EditorSeleccionado().getPath().equals(
					labels.getString("s79")) == true) {

				FactoriaES fac = FactoriaES.getInstance();
				Fichero f = fac.generaFichero();
				String archivo = " ";
				archivo = f.escribir();
				if (archivo.equals(" ")) {
					// v.getnuevaSalida().cargaTexto(labels.getString("s91"));
					logger.info(labels.getString("s92"));
				} else {
					boolean resultado = f.salvar(archivo, v.getCreadorEditor()
							.EditorSeleccionado().getTexto());
					// Muestra en el log
					if (resultado) {
						logger.info(labels.getString("s93") + archivo
								+ labels.getString("s94"));
						Ventana.getInstance().getCreadorEditor().greenButton();
						v.getCreadorEditor().dameEditorI(editor).setPath(
								archivo);
						v.getCreadorEditor().dameEditorI(editor)
								.setToolTipText(archivo);
						int in = archivo.lastIndexOf("\\");
						in++;
						String file = archivo.substring(in, archivo.length());
						v.getCreadorEditor().dameEditorI(editor).setName(file);
						// mig
						File fich = new File(v.getCreadorEditor()
								.EditorSeleccionado().getPath());
						Ventana.getInstance().getCreadorEditor()
								.EditorSeleccionado().setUltimoCambio(
										fich.lastModified());
						Ventana.getInstance().getCreadorEditor()
								.EditorSeleccionado().setUltimoTam(
										fich.length());

					} else {
						logger.info(labels.getString("s95") + archivo);
					}
				}
			} else {
				save.setEnabled(true);
				save.doClick();
				// save.setEnabled(false);
			}
		}

		return editor;
	}

	/**
	 * Metodo que deshabilita la opcion del menu pegar
	 * 
	 * 
	 */
	public void deshabilita_pegar() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		pegar.setEnabled(false);
		logger.info(labels.getString("s73"));
	}

	/**
	 * Metodo que habilita la opcion del menu pegar
	 * 
	 * 
	 */
	public void habilita_pegar() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		pegar.setEnabled(true);
		logger.info(labels.getString("s74"));
	}

	/**
	 * Metodo que habilita la opcion del menu guardar fichero
	 * 
	 * 
	 */
	public void habilita_salvarFich() {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		salvarFich.setEnabled(true);
		logger.info(labels.getString("s75"));
	}

	// mig
	public void habilitaMenuArchivo() {

		closeFile.setEnabled(true);
		closeAll.setEnabled(true);
		salvarFich.setEnabled(true);
		save.setEnabled(false);
		saveAll.setEnabled(false);
		setMain2.setEnabled(false);
		unsetMain2.setEnabled(false);
		setCompilable2.setEnabled(false);
		unsetCompilable2.setEnabled(false);
		print.setEnabled(true);
	}

	public void deshabilitaMenuArchivo() {

		closeFile.setEnabled(false);
		closeAll.setEnabled(false);
		salvarFich.setEnabled(false);
		save.setEnabled(false);
		saveAll.setEnabled(false);
		setMain2.setEnabled(false);
		unsetMain2.setEnabled(false);
		setCompilable2.setEnabled(false);
		unsetCompilable2.setEnabled(false);
		print.setEnabled(false);
	}

	public void habilitaMenuProyecto() {

		closeProject.setEnabled(true);
		guardarProyecto.setEnabled(false);
		saveAsProject.setEnabled(true);
		newProjectFile.setEnabled(true);
		anadirFichero.setEnabled(true);
		removeFile.setEnabled(false);
		deleteFile.setEnabled(false);
		addFolder.setEnabled(true);
		removeFolder.setEnabled(false);
		compilar.setEnabled(true);
		ejecutar.setEnabled(true);
		setMain.setEnabled(false);
		unsetMain.setEnabled(false);
		setCompilable.setEnabled(false);
		unsetCompilable.setEnabled(false);
	}

	public void deshabilitaMenuProyecto() {

		closeProject.setEnabled(false);
		guardarProyecto.setEnabled(false);
		saveAsProject.setEnabled(false);
		newProjectFile.setEnabled(false);
		anadirFichero.setEnabled(false);
		removeFile.setEnabled(false);
		deleteFile.setEnabled(false);
		addFolder.setEnabled(false);
		removeFolder.setEnabled(false);
		compilar.setEnabled(false);
		ejecutar.setEnabled(false);
		setMain.setEnabled(false);
		unsetMain.setEnabled(false);
		setCompilable.setEnabled(false);
		unsetCompilable.setEnabled(false);
	}

	public void habilitaMenuEdicion() {
		deshacer.setEnabled(false);
		repetir.setEnabled(false);
		copiar.setEnabled(false);
		pegar.setEnabled(false);
		cortar.setEnabled(false);
		selectAll.setEnabled(true);
		goTo.setEnabled(true);
		buscar.setEnabled(true);
		reemplazar.setEnabled(true);
	}

	public void deshabilitaMenuEdicion() {
		deshacer.setEnabled(false);
		repetir.setEnabled(false);
		copiar.setEnabled(false);
		pegar.setEnabled(false);
		cortar.setEnabled(false);
		selectAll.setEnabled(false);
		goTo.setEnabled(false);
		buscar.setEnabled(false);
		reemplazar.setEnabled(false);
	}

	public JMenuItem getBuscar() {
		return buscar;
	}

	public JMenuItem getNuevoProyecto() {
		return nuevoProyecto;
	}

	public JMenuItem getGuardarProyecto() {
		return guardarProyecto;
	}

	public JMenuItem getAbrirProyecto() {
		return abrirProyecto;
	}

	public JMenuItem getNewProjectFile() {
		return newProjectFile;
	}

	public JMenuItem getAnadirFichero() {
		return anadirFichero;
	}

	public JMenuItem getAnadirFichero2() {
		return anadirFichero2;
	}

	public JMenuItem getPrint() {
		return print;
	}

	public JMenuItem getAbrirFich() {
		return abrirFich;
	}

	public void setAbrirFich(JMenuItem abrirFich) {
		this.abrirFich = abrirFich;
	}

	public JMenuItem getAcerca_de() {
		return acerca_de;
	}

	public void setAcerca_de(JMenuItem acerca_de) {
		this.acerca_de = acerca_de;
	}

	public JMenuItem getAddFolder() {
		return addFolder;
	}

	public void setAddFolder(JMenuItem addFolder) {
		this.addFolder = addFolder;
	}

	public JMenu getArchivo() {
		return archivo;
	}

	public void setArchivo(JMenu archivo) {
		this.archivo = archivo;
	}

	public JMenu getAyuda() {
		return ayuda;
	}

	public void setAyuda(JMenu ayuda) {
		this.ayuda = ayuda;
	}

	public JMenuItem getCargarParam() {
		return cargarParam;
	}

	public void setCargarParam(JMenuItem cargarParam) {
		this.cargarParam = cargarParam;
	}

	public JMenuItem getComandoExterno() {
		return comandoExterno;
	}

	public void setComandoExterno(JMenuItem comandoExterno) {
		this.comandoExterno = comandoExterno;
	}

	public JMenuItem getCompilador() {
		return compilador;
	}

	public void setCompilador(JMenuItem compilador) {
		this.compilador = compilador;
	}

	public JMenuItem getCompilar() {
		return compilar;
	}

	public void setCompilar(JMenuItem compilar) {
		this.compilar = compilar;
	}

	public JMenuItem getConfigurar() {
		return configurar;
	}

	public void setConfigurar(JMenuItem configurar) {
		this.configurar = configurar;
	}

	public JMenu getConsola() {
		return consola;
	}

	public void setConsola(JMenu consola) {
		this.consola = consola;
	}

	public JMenuItem getCopiar() {
		return copiar;
	}

	public void setCopiar(JMenuItem copiar) {
		this.copiar = copiar;
	}

	public JMenuItem getCortar() {
		return cortar;
	}

	public void setCortar(JMenuItem cortar) {
		this.cortar = cortar;
	}

	public JMenuItem getDepurar() {
		return depurar;
	}

	public void setDepurar(JMenuItem depurar) {
		this.depurar = depurar;
	}

	public JMenuItem getDeshacer() {
		return deshacer;
	}

	public void setDeshacer(JMenuItem deshacer) {
		this.deshacer = deshacer;
	}

	public JMenu getEdicion() {
		return edicion;
	}

	public void setEdicion(JMenu edicion) {
		this.edicion = edicion;
	}

	public JMenu getEdicionIconos() {
		return edicionIconos;
	}

	public void setEdicionIconos(JMenu edicionIconos) {
		this.edicionIconos = edicionIconos;
	}

	public JMenuItem getEjecutar() {
		return ejecutar;
	}

	public void setEjecutar(JMenuItem ejecutar) {
		this.ejecutar = ejecutar;
	}

	public JMenuItem getEnglish() {
		return english;
	}

	public void setEnglish(JMenuItem english) {
		this.english = english;
	}

	public JMenuItem getEspañol() {
		return español;
	}

	public void setEspañol(JMenuItem español) {
		this.español = español;
	}

	public JMenuItem getGoTo() {
		return goTo;
	}

	public void setGoTo(JMenuItem goTo) {
		this.goTo = goTo;
	}

	public JMenu getIdioma() {
		return idioma;
	}

	public void setIdioma(JMenu idioma) {
		this.idioma = idioma;
	}

	public JMenuItem getInterprete() {
		return interprete;
	}

	public void setInterprete(JMenuItem interprete) {
		this.interprete = interprete;
	}

	public JMenuItem getLexica() {
		return lexmodify;
	}

	public void setLexica(JMenuItem lexica) {
		this.lexmodify = lexica;
	}

	public JMenu getLexicSubMenu() {
		return lexicSubMenu;
	}

	public void setLexicSubMenu(JMenu lexicSubMenu) {
		this.lexicSubMenu = lexicSubMenu;
	}

	public JMenuItem getLoadSyntax() {
		return loadSyntax;
	}

	public void setLoadSyntax(JMenuItem loadSyntax) {
		this.loadSyntax = loadSyntax;
	}

	public Logger getLogger() {
		return logger;
	}

	public void setLogger(Logger logger) {
		this.logger = logger;
	}

	public JMenu getMenu() {
		return menu;
	}

	public void setMenu(JMenu menu) {
		this.menu = menu;
	}

	public JMenuItem getModifySyntax() {
		return modifySyntax;
	}

	public void setModifySyntax(JMenuItem modifySyntax) {
		this.modifySyntax = modifySyntax;
	}

	public JMenuItem getMostrarAyuda() {
		return mostrarAyuda;
	}

	public void setMostrarAyuda(JMenuItem mostrarAyuda) {
		this.mostrarAyuda = mostrarAyuda;
	}

	public JMenuItem getMostrarLog() {
		return mostrarLog;
	}

	public void setMostrarLog(JMenuItem mostrarLog) {
		this.mostrarLog = mostrarLog;
	}

	public JMenuItem getNuevaConfLenguaje() {
		return nuevaConfLenguaje;
	}

	public void setNuevaConfLenguaje(JMenuItem nuevaConfLenguaje) {
		this.nuevaConfLenguaje = nuevaConfLenguaje;
	}

	public JMenuItem getNuevoFich() {
		return nuevoFich;
	}

	public void setNuevoFich(JMenuItem nuevoFich) {
		this.nuevoFich = nuevoFich;
	}

	public JMenu getParametrizacion() {
		return parametrizacion;
	}

	public void setParametrizacion(JMenu parametrizacion) {
		this.parametrizacion = parametrizacion;
	}

	public JMenuItem getParser() {
		return parser;
	}

	public void setParser(JMenuItem parser) {
		this.parser = parser;
	}

	public JMenuItem getPegar() {
		return pegar;
	}

	public void setPegar(JMenuItem pegar) {
		this.pegar = pegar;
	}

	public JMenu getProyecto() {
		return proyecto;
	}

	public void setProyecto(JMenu proyecto) {
		this.proyecto = proyecto;
	}

	public JMenuItem getReemplazar() {
		return reemplazar;
	}

	public void setReemplazar(JMenuItem reemplazar) {
		this.reemplazar = reemplazar;
	}

	public void setRemoveFile(JMenuItem removeFile) {
		this.removeFile = removeFile;
	}

	public JMenuItem getRemoveFolder() {
		return removeFolder;
	}

	public void setRemoveFolder(JMenuItem removeFolder) {
		this.removeFolder = removeFolder;
	}

	public JMenuItem getRepetir() {
		return repetir;
	}

	public void setRepetir(JMenuItem repetir) {
		this.repetir = repetir;
	}

	public JMenuItem getSalir() {
		return salir;
	}

	public void setSalir(JMenuItem salir) {
		this.salir = salir;
	}

	public JMenuItem getSalvarFich() {
		return salvarFich;
	}

	public void setSalvarFich(JMenuItem salvarFich) {
		this.salvarFich = salvarFich;
	}

	public JMenuItem getSave() {
		return save;
	}

	public void setSave(JMenuItem save) {
		this.save = save;
	}

	public JMenuItem getSaveAll() {
		return saveAll;
	}

	public void setSaveAll(JMenuItem saveAll) {
		this.saveAll = saveAll;
	}

	public JMenuItem getSelectAll() {
		return selectAll;
	}

	public void setSelectAll(JMenuItem selectAll) {
		this.selectAll = selectAll;
	}

	public JCheckBoxMenuItem getShowBrowserCBox() {
		return showBrowserCBox;
	}

	public void setShowBrowserCBox(JCheckBoxMenuItem showBrowserCBox) {
		this.showBrowserCBox = showBrowserCBox;
	}

	public JCheckBoxMenuItem getShowShellWindowCBox() {
		return showShellWindowCBox;
	}

	public void setShowShellWindowCBox(JCheckBoxMenuItem showShellWindowCBox) {
		this.showShellWindowCBox = showShellWindowCBox;
	}

	public JMenuItem getSintactica() {
		return sintactica;
	}

	public void setSintactica(JMenuItem sintactica) {
		this.sintactica = sintactica;
	}

	public JMenu getSyntaxSubMenu() {
		return syntaxSubMenu;
	}

	public void setSyntaxSubMenu(JMenu syntaxSubMenu) {
		this.syntaxSubMenu = syntaxSubMenu;
	}

	public JMenu getView() {
		return view;
	}

	public void setView(JMenu view) {
		this.view = view;
	}

	public void setAbrirProyecto(JMenuItem abrirProyecto) {
		this.abrirProyecto = abrirProyecto;
	}

	public void setNewProjectFile(JMenuItem newProjectFile) {
		this.newProjectFile = newProjectFile;
	}

	public void setAnadirFichero(JMenuItem anadirFichero) {
		this.anadirFichero = anadirFichero;
	}

	public void setBuscar(JMenuItem buscar) {
		this.buscar = buscar;
	}

	public void setGuardarProyecto(JMenuItem guardarProyecto) {
		this.guardarProyecto = guardarProyecto;
	}

	public void setMenuBar(JMenuBar menuBar) {
		this.menuBar = menuBar;
	}

	public void setNuevoProyecto(JMenuItem nuevoProyecto) {
		this.nuevoProyecto = nuevoProyecto;
	}

	public void setPrint(JMenuItem print) {
		this.print = print;
	}

	public void setUndo(UndoManager undo) {
		this.undo = undo;
	}

	public JMenuItem getRemoveFile() {
		return removeFile;
	}

	public JMenuItem getCloseProject() {
		return closeProject;
	}

	public void setCloseProject(JMenuItem closeProject) {
		this.closeProject = closeProject;
	}

	public JMenuItem getCloseAll() {
		return closeAll;
	}

	public void setCloseAll(JMenuItem closeAll) {
		this.closeAll = closeAll;
	}

	public JMenuItem getCloseFile() {
		return closeFile;
	}

	public void setCloseFile(JMenuItem closeFile) {
		this.closeFile = closeFile;
	}

	public JMenuItem getCompiler() {
		return compiler;
	}

	public void setCompiler(JMenuItem compiler) {
		this.compiler = compiler;
	}

	public JMenuItem getNewLexical() {
		return newLexical;
	}

	public void setNewLexical(JMenuItem newLexical) {
		this.newLexical = newLexical;
	}

	public JMenuItem getSaveGrammar() {
		return saveGrammar;
	}

	public void setSaveGrammar(JMenuItem saveGrammar) {
		this.saveGrammar = saveGrammar;
	}

	public JMenuItem getSaveLexical() {
		return saveLexical;
	}

	public void setSaveLexical(JMenuItem saveLexical) {
		this.saveLexical = saveLexical;
	}

	public JMenuItem getSetCompilable() {
		return setCompilable;
	}

	public void setSetCompilable(JMenuItem setFile) {
		this.setCompilable = setFile;
	}

	public JMenuItem getSetMain() {
		return setMain;
	}

	public void setSetMain(JMenuItem setMain) {
		this.setMain = setMain;
	}

	public JMenuItem getUnsetCompilable() {
		return unsetCompilable;
	}

	public void setUnsetCompilable(JMenuItem unsetFile) {
		this.unsetCompilable = unsetFile;
	}

	public JMenuItem getLoadMenu() {
		return loadMenu;
	}

	public void setLoadMenu(JMenuItem loadMenu) {
		this.loadMenu = loadMenu;
	}

	public JMenuItem getLoadTB() {
		return loadTB;
	}

	public void setLoadTB(JMenuItem loadTB) {
		this.loadTB = loadTB;
	}

	public JMenuItem getModifyMenu() {
		return modifyMenu;
	}

	public void setModifyMenu(JMenuItem modifyMenu) {
		this.modifyMenu = modifyMenu;
	}

	public JMenuItem getModifyTB() {
		return modifyTB;
	}

	public void setModifyTB(JMenuItem modifyTB) {
		this.modifyTB = modifyTB;
	}

	public JMenuItem getNewMenu() {
		return newMenu;
	}

	public void setNewMenu(JMenuItem newMenu) {
		this.newMenu = newMenu;
	}

	public JMenuItem getNewTB() {
		return newTB;
	}

	public void setNewTB(JMenuItem newTB) {
		this.newTB = newTB;
	}

	public JMenuItem getSaveAsGrammar() {
		return saveAsGrammar;
	}

	public void setSaveAsGrammar(JMenuItem saveAsGrammar) {
		this.saveAsGrammar = saveAsGrammar;
	}

	public JMenuItem getSaveAsLexical() {
		return saveAsLexical;
	}

	public void setSaveAsLexical(JMenuItem saveAsLexical) {
		this.saveAsLexical = saveAsLexical;
	}

	public JMenuItem getSaveAsMenu() {
		return saveAsMenu;
	}

	public void setSaveAsMenu(JMenuItem saveAsMenu) {
		this.saveAsMenu = saveAsMenu;
	}

	public JMenuItem getSaveAsTB() {
		return saveAsTB;
	}

	public void setSaveAsTB(JMenuItem saveAsTB) {
		this.saveAsTB = saveAsTB;
	}

	public JMenuItem getSaveMenu() {
		return saveMenu;
	}

	public void setSaveMenu(JMenuItem saveMenu) {
		this.saveMenu = saveMenu;
	}

	public JMenuItem getSaveTB() {
		return saveTB;
	}

	public void setSaveTB(JMenuItem saveTB) {
		this.saveTB = saveTB;
	}

	public JMenuItem getSaveAsProject() {
		return saveAsProject;
	}

	public void setSaveAsProject(JMenuItem saveAsProject) {
		this.saveAsProject = saveAsProject;
	}

	// mig
	public void setDeleteFile(JMenuItem deleteFile) {
		this.deleteFile = deleteFile;
	}

	public void setDeleteFile2(JMenuItem deleteFile) {
		this.deleteFile2 = deleteFile;
	}

	public JMenuItem getDeleteFile() {
		return deleteFile;
	}

	public JMenuItem getDeleteFile2() {
		return deleteFile2;
	}

	public void setRemoveFile2(JMenuItem removeFile) {
		this.removeFile2 = removeFile;
	}

	public JMenuItem getRemoveFile2() {
		return removeFile2;
	}

	public JMenuItem getUnsetMain() {
		return unsetMain;
	}

	public void setUnsetMain(JMenuItem a) {
		unsetMain = a;
	}

	public JMenuItem getSetCompilable2() {
		return setCompilable2;
	}

	public void setSetCompilable2(JMenuItem a) {
		this.setCompilable2 = a;
	}

	public JMenuItem getUnsetCompilable2() {
		return unsetCompilable2;
	}

	public void setUnsetCompilable2(JMenuItem a) {
		this.unsetCompilable2 = a;
	}

	public JMenuItem getSetMain2() {
		return setMain2;
	}

	public void setSetMain2(JMenuItem a) {
		this.setMain2 = a;
	}

	public JMenuItem getUnsetMain2() {
		return unsetMain2;
	}

	public void setUnsetMain2(JMenuItem a) {
		this.unsetMain2 = a;
	}

	public boolean isShellIsFocus() {
		return shellIsFocus;
	}

	public void setShellIsFocus(boolean shellIsFocus) {
		this.shellIsFocus = shellIsFocus;
	}

	public boolean isNPF() {
		return isNPF;
	}

	public void setIsNPF(boolean b) {
		isNPF = b;
	}

}// Menu

// ********CLASES OYENTES**********
/**
 * Clase Oyente del evento menu (Salir). Clase que construye el interfaz de los
 * menus
 */
class Menu_salir_Adaptador implements ActionListener {

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	public void actionPerformed(ActionEvent e) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		int opcion = JOptionPane.showConfirmDialog(null, labels
				.getString("s76"), labels.getString("s953"),
				JOptionPane.YES_NO_OPTION);
		if (opcion == JOptionPane.OK_OPTION) {
			logger.info(labels.getString("s77"));

			// mig
			Ventana v = Ventana.getInstance();

			if (v.getProyecto().isModified()) {

				int res = JOptionPane.showConfirmDialog(null, labels
						.getString("s657"), labels.getString("s953"),
						JOptionPane.YES_NO_OPTION);

				if (res == JOptionPane.OK_OPTION) {
					v.getnuevoMenu().getGuardarProyecto().setEnabled(true);
					v.getnuevoMenu().getGuardarProyecto().doClick();
					// v.getnuevoMenu().getGuardarProyecto().setEnabled(false);
				}
			}
			int eS = v.getCreadorEditor().getEditorSeleccionado();
			int editor = v.getCreadorEditor().dameNumEditores();
			v.getCreadorEditor().setEditorSeleccionado(editor - 1);
			for (int z = editor - 1; z >= 0; z--) {
				v.getCreadorEditor().setEditorSeleccionado(z);
				if (v.getCreadorEditor().isRedButton() == true) {
					int opt = JOptionPane.showConfirmDialog(null, labels
							.getString("s643"), labels.getString("s953"),
							JOptionPane.YES_NO_OPTION);

					if (opt == JOptionPane.OK_OPTION) {
						v.getnuevoMenu().save_o_saveAS();
					}
				}
			}
			v.getCreadorEditor().setEditorSeleccionado(eS);

			Ventana.getInstance().getnuevaSalida().ejecutaComandoSalida();
			try {
				String currentMenu = almacenPropiedades
						.getPropiedad("currentMenuCfg");
				if ((currentMenu.endsWith("lastModified.menuCfg"))
						|| (currentMenu.endsWith("newMenu.menuCfg"))) {
					String previous = almacenPropiedades
							.getPropiedad("previousMenuCfg");
					almacenPropiedades.setPropiedad("currentMenuCfg", previous);
				}
				String currentTB = almacenPropiedades
						.getPropiedad("currentTBCfg");
				if ((currentTB.endsWith("lastModified.BHcfg"))
						|| currentTB.endsWith("newTB.BHcfg")) {
					String previous = almacenPropiedades
							.getPropiedad("previousTBCfg");
					almacenPropiedades.setPropiedad("currentTBCfg", previous);
				}
				String currentGrammar = almacenPropiedades
						.getPropiedad("currentGrammar");
				if ((currentGrammar.endsWith("lastModified.jar"))
						|| (currentGrammar.endsWith("newGrammar.jar"))) {
					String previous = almacenPropiedades
							.getPropiedad("previousGrammar");
					almacenPropiedades.setPropiedad("currentGrammar", previous);
				}
			} catch (Exception e1) {
				JOptionPane.showMessageDialog(null, e1.getMessage(), labels
						.getString("s294"), JOptionPane.ERROR_MESSAGE);
			}
			// ////////// SAVE DEFAULT CONFIGURATION//////
			FactoriaOperaciones facto = FactoriaOperaciones.getInstance();
			DefaultConfiguration dc = facto.buildDefaultConfiguration();
			dc.saveDefaultConf();
			// ///////////////
			// /SAVE DEFAULT PROJECT
			Ventana.getInstance().closeDefaultPrj();
			v.getProyecto().salvarProy2();

			System.exit(0);

			// //////////////////////////////////
		} else if ((opcion == JOptionPane.NO_OPTION)
				|| (opcion == JOptionPane.CANCEL_OPTION)) {
			// Muestra resultado operacion en el log
			logger.info(labels.getString("s78"));
		}
	}
}

/**
 * Clase Oyente del evento menu (Nuevo)
 * 
 * 
 */
class Menu_nuevo_Adaptador implements ActionListener {

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	Ventana v = Ventana.getInstance();

	public void actionPerformed(ActionEvent e) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		// v.getnuevoMenu().habilita_salvarFich();
		// mig
		v.getnuevoMenu().habilitaMenuArchivo();
		v.getnuevoMenu().habilitaMenuEdicion();
		CreadorEditor ce = v.getCreadorEditor();
		ce.nuevaPestaña(labels.getString("s79"), labels.getString("s79"), "",
				true, 0);
		// Muestra en el log
		logger.info(labels.getString("s80"));
		// v.getnuevaSalida().cargaTexto(labels.getString("s81"));

		// UNDO REDO
		// v.getnuevoMenu().habilita_salvarFich();
		v.getnuevoMenu().habilitaMenuArchivo();
		v.getnuevoMenu().habilitaMenuEdicion();
		int numeditor = v.getCreadorEditor().getEditorSeleccionado();
		DefaultStyledDocument doc = v.getCreadorEditor().EditorSeleccionado()
				.getDoc();
		// status
		v.getStatusBar().setMessage(labels.getString("s79"));
		doc.addUndoableEditListener(new UndoableEditListener() {
			public void undoableEditHappened(UndoableEditEvent evt) {
				Ventana v = Ventana.getInstance();
				UndoableEdit edit = evt.getEdit();
				if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
						.getType() == DefaultDocumentEvent.EventType.CHANGE))) {
					v.getnuevoMenu().getUndo().addEdit(evt.getEdit());
				}

			}
		});
	}

}

/**
 * Clase Oyente del evento menu (Abrir Fichero)
 * 
 * 
 */
class Menu_abrirFich_Adaptador implements ActionListener {

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	FactoriaES fact = FactoriaES.getInstance();

	public void actionPerformed(ActionEvent e) {

		Ventana v = Ventana.getInstance();
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Fichero f = fact.generaFichero();
		String archivo = " ";
		archivo = f.leer();
		if (archivo == null) {
			// v.getnuevaSalida().cargaTexto(labels.getString("s82"));
			logger.info(labels.getString("s83"));
		} else {

			boolean b = false;
			int s = -1;
			for (int j = 0; j < v.getCreadorEditor().dameNumEditores(); j++) {
				if (v.getCreadorEditor().dameEditorI(j).getPath().equals(
						archivo)) {
					b = true;
					s = j;
				}
			}
			if (!b) {// si no esta abierto

				String texto = null;
				texto = f.cargar(archivo);

				if (texto != null) {
					int j = -1;
					for (int z = 0; z < v.getProyecto().dameNumFich(); z++) {
						if (v.getProyecto().getfich(z).getPath()
								.equals(archivo))
							j = z;
					}

					// si el archivo pertenece al proyecto
					if (j > -1) {
						// se mira el tipo
						int t = 0;
						// status
						v.getStatusBar().setMessage(
								v.getProyecto().getfich(j).getPath());
						if (v.getProyecto().getfich(j).isSetFile()) {
							t = 2;
							// status
							v.getStatusBar().setMessage(
									v.getProyecto().getfich(j).getPath()
											+ " <COMPILABLE>");
						}
						if (v.getProyecto().getfich(j).isMainFile()) {
							t = 1;
							// status
							v.getStatusBar().setMessage(
									v.getProyecto().getfich(j).getPath()
											+ " <MAIN>");
						}
						v.getCreadorEditor().nuevaPestaña(archivo, archivo,
								texto, true, t);
					} else {
						v.getStatusBar().setMessage(archivo);
						v.getCreadorEditor().nuevaPestaña(archivo, archivo,
								texto, true, 0);
					}

					// Muestra en el log
					logger.info(labels.getString("s84") + archivo);
					/*
					 * v.getnuevaSalida().cargaTexto( labels.getString("s85") +
					 * archivo + labels.getString("s86"));
					 */
					logger.info(labels.getString("s85") + archivo
							+ labels.getString("s86"));

					// UNDO REDO
					// v.getnuevoMenu().habilita_salvarFich();
					v.getnuevoMenu().habilitaMenuArchivo();
					v.getnuevoMenu().habilitaMenuEdicion();
					int numeditor = v.getCreadorEditor()
							.getEditorSeleccionado();
					DefaultStyledDocument doc = v.getCreadorEditor()
							.EditorSeleccionado().getDoc();

					doc.addUndoableEditListener(new UndoableEditListener() {
						public void undoableEditHappened(UndoableEditEvent evt) {
							Ventana v = Ventana.getInstance();
							UndoableEdit edit = evt.getEdit();
							if (edit instanceof DefaultDocumentEvent
									&& ((DefaultDocumentEvent) edit).getType() == DefaultDocumentEvent.EventType.CHANGE) {
								return;
							} else {
								v.getnuevoMenu().getUndo().addEdit(
										evt.getEdit());
							}
						}
					});
					// CURSOR EN LA PRIMERA POSICION DEL EDITOR DE TEXTO
					numeditor = v.getCreadorEditor().getEditorSeleccionado();
					Editor.getEditor().setCaretPosition(0);

					// mig
					for (int z = 0; z < v.getProyecto().getFichSize(); z++) {
						if (v.getProyecto().getfich(z).getPath()
								.equals(archivo)) {
							v.getProyecto().getfich(z).setOpened(true);
						}
					}
					String prj = null;
					try {
						prj = almacenPropiedades
								.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if (!(prj.equals(".//configuration/Default.acidePrj") && v
							.getProyecto().getnombreProy().equals(""))) {
						v.getProyecto().setModified(true);
					}

				} else {
					// v.getnuevaSalida().cargaTexto(labels.getString("s87"));
					logger.info(labels.getString("s88"));
				}

			} else {
				v.getCreadorEditor().setEditorSeleccionado(s);
			}
		}

	}

}

/**
 * Clase Oyente del evento menu (salvar Fichero Como...)
 * 
 * 
 */
class Menu_salvarFich_Adaptador implements ActionListener {

	Logger logger = Log.getLog();

	FactoriaES fact = FactoriaES.getInstance();

	Ventana v = Ventana.getInstance();

	public void actionPerformed(ActionEvent e) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Fichero f = fact.generaFichero();
		String archivo = " ";
		Ventana v = Ventana.getInstance();
		if (v.getCreadorEditor().dameNumEditores() == 0) {
			logger.info(labels.getString("s89"));

		} else {
			archivo = f.escribir();

			if (archivo.equals(" ")) {

				logger.info(labels.getString("s92"));
			} else {
				boolean resultado = f.salvar(archivo, v.getCreadorEditor()
						.EditorSeleccionado().getTexto());

				// Muestra en el log
				if (resultado) {
					logger.info(labels.getString("s93") + archivo
							+ labels.getString("s94"));
					Ventana.getInstance().getCreadorEditor().greenButton();
					int in = archivo.lastIndexOf("\\");
					in++;
					String file = archivo.substring(in, archivo.length());
					v.getCreadorEditor().getPane().setTitleAt(
							v.getCreadorEditor().getPane().getSelectedIndex(),
							file);
					v.getCreadorEditor().EditorSeleccionado().setPath(archivo);
					v.getCreadorEditor().getPane().setToolTipText(archivo);
					// mig
					File fich = new File(v.getCreadorEditor()
							.EditorSeleccionado().getPath());
					Ventana.getInstance().getCreadorEditor()
							.EditorSeleccionado().setUltimoCambio(
									fich.lastModified());
					Ventana.getInstance().getCreadorEditor()
							.EditorSeleccionado().setUltimoTam(fich.length());

				} else {
					logger.info(labels.getString("s95") + archivo);
				}
			}
		}
	}
}

/**
 * Clase Menu salvar Fichero
 * 
 * 
 */
class Menu_save_Adaptador implements ActionListener {
	// mig
	Logger logger = Log.getLog();

	FactoriaES fact = FactoriaES.getInstance();

	Ventana v = Ventana.getInstance();

	public void actionPerformed(ActionEvent e) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Fichero f = fact.generaFichero();
		String archivo = " ";
		Ventana v = Ventana.getInstance();
		if (v.getCreadorEditor().dameNumEditores() == 0) {
			logger.info(labels.getString("s89"));
			// v.getnuevaSalida().cargaTexto(labels.getString("s90"));
		} else {
			if (v.getCreadorEditor().EditorSeleccionado().getPath().equals(
					labels.getString("s79")) == false) {
				boolean resultado = f.salvar(v.getCreadorEditor()
						.EditorSeleccionado().getPath(), v.getCreadorEditor()
						.EditorSeleccionado().getTexto());

				// Muestra en el log
				if (resultado) {
					/*
					 * v.getnuevaSalida() .cargaTexto(
					 * 
					 * labels.getString("s93") + archivo +
					 * labels.getString("s94"));
					 */logger.info(labels.getString("s93") + archivo
							+ labels.getString("s94"));
					/*
					 * String sold = Ventana.getInstance().getCreadorEditor()
					 * .getPane().getTitleAt(
					 * Ventana.getInstance().getCreadorEditor()
					 * .getEditorSeleccionado()); sold = sold.replace(" *", "");
					 * Ventana.getInstance().getCreadorEditor().getPane().setTitleAt(
					 * Ventana.getInstance().getCreadorEditor()
					 * .getEditorSeleccionado(), sold);
					 */
					Ventana.getInstance().getCreadorEditor().greenButton();
					// mig
					File fich = new File(v.getCreadorEditor()
							.EditorSeleccionado().getPath());
					Ventana.getInstance().getCreadorEditor()
							.EditorSeleccionado().setUltimoCambio(
									fich.lastModified());
					Ventana.getInstance().getCreadorEditor()
							.EditorSeleccionado().setUltimoTam(fich.length());

				} else {
					/*
					 * v.getnuevaSalida().cargaTexto( labels.getString("s95") +
					 * archivo);
					 */logger.info(labels.getString("s95") + archivo);
				}
			} else {
				v.getnuevoMenu().getSalvarFich().setEnabled(true);
				v.getnuevoMenu().getSalvarFich().doClick();
				// v.getnuevoMenu().getSalvarFich().setEnabled(false);
			}
		}
	}
}

/**
 * Clase Oyente del evento menu (Buscar)
 * 
 * 
 */
class Menu_buscar_Adaptador implements ActionListener {

	FactoriaGUI factGUI = FactoriaGUI.getInstance();

	Ventana v = Ventana.getInstance();

	public void actionPerformed(ActionEvent e) {

		SearchGUI bu = SearchGUI.getInstance();
		if (bu.isShowing())
			bu.setVisible(false);
		else
			bu.setVisible(true);
	}

}

/**
 * Clase Oyente del evento menu (Reemplazar)
 * 
 * 
 */
class Menu_reemplazar_Adaptador implements ActionListener {

	Logger logger = Log.getLog();

	FactoriaGUI fact = FactoriaGUI.getInstance();

	public void actionPerformed(ActionEvent e) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		logger.info(labels.getString("s96"));
		ReplaceGUI re = ReplaceGUI.getInstance();
		if (re.isShowing())
			re.setVisible(false);
		else
			re.setVisible(true);
		Ventana v = Ventana.getInstance();
		v.getnuevaSalida().cargaTexto(labels.getString("s96"));
	}
}

class Menu_deshacer_Adaptador implements ActionListener {
	Ventana v = Ventana.getInstance();

	public void actionPerformed(ActionEvent arg0) {

		try {

			if (v.getnuevoMenu().getUndo().canUndo()) {
				v.getnuevoMenu().getUndo().undo();
				;
			} else {
				v.getnuevoMenu().getUndo().die();
			}
		} catch (CannotUndoException e) {
		}

	}

}

class Menu_repetir_Adaptador implements ActionListener {
	Ventana v = Ventana.getInstance();

	public void actionPerformed(ActionEvent arg0) {
		try {
			if (v.getnuevoMenu().getUndo().canRedo()) {
				v.getnuevoMenu().getUndo().redo();
			}
		} catch (CannotRedoException e) {
		}

	}
}

/**
 * Clase oyente de la opcion de menu CORTAR
 */
class Menu_cortar_Adaptador implements ActionListener {
	Ventana v = Ventana.getInstance();

	Logger logger = Log.getLog();

	public void actionPerformed(ActionEvent e) {
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
		if (v.getCreadorEditor().dameNumEditores() > 0)
			v.getCreadorEditor().dameEditorI(
					v.getCreadorEditor().getEditorSeleccionado()).getEditor()
					.cut();
		v.getnuevaSalida().getSalida().cut();
	}
}

/**
 * Clase oyente de la opcion de menu PEGAR
 */
class Menu_pegar_Adaptador implements ActionListener {
	Ventana v = Ventana.getInstance();

	Logger logger = Log.getLog();

	public void actionPerformed(ActionEvent e) {
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

		int editor = v.getCreadorEditor().dameNumEditores();
		if (editor > 0) {
			if (v.getnuevoMenu().isShellIsFocus()) {
				v.getnuevaSalida().getSalida().paste();
			} else
				v.getCreadorEditor().dameEditorI(
						v.getCreadorEditor().getEditorSeleccionado())
						.getEditor().paste();
		} else {
			v.getnuevaSalida().getSalida().paste();
		}
	}
}

/**
 * Clase oyente de la opcion de menu COPIAR
 */
class Menu_copiar_Adaptador implements ActionListener {
	Ventana v = Ventana.getInstance();

	Logger logger = Log.getLog();

	public void actionPerformed(ActionEvent e) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		logger.info(labels.getString("s99"));
		if (v.getCreadorEditor().dameNumEditores() > 0)
			v.getCreadorEditor().dameEditorI(
					v.getCreadorEditor().getEditorSeleccionado()).getEditor()
					.copy();
		v.getnuevaSalida().cargaTexto(labels.getString("s99"));
		v.getnuevaSalida().getSalida().copy();
	}
}

/**
 * Clase Oyente del evento menu (Parametrizacion-Lexical)
 * 
 * 
 */
class Menu_lexico_Adaptador implements ActionListener {
	FactoriaGUI fact = FactoriaGUI.getInstance();

	public void actionPerformed(ActionEvent e) {

		fact.generaLexicaGUI();

	}

}

/**
 * Clase oyente de la configuración de menús
 */
class Menu_menu_Adaptor implements ActionListener {
	FactoriaGUI fact = FactoriaGUI.getInstance();

	public void actionPerformed(ActionEvent e) {
		fact.generaMenuGUI();
		/*
		 * Ventana v = Ventana.getInstance(); v.getnuevoMenu().setMenuConfig();
		 * v.repaint();
		 */
	}
}

/**
 * Clase oyente de la muestra del Log de la aplicación
 */
class Menu_mostrarLog_Adaptor implements ActionListener {
	FactoriaGUI fact = FactoriaGUI.getInstance();

	public void actionPerformed(ActionEvent e) {
		fact.generaLogTab();
	}
}

/**
 * Clase oyente de la opcion de menu que guarda una configuración
 */
/*
 * class Menu_salvarParam_Adaptor implements ActionListener {
 * 
 * public void actionPerformed(ActionEvent e) { Lenguaje l =
 * Lenguaje.getInstance(); l.guardar(l.getNombre(),
 * l.isCompiladoInterpretado()); System.out.println("11"); } }
 */

/**
 * Clase oyente de la opcion de menu que carga una configuración
 */
class Menu_compilador_Adaptor implements ActionListener {

	public void actionPerformed(ActionEvent e) {
		Fichero f = new Fichero();
		String path = f.leer();
	}
}

/**
 * Clase oyente de la opcion de menu que carga una configuración
 */
class Menu_interprete_Adaptor implements ActionListener {

	public void actionPerformed(ActionEvent e) {
		Fichero f = new Fichero();
		String path = f.leer();
	}
}

/**
 * Clase oyente de la opcion de menu que carga una configuración
 */
class Menu_parser_Adaptor implements ActionListener {

	public void actionPerformed(ActionEvent e) {
		Fichero f = new Fichero();
		String path = f.leer();
	}

}

/**
 * Clase Oyente del evento menu (Español)
 * 
 * 
 */
class Menu_español_Adaptor implements ActionListener {

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	public void actionPerformed(ActionEvent e) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		Ventana v = Ventana.getInstance();
		almacenPropiedades.setPropiedad("idioma", "0");
		ResourceBundle labels = Idioma.getInstance().getLabels();
		// Muestra en el log
		logger.info(labels.getString("s100"));
		v.getnuevaSalida().cargaTexto(labels.getString("s100"));
		v.getnuevoMenu().setMenu();
		Iconos.generaToolBarFija();
		Iconos.generaToolBarEditable();
		v.getNuevoExplorador().init_Popup();
		v.validate();
		v.repaint();

		SearchGUI bu = SearchGUI.getInstance();
		bu.inicializa();
		bu.validate();
		bu.repaint();
		ReplaceGUI ru = ReplaceGUI.getInstance();
		ru.inicializa();
		ru.validate();
		ru.repaint();
		labels = Idioma.getInstance().getLabels();
		Ventana.getInstance().getStatusBar().setMessageLexical(
				labels.getString("s449") + " "
						+ Lenguaje.getInstance().getNombre());
		// Fichero f = new Fichero();
		// String grammarName =
		// f.cargar(".\\configuration\\grammars\\currentGrammar\\grammarName.txt");
		// grammarName = grammarName.substring(0,grammarName.length() - 1);
		// Ventana.getInstance().getStatusBar().getMessageGrammar().setText(labels.getString("s248")
		// + " " + grammarName);
		try {
			String currentGrammar = almacenPropiedades
					.getPropiedad("currentGrammar");
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");
			String grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);
			v.getStatusBar().setMessageGrammar(
					labels.getString("s248") + " " + grammarName);
		} catch (Exception e1) {
			logger.error(e1.getMessage());
			JOptionPane.showMessageDialog(null, e1.getMessage(), labels
					.getString("s944"), JOptionPane.ERROR_MESSAGE);
		}
		// mig
		String prj = null;
		try {
			prj = almacenPropiedades.getPropiedad("DefaultAcidePrj");
		} catch (Exception e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		if (!(prj.equals(".//configuration/Default.acidePrj") && v
				.getProyecto().getnombreProy().equals(""))) {
			v.getnuevoMenu().habilitaMenuProyecto();
		}

		int editor = v.getCreadorEditor().dameNumEditores();
		if (editor > 0) {
			v.getnuevoMenu().habilitaMenuArchivo();
			v.getnuevoMenu().habilitaMenuEdicion();
		}
		// v.getProyecto().setModified(true);

	}

}

/**
 * Clase Oyente del evento menu (English)
 * 
 * 
 */
class Menu_english_Adaptor implements ActionListener {

	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private Logger logger = Log.getLog();

	public void actionPerformed(ActionEvent e) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		Ventana v = Ventana.getInstance();
		almacenPropiedades.setPropiedad("idioma", "1");
		ResourceBundle labels = Idioma.getInstance().getLabels();
		// Muestra en el log
		logger.info(labels.getString("s101"));
		v.getnuevaSalida().cargaTexto(labels.getString("s101"));
		v.getnuevoMenu().setMenu();
		v.getNuevoExplorador().init_Popup();
		Iconos.generaToolBarFija();
		Iconos.generaToolBarEditable();
		v.validate();
		v.repaint();
		SearchGUI bu = SearchGUI.getInstance();
		bu.inicializa();
		bu.validate();
		bu.repaint();
		ReplaceGUI ru = ReplaceGUI.getInstance();
		ru.inicializa();
		ru.validate();
		ru.repaint();
		labels = Idioma.getInstance().getLabels();
		Ventana.getInstance().getStatusBar().setMessageLexical(
				labels.getString("s449") + " "
						+ Lenguaje.getInstance().getNombre());
		// Fichero f = new Fichero();
		// String grammarName =
		// f.cargar(".\\configuration\\grammars\\currentGrammar\\grammarName.txt");
		// grammarName = grammarName.substring(0,grammarName.length() - 1);
		// Ventana.getInstance().getStatusBar().getMessageGrammar().setText(labels.getString("s248")
		// + " " + grammarName);
		try {
			String currentGrammar = almacenPropiedades
					.getPropiedad("currentGrammar");
			int index = currentGrammar.lastIndexOf("\\");
			if (index == -1)
				index = currentGrammar.lastIndexOf("/");
			String grammarName = currentGrammar.substring(index + 1,
					currentGrammar.length() - 4);
			v.getStatusBar().setMessageGrammar(
					labels.getString("s248") + " " + grammarName);
		} catch (Exception e1) {
			logger.error(e1.getMessage());
			JOptionPane.showMessageDialog(null, e1.getMessage(), labels
					.getString("s944"), JOptionPane.ERROR_MESSAGE);
		}
		// mig
		String prj = null;
		try {
			prj = almacenPropiedades.getPropiedad("DefaultAcidePrj");
		} catch (Exception e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		if (!(prj.equals(".//configuration/Default.acidePrj") && v
				.getProyecto().getnombreProy().equals(""))) {
			v.getnuevoMenu().habilitaMenuProyecto();
		}

		int editor = v.getCreadorEditor().dameNumEditores();
		if (editor > 0) {
			v.getnuevoMenu().habilitaMenuArchivo();
			v.getnuevoMenu().habilitaMenuEdicion();
		}
		// v.getProyecto().setModified(true);
	}
}

/**
 * Clase oyente de la muestra del nuevo Proyecto de la aplicación
 */
class Menu_nuevoProyecto_Adaptor implements ActionListener {
	FactoriaGUI fact = FactoriaGUI.getInstance();

	public void actionPerformed(ActionEvent e) {
		Ventana v = Ventana.getInstance();
		ResourceBundle labels = Idioma.getInstance().getLabels();
		boolean c = false;

		if (v.getProyecto().isModified()) {

			int res = JOptionPane.showConfirmDialog(null, labels
					.getString("s657"), labels.getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);
			if (res == JOptionPane.CANCEL_OPTION) {
				c = true;
			}
			if (res == JOptionPane.OK_OPTION) {
				v.getnuevoMenu().getGuardarProyecto().setEnabled(true);
				v.getnuevoMenu().getGuardarProyecto().doClick();
			}
		}
		int eS = v.getCreadorEditor().getEditorSeleccionado();
		int editor = v.getCreadorEditor().dameNumEditores();
		v.getCreadorEditor().setEditorSeleccionado(editor - 1);
		for (int z = editor - 1; z >= 0; z--) {
			v.getCreadorEditor().setEditorSeleccionado(z);
			if (v.getCreadorEditor().isRedButton() == true) {
				int opt = JOptionPane.showConfirmDialog(null, labels
						.getString("s643"), labels.getString("s953"),
						JOptionPane.YES_NO_OPTION);

				if (opt == JOptionPane.OK_OPTION) {
					v.getnuevoMenu().save_o_saveAS();
				}
			}
		}
		v.getCreadorEditor().setEditorSeleccionado(eS);

		if (!c) {

			v.setProyGUI(fact.generaProyectoGUI());

		}
	}
}

/**
 * Clase oyente Salvar proyecto
 */
class Menu_salvarProyecto_Adaptor implements ActionListener {

	public void actionPerformed(ActionEvent e) {
		FactoriaES fact = FactoriaES.getInstance();

		Ventana v = Ventana.getInstance();

		ResourceBundle labels = Idioma.getInstance().getLabels();
		Fichero f = fact.generaFichero();
		try {
			if (!v.getProyecto().getnombreProy().equals("")) {
				if (v.getProyecto().isFirstSave() == false) {
					v.getnuevoMenu().getSaveAsProject().setEnabled(true);
					v.getnuevoMenu().getSaveAsProject().doClick();
				} else {
					String len = almacenPropiedades.getPropiedad("idioma");
					String currentMenu = almacenPropiedades
							.getPropiedad("currentMenuCfg");
					String currentTB = almacenPropiedades
							.getPropiedad("currentTBCfg");
					String currentGrammar = almacenPropiedades
							.getPropiedad("currentGrammar");
					String pathLenguaje = almacenPropiedades
							.getPropiedad("pathLenguaje");
					String ejecutable = almacenPropiedades
							.getPropiedad("ejecutable");
					String pathEjecutable = almacenPropiedades
							.getPropiedad("pathEjecutable");
					v.getProyecto().setLenguaje(len);
					v.getProyecto().setCurrentMenu(currentMenu);
					v.getProyecto().setCurrentTB(currentTB);
					v.getProyecto().setGrammarConfig(currentGrammar);
					v.getProyecto().setLenguajeConfig(pathLenguaje);
					v.getProyecto().setShellPath(ejecutable);
					v.getProyecto().setShellDir(pathEjecutable);
					String cad = v.getProyecto().salvarProy();
					f.salvar(v.getProyecto().getpathProy(), cad);
					// mig
					v.getProyecto().setModified(false);
				}
			}
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
}

/**
 * Clase oyente abrir Proyecto
 * 
 * 
 */
class Menu_abrirProyecto_Adaptor implements ActionListener {
	FactoriaES fact = FactoriaES.getInstance();

	private DefaultMutableTreeNode search_listdir(ArrayList list, String f) {
		int i = 0;
		boolean encontrado = false;
		while (i < list.size() && !encontrado) {
			DefaultMutableTreeNode temp = (DefaultMutableTreeNode) list.get(i);
			Fich fich = (Fich) temp.getUserObject();
			if (fich.getName().equals(f)) {
				encontrado = true;
				return (DefaultMutableTreeNode) list.get(i);
			} else
				i++;
		}
		return null;
	}

	public void actionPerformed(ActionEvent e) {

		/*
		 * ResourceBundle labels = Idioma.getInstance().getLabels(); Fichero f =
		 * fact.generaFichero(); String[] ExtPide = new String[] { "acidePrj" };
		 * f.getFileChooser().addChoosableFileFilter( new ExtFilter(ExtPide,
		 * labels.getString("s328"))); String file =""; file = f.leer(); String
		 * cad = null; cad = f.cargar(file);
		 * 
		 * FactoriaOperaciones facto = FactoriaOperaciones.getInstance();
		 * LoadDefaultProject defaultPrj = facto.BuildLoadDefaultProject();
		 * defaultPrj.loadDefault(cad);
		 */

		Ventana v = Ventana.getInstance();
		ResourceBundle labels = Idioma.getInstance().getLabels();
		Fichero f = fact.generaFichero();
		Idioma id = Idioma.getInstance();

		boolean c = false;

		// v.getCreadorEditor().dameEditores().removeAll();
		// v.getnuevoMenu().getCloseAll().doClick();

		// Selecciona la extension del proyecto
		String[] ExtPide = new String[] { "acidePrj" };
		f.getFileChooser().addChoosableFileFilter(
				new ExtFilter(ExtPide, labels.getString("s328")));
		final String file;
		file = f.leer();
		
		if (file != null) {// se abre proyecto

			// salvar proyecto
			if (v.getProyecto().isModified()) {
				int res = JOptionPane.showConfirmDialog(null, labels
						.getString("s657"), labels.getString("s953"),
						JOptionPane.YES_NO_CANCEL_OPTION);
				if (res == JOptionPane.CANCEL_OPTION) {
					c = true;
				}
				if (res == JOptionPane.OK_OPTION) {
					v.getnuevoMenu().getGuardarProyecto().setEnabled(true);
					v.getnuevoMenu().getGuardarProyecto().doClick();
				}
			}

			if (!c) {// no se cancela a la pregunta de si salvar el proyecto
				
				// poner cursor de espera
				Cursor cu = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
				v.setCursor(cu);
				
				v.getStatusBar().setMessage("");
				
				Thread th = new Thread(){
					public void run(){
						load( file);
					}
				};
				th.start();

				

				// Ventana.getInstance().getStatusBar().setMessageLexical(labels.getString("s449")
				// + " " + Lenguaje.getInstance().getNombre());
				// v.getProyecto().setLenguajeConfig(Lenguaje.getInstance().getNombre());

				// Load idioma
				String idioma = v.getProyecto().getLenguaje();
				if (idioma.equals("0")) {
					v.getnuevoMenu().getEspañol().doClick();
				} else {
					v.getnuevoMenu().getEnglish().doClick();
				}

				try {
					id.seleccionIdioma(Integer.parseInt(almacenPropiedades
							.getPropiedad("idioma")));
				} catch (NumberFormatException e2) {
					e2.printStackTrace();
				} catch (Exception e2) {
					e2.printStackTrace();
				}
				labels = id.getLabels();
				v.getStatusBar().setMessageLexical(
						labels.getString("s449") + " ");

				// Load Menu config
				/*
				 * boolean[] valores = null; valores =
				 * MenuConfig.cargarMenuCfgFich(v.getProyecto().getCurrentMenu());
				 * almacenPropiedades.setPropiedad("currentMenuCfg",v.getProyecto().getCurrentMenu());
				 * MenuConfig.setAll(valores); v.getnuevoMenu().setMenuConfig();
				 * v.validate(); v.repaint();
				 * v.getnuevoMenu().getSaveMenu().setEnabled(true);
				 * MenuGUI.setChangesSaved(true);
				 */

				// Load Menu config
				// ///VERSION ANTIERRORES
				String currentMenu = null;
				try {
					currentMenu = v.getProyecto().getCurrentMenu();
					boolean[] valores = MenuConfig
							.cargarMenuCfgFich(currentMenu);
					MenuConfig.setAll(valores);
					almacenPropiedades.setPropiedad("currentMenuCfg",
							currentMenu);
					// logger.info(labels.getString("s70") + " " + currentMenu);
				} catch (Exception e2) {
					// logger.info(labels.getString("s71") + e.getMessage());
					// sacar ruta relativa
					String currentMenu2;
					int index = currentMenu.lastIndexOf("\\");
					if (index == -1)
						index = currentMenu.lastIndexOf("/");
					currentMenu2 = ".\\configuration\\menu\\"
							+ currentMenu.substring(index + 1, currentMenu
									.length());
					boolean[] valores = null;
					try {
						valores = MenuConfig.cargarMenuCfgFich(currentMenu2);
						MenuConfig.setAll(valores);
						almacenPropiedades.setPropiedad("currentMenuCfg",
								currentMenu2);
						// logger.info(labels.getString("s70") + " " +
						// currentMenu2);
						JOptionPane.showMessageDialog(null, labels
								.getString("s956")
								+ currentMenu
								+ labels.getString("s957")
								+ currentMenu2);
					} catch (Exception e1) {
						// e1.printStackTrace();
						try {
							valores = MenuConfig
									.cargarMenuCfgFich(".//configuration/menu/default_allOn.menuCfg");
						} catch (Exception e3) {
							e3.printStackTrace();
						}
						MenuConfig.setAll(valores);
						almacenPropiedades.setPropiedad("currentMenuCfg",
								".//configuration/menu/default_allOn.menuCfg");
						JOptionPane.showMessageDialog(null, labels
								.getString("s956")
								+ currentMenu + labels.getString("s959"));
					}

					/*
					 * MenuConfig.todosActivados(); boolean[] valores = new
					 * boolean[73]; for (int j = 0; j < valores.length; j++)
					 * valores[j] = true; String defaultMenu =
					 * ".//configuration/menu/default_allOn.menuCfg";
					 * MenuConfig.guardarMenuCfgFich(defaultMenu,valores);
					 */
				}
				v.getnuevoMenu().setMenuConfig();
				v.validate();
				v.repaint();
				v.getnuevoMenu().getSaveMenu().setEnabled(true);
				MenuGUI.setChangesSaved(true);
				// ///

				// Load Grammar
				// Fichero fi=new Fichero();
				// String grammarName =
				// fi.cargar(".\\configuration\\grammars\\currentGrammar\\grammarName.txt");
				// grammarName = grammarName.substring(0,grammarName.length() -
				// 1);
				// Ventana.getInstance().getStatusBar().getMessageGrammar().setText(labels.getString("s248")
				// + " " + grammarName);
				try {
					// String currentGrammar =
					// almacenPropiedades.getPropiedad("currentGrammar");
					String currentGrammar = v.getProyecto().getGrammarConfig();
					int index = currentGrammar.lastIndexOf("\\");
					if (index == -1)
						index = currentGrammar.lastIndexOf("/");
					String grammarName = currentGrammar.substring(index + 1,
							currentGrammar.length() - 4);
					v.getStatusBar().setMessageGrammar(
							labels.getString("s248") + " " + grammarName);
					almacenPropiedades.setPropiedad("currentGrammar",
							currentGrammar);
				} catch (Exception e1) {
					// logger.error(e1.getMessage());
					JOptionPane.showMessageDialog(null, e1.getMessage(), labels
							.getString("s944"), JOptionPane.ERROR_MESSAGE);
				}

				// mig
				// Load shell
				almacenPropiedades.setPropiedad("pathEjecutable", v
						.getProyecto().getShellDir());
				almacenPropiedades.setPropiedad("ejecutable", v.getProyecto()
						.getShellPath());
				// almacenPropiedades.setPropiedad("echoCommand",
				// v.getProyecto().getEchoCommand());
				// almacenPropiedades.setPropiedad("exitCommand",
				// v.getProyecto().getExitCommand());
				Ventana.getInstance().getnuevaSalida().ejecutaComandoSalida();
				v.getnuevaSalida().resetSalida();

				// Load TB config
				/*
				 * ListaIconosEditables.cargaLista(v.getProyecto().getCurrentTB());
				 * ListaIconosEditables.cargaListaAux(v.getProyecto().getCurrentTB());
				 * Iconos.generaToolBarFija(); Iconos.generaToolBarEditable();
				 * almacenPropiedades.setPropiedad("currentTBCfg",v.getProyecto().getCurrentTB());
				 * v.validate(); v.repaint();
				 * v.getnuevoMenu().getSaveTB().setEnabled(true);
				 * EdicionIconosGUI.setChangesSaved(true);
				 */

				// Load TB config
				// //VERSION ANTIERRORES
				String currentTBCfg = null;
				try {
					ListaIconosEditables.limpiaListas();
					currentTBCfg = v.getProyecto().getCurrentTB();
					ListaIconosEditables.cargaLista(currentTBCfg);
					almacenPropiedades.setPropiedad("currentTBCfg",
							currentTBCfg);
				} catch (Exception e2) {
					// sacar ruta relativa
					String currentTBCfg2;
					int index = currentTBCfg.lastIndexOf("\\");
					if (index == -1)
						index = currentTBCfg.lastIndexOf("/");
					currentTBCfg2 = ".\\configuration\\toolbar\\"
							+ currentTBCfg.substring(index + 1, currentTBCfg
									.length());
					try {
						ListaIconosEditables.cargaLista(currentTBCfg2);
						JOptionPane.showMessageDialog(null, labels
								.getString("s958")
								+ currentTBCfg
								+ labels.getString("s957")
								+ currentTBCfg2);
						almacenPropiedades.setPropiedad("currentTBCfg",
								currentTBCfg2);
					} catch (Exception e1) {
						// logger.error(labels.getString("s127"));
						try {
							ListaIconosEditables
									.cargaLista(".//configuration/toolbar/default.BHcfg");
						} catch (Exception e3) {
							e3.printStackTrace();
						}
						JOptionPane.showMessageDialog(null, labels
								.getString("s958")
								+ currentTBCfg + labels.getString("s959"));
						almacenPropiedades.setPropiedad("currentTBCfg",
								".//configuration/toolbar/default.BHcfg");
					}
					// logger.error(labels.getString("s127"));
				}
				Iconos.generaToolBarFija();
				Iconos.generaToolBarEditable();
				v.validate();
				v.repaint();
				v.getnuevoMenu().getSaveTB().setEnabled(true);
				EdicionIconosGUI.setChangesSaved(true);
				// //

				// dibuja arbol del explorador
				v.getNuevoExplorador().getRaiz().removeAllChildren();
				Fich pa = new Fich();
				pa.setPath(v.getProyecto().getnombreProy());
				pa.setName(v.getProyecto().getnombreProy());
				pa.setDirectory(true);
				pa.setPadre(null);
				v.setTitle(labels.getString("s425") + " - "
						+ v.getProyecto().getnombreProy());
				DefaultMutableTreeNode d = new DefaultMutableTreeNode(pa);
				d.setAllowsChildren(true);
				v.getNuevoExplorador().getRaiz().add(d);
				ArrayList listdir = new ArrayList();
				for (int i = 0; i < v.getProyecto().dameNumFich(); i++) {

					/*
					 * if(v.getProyecto().getfich(i).isOpened()){ //ABRIR
					 * ARCHIVO EN EL EDITOR boolean b=false; for(int a=0; a<v.getCreadorEditor().dameNumEditores();
					 * a++){
					 * if(v.getCreadorEditor().dameEditorI(a).getPath().equals(v.getProyecto().getfich(i).getPath())){
					 * b = true; } }
					 * 
					 * if(!b){ Fichero fd = new Fichero(); CreadorEditor ce =
					 * v.getCreadorEditor();
					 * ce.nuevaPestaña(v.getProyecto().getfich(i).getPath(),
					 * v.getProyecto().getfich(i).getPath(),
					 * fd.cargar(v.getProyecto().getfich(i).getPath()), true);
					 *  // UNDO REDO //v.getnuevoMenu().habilita_salvarFich();
					 * v.getnuevoMenu().habilitaMenuArchivo();
					 * v.getnuevoMenu().habilitaMenuEdicion(); int numeditor =
					 * v.getCreadorEditor() .getEditorSeleccionado(); Document
					 * doc =
					 * v.getCreadorEditor().dameEditorI(numeditor).getEditor().getDocument();
					 * doc.addUndoableEditListener(new UndoableEditListener() {
					 * public void undoableEditHappened(UndoableEditEvent evt) {
					 * Ventana v = Ventana.getInstance(); UndoableEdit edit =
					 * evt.getEdit(); if (edit instanceof DefaultDocumentEvent &&
					 * ((DefaultDocumentEvent)edit).getType() ==
					 * DefaultDocumentEvent.EventType.CHANGE) { return; }else{
					 * v.getnuevoMenu().getUndo().addEdit(evt.getEdit()); } }
					 * }); // CURSOR EN LA PRIMERA POSICION DEL EDITOR DE TEXTO
					 * numeditor = v.getCreadorEditor()
					 * .getEditorSeleccionado();
					 * v.getCreadorEditor().dameEditorI(numeditor).getEditor()
					 * .setCaretPosition(0);
					 *  }
					 *  }
					 */

					DefaultMutableTreeNode h = new DefaultMutableTreeNode(v
							.getProyecto().getfich(i));
					if (v.getProyecto().getfich(i).isDirectory()) {
						h.setAllowsChildren(true);
						listdir.add(h);
					} else
						h.setAllowsChildren(false);

					if (v.getProyecto().getfich(i).getPadre().equals(
							v.getProyecto().getnombreProy())) {
						d.add(h);
					} else {
						DefaultMutableTreeNode fh = search_listdir(listdir, v
								.getProyecto().getfich(i).getPadre());

						fh.add(h);
					}
				}

				v.getNuevoExplorador().getTreeModel().reload();
				v.getNuevoExplorador().expandTree();
				if (v.getProyecto().dameNumFich() > 0) {
					v.getNuevoExplorador().setEnabledRemoveFile();
					v.getNuevoExplorador().setEnabledDeleteFile();
				} else {
					v.getNuevoExplorador().getRemoveFile().setEnabled(false);
					v.getNuevoExplorador().getDeleteFile().setEnabled(false);
				}
				v.validate();
				v.repaint();
				// mig
				if (!v.getnuevoMenu().getShowBrowserCBox().isSelected())
					v.getNuevoExplorador().showExplorer();
				v.getnuevoMenu().getShowBrowserCBox().setSelected(true);
				v.getProyecto().setModified(false);
				v.getProyecto().setFirstSave(true);
				v.getnuevoMenu().habilitaMenuProyecto();
				// poner cursor por defecto
				cu = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
				v.setCursor(cu);
			}
		}
	}
	
	public void load(String file){
		
		String cad = null;
		Ventana v = Ventana.getInstance();
		ResourceBundle labels = Idioma.getInstance().getLabels();
		Fichero f = fact.generaFichero();
		Idioma id = Idioma.getInstance();
		
		// poner cursor de espera
		Cursor cu = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR);
		v.setCursor(cu);

		// guardar ventana y paneles del proyecto anterior
		v.getProyecto().salvarProy2();

		
		// salvar archivos
		int eS = v.getCreadorEditor().getEditorSeleccionado();
		int ed = v.getCreadorEditor().dameNumEditores();
		v.getCreadorEditor().setEditorSeleccionado(ed - 1);

		for (int z = ed - 1; z >= 0; z--) {
			v.getCreadorEditor().setEditorSeleccionado(z);
			if (v.getCreadorEditor().isRedButton() == true) {
				int opt = JOptionPane.showConfirmDialog(null, labels
						.getString("s643"), labels.getString("s953"),
						JOptionPane.YES_NO_OPTION);

				if (opt == JOptionPane.OK_OPTION) {
					v.getnuevoMenu().save_o_saveAS();
				}
			}
		}
		v.getCreadorEditor().setEditorSeleccionado(eS);

		// cerrar editores
		for (int i = 0; i < ed; i++) {
			v.getCreadorEditor().setEditorSeleccionado(0);
			v.getCreadorEditor().getPane().remove(0);
			v.getCreadorEditor().getPane().validate();
		}

		cad = f.cargar(file);

		almacenPropiedades.setPropiedad("DefaultAcidePrj", file);
		// mig
		v.getProyecto().setpathProy(file);
		v.getProyecto().borraFich();
		v.getProyecto().cargaProy(cad);

		// antiguo rundefaultconf
		if (!v.getProyecto().isExplorer())
			v.getnuevoMenu().getShowBrowserCBox().doClick();
		if (!v.getProyecto().isShell())
			v.getnuevoMenu().getShowShellWindowCBox().doClick();

		v.setSize(v.getProyecto().getWidthWindow(), v.getProyecto()
				.getHeightWindow());
		v.setLocation(v.getProyecto().getPosx(), v.getProyecto()
				.getPosy());

		// mig actualizar tamaño de los paneles
		v.getSplitPaneV().setDividerLocation(
				v.getProyecto().getWidth1());
		v.getSplitPaneH().setDividerLocation(
				v.getProyecto().getHeight1());

		v.validate();
		v.repaint();
		v.setVisible(true);

		// v.setTitle(labels.getString("s425") + " - "
		// + v.getProyecto().getnombreProy());

		// abrir archivos
		for (int j = 0; j < v.getProyecto().dameNumFich(); j++) {

			if (!v.getProyecto().getfich(j).isDirectory()) {

				FactoriaES fact = FactoriaES.getInstance();
				Fichero ff = fact.generaFichero();
				String text = ff.cargar(v.getProyecto().getfich(j)
						.getPath());
				String fich = null;
				String file2 = v.getProyecto().getfich(j).getPath();
				if (file2 != null) {
					int in = file2.lastIndexOf("/");
					in++;
					fich = file2.substring(in, file2.length());
					String ruta = file2.substring(0, in);
				}
				if (v.getProyecto().getfich(j).isOpened()) {
					// mig
					v.getnuevoMenu().habilitaMenuArchivo();
					v.getnuevoMenu().habilitaMenuEdicion();

					// se mira el tipo
					int t = 0;
					// status
					v.getStatusBar().setMessage(
							v.getProyecto().getfich(j).getPath());
					if (v.getProyecto().getfich(j).isSetFile()) {
						t = 2;
						// status
						v.getStatusBar().setMessage(
								v.getProyecto().getfich(j).getPath()
										+ " <COMPILABLE>");
					}
					if (v.getProyecto().getfich(j).isMainFile()) {
						t = 1;
						// status
						v.getStatusBar().setMessage(
								v.getProyecto().getfich(j).getPath()
										+ " <MAIN>");
					}

					v.getCreadorEditor().nuevaPestaña(fich, file2,
							text, true, t);
					v.validate();
					v.repaint();

					// UNDO REDO
					// v.getnuevoMenu().habilita_salvarFich();
					int numeditor = v.getCreadorEditor()
							.getEditorSeleccionado();
					DefaultStyledDocument doc = v.getCreadorEditor()
							.EditorSeleccionado().getDoc();

					doc
							.addUndoableEditListener(new UndoableEditListener() {
								public void undoableEditHappened(
										UndoableEditEvent evt) {
									Ventana v = Ventana.getInstance();
									UndoableEdit edit = evt.getEdit();
									if (!((edit instanceof DefaultDocumentEvent) && (((DefaultDocumentEvent) edit)
											.getType() == DefaultDocumentEvent.EventType.CHANGE))) {
										v.getnuevoMenu().getUndo()
												.addEdit(evt.getEdit());
									}
								}
							});
					// CURSOR EN LA PRIMERA POSICION DEL EDITOR DE TEXTO
					numeditor = v.getCreadorEditor()
							.getEditorSeleccionado();
					v.getCreadorEditor().dameEditorI(numeditor)
							.getEditor().setCaretPosition(0);
				}
			}
		}
		// Load Lexical
		if (!v.getProyecto().getLenguajeConfig().contains("\\"))
			almacenPropiedades.setPropiedad("pathLenguaje",
					".//configuration/Lexical/"
							+ v.getProyecto().getLenguajeConfig()
							+ ".xml");
		else
			almacenPropiedades.setPropiedad("pathLenguaje", v
					.getProyecto().getLenguajeConfig());

		Lenguaje leng = Lenguaje.getInstance();
		// leng.cargar(".//configuration/Lexical/"+v.getProyecto().getLenguajeConfig()+".xml");
		leng.cargar(v.getProyecto().getLenguajeConfig());

		int editor = Ventana.getInstance().getCreadorEditor()
				.dameNumEditores();
		for (int i = 0; i < editor; i++) {
			Ventana.getInstance().getCreadorEditor().dameEditorI(i).resetDoc();
		}
		
		// Load idioma
		String idioma = v.getProyecto().getLenguaje();
		if (idioma.equals("0")) {
			v.getnuevoMenu().getEspañol().doClick();
		} else {
			v.getnuevoMenu().getEnglish().doClick();
		}

		try {
			id.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (NumberFormatException e2) {
			e2.printStackTrace();
		} catch (Exception e2) {
			e2.printStackTrace();
		}
		labels = id.getLabels();
		v.getStatusBar().setMessageLexical(
				labels.getString("s449") + " " + leng.getNombre());
		
		// poner cursor por defecto
		cu = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
		v.setCursor(cu);
	}
}

// //mig///////////////////////////////////////////////////////////////////////////
/**
 * Clase Oyente del evento menu (Nuevo Archivo)
 * 
 * 
 */
class Menu_newProjectFile_Adaptor implements ActionListener {

	FactoriaES fact = FactoriaES.getInstance();
	Ventana v = Ventana.getInstance();
	Logger logger = Log.getLog();

	public void actionPerformed(ActionEvent e) {

		int eS = v.getCreadorEditor().getEditorSeleccionado();

		// CREAR
		v.getnuevoMenu().getNuevoFich().doClick();

		// SALVAR COMO
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Fichero f = fact.generaFichero();
		String archivo = " ";
		Ventana v = Ventana.getInstance();
		if (v.getCreadorEditor().dameNumEditores() == 0) {
			logger.info(labels.getString("s89"));

		} else {
			v.getnuevoMenu().setIsNPF(true);
			archivo = f.escribir();
			v.getnuevoMenu().setIsNPF(false);

			if (archivo.equals(" ")) {
				v.getCreadorEditor().setEditorSeleccionado(eS);
				logger.info(labels.getString("s92"));
			} else {
				boolean resultado = f.salvar(archivo, v.getCreadorEditor()
						.EditorSeleccionado().getTexto());

				// Muestra en el log
				if (resultado) {
					logger.info(labels.getString("s93") + archivo
							+ labels.getString("s94"));
					Ventana.getInstance().getCreadorEditor().greenButton();
					int in = archivo.lastIndexOf("\\");
					in++;
					String file = archivo.substring(in, archivo.length());
					v.getCreadorEditor().getPane().setTitleAt(
							v.getCreadorEditor().getPane().getSelectedIndex(),
							file);
					v.getCreadorEditor().EditorSeleccionado().setPath(archivo);
					v.getCreadorEditor().getPane().setToolTipText(archivo);
					// mig
					File fich = new File(v.getCreadorEditor()
							.EditorSeleccionado().getPath());
					Ventana.getInstance().getCreadorEditor()
							.EditorSeleccionado().setUltimoCambio(
									fich.lastModified());
					Ventana.getInstance().getCreadorEditor()
							.EditorSeleccionado().setUltimoTam(fich.length());

				} else {
					logger.info(labels.getString("s95") + archivo);
				}
			}
		}

		// AÑADIR
		// Fichero f = fact.generaFichero();
		try {

			String file = archivo;
			// file = f.leer();
			if (file != null && file != " ") {
				TreePath path = v.getNuevoExplorador().getArbol()
						.getSelectionPath();
				DefaultMutableTreeNode filePath;
				Fich fc;

				if (path != null) {// hay carpeta selecionada
					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					fc = (Fich) filePath.getUserObject();

					if (!fc.isDirectory()) {// hay archivo seleccionado
						filePath = v.getNuevoExplorador().getRaiz()
								.getNextNode();
						fc = (Fich) filePath.getUserObject();
					}

				} else {// no hay nada seleccionado
					filePath = v.getNuevoExplorador().getRaiz().getNextNode();
					fc = (Fich) filePath.getUserObject();
				}

				// if (fc.isDirectory()){
				int in = file.lastIndexOf("\\");
				String fich = "";
				String ruta;
				if (in != -1) {
					in++;
					fich = file.substring(in, file.length());
					ruta = file.substring(0, in);
				} else {
					in = file.lastIndexOf("/");
					fich = file.substring(in, file.length());
					ruta = file.substring(0, in);
				}
				Fich fic = new Fich();
				fic.setPath(file);
				fic.setName(fich);
				fic.setPadre(fc.getName());
				v.getProyecto().setfich(fic);
				v.getProyecto().setNumFich(
						Integer.toString(v.getProyecto().dameNumFich()));
				// mig
				v.getProyecto().getfich(v.getProyecto().dameNumFich() - 1)
						.setOpened(true);
				// System.out.println(fich);
				// System.out.println(ruta);
				DefaultMutableTreeNode d = new DefaultMutableTreeNode(fic);
				d.setAllowsChildren(false);
				filePath.add(d);
				v.validate();
				v.repaint();
				v.getNuevoExplorador().getTreeModel().reload();
				v.getNuevoExplorador().expandTree();
				v.getNuevoExplorador().setEnabledRemoveFile();
				v.getNuevoExplorador().setEnabledDeleteFile();
				// mig
				v.getProyecto().setModified(true);
				// status
				v.getStatusBar().setMessage(fic.getPath());
				// }

			}
		} catch (Exception ex) {
			ex.printStackTrace();
		}

	}

}

// ////////////////////////////////////////////////////////////////////////////////
// mig
/**
 * Clase oyente (Añadir ficheros al proyecto)
 * 
 */
class Menu_anadirFich_Adaptor implements ActionListener {
	FactoriaES fact = FactoriaES.getInstance();

	Ventana v = Ventana.getInstance();

	ExtensionesValidas d = ExtensionesValidas.getInstance();

	public void actionPerformed(ActionEvent arg0) {
		Fichero f = fact.generaFichero();
		try {

			String file = "";
			file = f.leer();

			if (file != null) {
				TreePath path = v.getNuevoExplorador().getArbol()
						.getSelectionPath();
				DefaultMutableTreeNode filePath;
				Fich fc;
				// mig
				if (path != null) {// hay carpeta seleccionada
					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					fc = (Fich) filePath.getUserObject();

					if (!fc.isDirectory()) {// hay archivo seleccionado
						filePath = v.getNuevoExplorador().getRaiz()
								.getNextNode();
						fc = (Fich) filePath.getUserObject();
					}

				} else {// no hay nada seleccionado
					filePath = v.getNuevoExplorador().getRaiz().getNextNode();
					fc = (Fich) filePath.getUserObject();
				}

				// if (fc.isDirectory()){

				int in = file.lastIndexOf("\\");
				String fich = "";
				String ruta;
				if (in != -1) {
					in++;
					fich = file.substring(in, file.length());
					ruta = file.substring(0, in);
				} else {
					in = file.lastIndexOf("/");
					fich = file.substring(in, file.length());
					ruta = file.substring(0, in);
				}

				Fich fic = new Fich();
				fic.setPath(file);
				fic.setName(fich);
				fic.setPadre(fc.getName());

				boolean a = false;
				for (int i = 0; i < v.getProyecto().dameNumFich(); i++) {
					if (v.getProyecto().getfich(i).getPath().equals(
							fic.getPath())) {
						a = true;
					}
				}

				if (!a) {// si no esta ya añadido

					// fic.setPadre(v.getProyecto().getnombreProy());
					v.getProyecto().setfich(fic);
					v.getProyecto().setNumFich(
							Integer.toString(v.getProyecto().dameNumFich()));
					// mig
					v.getProyecto().getfich(v.getProyecto().dameNumFich() - 1)
							.setOpened(true);
					System.out.println(fich);
					System.out.println(ruta);
					DefaultMutableTreeNode d = new DefaultMutableTreeNode(fic);
					d.setAllowsChildren(false);
					filePath.add(d);
					v.validate();
					v.repaint();
					v.getNuevoExplorador().getTreeModel().reload();
					v.getNuevoExplorador().expandTree();
					v.getNuevoExplorador().setEnabledRemoveFile();
					v.getNuevoExplorador().setEnabledDeleteFile();
					// mig
					v.getProyecto().setModified(true);
				}

				// ABRIR ARCHIVO EN EL EDITOR
				boolean b = false;
				for (int i = 0; i < v.getCreadorEditor().dameNumEditores(); i++) {
					if (v.getCreadorEditor().dameEditorI(i).getPath().equals(
							fic.getPath())) {
						b = true;
					}
				}

				// if (!fic.isDirectory()) {
				if (!b) {// si no esta ya abierto

					Fichero fd = new Fichero();
					CreadorEditor ce = v.getCreadorEditor();

					// se mira el tipo
					int t = 0;
					// status
					v.getStatusBar().setMessage(fic.getPath());

					ce.nuevaPestaña(fic.getPath(), fic.getPath(), fd.cargar(fic
							.getPath()), true, t);
					// UNDO REDO
					// v.getnuevoMenu().habilita_salvarFich();
					v.getnuevoMenu().habilitaMenuArchivo();
					v.getnuevoMenu().habilitaMenuEdicion();
					int numeditor = v.getCreadorEditor()
							.getEditorSeleccionado();
					Document doc = v.getCreadorEditor().dameEditorI(numeditor)
							.getEditor().getDocument();
					doc.addUndoableEditListener(new UndoableEditListener() {
						public void undoableEditHappened(UndoableEditEvent evt) {
							Ventana v = Ventana.getInstance();
							UndoableEdit edit = evt.getEdit();
							if (edit instanceof DefaultDocumentEvent
									&& ((DefaultDocumentEvent) edit).getType() == DefaultDocumentEvent.EventType.CHANGE) {
								return;
							} else {
								v.getnuevoMenu().getUndo().addEdit(
										evt.getEdit());
							}
						}
					});
					// CURSOR EN LA PRIMERA POSICION DEL EDITOR DE TEXTO
					numeditor = v.getCreadorEditor().getEditorSeleccionado();
					v.getCreadorEditor().dameEditorI(numeditor).getEditor()
							.setCaretPosition(0);

				}

				// }

			}
		} catch (Exception ex) {
			ex.printStackTrace();
		}

	}

}

// ////mig
class Menu_anadirFich2_Adaptor implements ActionListener {
	FactoriaES fact = FactoriaES.getInstance();

	Ventana v = Ventana.getInstance();

	ExtensionesValidas d = ExtensionesValidas.getInstance();

	public void actionPerformed(ActionEvent arg0) {
		Fichero f = fact.generaFichero();
		try {

			String file = "";
			file = v.getCreadorEditor().dameEditorI(
					v.getCreadorEditor().getEditorSeleccionado()).getPath();
			System.out.println("NO.  "
					+ v.getCreadorEditor().getEditorSeleccionado());
			if (file != null) {
				TreePath path = v.getNuevoExplorador().getArbol()
						.getSelectionPath();
				DefaultMutableTreeNode filePath;
				Fich fc;
				// mig
				if (path != null) {// hay carpeta seleccionada
					filePath = (DefaultMutableTreeNode) path
							.getLastPathComponent();
					fc = (Fich) filePath.getUserObject();

					if (!fc.isDirectory()) {// hay archivo seleccionado
						filePath = v.getNuevoExplorador().getRaiz()
								.getNextNode();
						fc = (Fich) filePath.getUserObject();
					}

				} else {// no hay nada seleccionado
					filePath = v.getNuevoExplorador().getRaiz().getNextNode();
					fc = (Fich) filePath.getUserObject();
				}

				// if (fc.isDirectory()){
				int in = file.lastIndexOf("\\");
				String fich = "";
				String ruta;
				if (in != -1) {
					in++;
					fich = file.substring(in, file.length());
					ruta = file.substring(0, in);
				} else {
					in = file.lastIndexOf("/");
					fich = file.substring(in, file.length());
					ruta = file.substring(0, in);
				}
				Fich fic = new Fich();
				fic.setPath(file);
				fic.setName(fich);
				// mig
				// fic.setPadre(fc.getName());

				boolean a = false;
				for (int i = 0; i < v.getProyecto().dameNumFich(); i++) {
					if (v.getProyecto().getfich(i).getPath().equals(
							fic.getPath())) {
						a = true;
					}
				}

				if (!a) {// si no esta ya añadido

					fic.setPadre(v.getProyecto().getnombreProy());
					v.getProyecto().setfich(fic);
					v.getProyecto().setNumFich(
							Integer.toString(v.getProyecto().dameNumFich()));
					// mig
					v.getProyecto().getfich(v.getProyecto().dameNumFich() - 1)
							.setOpened(true);
					System.out.println(fich);
					System.out.println(ruta);
					DefaultMutableTreeNode d = new DefaultMutableTreeNode(fic);
					d.setAllowsChildren(false);
					filePath.add(d);
					v.validate();
					v.repaint();
					v.getNuevoExplorador().getTreeModel().reload();
					v.getNuevoExplorador().expandTree();
					v.getNuevoExplorador().setEnabledRemoveFile();
					v.getNuevoExplorador().setEnabledDeleteFile();
					// mig
					v.getProyecto().setModified(true);
				}
			}

		} catch (Exception ex) {
			ex.printStackTrace();
		}

	}

}

/**
 * Clase Oyente del evento menu (configurarConsola)
 * 
 * 
 */
class Menu_configurarConsola_Adaptor implements ActionListener {
	FactoriaGUI fact = FactoriaGUI.getInstance();

	public void actionPerformed(ActionEvent e) {

		fact.generaConfigurarConsolaGUI();

	}

}

/**
 * Clase Oyente del evento menu (comandoExterno)
 * 
 * 
 */
class Menu_comandoExterno_Adaptor implements ActionListener {
	FactoriaGUI fact = FactoriaGUI.getInstance();

	public void actionPerformed(ActionEvent e) {

		fact.generaComandoExternoGUI();

	}

}

class Menu_edicionIconos_Adaptor implements ActionListener {
	FactoriaGUI fact = FactoriaGUI.getInstance();

	public void actionPerformed(ActionEvent e) {
		Ventana v = Ventana.getInstance();
		v.setEnabled(false);
		fact.generaEdicionIconosGUI();
	}
}

class Menu_selectAll_Adaptor implements ActionListener {
	public void actionPerformed(ActionEvent e) {
		Ventana v = Ventana.getInstance();
		v.getCreadorEditor().dameEditorI(
				v.getCreadorEditor().getEditorSeleccionado()).getEditor()
				.setCaretPosition(0);
		int length = v.getCreadorEditor().dameEditorI(
				v.getCreadorEditor().getEditorSeleccionado()).getEditor()
				.getText().length();
		v.getCreadorEditor().dameEditorI(
				v.getCreadorEditor().getEditorSeleccionado()).getEditor()
				.setSelectionEnd(length);
	}
}

class Menu_About_Adaptor implements ActionListener {

	public void actionPerformed(ActionEvent arg0) {
		// TODO Auto-generated method stub
		// FactoriaGUI f=FactoriaGUI.getInstance();
		About a = new About();
		// f.buildAbout();
	}
}

class Menu_Print_Adapter implements ActionListener {

	public void actionPerformed(ActionEvent arg0) {

		PrintGUI p = PrintGUI.getInstance();
		p.getFrame().setVisible(true);
	}
}

class Menu_Execution_Adapter implements ActionListener {
	FactoriaGUI factGUI = FactoriaGUI.getInstance();

	public void actionPerformed(ActionEvent arg0) {

		factGUI.buildExecutionGUI();

	}
}

class Menu_Compiler_Adapter implements ActionListener {

	public void actionPerformed(ActionEvent arg0) {
		Ventana v = Ventana.getInstance();
		Runtime obj = Runtime.getRuntime();
		v.closeDefaultPrj();
		try {
			if (v.getProyecto().isCheckCompiler() == true) {
				String filetoCompiler = "";
				for (int i = 0; i < v.getProyecto().dameNumFich(); i++) {
					if (v.getProyecto().getfich(i).isSetFile())
						filetoCompiler = filetoCompiler + "\""
								+ v.getProyecto().getfich(i).getPath() + "\""
								+ v.getProyecto().getSeparatorFile();
				}
				if (filetoCompiler.length() > 0) {
					filetoCompiler = filetoCompiler.substring(0, filetoCompiler
							.length() - 1);
					System.out.println(v.getProyecto().getpathEjecu() + " "
							+ v.getProyecto().getarg() + " " + filetoCompiler);
					if (v.getProyecto().getpathEjecu() != null)
						obj.exec(v.getProyecto().getpathEjecu() + " "
								+ v.getProyecto().getarg() + " "
								+ filetoCompiler);
				}
			} else {
				String extension = v.getProyecto().getExtensionFile();
				for (int i = 0; i < v.getProyecto().dameNumFich(); i++) {
					Fich file = v.getProyecto().getfich(i);
					if (file.isDirectory() == false) {
						String name = file.getPath();
						String ext = name.substring(name.lastIndexOf(".") + 1,
								name.length());
						if (ext.equals(extension)) {
							if (v.getProyecto().getpathEjecu() != null) {
								obj.exec(v.getProyecto().getpathEjecu() + " "
										+ v.getProyecto().getarg() + " \""
										+ name + "\"");

							}
						}

					}

				}
			}
		} catch (IOException e1) {
			JOptionPane.showMessageDialog(null, e1.getMessage());
		}

	}
}

class Menu_GoTo_Adapter implements ActionListener {

	public void actionPerformed(ActionEvent arg0) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Object[] options = { labels.getString("s445"), labels.getString("s446") };

		String n = (String) JOptionPane.showInputDialog(null, labels
				.getString("s448"), labels.getString("s447"),
				JOptionPane.YES_NO_CANCEL_OPTION, null, null, null);

		if ((n != null)) {
			try {
				int line = Integer.valueOf(n);
				Ventana v = Ventana.getInstance();
				int e = v.getCreadorEditor().getEditorSeleccionado();
				if (e >= 0)
					v.getCreadorEditor().dameEditorI(e).gotoLine(
							Integer.parseInt(n));
			} catch (Exception e) {
				// Not a number
			}
		}
	}
}

class Menu_RemoveFile_Adapter implements ActionListener {

	public void actionPerformed(ActionEvent arg0) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Ventana v = Ventana.getInstance();
		int opcion = JOptionPane.showConfirmDialog(null, labels
				.getString("s623"));
		if (opcion == JOptionPane.OK_OPTION) {
			// logger.info(labels.getString("s77"));
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			TreePath currentSelection = v.getNuevoExplorador().getArbol()
					.getSelectionPath();
			if (currentSelection != null) {
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
						.getLastPathComponent());
				// System.out.println("PP "+currentNode);
				Fich p = (Fich) currentNode.getUserObject();
				if (!p.isDirectory()) {
					MutableTreeNode parent = (MutableTreeNode) (currentNode
							.getParent());
					if (parent != null) {
						v.getNuevoExplorador().getTreeModel()
								.removeNodeFromParent(currentNode);
						toolkit.beep();
						int cont = -1;
						for (int j = 0; j < v.getProyecto().dameNumFich(); j++) {
							if (v.getProyecto().getfich(j).getPath().equals(
									p.getPath())) {
								System.out.println(v.getProyecto().getfich(j)
										.getPath());
								cont = j;
							}
						}
						Fich f = v.getProyecto().getfich(cont);
						String fileRemove = f.getPath();
						v.getProyecto().removeFich(cont);
						v.getProyecto()
								.setNumFich(
										Integer.toString(v.getProyecto()
												.dameNumFich()));
						v.getStatusBar().setMessage("");
						int editor = -1;
						for (int z = 0; z < v.getCreadorEditor()
								.dameNumEditores(); z++) {
							if (v.getCreadorEditor().dameEditorI(z).getPath()
									.equals(p.getPath()))
								editor = z;
						}
						if (editor != -1) {
							// pregunta si salvar
							if (v.getCreadorEditor().isRedButton(editor)) {
								int opt = JOptionPane.showConfirmDialog(null,
										labels.getString("s643"), labels
												.getString("s953"),
										JOptionPane.YES_NO_OPTION);

								if (opt == JOptionPane.OK_OPTION) {
									// //////////////////////////////

									FactoriaES fact = FactoriaES.getInstance();
									Fichero f2 = fact.generaFichero();
									// String archivo = " ";

									boolean resultado = f2.salvar(v
											.getCreadorEditor().dameEditorI(
													editor).getPath(), v
											.getCreadorEditor().dameEditorI(
													editor).getTexto());
									// Muestra en el log
									if (resultado) {
										// logger.info(labels.getString("s93") +
										// archivo+ labels.getString("s94"));
										Ventana.getInstance()
												.getCreadorEditor()
												.greenButton(editor);
									} else {
										// logger.info(labels.getString("s95") +
										// archivo);
									}

									/*
									 * //mig for(int i2=0; i2<v.getProyecto().getFichSize();
									 * i2++){
									 * if(v.getProyecto().getfich(i2).getPath().equals(v.getCreadorEditor().dameEditorI(editor).getPath())){
									 * v.getProyecto().getfich(i2).setOpened(false); } }
									 * String prj=null; try {
									 * prj=almacenPropiedades.getPropiedad("DefaultAcidePrj"); }
									 * catch (Exception e1) { // TODO
									 * Auto-generated catch block
									 * e1.printStackTrace(); } if
									 * (!(prj.equals(".//configuration/Default.acidePrj") &&
									 * v.getProyecto().getnombreProy().equals(""))){
									 * v.getProyecto().setModified(true); }
									 */
								}
								// //////////////////////////////77
								// v.getnuevoMenu().getSave().setEnabled(true);
								// v.getnuevoMenu().getSave().doClick();

							}
						}
						// cerrar
						v.getCreadorEditor().getPane().remove(editor);

						if (v.getCreadorEditor().getPane().getTabCount() == 0) {
							v.getnuevoMenu().deshabilitaMenuArchivo();
							v.getnuevoMenu().deshabilitaMenuEdicion();
						}

					}
					// mig
					v.getProyecto().setModified(true);

					return;

				}
				toolkit.beep();
			}

		} else if ((opcion == JOptionPane.NO_OPTION)
				|| (opcion == JOptionPane.CANCEL_OPTION)) {
			// Muestra resultado operacion en el log
			// logger.info(labels.getString("s78"));

		}
		if (v.getProyecto().dameNumFich() > 0) {
			v.getNuevoExplorador().setEnabledRemoveFile();
			v.getNuevoExplorador().setEnabledDeleteFile();
		} else {
			v.getNuevoExplorador().getRemoveFile().setEnabled(false);
			v.getNuevoExplorador().getDeleteFile().setEnabled(false);
		}

	}
}

// ///////////////////////mig

class Menu_DeleteFile_Adapter implements ActionListener {

	public void actionPerformed(ActionEvent arg0) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Ventana v = Ventana.getInstance();

		int opcion = JOptionPane.showConfirmDialog(null, labels
				.getString("s951"));
		if (opcion == JOptionPane.OK_OPTION) {
			// logger.info(labels.getString("s77"));
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			TreePath currentSelection = v.getNuevoExplorador().getArbol()
					.getSelectionPath();
			if (currentSelection != null) {
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
						.getLastPathComponent());
				Fich p = (Fich) currentNode.getUserObject();
				if (!p.isDirectory()) {
					MutableTreeNode parent = (MutableTreeNode) (currentNode
							.getParent());
					if (parent != null) {
						v.getNuevoExplorador().getTreeModel()
								.removeNodeFromParent(currentNode);
						toolkit.beep();
						int cont = -1;
						for (int j = 0; j < v.getProyecto().dameNumFich(); j++) {
							if (v.getProyecto().getfich(j).getPath().equals(
									p.getPath())) {
								System.out.println(v.getProyecto().getfich(j)
										.getPath());
								cont = j;
							}
						}
						Fich f = v.getProyecto().getfich(cont);
						String fileRemove = f.getPath();
						v.getProyecto().removeFich(cont);
						v.getProyecto()
								.setNumFich(
										Integer.toString(v.getProyecto()
												.dameNumFich()));

						System.out.println(fileRemove);
						File fi = new File(fileRemove);
						fi.delete();

						v.getStatusBar().setMessage("");
						// mig
						v.getProyecto().setModified(true);

						return;

					}
					toolkit.beep();
				}

			} else if ((opcion == JOptionPane.NO_OPTION)
					|| (opcion == JOptionPane.CANCEL_OPTION)) {
				// Muestra resultado operacion en el log
				// logger.info(labels.getString("s78"));

			}
			if (v.getProyecto().dameNumFich() > 0) {
				v.getNuevoExplorador().setEnabledRemoveFile();
				v.getNuevoExplorador().setEnabledDeleteFile();
			} else {
				v.getNuevoExplorador().getRemoveFile().setEnabled(false);
				v.getNuevoExplorador().getDeleteFile().setEnabled(false);
			}

		}
	}
}

// /mig
class Menu_RemoveFile2_Adapter implements ActionListener {

	public void actionPerformed(ActionEvent arg0) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Ventana v = Ventana.getInstance();
		int opcion = JOptionPane.showConfirmDialog(null, labels
				.getString("s623"));
		if (opcion == JOptionPane.OK_OPTION) {
			// logger.info(labels.getString("s77"));
			Toolkit toolkit = Toolkit.getDefaultToolkit();

			// TreePath currentSelection =
			// v.getNuevoExplorador().getArbol().getSelectionPath();
			Fich f = new Fich();
			int y = -1;
			int editor = v.getCreadorEditor().getEditorSeleccionado();
			for (int j = 0; j < v.getProyecto().dameNumFich(); j++) {

				if (v.getProyecto().getfich(j).getPath().equals(
						v.getCreadorEditor().EditorSeleccionado().getPath())) {
					f = v.getProyecto().getfich(j);
					for (int m = 0; m < v.getProyecto().dameNumFich() + 1; m++) {
						if (v.getNuevoExplorador().getArbol().getPathForRow(m)
								.getLastPathComponent().toString().equals(
										f.getLastPathComponent())) {

							y = m;
						}
					}
				}
			}

			TreePath currentSelection = v.getNuevoExplorador().getArbol()
					.getPathForRow(y);

			if (currentSelection != null) {
				DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
						.getLastPathComponent());
				Fich p = (Fich) currentNode.getUserObject();
				if (!p.isDirectory()) {
					MutableTreeNode parent = (MutableTreeNode) (currentNode
							.getParent());
					if (parent != null) {
						v.getNuevoExplorador().getTreeModel()
								.removeNodeFromParent(currentNode);
						toolkit.beep();
						int cont = -1;
						for (int j = 0; j < v.getProyecto().dameNumFich(); j++) {
							if (v.getProyecto().getfich(j).getPath().equals(
									p.getPath())) {
								System.out.println(v.getProyecto().getfich(j)
										.getPath());
								cont = j;
							}
						}
						// Fich f=v.getProyecto().getfich(cont);
						// String fileRemove = f.getPath();
						v.getProyecto().removeFich(cont);
						v.getProyecto()
								.setNumFich(
										Integer.toString(v.getProyecto()
												.dameNumFich()));
						v.getStatusBar().setMessage("");
						// mig
						v.getProyecto().setModified(true);

						// pregunta si salvar
						if (v.getCreadorEditor().isRedButton(editor)) {
							int opt = JOptionPane.showConfirmDialog(null,
									labels.getString("s643"), labels
											.getString("s953"),
									JOptionPane.YES_NO_OPTION);

							if (opt == JOptionPane.OK_OPTION) {
								v.getnuevoMenu().getSave().setEnabled(true);
								v.getnuevoMenu().getSave().doClick();
							}
						}
						// cerrar
						v.getCreadorEditor().getPane().remove(editor);
						if (v.getCreadorEditor().getPane().getTabCount() == 0) {
							v.getnuevoMenu().deshabilitaMenuArchivo();
							v.getnuevoMenu().deshabilitaMenuEdicion();
						}

						return;

					}
					toolkit.beep();
				}

			}
			if (v.getProyecto().dameNumFich() > 0) {
				v.getNuevoExplorador().setEnabledRemoveFile();
				v.getNuevoExplorador().setEnabledDeleteFile();
			} else {
				v.getNuevoExplorador().getRemoveFile().setEnabled(false);
				v.getNuevoExplorador().getDeleteFile().setEnabled(false);
			}

		} else if ((opcion == JOptionPane.NO_OPTION)
				|| (opcion == JOptionPane.CANCEL_OPTION)) {
			// Muestra resultado operacion en el log
			// logger.info(labels.getString("s78"));

		}
	}
}

// /mig
class Menu_DeleteFile2_Adapter implements ActionListener {

	public void actionPerformed(ActionEvent arg0) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Ventana v = Ventana.getInstance();

		int opcion = JOptionPane.showConfirmDialog(null, labels
				.getString("s951"));
		if (opcion == JOptionPane.OK_OPTION) {

			// logger.info(labels.getString("s77"));
			Toolkit toolkit = Toolkit.getDefaultToolkit();
			String prj = null;
			try {
				prj = almacenPropiedades.getPropiedad("DefaultAcidePrj");
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			if ((prj.equals(".//configuration/Default.acidePrj") && v
					.getProyecto().getnombreProy().equals(""))) {
				// no hay proyecto

				String fileRemove = v.getCreadorEditor().EditorSeleccionado()
						.getPath();
				File fi = new File(fileRemove);
				fi.delete();
				v.getCreadorEditor().getPane().remove(
						v.getCreadorEditor().getEditorSeleccionado());
				v.getStatusBar().setMessage("");
				toolkit.beep();
			} else {// hay proyecto

				// TreePath currentSelection =
				// v.getNuevoExplorador().getArbol().getSelectionPath();

				Fich ff = new Fich();
				int y = -1;
				int editor = v.getCreadorEditor().getEditorSeleccionado();
				for (int j = 0; j < v.getProyecto().dameNumFich(); j++) {

					if (v.getProyecto().getfich(j).getPath()
							.equals(
									v.getCreadorEditor().EditorSeleccionado()
											.getPath())) {

						ff = v.getProyecto().getfich(j);
						for (int m = 0; m < v.getProyecto().dameNumFich() + 1; m++) {

							if (v.getNuevoExplorador().getArbol()
									.getPathForRow(m).getLastPathComponent()
									.toString().equals(
											ff.getLastPathComponent())) {

								y = m;
							}
						}
					}
				}

				TreePath currentSelection = v.getNuevoExplorador().getArbol().getPathForRow(y);

				if (currentSelection != null) {
					DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) (currentSelection
							.getLastPathComponent());
					Fich p = (Fich) currentNode.getUserObject();
					if (!p.isDirectory()) {
						MutableTreeNode parent = (MutableTreeNode) (currentNode
								.getParent());
						if (parent != null) {
							v.getNuevoExplorador().getTreeModel()
									.removeNodeFromParent(currentNode);
							toolkit.beep();
							int cont = -1;
							for (int j = 0; j < v.getProyecto().dameNumFich(); j++) {
								if (v.getProyecto().getfich(j).getPath()
										.equals(p.getPath())) {
									System.out.println(v.getProyecto().getfich(
											j).getPath());
									cont = j;
								}
							}
							Fich f = v.getProyecto().getfich(cont);
							String fileRemove = f.getPath();
							v.getProyecto().removeFich(cont);
							v.getProyecto().setNumFich(
									Integer.toString(v.getProyecto()
											.dameNumFich()));

							System.out.println(fileRemove);
							File fi = new File(fileRemove);
							fi.delete();

							v.getStatusBar().setMessage("");
							// mig
							v.getProyecto().setModified(true);
							v.getCreadorEditor().getPane().remove(editor);

							return;

						}
						toolkit.beep();
					}

				} else {// el archivo no pertenece al proyecto
					String fileRemove = v.getCreadorEditor()
							.EditorSeleccionado().getPath();
					File fi = new File(fileRemove);
					fi.delete();
					v.getCreadorEditor().getPane().remove(
							v.getCreadorEditor().getEditorSeleccionado());
					v.getStatusBar().setMessage("");
					toolkit.beep();
				}
				if (v.getProyecto().dameNumFich() > 0) {
					v.getNuevoExplorador().setEnabledRemoveFile();
					v.getNuevoExplorador().setEnabledDeleteFile();
				} else {
					v.getNuevoExplorador().getRemoveFile().setEnabled(false);
					v.getNuevoExplorador().getDeleteFile().setEnabled(false);
				}

			}

		} else if ((opcion == JOptionPane.NO_OPTION)
				|| (opcion == JOptionPane.CANCEL_OPTION)) {
			// Muestra resultado operacion en el log
			// logger.info(labels.getString("s78"));

		}
	}
}

class Menu_CloseProj_Adapter implements ActionListener {

	public void actionPerformed(ActionEvent arg0) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}

		ResourceBundle labels = i.getLabels();
		Ventana v = Ventana.getInstance();
		boolean c = false;

		if (v.getProyecto().isModified()) {
			// if (v.getProyecto().isFirstSave()==false){
			int res = JOptionPane.showConfirmDialog(null, labels
					.getString("s657"), labels.getString("s953"),
					JOptionPane.YES_NO_CANCEL_OPTION);
			if (res == JOptionPane.CANCEL_OPTION) {
				c = true;
			}
			if (res == JOptionPane.OK_OPTION) {
				v.getnuevoMenu().getGuardarProyecto().setEnabled(true);
				v.getnuevoMenu().getGuardarProyecto().doClick();
			}
			// }
		}
		if (!c) {
			v.getNuevoExplorador().getRaiz().removeAllChildren();
			v.getNuevoExplorador().getTreeModel().reload();
			v.getNuevoExplorador().getAddFile().setEnabled(false);
			v.getNuevoExplorador().getSaveProj().setEnabled(false);
			v.getNuevoExplorador().getRemoveFile().setEnabled(false);
			v.getNuevoExplorador().getDeleteFile().setEnabled(false);
			//if (v.getProyecto().isExplorer()) v.getnuevoMenu().getShowBrowserCBox().doClick();
			v.getProyecto().salvarProy2();
			v.setTitle(labels.getString("s425") + " - <empty>");
			v.getProyecto().borraFich();
			Fichero f = new Fichero();
			v.validate();
			v.repaint();
			almacenPropiedades.setPropiedad("DefaultAcidePrj",
					".//configuration/Default.acidePrj");
			// mig
			v.getProyecto().setnombreProy("");
			v.getProyecto().setModified(false);
			v.getnuevoMenu().getCloseAll().setEnabled(true);
			v.getnuevoMenu().getCloseAll().doClick();
			v.getnuevoMenu().deshabilitaMenuProyecto();
			v.getStatusBar().setMessage("");
		}
	}
}

class Menu_SaveAsLexical_Adapter implements ActionListener {
	Logger logger = Log.getLog();

	FactoriaES fact = FactoriaES.getInstance();

	Ventana v = Ventana.getInstance();

	public void actionPerformed(ActionEvent e) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		JFileChooser chooser = new JFileChooser(labels.getString("s126"));
		FiltroFicheros filtro = new FiltroFicheros(labels.getString("s287"));
		filtro.addExtension("xml");
		chooser.setFileFilter(filtro);
		chooser.setCurrentDirectory(new File(".//configuration/Lexical"));
		int option = chooser.showSaveDialog(null);
		String archivo = " ";
		if (option == JFileChooser.APPROVE_OPTION) {
			archivo = chooser.getSelectedFile().getAbsolutePath();
		}
		Ventana v = Ventana.getInstance();
		if (archivo.equals(" ")) {
			logger.info(labels.getString("s92"));
		} else {
			int index = archivo.lastIndexOf("\\");
			String nombre = archivo.substring(index + 1, archivo.length());
			if (nombre.contains(".")) {
				index = nombre.lastIndexOf(".");
				nombre = nombre.substring(0, index);
			}
			Lenguaje l = Lenguaje.getInstance();
			boolean resultado = l.guardarComo(nombre, false, archivo);
			if (resultado) {
				Object[] options = { labels.getString("s445"),
						labels.getString("s446") };
				JOptionPane.showMessageDialog(null, labels.getString("s451"),
						labels.getString("s450"), 1);

			} else {
				Object[] options = { labels.getString("s445"),
						labels.getString("s446") };
				JOptionPane.showMessageDialog(null, labels.getString("s452"),
						labels.getString("s450"), 0);
			}
		}
	}
}

class Menu_SaveLexical_Adapter implements ActionListener {
	Logger logger = Log.getLog();

	FactoriaES fact = FactoriaES.getInstance();

	Ventana v = Ventana.getInstance();

	public void actionPerformed(ActionEvent e) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		Lenguaje l = Lenguaje.getInstance();
		String archivo = l.getNombre();
		System.out.println(archivo);
		Ventana v = Ventana.getInstance();
		if (archivo.equals(" ")) {
			logger.info(labels.getString("s92"));
		} else {
			int index = archivo.lastIndexOf("\\");
			String nombre = archivo.substring(index + 1, archivo.length());
			if (nombre.contains(".")) {
				index = nombre.lastIndexOf(".");
				nombre = nombre.substring(0, index);
			}
			boolean resultado = l.guardar(nombre, false);
			if (resultado) {
				Object[] options = { labels.getString("s445"),
						labels.getString("s446") };
				JOptionPane.showMessageDialog(null, labels.getString("s451"),
						labels.getString("s450"), 1);

			} else {
				Object[] options = { labels.getString("s445"),
						labels.getString("s446") };
				JOptionPane.showMessageDialog(null, labels.getString("s452"),
						labels.getString("s450"), 0);
			}
		}
	}
}

class Menu_NewLexical_Adapter implements ActionListener {
	Logger logger = Log.getLog();

	FactoriaES fact = FactoriaES.getInstance();

	Ventana v = Ventana.getInstance();

	public void actionPerformed(ActionEvent e) {
		Idioma i = Idioma.getInstance();
		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		ResourceBundle labels = i.getLabels();
		String s = "";
		String ss = "";
		ss = JOptionPane.showInputDialog(null, labels.getString("s453"), labels
				.getString("s454"), 2);
		s = ".\\configuration\\Lexical\\" + ss + ".xml";
		if (!ss.trim().equals("")) {
			Lenguaje.getInstance().newLanguaje(s);
			int editor = Ventana.getInstance().getCreadorEditor()
					.dameNumEditores();
			for (int j = 0; j < editor; j++) {
				Ventana.getInstance().getCreadorEditor().dameEditorI(j)
						.resetDoc();
			}
			Ventana.getInstance().getStatusBar().getMessageLexical().setText(
					labels.getString("s449") + " "
							+ Lenguaje.getInstance().getNombre());
			Ventana.getInstance().getProyecto().setLenguajeConfig(
					Lenguaje.getInstance().getNombre());
			// mig
			String prj = null;
			try {
				prj = almacenPropiedades.getPropiedad("DefaultAcidePrj");
			} catch (Exception e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			if (!(prj.equals(".//configuration/Default.acidePrj") && v
					.getProyecto().getnombreProy().equals(""))) {
				Ventana.getInstance().getProyecto().setModified(true);
			}

		}
	}
}

/**
 * Clase oyente de la opcion de menu que carga una configuración lexmodify
 */
class Menu_cargarLexParam_Adaptor implements ActionListener {

	public void actionPerformed(ActionEvent e) {
		ResourceBundle labels = Idioma.getInstance().getLabels();
		FiltroFicheros filtro = new FiltroFicheros(labels.getString("s327"));
		filtro.addExtension(".xml");
		JFileChooser selector = new JFileChooser();
		selector.setFileSelectionMode(JFileChooser.FILES_ONLY);
		selector.addChoosableFileFilter(filtro);
		selector.setCurrentDirectory(new File(".//configuration/Lexical"));
		int valor = selector.showOpenDialog(selector);
		if (valor == JFileChooser.APPROVE_OPTION) {
			Lenguaje l = Lenguaje.getInstance();
			l.cargar(selector.getSelectedFile().getAbsolutePath());
			int editor = Ventana.getInstance().getCreadorEditor()
					.dameNumEditores();
			for (int i = 0; i < editor; i++) {
				Ventana.getInstance().getCreadorEditor().dameEditorI(i)
						.resetDoc();
			}

			Ventana.getInstance().getStatusBar().getMessageLexical().setText(
					labels.getString("s449") + " "
							+ Lenguaje.getInstance().getNombre());
			Ventana.getInstance().getProyecto().setLenguajeConfig(
					Lenguaje.getInstance().getPathLenguaje());
			// mig
			String prj = null;
			try {
				prj = almacenPropiedades.getPropiedad("DefaultAcidePrj");
			} catch (Exception e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			if (!(prj.equals(".//configuration/Default.acidePrj") && Ventana
					.getInstance().getProyecto().getnombreProy().equals(""))) {
				Ventana.getInstance().getProyecto().setModified(true);
			}

		}
	}
}

class Click implements MouseListener {

	public void mouseClicked(MouseEvent arg0) {
	}

	public void mousePressed(MouseEvent arg0) {
		Ventana v = Ventana.getInstance();

		// MENU PROYECTO
		if (v.getnuevoMenu().getProyecto().isSelected()) {
			v.getnuevoMenu().getGuardarProyecto().setEnabled(false);
			// v.getnuevoMenu().getSaveAsProject().setEnabled(false);
			v.getnuevoMenu().getRemoveFile().setEnabled(false);
			v.getnuevoMenu().getDeleteFile().setEnabled(false);
			v.getnuevoMenu().getSetMain().setEnabled(false);
			v.getnuevoMenu().getUnsetMain().setEnabled(false);
			v.getnuevoMenu().getSetCompilable().setEnabled(false);
			v.getnuevoMenu().getUnsetCompilable().setEnabled(false);
			v.getnuevoMenu().getRemoveFolder().setEnabled(false);

			if (v.getProyecto().isModified()) {
				v.getnuevoMenu().getGuardarProyecto().setEnabled(true);
				// v.getnuevoMenu().getSaveAsProject().setEnabled(true);
			}

			TreePath path = v.getNuevoExplorador().getArbol()
					.getSelectionPath();
			DefaultMutableTreeNode filePath;
			Fich fc;
			if (path != null) {

				filePath = (DefaultMutableTreeNode) path.getLastPathComponent();
				fc = (Fich) filePath.getUserObject();

				if (!fc.isDirectory()) {
					v.getnuevoMenu().getRemoveFile().setEnabled(true);
					v.getnuevoMenu().getDeleteFile().setEnabled(true);
					if (!fc.isMainFile())
						v.getnuevoMenu().getSetMain().setEnabled(true);
					if (fc.isMainFile())
						v.getnuevoMenu().getUnsetMain().setEnabled(true);
					if (!fc.isSetFile() || (fc.isSetFile() && fc.isMainFile()))
						v.getnuevoMenu().getSetCompilable().setEnabled(true);
					if (fc.isSetFile() && !fc.isMainFile())
						v.getnuevoMenu().getUnsetCompilable().setEnabled(true);
				} else {
					v.getnuevoMenu().getRemoveFolder().setEnabled(true);
				}
			}

			// si no hay proyecto
			String prj = null;
			try {
				prj = almacenPropiedades.getPropiedad("DefaultAcidePrj");
			} catch (Exception e1) {
				e1.printStackTrace();
			}
			if ((prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))) {
				int editor = v.getCreadorEditor().dameNumEditores();
				if (editor > 0) {
					if (!v.getCreadorEditor().EditorSeleccionado().isMainFile())
						v.getnuevoMenu().getSetMain().setEnabled(true);
					if (v.getCreadorEditor().EditorSeleccionado().isMainFile())
						v.getnuevoMenu().getUnsetMain().setEnabled(true);
					if (!v.getCreadorEditor().EditorSeleccionado()
							.isCompilerFile()
							|| (v.getCreadorEditor().EditorSeleccionado()
									.isCompilerFile() && v.getCreadorEditor()
									.EditorSeleccionado().isMainFile()))
						v.getnuevoMenu().getSetCompilable().setEnabled(true);
					if (v.getCreadorEditor().EditorSeleccionado()
							.isCompilerFile()
							&& !v.getCreadorEditor().EditorSeleccionado()
									.isMainFile())
						v.getnuevoMenu().getUnsetCompilable().setEnabled(true);

					if (!v.getCreadorEditor().EditorSeleccionado().isMainFile())
						v.getnuevoMenu().getSetMain2().setEnabled(true);
					if (v.getCreadorEditor().EditorSeleccionado().isMainFile())
						v.getnuevoMenu().getUnsetMain2().setEnabled(true);
					if (!v.getCreadorEditor().EditorSeleccionado()
							.isCompilerFile()
							|| (v.getCreadorEditor().EditorSeleccionado()
									.isCompilerFile() && v.getCreadorEditor()
									.EditorSeleccionado().isMainFile()))
						v.getnuevoMenu().getSetCompilable2().setEnabled(true);
					if (v.getCreadorEditor().EditorSeleccionado()
							.isCompilerFile()
							&& !v.getCreadorEditor().EditorSeleccionado()
									.isMainFile())
						v.getnuevoMenu().getUnsetCompilable2().setEnabled(true);
				}
			}

		}
		// MENU ARCHIVO
		if (v.getnuevoMenu().getArchivo().isSelected()) {
			// v.getnuevoMenu().getSalvarFich().setEnabled(false);
			v.getnuevoMenu().getSave().setEnabled(false);
			v.getnuevoMenu().getSaveAll().setEnabled(false);
			v.getnuevoMenu().getSetMain2().setEnabled(false);
			v.getnuevoMenu().getUnsetMain2().setEnabled(false);
			v.getnuevoMenu().getSetCompilable2().setEnabled(false);
			v.getnuevoMenu().getUnsetCompilable2().setEnabled(false);

			String prj = null;
			try {
				prj = almacenPropiedades.getPropiedad("DefaultAcidePrj");
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			int editor = v.getCreadorEditor().dameNumEditores();
			if (editor > 0) {

				if ((prj.equals(".//configuration/Default.acidePrj") && v
						.getProyecto().getnombreProy().equals(""))) {
					// no hay proyecto
					v.getnuevoMenu().getSetMain2().setEnabled(true);
					v.getnuevoMenu().getUnsetMain2().setEnabled(true);
					v.getnuevoMenu().getSetCompilable2().setEnabled(true);
					v.getnuevoMenu().getUnsetCompilable2().setEnabled(true);
				}

				if (v.getCreadorEditor().isRedButton() == true) {
					// v.getnuevoMenu().getSalvarFich().setEnabled(true);
					v.getnuevoMenu().getSave().setEnabled(true);
				}
			}
			int eS = v.getCreadorEditor().getEditorSeleccionado();
			v.getCreadorEditor().setEditorSeleccionado(editor - 1);
			for (int i = editor - 1; i >= 0; i--) {
				v.getCreadorEditor().setEditorSeleccionado(i);
				if (v.getCreadorEditor().isRedButton() == true) {
					v.getnuevoMenu().getSaveAll().setEnabled(true);
				}
			}
			v.getCreadorEditor().setEditorSeleccionado(eS);
		}
		// MENU EDICION
		if (v.getnuevoMenu().getEdicion().isSelected()) {
			v.getnuevoMenu().getDeshacer().setEnabled(false);
			v.getnuevoMenu().getRepetir().setEnabled(false);
			v.getnuevoMenu().getCopiar().setEnabled(false);
			v.getnuevoMenu().getPegar().setEnabled(false);
			v.getnuevoMenu().getCortar().setEnabled(false);

			if (v.getnuevoMenu().getUndo().canUndo()) {
				v.getnuevoMenu().getDeshacer().setEnabled(true);
			}
			if (v.getnuevoMenu().getUndo().canRedo()) {
				v.getnuevoMenu().getRepetir().setEnabled(true);
			}
			if (Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null) != null) {
				if(!v.getnuevaSalida().getSalida().hasFocus())
					v.getnuevoMenu().getPegar().setEnabled(true);
				else if (v.getnuevaSalida().getSalida().getSelectionStart() 
						>= v.getnuevaSalida().getMaxPos())
					v.getnuevoMenu().getPegar().setEnabled(true);
			}
			int editor = v.getCreadorEditor().dameNumEditores();
			if (editor > 0) {
				if (v.getnuevaSalida().getSalida().hasFocus()
						&& v.getnuevaSalida().getSalida().getSelectedText() != null) {
					v.getnuevoMenu().getCopiar().setEnabled(true);
					if (v.getnuevaSalida().getSalida().getSelectionStart() >= v
							.getnuevaSalida().getMaxPos())
						v.getnuevoMenu().getCortar().setEnabled(true);
				} else if (v.getCreadorEditor().EditorSeleccionado().getEditor().hasFocus()
						&& v.getCreadorEditor().EditorSeleccionado().getEditor().getSelectedText() != null) {
					v.getnuevoMenu().getCopiar().setEnabled(true);
					v.getnuevoMenu().getCortar().setEnabled(true);
				}
			} else {// se puede copiar de la consola
				if (v.getnuevaSalida().getSalida().getSelectedText() != null) {
					v.getnuevoMenu().getCopiar().setEnabled(true);
					if (v.getnuevaSalida().getSalida().getSelectionStart() >= v
							.getnuevaSalida().getMaxPos())
						v.getnuevoMenu().getCortar().setEnabled(true);
				}
			}
		}
		if (v.getnuevaSalida().getSalida().isFocusOwner()) {
			v.getnuevoMenu().setShellIsFocus(true);
		} else
			v.getnuevoMenu().setShellIsFocus(false);
	}

	public void mouseReleased(MouseEvent arg0) {
	}

	public void mouseEntered(MouseEvent arg0) {
	}

	public void mouseExited(MouseEvent arg0) {
	}
}
