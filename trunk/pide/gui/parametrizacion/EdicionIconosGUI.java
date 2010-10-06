package gui.parametrizacion;

import gui.Ventana;
import gui.iconos.Iconos;
import idioma.Idioma;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import operaciones.configuracion.IconoEditable;
import operaciones.configuracion.ListaIconosEditables;
import operaciones.genericas.IconosEditablesTableModel;
import operaciones.genericas.Windows;
import operaciones.log.Log;
import org.apache.log4j.Logger;

import es.texto.Fichero;
import es.texto.FiltroFicheros;
import principal.almacenPropiedades;

public class EdicionIconosGUI {
	
	private ResourceBundle labels = Idioma.getInstance().getLabels();
	private JFrame frame;
	private Vector vectorIconos;
	private Vector vectorStringIconos;
	private Vector vectorTableIcons;
	private String[][] matrixTableIcons;
	//private JList listado;
	//private int indexShown;
	private int rowShown;
	private JTable iconsTable;
	private IconosEditablesTableModel myModel;
	private String[] tableColumns = {labels.getString("s260"), labels.getString("s261"), labels.getString("s262"), labels.getString("s263")};
	private static boolean changesSaved;
	/**
	 * Atributo que se encargará de realizar el log de la clase
	 */
	private static Logger logger = Log.getLog();
	
	public EdicionIconosGUI(boolean modify) {
		changesSaved = true;
		if(modify) modifyIconosGUI();
		else newIconosGUI();
	}
	
	public void newIconosGUI() {
		Idioma id = Idioma.getInstance();
		try {
			id.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = id.getLabels();
		logger.info(labels.getString("s132"));
		frame = new JFrame();
		frame.setTitle(labels.getString("s910"));
		JPanel iconoPanel = new JPanel();
		JPanel listaPanel = new JPanel();
		JPanel botonesPanel = new JPanel();
		JPanel iconButtonsPanel = new JPanel();
		JPanel listButtonsPanel = new JPanel();
		frame.setLayout(new GridBagLayout());
		iconoPanel.setLayout(new GridBagLayout());
		listaPanel.setLayout(new GridBagLayout());
		botonesPanel.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		iconButtonsPanel.setLayout(new GridBagLayout());
		listButtonsPanel.setLayout(new GridBagLayout());
		// Elementos de iconoPanel e iconsButtonPanel
		JLabel nombreLabel = new JLabel(labels.getString("s133"),JLabel.LEFT);
		final JTextField nombreTextField = new JTextField();
		JLabel comandoLabel = new JLabel(labels.getString("s134"),JLabel.LEFT);
		final JTextField comandoTextField = new JTextField();
		JLabel comandoItalicLabel = new JLabel(labels.getString("s146"),JLabel.CENTER);
		Font f = nombreLabel.getFont();
		comandoItalicLabel.setFont(new Font(f.getFontName(),Font.ITALIC,f.getSize()));
		JLabel txtAyudaLabel = new JLabel(labels.getString("s135"),JLabel.LEFT);
		final JTextField txtAyudaTextField = new JTextField();
		JLabel imagenLabel = new JLabel(labels.getString("s136"),JLabel.LEFT);
		final JTextField imagenTextField = new JTextField();
		JLabel notaLabel1 = new JLabel(labels.getString("s139"),JLabel.CENTER);
		JButton añadirBoton = new JButton(labels.getString("s137"));
		añadirBoton.setToolTipText(labels.getString("s138"));
		JButton examinarBoton = new JButton(labels.getString("s142"));
		examinarBoton.setToolTipText(labels.getString("s143"));
		JButton quitarBoton = new JButton(labels.getString("s148"));
		quitarBoton.setToolTipText(labels.getString("s149"));	
		JButton modifyButton = new JButton(labels.getString("s257"));
		modifyButton.setToolTipText(labels.getString("s258"));
		// Añadimos los elementos a iconoPanel
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(5,5,5,5);
		c.gridx = 0;
		c.gridy = 0;
		iconoPanel.add(nombreLabel,c);
		c.gridx = 1;
		c.ipadx = 200;
		c.ipady = 5;
		iconoPanel.add(nombreTextField,c);
		c.ipadx = 0;
		c.ipady = 0;
		c.gridx = 0;
		c.gridy = 1;
		iconoPanel.add(comandoLabel,c);
		c.gridx = 1;
		c.ipady = 5;
		iconoPanel.add(comandoTextField,c);
		c.gridx = 0;
		c.gridy = 2;
		c.ipady = 0;
		c.gridwidth = 3;
		iconoPanel.add(comandoItalicLabel,c);
		c.gridy = 3;
		c.gridwidth = 1;
		iconoPanel.add(txtAyudaLabel,c);
		c.gridx = 1;
		c.ipady = 5;
		iconoPanel.add(txtAyudaTextField,c);
		c.gridx = 0;
		c.gridy = 4;
		c.ipady = 0;
		iconoPanel.add(imagenLabel,c);
		c.gridx = 1;
		c.ipady = 5;
		iconoPanel.add(imagenTextField,c);
		c.gridx = 2;
		c.ipady = 0;
		iconoPanel.add(examinarBoton,c);
		c.gridy = 5;
		c.gridwidth = 3;
		c.gridx = 0;
		iconoPanel.add(notaLabel1,c);
		// Add elements to iconButtonsPanel
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		c.ipadx = 0;
		c.ipady = 0;
		c.insets = new Insets(5,5,5,5);
		iconButtonsPanel.add(añadirBoton,c);
		c.gridx = 1;
		iconButtonsPanel.add(modifyButton,c);
		c.gridx = 2;
		iconButtonsPanel.add(quitarBoton,c);
		// Elementos de listaPanel
		vectorIconos = new Vector();
		vectorStringIconos = new Vector();
		vectorTableIcons = new Vector();
		matrixTableIcons = new String[vectorIconos.size()][4];
		transformaIconos();
		//listado = new JList(vectorStringIconos);
		//listado.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		myModel = new IconosEditablesTableModel();
		myModel.setValues(tableColumns, matrixTableIcons);
		iconsTable = new JTable(myModel);
		iconsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		iconsTable.setPreferredScrollableViewportSize(new Dimension(300,100));
		//JScrollPane listaScrollPane = new JScrollPane(listado);		
		JScrollPane tableScrollPane = new JScrollPane(iconsTable);
		JButton guardarBoton = new JButton(labels.getString("s150"));
		guardarBoton.setToolTipText(labels.getString("s151"));
		JButton cargarBoton = new JButton(labels.getString("s152"));
		cargarBoton.setToolTipText(labels.getString("s153"));		
		// Añadimos los elementos a listaPanel
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		c.ipadx = 150;
		c.ipady = 40;
		c.insets = new Insets(5,5,5,5);
		//listaPanel.add(listaScrollPane,c);
		//listaPanel.add(tableScrollPane,c);
		c.gridy = 1;
		listaPanel.add(tableScrollPane,c);
//		c.gridx = 0;
//		c.gridy = 0;
//		c.gridwidth = 1;
//		c.ipadx = 0;
//		c.ipady = 0;
//		listButtonsPanel.add(guardarBoton,c);
//		c.gridx = 1;
//		listButtonsPanel.add(cargarBoton,c);	
//		c.gridx = 0;
//		c.gridy = 2;
//		listaPanel.add(listButtonsPanel,c);
		// Elementos de botonesPanel
		JButton aceptarBoton = new JButton(labels.getString("s154"));
		aceptarBoton.setToolTipText(labels.getString("s155"));
		JButton cancelarBoton = new JButton(labels.getString("s162"));
		cancelarBoton.setToolTipText(labels.getString("s163"));
		// Oyentes
		aceptarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				ListaIconosEditables.setListaIconos(vectorIconos);
				String newName = ".//configuration/toolbar/newTB.BHcfg";
				ListaIconosEditables.guardarLista(newName);
				try {
					String previous = almacenPropiedades.getPropiedad("currentTBCfg");	
					if(changesSaved)
						almacenPropiedades.setPropiedad("previousTBCfg",previous);
					/*if(!previous.endsWith("lastModified.BHcfg"))
						almacenPropiedades.setPropiedad("previousTBCfg",previous);*/
					almacenPropiedades.setPropiedad("currentTBCfg",newName);
					Iconos.generaToolBarFija();
					Iconos.generaToolBarEditable();
					changesSaved = false;
					Ventana v = Ventana.getInstance();
					v.getnuevoMenu().getSaveTB().setEnabled(false);
					v.validate();
					v.repaint();
					frame.dispose();
					v.setEnabled(true);
					v.setAlwaysOnTop(true);
					v.setAlwaysOnTop(false);
					logger.info(labels.getString("s170"));
					//mig
					String prj=null;
					try {
						prj=almacenPropiedades.getPropiedad("DefaultAcidePrj");
					} catch (Exception e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					if (!(prj.equals(".//configuration/Default.acidePrj") && v.getProyecto().getnombreProy().equals(""))){
						Ventana.getInstance().getProyecto().setModified(true);
					}
					
				}
				catch (Exception e1) {
					JOptionPane.showMessageDialog(null,e1.getMessage(),labels.getString("s909"),JOptionPane.ERROR_MESSAGE);
					logger.error(e1.getMessage());
				}				
			}
		});
		cancelarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {	
				logger.info(labels.getString("s164"));
				frame.dispose();
				Ventana v = Ventana.getInstance();
				v.setEnabled(true);
				v.setAlwaysOnTop(true);
				v.setAlwaysOnTop(false);
			}
		});
		quitarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {	
//				if(listado.isSelectionEmpty()) {
//					JOptionPane.showMessageDialog(null,
//							labels.getString("s156"),
//							labels.getString("s157"), JOptionPane.ERROR_MESSAGE);
//				}
//				else {
//					int[] indices = listado.getSelectedIndices();
//					for(int i = indices.length - 1; i >= 0; i--) {
//						vectorIconos.remove(indices[i]);
//						vectorStringIconos.remove(indices[i]);
//					}
////					int index = iconsTable.getSelectedRow();
////					for(int j = index; j < matrixTableIcons; j++) matrix 
//					transformaIconos();
//				}
				int selectedRow = iconsTable.getSelectedRow(); 
				if(selectedRow == -1) {
					JOptionPane.showMessageDialog(null,
							labels.getString("s156"),
							labels.getString("s157"), JOptionPane.ERROR_MESSAGE);
				}	
				else {
					/*String[][] aux = new String[matrixTableIcons.length - 1][4];
					for(int i = 0; i < selectedRow; i++) {
						aux[i][0] = matrixTableIcons[i][0];
						aux[i][1] = matrixTableIcons[i][1];
						aux[i][2] = matrixTableIcons[i][2];
						aux[i][3] = matrixTableIcons[i][3];
					}
					for(int i = selectedRow; i < aux.length; i++) {
						aux[i][0] = matrixTableIcons[i + 1][0];
						aux[i][1] = matrixTableIcons[i + 1][1];
						aux[i][2] = matrixTableIcons[i + 1][2];
						aux[i][3] = matrixTableIcons[i + 1][3];
					}*/
					vectorIconos.remove(selectedRow);
					//vectorStringIconos.remove(selectedRow);
					transformaIconos();
				}
				//listado.setListData(vectorStringIconos);
				myModel.setValues(tableColumns, matrixTableIcons);
				myModel.fireTableDataChanged();
				logger.info(labels.getString("s168"));
			}
		});
		añadirBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String nombre = nombreTextField.getText();
				String comando = comandoTextField.getText();
				String txt = txtAyudaTextField.getText();
				String imagen = imagenTextField.getText();
				IconoEditable i;
				if(imagen.equals("")) {
					i = new IconoEditable(nombre,comando,txt);
					vectorIconos.add(i);
				}
				else {
					i = new IconoEditable(nombre,comando,txt,true,imagen);
					vectorIconos.add(i);
				}
				añadeStringIcono(i);
				myModel.setValues(tableColumns, matrixTableIcons);
				myModel.fireTableDataChanged();
				//listado.setListData(vectorStringIconos);
				logger.info(labels.getString("s167"));
			}
		});		
//		guardarBoton.addActionListener(new ActionListener() {
//			public void actionPerformed(ActionEvent e) {
//				JFileChooser selector = new JFileChooser();
//				FiltroFicheros filtro = new FiltroFicheros(labels.getString("s158"));
//				filtro.addExtension("BHcfg");
//				selector.setFileFilter(filtro);
//				String nombreFichero = "";
//				int valor = selector.showSaveDialog(selector);
//				if (valor == JFileChooser.APPROVE_OPTION) {
//					nombreFichero = selector.getSelectedFile().getAbsolutePath();
//					if(!nombreFichero.endsWith(".BHcfg")) nombreFichero += ".BHcfg";
//					ListaIconosEditables.setListaIconosAux(vectorIconos);
//					ListaIconosEditables.guardarListaAux(nombreFichero);
//					// Muestra operacion en el log
//					logger.info(labels.getString("s159") + nombreFichero + labels.getString("s160"));				
//				} 
//				else if (valor == JFileChooser.CANCEL_OPTION) {
//					selector.cancelSelection();
//					// Muestra operacion en el log
//					logger.info(labels.getString("s161"));
//				}		
//				//logger.info(labels.getString("s166") + nombreFichero);
//			}
//		});
//		cargarBoton.addActionListener(new ActionListener() {
//			public void actionPerformed(ActionEvent e) {
//				Fichero f = new Fichero();
//				FiltroFicheros filtro = new FiltroFicheros(labels.getString("s158"));
//				filtro.addExtension("BHcfg");
//				String ruta = f.leer(filtro);
//				try {
//					ListaIconosEditables.cargaListaAux(ruta);
//					vectorIconos = ListaIconosEditables.getListaIconosAux();
//					transformaIconos();
//					myModel.setValues(tableColumns, matrixTableIcons);
//					myModel.fireTableDataChanged();
//					listado.setListData(vectorStringIconos);
//					logger.info(labels.getString("s165") + ruta);
//				}
//				catch (Exception e1) {
//					JOptionPane.showMessageDialog(null,
//							e1.getMessage(),
//							labels.getString("s264"), JOptionPane.ERROR_MESSAGE);
//				}				
//			}
//		});
		examinarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero f = new Fichero();
				String ruta = f.leer();
				imagenTextField.setText(ruta);
			}
		});
//		listado.addListSelectionListener(new ListSelectionListener() {
//			public void valueChanged(ListSelectionEvent arg0) {
//				if(arg0.getValueIsAdjusting()) return;
//				indexShown = listado.getSelectedIndex();
//				String command = (String) listado.getSelectedValue();
//				int index = command.indexOf("Nombre: ");
//				index += 8;
//				command = command.substring(index);
//				int indexAux = command.indexOf(" - Comando: ");
//				String txt = command.substring(0,indexAux);
//				nombreTextField.setText(txt);
//				index = indexAux;
//				index += 12;
//				command = command.substring(index);
//				indexAux = command.indexOf(" - Texto de Ayuda: ");
//				txt = command.substring(0,indexAux);
//				comandoTextField.setText(txt);
//				index = indexAux;
//				index += 19;
//				command = command.substring(index);
//				indexAux = command.indexOf(" - Imagen: ");
//				// Si no tiene imagen
//				if(indexAux == -1) {
//					txt = command;
//					txtAyudaTextField.setText(txt);
//					imagenTextField.setText("");
//				}
//				// Si tiene imagen
//				else {
//					txt = command.substring(0,indexAux);
//					txtAyudaTextField.setText(txt);
//					index = indexAux;
//					index += 12;
//					txt = command.substring(index);
//					imagenTextField.setText(txt);
//				}
//			}			
//		});
		ListSelectionModel rowSM = iconsTable.getSelectionModel();
		rowSM.addListSelectionListener(new ListSelectionListener() {
		    public void valueChanged(ListSelectionEvent e) {
		        ListSelectionModel lsm = (ListSelectionModel)e.getSource();
		        if (lsm.isSelectionEmpty()) {
		            // no rows are selected
		        } else {
		            rowShown = lsm.getMinSelectionIndex();
		            // rowShown is selected
		            nombreTextField.setText(matrixTableIcons[rowShown][0]);
		            comandoTextField.setText(matrixTableIcons[rowShown][1]);
		            txtAyudaTextField.setText(matrixTableIcons[rowShown][2]);
		            imagenTextField.setText(matrixTableIcons[rowShown][3]);
		        }
		    }
		});
		modifyButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String nombre = nombreTextField.getText();
				String comando = comandoTextField.getText();
				String txt = txtAyudaTextField.getText();
				String imagen = imagenTextField.getText();
				IconoEditable i;
				if(imagen.equals("")) {
					i = new IconoEditable(nombre,comando,txt);
					//vectorIconos.set(indexShown,i);
					vectorIconos.set(rowShown,i);
				}
				else {
					i = new IconoEditable(nombre,comando,txt,true,imagen);
					//vectorIconos.set(indexShown,i);
					vectorIconos.set(rowShown,i);
				}
				modifyStringIcono(i);
				myModel.setValues(tableColumns, matrixTableIcons);
				myModel.fireTableDataChanged();
				//listado.setListData(vectorStringIconos);
				logger.info(labels.getString("s259"));				
			}
		});
//		frame.addWindowListener(new Windows());
//		nombreTextField.addKeyListener(new Keyboard());
//		comandoTextField.addKeyListener(new Keyboard());
//		txtAyudaTextField.addKeyListener(new Keyboard());
//		imagenTextField.addKeyListener(new Keyboard());
//		añadirBoton.addKeyListener(new Keyboard());
//		examinarBoton.addKeyListener(new Keyboard());
//		quitarBoton.addKeyListener(new Keyboard());
//		guardarBoton.addKeyListener(new Keyboard());
//		cargarBoton.addKeyListener(new Keyboard());
//		aceptarBoton.addKeyListener(new Keyboard());
//		cancelarBoton.addKeyListener(new Keyboard());
//		listado.addKeyListener(new Keyboard());
//		try {
//			String current = almacenPropiedades.getPropiedad("currentTBCfg");
//			ListaIconosEditables.cargaListaAux(current);
//			vectorIconos = ListaIconosEditables.getListaIconosAux();
//			transformaIconos();
//			myModel.setValues(tableColumns, matrixTableIcons);
//			myModel.fireTableDataChanged();
//			listado.setListData(vectorStringIconos);
//		}
//		catch (Exception e2) {
//			JOptionPane.showMessageDialog(null,
//					e2.getMessage(),
//					labels.getString("s269"), JOptionPane.ERROR_MESSAGE);
//		}		
		ActionListener escPressed = new ActionListener()	{
			public void actionPerformed(ActionEvent e) {
				frame.dispose();
				Ventana v = Ventana.getInstance();
				v.setEnabled(true);
				v.setAlwaysOnTop(true);
				v.setAlwaysOnTop(false);
			}
		};
		cancelarBoton.registerKeyboardAction(escPressed,"EscapeKey",KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE,0,true),JComponent.WHEN_IN_FOCUSED_WINDOW);
		// Añadimos los elementos a botonesPanel
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		c.ipadx = 0;
		c.ipady = 0;
		c.insets = new Insets(5,5,5,5);
		botonesPanel.add(aceptarBoton,c);
		c.gridx = 1;
		botonesPanel.add(cancelarBoton,c);
		// Añadimos los paneles al frame
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		frame.add(iconoPanel,c);
		c.gridy = 1;
		frame.add(iconButtonsPanel,c);
		c.gridy = 2;
		frame.add(listaPanel,c);
		c.gridy = 3;
		frame.add(botonesPanel,c);
		frame.setResizable(false);
		frame.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = frame.getSize();
		frame.setLocation((screenSize.width - frameSize.width) / 2,
						  (screenSize.height - frameSize.height) / 2);
		frame.setVisible(true);
		logger.info(labels.getString("s207"));
	}
	
	public void modifyIconosGUI() {
		Idioma id = Idioma.getInstance();
		try {
			id.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = id.getLabels();
		logger.info(labels.getString("s132"));
		frame = new JFrame();
		String s = null;
		try {
			s = almacenPropiedades.getPropiedad("currentTBCfg");
			int index = s.lastIndexOf("\\");
			if (index == -1) index = s.lastIndexOf("/");
			s = s.substring(index + 1,s.length() - 6);
		}
		catch (Exception e2) {
			JOptionPane.showMessageDialog(null,e2.getMessage(),labels.getString("s295"),JOptionPane.ERROR_MESSAGE);
		}
		frame.setTitle(labels.getString("s147") + " - " + s);
		//frame.setTitle(labels.getString("s147"));
		JPanel iconoPanel = new JPanel();
		JPanel listaPanel = new JPanel();
		JPanel botonesPanel = new JPanel();
		JPanel iconButtonsPanel = new JPanel();
		JPanel listButtonsPanel = new JPanel();
		frame.setLayout(new GridBagLayout());
		iconoPanel.setLayout(new GridBagLayout());
		listaPanel.setLayout(new GridBagLayout());
		botonesPanel.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		iconButtonsPanel.setLayout(new GridBagLayout());
		listButtonsPanel.setLayout(new GridBagLayout());
		// Elementos de iconoPanel e iconsButtonPanel
		JLabel nombreLabel = new JLabel(labels.getString("s133"),JLabel.LEFT);
		final JTextField nombreTextField = new JTextField();
		JLabel comandoLabel = new JLabel(labels.getString("s134"),JLabel.LEFT);
		final JTextField comandoTextField = new JTextField();
		JLabel comandoItalicLabel = new JLabel(labels.getString("s146"),JLabel.CENTER);
		Font f = nombreLabel.getFont();
		comandoItalicLabel.setFont(new Font(f.getFontName(),Font.ITALIC,f.getSize()));
		JLabel txtAyudaLabel = new JLabel(labels.getString("s135"),JLabel.LEFT);
		final JTextField txtAyudaTextField = new JTextField();
		JLabel imagenLabel = new JLabel(labels.getString("s136"),JLabel.LEFT);
		final JTextField imagenTextField = new JTextField();
		JLabel notaLabel1 = new JLabel(labels.getString("s139"),JLabel.CENTER);
		JButton añadirBoton = new JButton(labels.getString("s137"));
		añadirBoton.setToolTipText(labels.getString("s138"));
		JButton examinarBoton = new JButton(labels.getString("s142"));
		examinarBoton.setToolTipText(labels.getString("s143"));
		JButton quitarBoton = new JButton(labels.getString("s148"));
		quitarBoton.setToolTipText(labels.getString("s149"));	
		JButton modifyButton = new JButton(labels.getString("s257"));
		modifyButton.setToolTipText(labels.getString("s258"));
		// Añadimos los elementos a iconoPanel
		c.fill = GridBagConstraints.BOTH;
		c.insets = new Insets(5,5,5,5);
		c.gridx = 0;
		c.gridy = 0;
		iconoPanel.add(nombreLabel,c);
		c.gridx = 1;
		c.ipadx = 200;
		c.ipady = 5;
		iconoPanel.add(nombreTextField,c);
		c.ipadx = 0;
		c.ipady = 0;
		c.gridx = 0;
		c.gridy = 1;
		iconoPanel.add(comandoLabel,c);
		c.gridx = 1;
		c.ipady = 5;
		iconoPanel.add(comandoTextField,c);
		c.gridx = 0;
		c.gridy = 2;
		c.ipady = 0;
		c.gridwidth = 3;
		iconoPanel.add(comandoItalicLabel,c);
		c.gridy = 3;
		c.gridwidth = 1;
		iconoPanel.add(txtAyudaLabel,c);
		c.gridx = 1;
		c.ipady = 5;
		iconoPanel.add(txtAyudaTextField,c);
		c.gridx = 0;
		c.gridy = 4;
		c.ipady = 0;
		iconoPanel.add(imagenLabel,c);
		c.gridx = 1;
		c.ipady = 5;
		iconoPanel.add(imagenTextField,c);
		c.gridx = 2;
		c.ipady = 0;
		iconoPanel.add(examinarBoton,c);
		c.gridy = 5;
		c.gridwidth = 3;
		c.gridx = 0;
		iconoPanel.add(notaLabel1,c);
		// Add elements to iconButtonsPanel
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		c.ipadx = 0;
		c.ipady = 0;
		c.insets = new Insets(5,5,5,5);
		iconButtonsPanel.add(añadirBoton,c);
		c.gridx = 1;
		iconButtonsPanel.add(modifyButton,c);
		c.gridx = 2;
		iconButtonsPanel.add(quitarBoton,c);
		// Elementos de listaPanel
		vectorIconos = new Vector();
		vectorStringIconos = new Vector();
		vectorTableIcons = new Vector();
		matrixTableIcons = new String[vectorIconos.size()][4];
		transformaIconos();
		//listado = new JList(vectorStringIconos);
		//listado.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		myModel = new IconosEditablesTableModel();
		myModel.setValues(tableColumns, matrixTableIcons);
		iconsTable = new JTable(myModel);
		iconsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		iconsTable.setPreferredScrollableViewportSize(new Dimension(300,100));
		//JScrollPane listaScrollPane = new JScrollPane(listado);		
		JScrollPane tableScrollPane = new JScrollPane(iconsTable);
		JButton guardarBoton = new JButton(labels.getString("s150"));
		guardarBoton.setToolTipText(labels.getString("s151"));
		JButton cargarBoton = new JButton(labels.getString("s152"));
		cargarBoton.setToolTipText(labels.getString("s153"));		
		// Añadimos los elementos a listaPanel
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		c.ipadx = 150;
		c.ipady = 40;
		c.insets = new Insets(5,5,5,5);
		//listaPanel.add(listaScrollPane,c);
		//listaPanel.add(tableScrollPane,c);
		c.gridy = 1;
		listaPanel.add(tableScrollPane,c);
//		c.gridx = 0;
//		c.gridy = 0;
//		c.gridwidth = 1;
//		c.ipadx = 0;
//		c.ipady = 0;
//		listButtonsPanel.add(guardarBoton,c);
//		c.gridx = 1;
//		listButtonsPanel.add(cargarBoton,c);	
//		c.gridx = 0;
//		c.gridy = 2;
//		listaPanel.add(listButtonsPanel,c);
		// Elementos de botonesPanel
		JButton aceptarBoton = new JButton(labels.getString("s154"));
		aceptarBoton.setToolTipText(labels.getString("s155"));
		JButton cancelarBoton = new JButton(labels.getString("s162"));
		cancelarBoton.setToolTipText(labels.getString("s163"));
		// Oyentes
		aceptarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				ListaIconosEditables.setListaIconos(vectorIconos);
				String newName = ".//configuration/toolbar/lastModified.BHcfg";
				ListaIconosEditables.guardarLista(newName);
				try {
					String previous = almacenPropiedades.getPropiedad("currentTBCfg");	
					if(!previous.endsWith("lastModified.BHcfg"))
						almacenPropiedades.setPropiedad("previousTBCfg",previous);
					almacenPropiedades.setPropiedad("currentTBCfg",newName);
					Iconos.generaToolBarFija();
					Iconos.generaToolBarEditable();
					changesSaved = false;
					Ventana v = Ventana.getInstance();
					v.validate();
					v.repaint();
					frame.dispose();
					v.setEnabled(true);
					v.setAlwaysOnTop(true);
					v.setAlwaysOnTop(false);
					v.getnuevoMenu().getSaveTB().setEnabled(true);
					logger.info(labels.getString("s170"));
				}
				catch (Exception e1) {
					JOptionPane.showMessageDialog(null,e1.getMessage(),labels.getString("s909"),JOptionPane.ERROR_MESSAGE);
					logger.error(e1.getMessage());
				}				
			}
		});
		cancelarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {	
				logger.info(labels.getString("s164"));
				frame.dispose();
				Ventana v = Ventana.getInstance();
				v.setEnabled(true);
				v.setAlwaysOnTop(true);
				v.setAlwaysOnTop(false);
			}
		});
		quitarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {	
//				if(listado.isSelectionEmpty()) {
//					JOptionPane.showMessageDialog(null,
//							labels.getString("s156"),
//							labels.getString("s157"), JOptionPane.ERROR_MESSAGE);
//				}
//				else {
//					int[] indices = listado.getSelectedIndices();
//					for(int i = indices.length - 1; i >= 0; i--) {
//						vectorIconos.remove(indices[i]);
//						vectorStringIconos.remove(indices[i]);
//					}
////					int index = iconsTable.getSelectedRow();
////					for(int j = index; j < matrixTableIcons; j++) matrix 
//					transformaIconos();
//				}
				int selectedRow = iconsTable.getSelectedRow(); 
				if(selectedRow == -1) {
					JOptionPane.showMessageDialog(null,
							labels.getString("s156"),
							labels.getString("s157"), JOptionPane.ERROR_MESSAGE);
				}	
				else {
					/*String[][] aux = new String[matrixTableIcons.length - 1][4];
					for(int i = 0; i < selectedRow; i++) {
						aux[i][0] = matrixTableIcons[i][0];
						aux[i][1] = matrixTableIcons[i][1];
						aux[i][2] = matrixTableIcons[i][2];
						aux[i][3] = matrixTableIcons[i][3];
					}
					for(int i = selectedRow; i < aux.length; i++) {
						aux[i][0] = matrixTableIcons[i + 1][0];
						aux[i][1] = matrixTableIcons[i + 1][1];
						aux[i][2] = matrixTableIcons[i + 1][2];
						aux[i][3] = matrixTableIcons[i + 1][3];
					}*/
					vectorIconos.remove(selectedRow);
					//vectorStringIconos.remove(selectedRow);
					transformaIconos();
				}
				//listado.setListData(vectorStringIconos);
				myModel.setValues(tableColumns, matrixTableIcons);
				myModel.fireTableDataChanged();
				logger.info(labels.getString("s168"));
			}
		});
		añadirBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String nombre = nombreTextField.getText();
				String comando = comandoTextField.getText();
				String txt = txtAyudaTextField.getText();
				String imagen = imagenTextField.getText();
				IconoEditable i;
				if(imagen.equals("")) {
					i = new IconoEditable(nombre,comando,txt);
					vectorIconos.add(i);
				}
				else {
					i = new IconoEditable(nombre,comando,txt,true,imagen);
					vectorIconos.add(i);
				}
				añadeStringIcono(i);
				myModel.setValues(tableColumns, matrixTableIcons);
				myModel.fireTableDataChanged();
				//listado.setListData(vectorStringIconos);
				logger.info(labels.getString("s167"));
			}
		});		
//		guardarBoton.addActionListener(new ActionListener() {
//			public void actionPerformed(ActionEvent e) {
//				JFileChooser selector = new JFileChooser();
//				FiltroFicheros filtro = new FiltroFicheros(labels.getString("s158"));
//				filtro.addExtension("BHcfg");
//				selector.setFileFilter(filtro);
//				String nombreFichero = "";
//				int valor = selector.showSaveDialog(selector);
//				if (valor == JFileChooser.APPROVE_OPTION) {
//					nombreFichero = selector.getSelectedFile().getAbsolutePath();
//					if(!nombreFichero.endsWith(".BHcfg")) nombreFichero += ".BHcfg";
//					ListaIconosEditables.setListaIconosAux(vectorIconos);
//					ListaIconosEditables.guardarListaAux(nombreFichero);
//					// Muestra operacion en el log
//					logger.info(labels.getString("s159") + nombreFichero + labels.getString("s160"));				
//				} 
//				else if (valor == JFileChooser.CANCEL_OPTION) {
//					selector.cancelSelection();
//					// Muestra operacion en el log
//					logger.info(labels.getString("s161"));
//				}		
//				//logger.info(labels.getString("s166") + nombreFichero);
//			}
//		});
//		cargarBoton.addActionListener(new ActionListener() {
//			public void actionPerformed(ActionEvent e) {
//				Fichero f = new Fichero();
//				FiltroFicheros filtro = new FiltroFicheros(labels.getString("s158"));
//				filtro.addExtension("BHcfg");
//				String ruta = f.leer(filtro);
//				try {
//					ListaIconosEditables.cargaListaAux(ruta);
//					vectorIconos = ListaIconosEditables.getListaIconosAux();
//					transformaIconos();
//					myModel.setValues(tableColumns, matrixTableIcons);
//					myModel.fireTableDataChanged();
//					listado.setListData(vectorStringIconos);
//					logger.info(labels.getString("s165") + ruta);
//				}
//				catch (Exception e1) {
//					JOptionPane.showMessageDialog(null,
//							e1.getMessage(),
//							labels.getString("s264"), JOptionPane.ERROR_MESSAGE);
//				}				
//			}
//		});
		examinarBoton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Fichero f = new Fichero();
				String ruta = f.leer();
				imagenTextField.setText(ruta);
			}
		});
//		listado.addListSelectionListener(new ListSelectionListener() {
//			public void valueChanged(ListSelectionEvent arg0) {
//				if(arg0.getValueIsAdjusting()) return;
//				indexShown = listado.getSelectedIndex();
//				String command = (String) listado.getSelectedValue();
//				int index = command.indexOf("Nombre: ");
//				index += 8;
//				command = command.substring(index);
//				int indexAux = command.indexOf(" - Comando: ");
//				String txt = command.substring(0,indexAux);
//				nombreTextField.setText(txt);
//				index = indexAux;
//				index += 12;
//				command = command.substring(index);
//				indexAux = command.indexOf(" - Texto de Ayuda: ");
//				txt = command.substring(0,indexAux);
//				comandoTextField.setText(txt);
//				index = indexAux;
//				index += 19;
//				command = command.substring(index);
//				indexAux = command.indexOf(" - Imagen: ");
//				// Si no tiene imagen
//				if(indexAux == -1) {
//					txt = command;
//					txtAyudaTextField.setText(txt);
//					imagenTextField.setText("");
//				}
//				// Si tiene imagen
//				else {
//					txt = command.substring(0,indexAux);
//					txtAyudaTextField.setText(txt);
//					index = indexAux;
//					index += 12;
//					txt = command.substring(index);
//					imagenTextField.setText(txt);
//				}
//			}			
//		});
		ListSelectionModel rowSM = iconsTable.getSelectionModel();
		rowSM.addListSelectionListener(new ListSelectionListener() {
		    public void valueChanged(ListSelectionEvent e) {
		        ListSelectionModel lsm = (ListSelectionModel)e.getSource();
		        if (lsm.isSelectionEmpty()) {
		            // no rows are selected
		        } else {
		            rowShown = lsm.getMinSelectionIndex();
		            // rowShown is selected
		            nombreTextField.setText(matrixTableIcons[rowShown][0]);
		            comandoTextField.setText(matrixTableIcons[rowShown][1]);
		            txtAyudaTextField.setText(matrixTableIcons[rowShown][2]);
		            imagenTextField.setText(matrixTableIcons[rowShown][3]);
		        }
		    }
		});
		modifyButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String nombre = nombreTextField.getText();
				String comando = comandoTextField.getText();
				String txt = txtAyudaTextField.getText();
				String imagen = imagenTextField.getText();
				IconoEditable i;
				if(imagen.equals("")) {
					i = new IconoEditable(nombre,comando,txt);
					//vectorIconos.set(indexShown,i);
					vectorIconos.set(rowShown,i);
				}
				else {
					i = new IconoEditable(nombre,comando,txt,true,imagen);
					//vectorIconos.set(indexShown,i);
					vectorIconos.set(rowShown,i);
				}
				modifyStringIcono(i);
				myModel.setValues(tableColumns, matrixTableIcons);
				myModel.fireTableDataChanged();
				//listado.setListData(vectorStringIconos);
				logger.info(labels.getString("s259"));				
			}
		});
//		frame.addWindowListener(new Windows());
//		nombreTextField.addKeyListener(new Keyboard());
//		comandoTextField.addKeyListener(new Keyboard());
//		txtAyudaTextField.addKeyListener(new Keyboard());
//		imagenTextField.addKeyListener(new Keyboard());
//		añadirBoton.addKeyListener(new Keyboard());
//		examinarBoton.addKeyListener(new Keyboard());
//		quitarBoton.addKeyListener(new Keyboard());
//		guardarBoton.addKeyListener(new Keyboard());
//		cargarBoton.addKeyListener(new Keyboard());
//		aceptarBoton.addKeyListener(new Keyboard());
//		cancelarBoton.addKeyListener(new Keyboard());
//		listado.addKeyListener(new Keyboard());
		try {
			String current = almacenPropiedades.getPropiedad("currentTBCfg");
			ListaIconosEditables.cargaListaAux(current);
			vectorIconos = ListaIconosEditables.getListaIconosAux();
			transformaIconos();
			myModel.setValues(tableColumns, matrixTableIcons);
			myModel.fireTableDataChanged();
			//listado.setListData(vectorStringIconos);
		}
		catch (Exception e2) {
			JOptionPane.showMessageDialog(null,
					e2.getMessage(),
					labels.getString("s269"), JOptionPane.ERROR_MESSAGE);
		}		
		ActionListener escPressed = new ActionListener()	{
			public void actionPerformed(ActionEvent e) {
				frame.dispose();
				Ventana v = Ventana.getInstance();
				v.setEnabled(true);
				v.setAlwaysOnTop(true);
				v.setAlwaysOnTop(false);
			}
		};
		cancelarBoton.registerKeyboardAction(escPressed,"EscapeKey",KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE,0,true),JComponent.WHEN_IN_FOCUSED_WINDOW);
		// Añadimos los elementos a botonesPanel
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		c.ipadx = 0;
		c.ipady = 0;
		c.insets = new Insets(5,5,5,5);
		botonesPanel.add(aceptarBoton,c);
		c.gridx = 1;
		botonesPanel.add(cancelarBoton,c);
		// Añadimos los paneles al frame
		c.gridx = 0;
		c.gridy = 0;
		c.gridwidth = 1;
		frame.add(iconoPanel,c);
		c.gridy = 1;
		frame.add(iconButtonsPanel,c);
		c.gridy = 2;
		frame.add(listaPanel,c);
		c.gridy = 3;
		frame.add(botonesPanel,c);
		frame.setResizable(false);
		frame.pack();
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = frame.getSize();
		frame.setLocation((screenSize.width - frameSize.width) / 2,
						  (screenSize.height - frameSize.height) / 2);
		frame.setVisible(true);
		logger.info(labels.getString("s207"));
	}
	
	private void transformaIconos() {
//		String s;
//		IconoEditable i;
//		vectorStringIconos.removeAllElements();
//		for(int j = 0; j < vectorIconos.size(); j++) {
//			i = (IconoEditable) vectorIconos.get(j);
//			s = "Nombre: " + i.getNombre() + " - Comando: " + i.getComando() + " - Texto de Ayuda: " + i.getTextoAyuda();
//			if(i.getTieneImagen()) s += " - Imagen: " + i.getImagen();
//			vectorStringIconos.add(s);
//		}
		
		////////////////////////////////////////////////////////////////////////////////////
		//																				////
		//			PARTE PARA LA TABLA													////
		//																				////
		////////////////////////////////////////////////////////////////////////////////////
		
		String[][] data = new String[vectorIconos.size()][4]; 
		IconoEditable icon;
		vectorTableIcons.removeAllElements();
		for(int j = 0; j < vectorIconos.size(); j++) {
			icon = (IconoEditable) vectorIconos.get(j);
			data[j][0] = icon.getNombre();
			data[j][1] = icon.getComando();
			data[j][2] = icon.getTextoAyuda();
			if(icon.getTieneImagen()) data[j][3] = icon.getImagen();
			else data[j][3] = "";
		}
		matrixTableIcons = data;
	}
	
	private void añadeStringIcono(IconoEditable i) {
//		String s = "Nombre: " + i.getNombre() + " - Comando: " + i.getComando() + " - Texto de Ayuda: " + i.getTextoAyuda();
//		if(i.getTieneImagen()) s += " - Imagen: " + i.getImagen();
//		vectorStringIconos.add(s);
		
		////////////////////////////////////////////////////////////////////////////////////
		//																				////
		//			PARTE PARA LA TABLA													////
		//																				////
		////////////////////////////////////////////////////////////////////////////////////
		
		String[][] aux = new String[vectorIconos.size()][4];
		for(int j = 0; j < matrixTableIcons.length; j++) {
			aux[j] = matrixTableIcons[j];
		}
		aux[matrixTableIcons.length][0] = i.getNombre();
		aux[matrixTableIcons.length][1] = i.getComando();
		aux[matrixTableIcons.length][2] = i.getTextoAyuda();
		if(i.getTieneImagen()) aux[matrixTableIcons.length][3] = i.getImagen();
		else aux[matrixTableIcons.length][3] = "";
		matrixTableIcons = aux;
	}
	
	private void modifyStringIcono(IconoEditable i) {
//		String s = "Nombre: " + i.getNombre() + " - Comando: " + i.getComando() + " - Texto de Ayuda: " + i.getTextoAyuda();
//		if(i.getTieneImagen()) s += " - Imagen: " + i.getImagen();
//		vectorStringIconos.set(indexShown,s);
		
		////////////////////////////////////////////////////////////////////////////////////
		//																				////
		//			PARTE PARA LA TABLA													////
		//																				////
		////////////////////////////////////////////////////////////////////////////////////
		
//		String[][] aux = new String[vectorIconos.size()][4];
//		for(int j = 0; j < matrixTableIcons.length; j++) {
//			aux[j] = matrixTableIcons[j];
//		}
//		aux[rowShown][0] = i.getNombre();
//		aux[rowShown][1] = i.getComando();
//		aux[rowShown][2] = i.getTextoAyuda();
//		if(i.getTieneImagen()) aux[rowShown][3] = i.getImagen();
//		else aux[rowShown][3] = "";
//		matrixTableIcons = aux;
		matrixTableIcons[rowShown][0] = i.getNombre();
		matrixTableIcons[rowShown][1] = i.getComando();
		matrixTableIcons[rowShown][2] = i.getTextoAyuda();
		if(i.getTieneImagen()) matrixTableIcons[rowShown][3] = i.getImagen();
		else matrixTableIcons[rowShown][3] = "";
	}

	public static boolean isChangesSaved() {
		return changesSaved;
	}

	public static void setChangesSaved(boolean changesSaved) {
		EdicionIconosGUI.changesSaved = changesSaved;
	}

	public JTable getIconsTable() {
		return iconsTable;
	}

	public void setIconsTable(JTable iconsTable) {
		this.iconsTable = iconsTable;
	}

//	public int getIndexShown() {
//		return indexShown;
//	}
//
//	public void setIndexShown(int indexShown) {
//		this.indexShown = indexShown;
//	}

//	public JList getListado() {
//		return listado;
//	}
//
//	public void setListado(JList listado) {
//		this.listado = listado;
//	}

	public String[][] getMatrixTableIcons() {
		return matrixTableIcons;
	}

	public void setMatrixTableIcons(String[][] matrixTableIcons) {
		this.matrixTableIcons = matrixTableIcons;
	}

	public int getRowShown() {
		return rowShown;
	}

	public void setRowShown(int rowShown) {
		this.rowShown = rowShown;
	}

	public String[] getTableColumns() {
		return tableColumns;
	}

	public void setTableColumns(String[] tableColumns) {
		this.tableColumns = tableColumns;
	}

	public Vector getVectorIconos() {
		return vectorIconos;
	}

	public void setVectorIconos(Vector vectorIconos) {
		this.vectorIconos = vectorIconos;
	}

	public Vector getVectorStringIconos() {
		return vectorStringIconos;
	}

	public void setVectorStringIconos(Vector vectorStringIconos) {
		this.vectorStringIconos = vectorStringIconos;
	}

	public Vector getVectorTableIcons() {
		return vectorTableIcons;
	}

	public void setVectorTableIcons(Vector vectorTableIcons) {
		this.vectorTableIcons = vectorTableIcons;
	}
	
//	class Keyboard extends KeyAdapter {
//		public void keyPressed(KeyEvent evt) {
//			if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
//				frame.dispose();
//				Ventana v = Ventana.getInstance();
//				v.setEnabled(true);
//				v.setAlwaysOnTop(true);
//				v.setAlwaysOnTop(false);
//			}
//		}
//	}

}
