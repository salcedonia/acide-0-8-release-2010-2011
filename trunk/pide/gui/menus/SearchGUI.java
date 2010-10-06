package gui.menus;

import gui.Ventana;
import gui.parametrizacion.LenguajeGUI;

import idioma.Idioma;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import org.apache.log4j.Logger;
import operaciones.fabrica.FactoriaOperaciones;
import operaciones.genericas.Search;
import operaciones.genericas.Key;
import operaciones.log.Log;
import principal.almacenPropiedades;

public class SearchGUI extends JFrame {

	private static SearchGUI instancia;

	// Crea una unica instancia de Ventana
	public static SearchGUI getInstance() {
		if (instancia == null)
			instancia = new SearchGUI();
		return instancia;
	}

	public void inicializa() {
		instancia = null;
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private JButton buscar;

	private JButton cancel;

	private JTextField t1;

	private JCheckBox opcion1;

	private JCheckBox opcion2;

	private JCheckBox opcion3;

	private JRadioButton radioAdelante;

	private JRadioButton radioAtras;

	private JRadioButton radioTodo;

	private JRadioButton docActual;

	private JRadioButton todosDoc;

	private JRadioButton seleccionado;

	private int res;

	private String textSelec;

	private int posIni;

	private int posFinal;

	private static int i;

	private static boolean fin = false;

	private int posActual;

	private int actualDoc;

	private int cont;

	private static boolean ciclo;

	private static boolean primera;

	Logger logger = Log.getLog();

	Ventana v = Ventana.getInstance();

	FactoriaOperaciones fact = FactoriaOperaciones.getInstance();

	private Search b = fact.generaBuscar();

	private ResourceBundle labels;

	public SearchGUI() {
		Idioma i = Idioma.getInstance();

		try {
			i.seleccionIdioma(Integer.parseInt(almacenPropiedades
					.getPropiedad("idioma")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.print("");
		labels = i.getLabels();
		ciclo = false;
		posActual = -2;
		actualDoc = -1;
		posIni = -1;
		textSelec = null;
		res = -1;
		primera = true;
		GridBagLayout grid = new GridBagLayout();
		JPanel panel = new JPanel();
		panel.setLayout(grid);
		GridBagConstraints cons = new GridBagConstraints();
		// this.setSize(new Dimension(400, 325));
		this.setTitle(labels.getString("s556"));
		JPanel panelDireccion = new JPanel();
		panelDireccion.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s567")));
		panelDireccion.setLayout(new GridLayout(0, 1));

		JPanel panelOpciones = new JPanel();
		panelOpciones.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s559")));
		panelOpciones.setLayout(new GridLayout(0, 1));

		JPanel panelAlcance = new JPanel();
		panelAlcance.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s563")));
		panelAlcance.setLayout(new GridLayout(0, 1));

		buscar = new JButton();
		buscar.setText(labels.getString("s556"));
		buscar.setMnemonic(java.awt.event.KeyEvent.VK_F3);
		cancel = new JButton();
		cancel.setText(labels.getString("s42"));
		JLabel etiq1 = new JLabel(labels.getString("s557"), JLabel.CENTER);
		t1 = new JTextField();
		t1.setText("");
		opcion1 = new JCheckBox(labels.getString("s560"), false);
		opcion2 = new JCheckBox(labels.getString("s561"), false);
		opcion3 = new JCheckBox(labels.getString("s562"), false);

		// Panel Opciones
		panelOpciones.add(opcion1);
		panelOpciones.add(opcion2);
		panelOpciones.add(opcion3);

		// Panel Direccion
		ButtonGroup g = new ButtonGroup();
		radioAdelante = new JRadioButton(labels.getString("s568"), true);
		radioAtras = new JRadioButton(labels.getString("s569"), false);
		radioTodo = new JRadioButton(labels.getString("s570"), false);
		g.add(radioAdelante);
		g.add(radioAtras);
		g.add(radioTodo);
		panelDireccion.add(radioAdelante);
		panelDireccion.add(radioAtras);
		panelDireccion.add(radioTodo);

		// Panel Alcance
		g = new ButtonGroup();
		docActual = new JRadioButton(labels.getString("s565"), true);
		todosDoc = new JRadioButton(labels.getString("s566"), false);
		seleccionado = new JRadioButton(labels.getString("s564"), false);
		g.add(seleccionado);
		g.add(docActual);
		g.add(todosDoc);
		panelAlcance.add(seleccionado);
		panelAlcance.add(docActual);
		panelAlcance.add(todosDoc);

		// Posiciones de los componentes
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.gridx = 0;
		cons.gridy = 0;
		cons.gridwidth = 2;
		cons.insets = new Insets(5, 5, 5, 5);
		panel.add(etiq1, cons);
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.gridx = 0;
		cons.gridy = 1;
		cons.ipadx = 300;
		panel.add(t1, cons);
		cons.fill = GridBagConstraints.CENTER;
		cons.gridy = 2;
		cons.gridx = 0;
		panel.add(panelOpciones, cons);
		cons.fill = GridBagConstraints.CENTER;
		cons.gridy = 3;
		cons.gridx = 0;
		cons.ipadx = 100;
		cons.gridwidth = 1;
		panel.add(panelAlcance, cons);
		cons.gridy = 3;
		cons.gridx = 1;
		panel.add(panelDireccion, cons);
		cons.fill = GridBagConstraints.CENTER;
		cons.ipadx = 60;
		cons.weightx = 0.5;
		cons.gridwidth = 2;
		cons.gridy = 4;
		cons.gridx = 0;
		panel.add(buscar, cons);
		cons.fill = GridBagConstraints.CENTER;
		cons.ipadx = 60;
		cons.weightx = 0.5;
		cons.gridwidth = 2;
		cons.gridy = 4;
		cons.gridx = 1;
		panel.add(cancel, cons);
		this.add(panel);
		this.setResizable(false);
		this.setAlwaysOnTop(true);
		this.pack();
		this.setLocationRelativeTo(null);
		
		Buscar_Adaptador buscar_adapt = new Buscar_Adaptador();
		buscar.addActionListener(buscar_adapt);
		Key key = new Key();
		t1.addKeyListener(key);
		opcion1.addKeyListener(key);
		opcion2.addKeyListener(key);
		opcion3.addKeyListener(key);
		radioAdelante.addKeyListener(key);
		radioAtras.addKeyListener(key);
		radioTodo.addKeyListener(key);
		seleccionado.addKeyListener(key);
		todosDoc.addKeyListener(key);
		docActual.addKeyListener(key);

		cancel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				instancia.dispose();

			}
		});
	}

	/**
	 * Clase oyente del botón buscar
	 */
	class Buscar_Adaptador implements ActionListener {

		public void actionPerformed(ActionEvent arg0) {
		  SearchGUI s=SearchGUI.getInstance();
			int direccion = 0;
			if (radioAdelante.isSelected() == true) {
				direccion = 0;

			}
			if (radioAtras.isSelected() == true) {
				direccion = 1;
			}
			if (radioTodo.isSelected() == true)
				direccion = 2;
			if (opcion3.isSelected() == true) {
				opcion2.setSelected(false);
			}
			if (t1.getText().equals("")) {
				s.set_top(false);
				JOptionPane.showMessageDialog(null, labels.getString("s585"));
				s.set_top(true);
				v.getStatusBar().setMessage(labels.getString("s585"));
			}
			int numeditor = v.getCreadorEditor().getEditorSeleccionado();
			//documento actual
			if (docActual.isSelected() == true) {
				i = -1;
				cont = 0;
				res = -1;
				textSelec = null;
				numeditor = v.getCreadorEditor().getEditorSeleccionado();
				res = v.getCreadorEditor().dameEditorI(numeditor).getEditor()
						.getCaretPosition();
				if (direccion == 1) {
					res = v.getCreadorEditor().dameEditorI(numeditor)
							.getEditor().getSelectionStart();
				}
				numeditor = v.getCreadorEditor().getEditorSeleccionado();

				res = b.buscar(res, t1.getText(), v.getCreadorEditor()
						.dameEditorI(numeditor).getTexto(), opcion1
						.isSelected(), opcion2.isSelected(), opcion3
						.isSelected(), direccion);

				if (res != -1) {
					// Muestra la busqueda en el editor de texto
					v.getCreadorEditor().dameEditorI(
							v.getCreadorEditor().getEditorSeleccionado())
							.seleccionaTexto(res, t1.getText().length());

					// Muestra en el log
					logger.info(labels.getString("s583") + " " + t1.getText()
							+ " " + labels.getString("s574"));

					v.getStatusBar().setMessage(
							labels.getString("s583") + " " + t1.getText() + " "
									+ labels.getString("s574"));
					if (opcion2.isSelected() == true) {

						// Muestra la busqueda en el editor de texto
						v
								.getCreadorEditor()
								.dameEditorI(
										v.getCreadorEditor()
												.getEditorSeleccionado())
								.seleccionaTexto(res, b.getCadExpReg().length());
						// Muestra en el log
						logger.info(labels.getString("s577") + " "
								+ b.getCadExpReg() + " "
								+ labels.getString("s574"));
						v.getStatusBar().setMessage(
								labels.getString("s577") + " "
										+ b.getCadExpReg() + " "
										+ labels.getString("s574"));
					}

				}

				else {
					logger.info(labels.getString("s573"));
					s.set_top(false);
					JOptionPane.showMessageDialog(null, labels
							.getString("s573"));
					s.set_top(true);
					v.getStatusBar().setMessage(labels.getString("s573"));
				}
			}

			// TextoSeleccionado
			if (seleccionado.isSelected() == true) {
				cont = 0;
				i = -1;
				numeditor = v.getCreadorEditor().getEditorSeleccionado();
				if (textSelec == null) {
					textSelec = v.getCreadorEditor().dameEditorI(numeditor)
							.getEditor().getSelectedText();
					posIni = v.getCreadorEditor().dameEditorI(numeditor)
							.getEditor().getSelectionStart();
					posFinal = v.getCreadorEditor().dameEditorI(numeditor)
							.getEditor().getSelectionEnd();
					res = 0;
					if (direccion == 1) {
						res = posFinal;
					}
					if ((opcion2.isSelected()) && (direccion != 1))
						res = posIni;
				} else {
					res = v.getCreadorEditor().dameEditorI(numeditor)
							.getEditor().getCaretPosition()
							- posIni;

					if (direccion == 1) {
						res = v.getCreadorEditor().dameEditorI(numeditor)
								.getEditor().getSelectionStart()
								- posIni;
					}
				}

				if (textSelec == null) {
					s.set_top(false);
					JOptionPane.showMessageDialog(null, labels
							.getString("s616"));
					s.set_top(true);
					v.getStatusBar().setMessage(labels.getString("s616"));
				} else {
					res = b.buscar(res, t1.getText(), textSelec, opcion1
							.isSelected(), opcion2.isSelected(), opcion3
							.isSelected(), direccion);
					if (res != -1) {
						// Muestra la busqueda en el editor de texto
						v.getCreadorEditor().dameEditorI(
								v.getCreadorEditor().getEditorSeleccionado())
								.seleccionaTexto(res + posIni,
										t1.getText().length());

						// Muestra en el log
						logger
								.info(labels.getString("s583") + " "
										+ t1.getText() + " "
										+ labels.getString("s574"));

						v.getStatusBar().setMessage(
								labels.getString("s583") + " " + t1.getText()
										+ " " + labels.getString("s574"));

						if (opcion2.isSelected() == true) {

							// Muestra la busqueda en el editor de texto
							v.getCreadorEditor().dameEditorI(
									v.getCreadorEditor()
											.getEditorSeleccionado())
									.seleccionaTexto(res + posIni,
											b.getCadExpReg().length());
							// Muestra en el log
							logger.info(labels.getString("s329") + " "
									+ b.getCadExpReg() + " "
									+ labels.getString("s574"));
							v.getStatusBar().setMessage(
									labels.getString("s329") + " "
											+ t1.getText() + " "
											+ labels.getString("s574"));
						}

					}

					else {
						textSelec = null;
						logger.info(labels.getString("s573"));
						s.set_top(false);
						JOptionPane.showMessageDialog(null, labels
								.getString("s573"));
						s.set_top(true);
						v.getStatusBar().setMessage(labels.getString("s573"));
						s.set_top(false);
						int op = JOptionPane.showConfirmDialog(null, labels
								.getString("s575"));
                        s.set_top(true);
						if (op == JOptionPane.OK_OPTION) {
							docActual.setSelected(true);
							if (direccion != 1)
								res = posFinal;
							else
								res = posIni;
							buscar.doClick();

						}
					}
				}
			}
			// Busqueda en varios ficheros
			if (todosDoc.isSelected() == true) {
				textSelec = null;
				numeditor = v.getCreadorEditor().dameNumEditores();
				if ((ciclo == false) && (posActual == -2)) {
					i = v.getCreadorEditor().getEditorSeleccionado();
					actualDoc = i;
					posActual = v.getCreadorEditor().dameEditorI(i).getEditor()
							.getCaretPosition();
				}

				if (fin == false) {
					if (direccion == 0)
						res = v.getCreadorEditor().dameEditorI(i).getEditor()
								.getCaretPosition();
					if (direccion == 1)
						res = v.getCreadorEditor().dameEditorI(i).getEditor()
								.getSelectionStart();
					if (direccion == 2) {
						res = v.getCreadorEditor().dameEditorI(i).getEditor()
								.getCaretPosition();
					}
					int direc = direccion;
					if (direccion == 2)
						direc = 0;
					res = b.buscar(res, t1.getText(), v.getCreadorEditor()
							.dameEditorI(i).getTexto(), opcion1.isSelected(),
							opcion2.isSelected(), opcion3.isSelected(), direc);
					if ((ciclo == true) && (i == actualDoc)
							&& (res >= posActual))
						fin = true;
					else if ((ciclo == true) && (i == actualDoc) && (res == -1))
						fin = true;
					if (res != -1) {
						cont++;
						if (opcion2.isSelected() == false) {
							// Muestra la busqueda en el editor de texto
							v
									.getCreadorEditor()
									.dameEditorI(i)
									.seleccionaTexto(res, t1.getText().length());
							// Muestra en el log
							logger.info(labels.getString("s583") + " "
									+ t1.getText() + " "
									+ labels.getString("s574"));
							v.getStatusBar().setMessage(
									labels.getString("s583") + " "
											+ t1.getText() + " "
											+ labels.getString("s574"));
						} else {
							// Muestra la busqueda en el editor de texto
							v.getCreadorEditor().dameEditorI(i)
									.seleccionaTexto(res,
											b.getCadExpReg().length());
							// Muestra en el log
							logger.info(labels.getString("s577") + " "
									+ b.getCadExpReg()
									+ labels.getString("s577"));
							v.getStatusBar().setMessage(
									labels.getString("s577") + " "
											+ b.getCadExpReg()
											+ labels.getString("s577"));
						}
					} else {
						logger.info(labels.getString("s573"));
						v.getCreadorEditor().dameEditorI(i).getEditor()
								.setCaretPosition(0);
						if (radioAdelante.isSelected() == true)
							i++;
						else if (radioAtras.isSelected() == true)
							i--;
						else
							i++;
						if (direccion == 0) {
							if (i >= numeditor) {
								fin = true;
							} else {
								v.getCreadorEditor().setEditorSeleccionado(i);
								v.getCreadorEditor().dameEditorI(i).getEditor()
										.setCaretPosition(0);
								buscar.doClick();
							}
						}
						if (direccion == 1) {
							if (i < 0) {
								fin = true;
							} else {
								v.getCreadorEditor().setEditorSeleccionado(i);
								v.getCreadorEditor().dameEditorI(i).getEditor()
										.setCaretPosition(
												v.getCreadorEditor()
														.dameEditorI(i)
														.getEditor().getText()
														.length() - 1);
								buscar.doClick();
							}
						}
						if (direccion == 2) {
							if (i >= numeditor) {
								i = 0;
								ciclo = true;
								v.getCreadorEditor().setEditorSeleccionado(i);
								v.getCreadorEditor().dameEditorI(i).getEditor()
										.setCaretPosition(0);
								buscar.doClick();

							} else {
								v.getCreadorEditor().setEditorSeleccionado(i);
								v.getCreadorEditor().dameEditorI(i).getEditor()
										.setCaretPosition(0);
								buscar.doClick();
							}
						}
					}
				}

				if ((fin == true) && (primera == true)) {
					if (cont == 0) {
						s.set_top(false);
						JOptionPane.showMessageDialog(null, labels
								.getString("s576"));
						s.set_top(true);
						v.getStatusBar().setMessage(labels.getString("s576"));
					} else {
						s.set_top(false);
						JOptionPane.showMessageDialog(null, labels
								.getString("s586"));
						s.set_top(true);
						v.getStatusBar().setMessage(labels.getString("s586"));
					}
					primera = false;

				}
			}
		}
	}

	public JButton getBuscar() {
		// TODO Auto-generated method stub
		return buscar;
	}

	public boolean isCiclo() {
		return ciclo;
	}

	public void setCiclo(boolean ci) {
		ciclo = ci;
	}

	public boolean isFin() {
		return fin;
	}

	public void setFin(boolean f) {
		fin = f;
	}

	public int getPosActual() {
		return posActual;
	}

	public void setPosActual(int posAc) {
		posActual = posAc;
	}

	public Search getB() {
		return b;
	}

	public void setB(Search bu) {
		b = bu;
	}

	public JRadioButton getDocActual() {
		return docActual;
	}

	public void setDocActual(boolean d) {
		this.docActual.setSelected(d);
	}

	public JRadioButton getRadioTodo() {
		return radioTodo;
	}

	public JRadioButton getSeleccionado() {
		return seleccionado;
	}

	public void setT1(String b) {
		t1.setText(b);
	}

	public void setRadioAdelante(boolean b) {
		radioAdelante.setSelected(b);
	}

	public static void setPrimera(boolean primera) {
		SearchGUI.primera = primera;
	}

	public String getTextSelec() {
		return textSelec;
	}

	public void setTextSelec(String textSelec) {
		this.textSelec = textSelec;
	}
	public void set_top(boolean b){
		this.setAlwaysOnTop(b);
		}
}// class

