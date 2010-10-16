package gui.menu.edit;

import gui.MainWindow;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import javax.swing.JTextField;

import language.Language;
import operations.factory.OperationsFactory;
import operations.listeners.AcideKeyboardListener;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/**
 * 
 */
public class ReplaceGUI extends JFrame {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private static ReplaceGUI _instance;

	/**
	 * 
	 * @return
	 */
	public static ReplaceGUI getInstance() {
		if (_instance == null)
			_instance = new ReplaceGUI();
		return _instance;
	}

	/**
	 * 
	 */
	public void inicialize() {
		_instance = null;
	}

	private JTextField t1;

	private JTextField t2;

	private JCheckBox opcion1;

	private JCheckBox opcion2;

	private JCheckBox opcion3;

	private JRadioButton radioAdelante;

	private JRadioButton radioAtras;

	private JRadioButton radioTodo;

	private JRadioButton docActual;

	private JRadioButton todosDoc;

	private JRadioButton seleccionado;

	private JButton buscar, reemplazar, reemTodo;

	private JButton cancel;

	private int res;

	private int posIni;

	private int posFinal;

	private String textSelec;

	private int cont;

	private int posActual;

	private static int i;

	private static boolean fin = false;

	private static boolean ciclo = false;

	private static boolean primera;

	private int actualDoc;

	private static boolean primerReem = true;

	private ResourceBundle labels;

	Logger logger = Log.getLog();

	OperationsFactory fact = OperationsFactory.getInstance();

	Search b = fact.buildSearch();

	MainWindow v = MainWindow.getInstance();

	/**
	 * Constructor of the class.
	 */
	public ReplaceGUI() {

		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		labels = language.getLabels();

		posIni = 0;
		textSelec = null;
		res = -1;
		FlowLayout flow = new FlowLayout();
		GridBagLayout grid = new GridBagLayout();
		GridBagConstraints cons = new GridBagConstraints();
		JPanel panelBotones = new JPanel(flow);
		JPanel panel = new JPanel();
		panel.setLayout(grid);
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
		this.setLayout(grid);
		this.setSize(new Dimension(550, 390));
		this.setTitle(labels.getString("s572"));
		this.setIconImage(new ImageIcon(ICON).getImage());

		buscar = new JButton();
		buscar.setText(labels.getString("s556"));
		reemplazar = new JButton();
		reemplazar.setText(labels.getString("s572"));
		reemTodo = new JButton();
		reemTodo.setText(labels.getString("s571"));
		reemplazar.setEnabled(true);
		reemTodo.setEnabled(true);
		cancel = new JButton();
		cancel.setText(labels.getString("s42"));
		JLabel etiq1 = new JLabel(labels.getString("s557"), JLabel.CENTER);
		t1 = new JTextField();
		t1.setText("");
		JLabel etiq2 = new JLabel(labels.getString("s558"), JLabel.CENTER);
		t2 = new JTextField();
		t2.setText("");
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

		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.gridx = 0;
		cons.gridy = 2;
		cons.gridwidth = 2;
		cons.insets = new Insets(5, 5, 5, 5);
		panel.add(etiq2, cons);
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.gridx = 0;
		cons.gridy = 3;
		cons.ipadx = 300;
		panel.add(t2, cons);

		cons.fill = GridBagConstraints.CENTER;
		cons.gridy = 4;
		cons.gridx = 0;
		panel.add(panelOpciones, cons);
		cons.fill = GridBagConstraints.CENTER;
		cons.gridy = 5;
		cons.gridx = 0;
		cons.ipadx = 100;
		cons.gridwidth = 1;
		panel.add(panelAlcance, cons);
		cons.gridy = 5;
		cons.gridx = 1;
		panel.add(panelDireccion, cons);

		panelBotones.add(buscar);
		panelBotones.add(reemplazar);
		panelBotones.add(reemTodo);
		panelBotones.add(cancel);
		cons.fill = GridBagConstraints.CENTER;
		cons.gridy = 6;
		cons.gridx = 0;
		cons.gridwidth = 2;
		cons.insets = new Insets(0, 0, 0, 0);
		panel.add(panelBotones, cons);
		this.add(panel);
		this.setResizable(false);
		this.setAlwaysOnTop(true);
		this.setLocationRelativeTo(null);
		seleccionado.addKeyListener(new AcideKeyboardListener());
		t1.addKeyListener(new AcideKeyboardListener());
		t2.addKeyListener(new AcideKeyboardListener());
		opcion1.addKeyListener(new AcideKeyboardListener());
		opcion2.addKeyListener(new AcideKeyboardListener());
		opcion3.addKeyListener(new AcideKeyboardListener());
		radioAdelante.addKeyListener(new AcideKeyboardListener());
		radioAtras.addKeyListener(new AcideKeyboardListener());
		radioTodo.addKeyListener(new AcideKeyboardListener());
		reemplazar.addKeyListener(new AcideKeyboardListener());
		reemTodo.addKeyListener(new AcideKeyboardListener());
		todosDoc.addKeyListener(new AcideKeyboardListener());
		docActual.addKeyListener(new AcideKeyboardListener());
		buscar.addKeyListener(new AcideKeyboardListener());
		buscar.addActionListener(new Buscar_Adaptador());
		reemplazar.addActionListener(new Reemplazar_Adaptador());
		reemTodo.addActionListener(new ReemplazarTodo_Adaptador());

		cancel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				_instance.dispose();

			}
		});
	}

	/**
	 * 
	 */
	class Buscar_Adaptador implements ActionListener {

		@SuppressWarnings("static-access")
		public void actionPerformed(ActionEvent arg0) {
			ReplaceGUI r = ReplaceGUI.getInstance();
			primerReem = false;
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
				r.set_top(false);
				JOptionPane.showMessageDialog(null, labels.getString("s585"));

				r.set_top(true);
				v.getStatusBar().setMessage(labels.getString("s585"));
			}
			int numeditor = v.getEditorBuilder().getSelectedEditorIndex();
			if (docActual.isSelected() == true) {
				i = -1;
				cont = 0;
				res = -1;
				textSelec = null;
				numeditor = v.getEditorBuilder().getSelectedEditorIndex();
				res = v.getEditorBuilder().getEditorAt(numeditor).getEditor()
						.getCaretPosition();
				if (direccion == 1) {
					res = v.getEditorBuilder().getEditorAt(numeditor)
							.getEditor().getSelectionStart();
				}
				numeditor = v.getEditorBuilder().getSelectedEditorIndex();

				res = b.search(res, t1.getText(), v.getEditorBuilder()
						.getEditorAt(numeditor).getText(),
						opcion1.isSelected(), opcion2.isSelected(),
						opcion3.isSelected(), direccion);

				if (res != -1) {
					// Muestra la busqueda en el editor de texto
					v.getEditorBuilder()
							.getEditorAt(
									v.getEditorBuilder().getSelectedEditorIndex())
							.selectText(res, t1.getText().length());

					// Muestra en el log
					logger.info(labels.getString("s583") + t1.getText()
							+ labels.getString("s574"));
					v.getStatusBar().setMessage(
							labels.getString("s583") + " " + t1.getText() + " "
									+ labels.getString("s574"));

					if (opcion2.isSelected() == true) {

						// Muestra la busqueda en el editor de texto
						v.getEditorBuilder()
								.getEditorAt(
										v.getEditorBuilder()
												.getSelectedEditorIndex())
								.selectText(res,
										b.getRegularExpresion().length());
						// Muestra en el log
						logger.info(labels.getString("s577") + " "
								+ b.getRegularExpresion() + " "
								+ labels.getString("s574"));
						v.getStatusBar().setMessage(
								labels.getString("s577") + " "
										+ b.getRegularExpresion() + " "
										+ labels.getString("s574"));
					}

				}

				else {
					logger.info(labels.getString("s573"));
					r.set_top(false);
					JOptionPane.showMessageDialog(null,
							labels.getString("s573"));
					r.set_top(true);
					v.getStatusBar().setMessage(labels.getString("s573"));
				}
			}

			// TextoSeleccionado
			if (seleccionado.isSelected() == true) {
				cont = 0;
				i = -1;
				numeditor = v.getEditorBuilder().getSelectedEditorIndex();
				if (textSelec == null) {
					System.out.println("select");
					textSelec = v.getEditorBuilder().getEditorAt(numeditor)
							.getEditor().getSelectedText();
					posIni = v.getEditorBuilder().getEditorAt(numeditor)
							.getEditor().getSelectionStart();
					posFinal = v.getEditorBuilder().getEditorAt(numeditor)
							.getEditor().getSelectionEnd();
					res = 0;
					if (direccion == 1) {
						res = posFinal;
					}
					if ((opcion2.isSelected()) && (direccion != 1))
						res = posIni;
				} else {

					res = v.getEditorBuilder().getEditorAt(numeditor)
							.getEditor().getCaretPosition()
							- posIni;

					if (direccion == 1) {
						res = v.getEditorBuilder().getEditorAt(numeditor)
								.getEditor().getSelectionStart()
								- posIni;
					}
				}

				if (textSelec == null) {
					r.set_top(false);
					JOptionPane.showMessageDialog(null,
							labels.getString("s616"));
					r.set_top(true);
					v.getStatusBar().setMessage(labels.getString("s616"));
				}

				else {
					res = b.search(res, t1.getText(), textSelec,
							opcion1.isSelected(), opcion2.isSelected(),
							opcion3.isSelected(), direccion);

					if (res != -1) {
						// Muestra la busqueda en el editor de texto
						v.getEditorBuilder()
								.getEditorAt(
										v.getEditorBuilder()
												.getSelectedEditorIndex())
								.selectText(res + posIni, t1.getText().length());

						// Muestra en el log
						v.getOutput().setText(
								labels.getString("s583") + t1.getText()
										+ labels.getString("s574"));

						v.getStatusBar().setMessage(
								labels.getString("s583") + " " + t1.getText()
										+ " " + labels.getString("s574"));

						if (opcion2.isSelected() == true) {

							// Muestra la busqueda en el editor de texto
							v.getEditorBuilder()
									.getEditorAt(
											v.getEditorBuilder()
													.getSelectedEditorIndex())
									.selectText(res + posIni,
											b.getRegularExpresion().length());
							// Muestra en el log
							logger.info(labels.getString("s329") + " "
									+ b.getRegularExpresion() + " "
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
						r.set_top(false);
						JOptionPane.showMessageDialog(null,
								labels.getString("s573"));
						v.getStatusBar().setMessage(labels.getString("s573"));
						int op = JOptionPane.showConfirmDialog(null,
								labels.getString("s575"));
						r.set_top(true);
						if (op == JOptionPane.OK_OPTION) {
							docActual.setSelected(true);
							if (direccion != 1) {
								res = posFinal;
							} else
								res = posIni;
							buscar.doClick();

						}
					}
				}
			}
			// Busqueda en varios ficheros
			if (todosDoc.isSelected() == true) {
				textSelec = null;
				numeditor = v.getEditorBuilder().getNumEditors();
				if ((ciclo == false) && (posActual == -2)) {
					i = v.getEditorBuilder().getSelectedEditorIndex();
					actualDoc = i;
					posActual = v.getEditorBuilder().getEditorAt(i).getEditor()
							.getCaretPosition();
				}
				if (fin == false) {
					if (direccion == 0)
						res = v.getEditorBuilder().getEditorAt(i).getEditor()
								.getCaretPosition();
					if (direccion == 1)
						res = v.getEditorBuilder().getEditorAt(i).getEditor()
								.getSelectionStart();
					if (direccion == 2) {
						res = v.getEditorBuilder().getEditorAt(i).getEditor()
								.getCaretPosition();
					}
					int direc = direccion;
					if (direccion == 2)
						direc = 0;
					res = b.search(res, t1.getText(), v.getEditorBuilder()
							.getEditorAt(i).getText(), opcion1.isSelected(),
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
							v.getEditorBuilder().getEditorAt(i)
									.selectText(res, t1.getText().length());
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
							v.getEditorBuilder()
									.getEditorAt(i)
									.selectText(res,
											b.getRegularExpresion().length());
							// Muestra en el log
							logger.info(labels.getString("s577") + " "
									+ b.getRegularExpresion() + " "
									+ labels.getString("s577"));
							v.getStatusBar().setMessage(
									labels.getString("s577") + " "
											+ b.getRegularExpresion() + " "
											+ labels.getString("s577"));
						}

					} else {
						logger.info(labels.getString("s573"));
						v.getStatusBar().setMessage(labels.getString("s573"));
						v.getEditorBuilder().getEditorAt(i).getEditor()
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
								v.getEditorBuilder().setSelectedEditorAt(i);
								v.getEditorBuilder().getEditorAt(i).getEditor()
										.setCaretPosition(0);

								buscar.doClick();
							}
						}
						if (direccion == 1) {
							if (i < 0) {
								fin = true;
							} else {
								v.getEditorBuilder().setSelectedEditorAt(i);
								v.getEditorBuilder()
										.getEditorAt(i)
										.getEditor()
										.setCaretPosition(
												v.getEditorBuilder()
														.getEditorAt(i)
														.getEditor().getText()
														.length() - 1);

								buscar.doClick();
							}
						}
						if (direccion == 2) {
							if (i >= numeditor) {
								i = 0;
								ciclo = true;
								v.getEditorBuilder().setSelectedEditorAt(i);
								v.getEditorBuilder().getEditorAt(i).getEditor()
										.setCaretPosition(0);
								buscar.doClick();

							} else {
								v.getEditorBuilder().setSelectedEditorAt(i);
								v.getEditorBuilder().getEditorAt(i).getEditor()
										.setCaretPosition(0);
								buscar.doClick();
							}
						}
					}
				}

				if ((fin == true) && (primera == true)) {
					if (cont == 0) {
						r.set_top(false);
						JOptionPane.showMessageDialog(null,
								labels.getString("s576"));
						r.set_top(true);
						v.getStatusBar().setMessage(labels.getString("s576"));
					} else {
						r.set_top(false);
						JOptionPane.showMessageDialog(null,
								labels.getString("s586"));
						r.set_top(true);
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

	public void setCycle(boolean ci) {
		ciclo = ci;
	}

	public boolean isFin() {
		return fin;
	}

	public void setEnd(boolean f) {
		fin = f;
	}

	public int getPosActual() {
		return posActual;
	}

	public void setCurrentPosition(int posAc) {
		posActual = posAc;
	}

	public Search getB() {
		return b;
	}

	public void setB(Search bu) {
		b = bu;
	}

	public JRadioButton getCurrentDocument() {
		return docActual;
	}

	public void setDocActual(JRadioButton docActual) {
		this.docActual = docActual;
	}

	public JRadioButton getRadioTodo() {
		return radioTodo;
	}

	public JRadioButton getSelected() {
		return seleccionado;
	}

	public static void setFirst(boolean p) {
		ReplaceGUI.primera = p;
	}

	/**
	 * 
	 */
	int posOrig = -1;

	class Reemplazar_Adaptador implements ActionListener {
		@SuppressWarnings("static-access")
		public void actionPerformed(ActionEvent e) {

			if (seleccionado.isSelected() == true) {
				int numeditor = v.getEditorBuilder().getSelectedEditorIndex();
				String text = null;
				if (primerReem == true)
					buscar.doClick();
				text = v.getEditorBuilder().getEditorAt(numeditor).getEditor()
						.getSelectedText();
				if (res != -1) {
					if (text != null) {
						int pos;
						ReplaceGUI re = ReplaceGUI.getInstance();

						if (re.radioAdelante.isSelected() == true)
							pos = v.getEditorBuilder().getEditorAt(numeditor)
									.getEditor().getSelectionEnd();
						else
							pos = v.getEditorBuilder().getEditorAt(numeditor)
									.getEditor().getSelectionStart();

						v.getEditorBuilder().getEditorAt(numeditor).getEditor()
								.replaceSelection(t2.getText());
						if (re.radioAdelante.isSelected() == true)
							textSelec = re.textSelec.replaceFirst(t1.getText(),
									t2.getText());
						v.getEditorBuilder().getEditorAt(numeditor).getEditor()
								.setCaretPosition(pos);
						// Muestra en el log
						logger.info(labels.getString("s583") + " "
								+ t1.getText() + " " + labels.getString("s580")
								+ " " + t2.getText());
						v.getStatusBar().setMessage(
								labels.getString("s583") + " " + t1.getText()
										+ " " + labels.getString("s580") + " "
										+ t2.getText());
						primerReem = false;
						buscar.doClick();
					}
				}
			}
			if ((docActual.isSelected() == true)
					|| (todosDoc.isSelected() == true)) {

				int numeditor = v.getEditorBuilder().getSelectedEditorIndex();

				if (primerReem == true) {
					posOrig = v.getEditorBuilder().getEditorAt(numeditor)
							.getEditor().getCaretPosition();
					buscar.doClick();
				}
				if (v.getEditorBuilder().getEditorAt(numeditor).getEditor()
						.getSelectedText() != null) {
					v.getEditorBuilder().getEditorAt(numeditor).getEditor()
							.replaceSelection(t2.getText());
					// Muestra en el log
					logger.info(labels.getString("s579") + " " + t1.getText()
							+ " " + labels.getString("s580") + " "
							+ t2.getText());

					v.getStatusBar().setMessage(
							labels.getString("s579") + " " + t1.getText() + " "
									+ labels.getString("s580") + " "
									+ t2.getText());
					primerReem = false;
					buscar.doClick();
				}
				if (v.getEditorBuilder().getEditorAt(numeditor).getEditor()
						.getSelectedText() == null)
					v.getEditorBuilder().getEditorAt(numeditor).getEditor()
							.setCaretPosition(posOrig);
			}
		}
	}

	/**
	 * 
	 * 
	 */
	class ReemplazarTodo_Adaptador implements ActionListener {

		@SuppressWarnings("static-access")
		public void actionPerformed(ActionEvent e) {
			ReplaceGUI re = ReplaceGUI.getInstance();
			String n1 = null;
			String r = null;
			String textPre = null;
			String textPos = null;
			int numeditor = v.getEditorBuilder().getSelectedEditorIndex();
			int longitud = v.getEditorBuilder().getEditorAt(numeditor)
					.getEditor().getText().length();
			if (seleccionado.isSelected() == true) {
				re.set_top(false);
				int op = JOptionPane.showConfirmDialog(null,
						labels.getString("s581"));
				re.set_top(true);
				if (op == JOptionPane.OK_OPTION) {
					int p = v.getEditorBuilder().getEditorAt(numeditor)
							.getEditor().getSelectionStart();
					n1 = v.getEditorBuilder().getEditorAt(numeditor)
							.getEditor().getSelectedText();
					int f = p + n1.length();
					String text = v.getEditorBuilder().getEditorAt(numeditor)
							.getEditor().getText();
					textPre = text.substring(0, p);
					textPos = text.substring(f, longitud);
					r = n1.replaceAll(t1.getText(), t2.getText());
					String temp = textPre.concat(r);
					r = null;
					r = temp.concat(textPos);
					v.getEditorBuilder().getEditorAt(numeditor).getEditor()
							.setText(r);

				}
			}

			if (docActual.isSelected() == true) {
				numeditor = v.getEditorBuilder().getSelectedEditorIndex();
				int position = v.getEditorBuilder().getSelectedEditor()
						.getEditor().getCaretPosition();
				n1 = v.getEditorBuilder().getEditorAt(numeditor).getEditor()
						.getText();
				re.set_top(false);
				int op = JOptionPane.showConfirmDialog(null,
						labels.getString("s581"));
				re.set_top(true);
				if (op == JOptionPane.OK_OPTION) {
					r = n1.replaceAll(t1.getText(), t2.getText());
					v.getEditorBuilder().getEditorAt(numeditor).getEditor()
							.setText(r);
					v.getEditorBuilder().getSelectedEditor().getEditor()
							.setCaretPosition(position);
					// Muestra en el log
					v.getOutput().setText(
							labels.getString("s582") + t1.getText()
									+ labels.getString("s580") + t2.getText());
				}
			} else if (todosDoc.isSelected() == true) {
				numeditor = v.getEditorBuilder().getSelectedEditorIndex();
				int position = v.getEditorBuilder().getSelectedEditor()
						.getEditor().getCaretPosition();
				int numedi = v.getEditorBuilder().getNumEditors();
				re.set_top(false);
				int op = JOptionPane.showConfirmDialog(null,
						labels.getString("s581"));
				re.set_top(true);
				if (op == JOptionPane.OK_OPTION) {
					for (int j = 0; j < numedi; j++) {
						n1 = v.getEditorBuilder().getEditorAt(j).getText();
						r = n1.replaceAll(t1.getText(), t2.getText());
						v.getEditorBuilder().setSelectedEditorAt(j);
						v.getEditorBuilder().getEditorAt(j).getEditor()
								.setText(r);
					}

				}
				v.getEditorBuilder().setSelectedEditorAt(numeditor);
				v.getEditorBuilder().getSelectedEditor().getEditor()
						.setCaretPosition(position);
			}
		}
	}

	public void setT1(String c) {
		// TODO Auto-generated method stub
		t1.setText(c);
	}

	public void setDocActual(boolean c) {
		// TODO Auto-generated method stub
		docActual.setSelected(c);
	}

	public void setRadioAdelante(boolean c) {
		// TODO Auto-generated method stub
		radioAdelante.setSelected(c);
	}

	public String getTextSelec() {
		return textSelec;
	}

	public void setSelectedText(String textSelec) {
		this.textSelec = textSelec;
	}

	public static boolean isPrimerReem() {
		return primerReem;
	}

	public static void setFirstReplacement(boolean primerReem) {
		ReplaceGUI.primerReem = primerReem;
	}

	public JRadioButton getRadioAdelante() {
		return radioAdelante;
	}

	public void setRadioAdelante(JRadioButton radioAdelante) {
		this.radioAdelante = radioAdelante;
	}

	public int getPosFinal() {
		return posFinal;
	}

	public void setPosFinal(int posFinal) {
		this.posFinal = posFinal;
	}

	public void set_top(boolean b) {
		this.setAlwaysOnTop(b);
	}
}// class
