package gui.menu.edit;

import gui.MainWindow;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import language.Language;

import org.apache.log4j.Logger;

import operations.factory.OperationsFactory;
import operations.listeners.AcideKeyboardListener;
import operations.log.Log;
import properties.PropertiesManager;

/**
 * 
 */
public class SearchGUI extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private static SearchGUI _instance;
	/**
	 * 
	 */
	private JButton _btnSearch;
	/**
	 * 
	 */
	private JButton _btnCancel;

	private JTextField t1;

	private JCheckBox opcion1;

	private JCheckBox opcion2;

	private JCheckBox opcion3;
	/**
	 * 
	 */
	private JRadioButton _rdBtnForward;
	/**
	 * 
	 */
	private JRadioButton _rdBtnBackward;
	/**
	 * 
	 */
	private JRadioButton _rdBtnAll;
	/**
	 * 
	 */
	private JRadioButton _rbBtnCurrentDocument;
	/**
	 * 
	 */
	private JRadioButton _rdBtnAllDocuments;
	/**
	 * 
	 */
	private JRadioButton _rdBtnSelected;
	/**
	 * 
	 */
	private int _result;
	/**
	 * 
	 */
	private String _selectedText;
	/**
	 * 
	 */
	private int posIni;
	/**
	 * 
	 */
	private int posFinal;
	/**
	 * 
	 */
	private static int i;
	/**
	 * 
	 */
	private static boolean _isEnd = false;
	/**
	 * 
	 */
	private int _currentPosition;
	/**
	 * 
	 */
	private int actualDoc;
	/**
	 * 
	 */
	private int cont;
	/**
	 * 
	 */
	private static boolean _isCycle;
	/**
	 * 
	 */
	private static boolean _isFirst;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private MainWindow _mainWindow = MainWindow.getInstance();
	/**
	 * 
	 */
	private OperationsFactory _operationsFactory = OperationsFactory
			.getInstance();
	/**
	 * 
	 */
	private Search _search = _operationsFactory.buildSearch();
	/**
	 * 
	 */
	private ResourceBundle _labels;

	/**
	 * Constructor of the class.
	 */
	public SearchGUI() {

		Language language = Language.getInstance();

		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		_labels = language.getLabels();
		_isCycle = false;
		_currentPosition = -2;
		actualDoc = -1;
		posIni = -1;
		_selectedText = null;
		_result = -1;
		_isFirst = true;
		
		JPanel panel = new JPanel();
		GridBagLayout grid = new GridBagLayout();
		panel.setLayout(grid);
		GridBagConstraints constraints = new GridBagConstraints();

		setTitle(_labels.getString("s556"));
		setIconImage(new ImageIcon(ICON).getImage());
		
		JPanel panelDireccion = new JPanel();
		
		panelDireccion.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s567")));
		panelDireccion.setLayout(new GridLayout(0, 1));

		JPanel panelOpciones = new JPanel();
		panelOpciones.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s559")));
		panelOpciones.setLayout(new GridLayout(0, 1));

		JPanel panelAlcance = new JPanel();
		panelAlcance.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s563")));
		panelAlcance.setLayout(new GridLayout(0, 1));

		_btnSearch = new JButton();
		_btnSearch.setText(_labels.getString("s556"));
		_btnSearch.setMnemonic(java.awt.event.KeyEvent.VK_F3);
		_btnCancel = new JButton();
		_btnCancel.setText(_labels.getString("s42"));
		JLabel etiq1 = new JLabel(_labels.getString("s557"), JLabel.CENTER);
		t1 = new JTextField();
		t1.setText("");
		opcion1 = new JCheckBox(_labels.getString("s560"), false);
		opcion2 = new JCheckBox(_labels.getString("s561"), false);
		opcion3 = new JCheckBox(_labels.getString("s562"), false);

		// Panel options
		panelOpciones.add(opcion1);
		panelOpciones.add(opcion2);
		panelOpciones.add(opcion3);

		// Panel direction
		ButtonGroup buttonGroup = new ButtonGroup();
		_rdBtnForward = new JRadioButton(_labels.getString("s568"), true);
		_rdBtnBackward = new JRadioButton(_labels.getString("s569"), false);
		_rdBtnAll = new JRadioButton(_labels.getString("s570"), false);
		buttonGroup.add(_rdBtnForward);
		buttonGroup.add(_rdBtnBackward);
		buttonGroup.add(_rdBtnAll);
		panelDireccion.add(_rdBtnForward);
		panelDireccion.add(_rdBtnBackward);
		panelDireccion.add(_rdBtnAll);

		// Panel Scope
		buttonGroup = new ButtonGroup();
		_rbBtnCurrentDocument = new JRadioButton(_labels.getString("s565"),
				true);
		_rdBtnAllDocuments = new JRadioButton(_labels.getString("s566"), false);
		_rdBtnSelected = new JRadioButton(_labels.getString("s564"), false);
		buttonGroup.add(_rdBtnSelected);
		buttonGroup.add(_rbBtnCurrentDocument);
		buttonGroup.add(_rdBtnAllDocuments);
		panelAlcance.add(_rdBtnSelected);
		panelAlcance.add(_rbBtnCurrentDocument);
		panelAlcance.add(_rdBtnAllDocuments);

		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.gridwidth = 2;
		constraints.insets = new Insets(5, 5, 5, 5);
		panel.add(etiq1, constraints);
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.ipadx = 300;
		panel.add(t1, constraints);
		constraints.fill = GridBagConstraints.CENTER;
		constraints.gridy = 2;
		constraints.gridx = 0;
		panel.add(panelOpciones, constraints);
		constraints.fill = GridBagConstraints.CENTER;
		constraints.gridy = 3;
		constraints.gridx = 0;
		constraints.ipadx = 100;
		constraints.gridwidth = 1;
		panel.add(panelAlcance, constraints);
		constraints.gridy = 3;
		constraints.gridx = 1;
		panel.add(panelDireccion, constraints);
		constraints.fill = GridBagConstraints.CENTER;
		constraints.ipadx = 60;
		constraints.weightx = 0.5;
		constraints.gridwidth = 2;
		constraints.gridy = 4;
		constraints.gridx = 0;
		panel.add(_btnSearch, constraints);
		constraints.fill = GridBagConstraints.CENTER;
		constraints.ipadx = 60;
		constraints.weightx = 0.5;
		constraints.gridwidth = 2;
		constraints.gridy = 4;
		constraints.gridx = 1;
		panel.add(_btnCancel, constraints);
		
		add(panel);
		setResizable(false);
		setAlwaysOnTop(true);
		pack();
		setLocationRelativeTo(null);

		_btnSearch.addActionListener(new SearchButtonListener());
		AcideKeyboardListener key = new AcideKeyboardListener();
		t1.addKeyListener(key);
		opcion1.addKeyListener(key);
		opcion2.addKeyListener(key);
		opcion3.addKeyListener(key);
		_rdBtnForward.addKeyListener(key);
		_rdBtnBackward.addKeyListener(key);
		_rdBtnAll.addKeyListener(key);
		_rdBtnSelected.addKeyListener(key);
		_rdBtnAllDocuments.addKeyListener(key);
		_rbBtnCurrentDocument.addKeyListener(key);

		_btnCancel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				_instance.dispose();
			}
		});
	}

	/**
	 * 
	 */
	class SearchButtonListener implements ActionListener {

		@SuppressWarnings("static-access")
		public void actionPerformed(ActionEvent arg0) {
			
			SearchGUI s = SearchGUI.getInstance();
			
			int direccion = 0;
			if (_rdBtnForward.isSelected() == true) {
				direccion = 0;
			}
			if (_rdBtnBackward.isSelected() == true) {
				direccion = 1;
			}
			if (_rdBtnAll.isSelected() == true)
				direccion = 2;
			if (opcion3.isSelected() == true) {
				opcion2.setSelected(false);
			}
			if (t1.getText().equals("")) {
				s.setOnTop(false);
				JOptionPane.showMessageDialog(null, _labels.getString("s585"));
				s.setOnTop(true);
				_mainWindow.getStatusBar()
						.setMessage(_labels.getString("s585"));
			}
			int numeditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();

			if (_rbBtnCurrentDocument.isSelected() == true) {
				i = -1;
				cont = 0;
				_result = -1;
				_selectedText = null;
				numeditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
				_result = _mainWindow.getEditorBuilder().getEditorAt(numeditor)
						.getEditor().getCaretPosition();
				if (direccion == 1) {
					_result = _mainWindow.getEditorBuilder()
							.getEditorAt(numeditor).getEditor()
							.getSelectionStart();
				}
				numeditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();

				_result = _search.search(_result, t1.getText(), _mainWindow
						.getEditorBuilder().getEditorAt(numeditor).getText(),
						opcion1.isSelected(), opcion2.isSelected(),
						opcion3.isSelected(), direccion);

				if (_result != -1) {

					_mainWindow
							.getEditorBuilder()
							.getEditorAt(
									_mainWindow.getEditorBuilder()
											.getSelectedEditorIndex())
							.selectText(_result, t1.getText().length());

					_logger.info(_labels.getString("s583") + " " + t1.getText()
							+ " " + _labels.getString("s574"));

					_mainWindow.getStatusBar().setMessage(
							_labels.getString("s583") + " " + t1.getText()
									+ " " + _labels.getString("s574"));
					if (opcion2.isSelected() == true) {

						// Muestra la busqueda en el editor de texto
						_mainWindow
								.getEditorBuilder()
								.getEditorAt(
										_mainWindow.getEditorBuilder()
												.getSelectedEditorIndex())
								.selectText(_result,
										_search.getRegularExpresion().length());
						// Muestra en el log
						_logger.info(_labels.getString("s577") + " "
								+ _search.getRegularExpresion() + " "
								+ _labels.getString("s574"));
						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s577") + " "
										+ _search.getRegularExpresion() + " "
										+ _labels.getString("s574"));
					}

				}

				else {
					_logger.info(_labels.getString("s573"));
					s.setOnTop(false);
					JOptionPane.showMessageDialog(null,
							_labels.getString("s573"));
					s.setOnTop(true);
					_mainWindow.getStatusBar().setMessage(
							_labels.getString("s573"));
				}
			}

			// TextoSeleccionado
			if (_rdBtnSelected.isSelected() == true) {
				cont = 0;
				i = -1;
				numeditor = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
				if (_selectedText == null) {
					_selectedText = _mainWindow.getEditorBuilder()
							.getEditorAt(numeditor).getEditor()
							.getSelectedText();
					posIni = _mainWindow.getEditorBuilder()
							.getEditorAt(numeditor).getEditor()
							.getSelectionStart();
					posFinal = _mainWindow.getEditorBuilder()
							.getEditorAt(numeditor).getEditor()
							.getSelectionEnd();
					_result = 0;
					if (direccion == 1) {
						_result = posFinal;
					}
					if ((opcion2.isSelected()) && (direccion != 1))
						_result = posIni;
				} else {
					_result = _mainWindow.getEditorBuilder()
							.getEditorAt(numeditor).getEditor()
							.getCaretPosition()
							- posIni;

					if (direccion == 1) {
						_result = _mainWindow.getEditorBuilder()
								.getEditorAt(numeditor).getEditor()
								.getSelectionStart()
								- posIni;
					}
				}

				if (_selectedText == null) {
					s.setOnTop(false);
					JOptionPane.showMessageDialog(null,
							_labels.getString("s616"));
					s.setOnTop(true);
					_mainWindow.getStatusBar().setMessage(
							_labels.getString("s616"));
				} else {
					_result = _search.search(_result, t1.getText(),
							_selectedText, opcion1.isSelected(),
							opcion2.isSelected(), opcion3.isSelected(),
							direccion);
					if (_result != -1) {
						_mainWindow
								.getEditorBuilder()
								.getEditorAt(
										_mainWindow.getEditorBuilder()
												.getSelectedEditorIndex())
								.selectText(_result + posIni,
										t1.getText().length());

						_logger.info(_labels.getString("s583") + " "
								+ t1.getText() + " "
								+ _labels.getString("s574"));

						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s583") + " " + t1.getText()
										+ " " + _labels.getString("s574"));

						if (opcion2.isSelected() == true) {

							// Muestra la busqueda en el editor de texto
							_mainWindow
									.getEditorBuilder()
									.getEditorAt(
											_mainWindow.getEditorBuilder()
													.getSelectedEditorIndex())
									.selectText(
											_result + posIni,
											_search.getRegularExpresion()
													.length());
							// Muestra en el log
							_logger.info(_labels.getString("s329") + " "
									+ _search.getRegularExpresion() + " "
									+ _labels.getString("s574"));
							_mainWindow.getStatusBar().setMessage(
									_labels.getString("s329") + " "
											+ t1.getText() + " "
											+ _labels.getString("s574"));
						}

					}

					else {
						_selectedText = null;
						_logger.info(_labels.getString("s573"));
						s.setOnTop(false);
						JOptionPane.showMessageDialog(null,
								_labels.getString("s573"));
						s.setOnTop(true);
						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s573"));
						s.setOnTop(false);
						int op = JOptionPane.showConfirmDialog(null,
								_labels.getString("s575"));
						s.setOnTop(true);
						if (op == JOptionPane.OK_OPTION) {
							_rbBtnCurrentDocument.setSelected(true);
							if (direccion != 1)
								_result = posFinal;
							else
								_result = posIni;
							_btnSearch.doClick();

						}
					}
				}
			}
			
			if (_rdBtnAllDocuments.isSelected() == true) {
				_selectedText = null;
				numeditor = _mainWindow.getEditorBuilder().getNumEditors();
				if ((_isCycle == false) && (_currentPosition == -2)) {
					i = _mainWindow.getEditorBuilder().getSelectedEditorIndex();
					actualDoc = i;
					_currentPosition = _mainWindow.getEditorBuilder()
							.getEditorAt(i).getEditor().getCaretPosition();
				}

				if (_isEnd == false) {
					if (direccion == 0)
						_result = _mainWindow.getEditorBuilder().getEditorAt(i)
								.getEditor().getCaretPosition();
					if (direccion == 1)
						_result = _mainWindow.getEditorBuilder().getEditorAt(i)
								.getEditor().getSelectionStart();
					if (direccion == 2) {
						_result = _mainWindow.getEditorBuilder().getEditorAt(i)
								.getEditor().getCaretPosition();
					}
					int direc = direccion;
					if (direccion == 2)
						direc = 0;
					_result = _search.search(_result, t1.getText(), _mainWindow
							.getEditorBuilder().getEditorAt(i).getText(),
							opcion1.isSelected(), opcion2.isSelected(),
							opcion3.isSelected(), direc);
					if ((_isCycle == true) && (i == actualDoc)
							&& (_result >= _currentPosition))
						_isEnd = true;
					else if ((_isCycle == true) && (i == actualDoc)
							&& (_result == -1))
						_isEnd = true;
					if (_result != -1) {
						cont++;
						if (opcion2.isSelected() == false) {

							_mainWindow.getEditorBuilder().getEditorAt(i)
									.selectText(_result, t1.getText().length());

							_logger.info(_labels.getString("s583") + " "
									+ t1.getText() + " "
									+ _labels.getString("s574"));
							_mainWindow.getStatusBar().setMessage(
									_labels.getString("s583") + " "
											+ t1.getText() + " "
											+ _labels.getString("s574"));
						} else {
							_mainWindow
									.getEditorBuilder()
									.getEditorAt(i)
									.selectText(
											_result,
											_search.getRegularExpresion()
													.length());
							_logger.info(_labels.getString("s577") + " "
									+ _search.getRegularExpresion()
									+ _labels.getString("s577"));
							_mainWindow.getStatusBar().setMessage(
									_labels.getString("s577") + " "
											+ _search.getRegularExpresion()
											+ _labels.getString("s577"));
						}
					} else {
						_logger.info(_labels.getString("s573"));
						_mainWindow.getEditorBuilder().getEditorAt(i)
								.getEditor().setCaretPosition(0);
						if (_rdBtnForward.isSelected() == true)
							i++;
						else if (_rdBtnBackward.isSelected() == true)
							i--;
						else
							i++;
						if (direccion == 0) {
							if (i >= numeditor) {
								_isEnd = true;
							} else {
								_mainWindow.getEditorBuilder()
										.setSelectedEditorAt(i);
								_mainWindow.getEditorBuilder().getEditorAt(i)
										.getEditor().setCaretPosition(0);
								_btnSearch.doClick();
							}
						}
						if (direccion == 1) {
							if (i < 0) {
								_isEnd = true;
							} else {
								_mainWindow.getEditorBuilder()
										.setSelectedEditorAt(i);
								_mainWindow
										.getEditorBuilder()
										.getEditorAt(i)
										.getEditor()
										.setCaretPosition(
												_mainWindow.getEditorBuilder()
														.getEditorAt(i)
														.getEditor().getText()
														.length() - 1);
								_btnSearch.doClick();
							}
						}
						if (direccion == 2) {
							if (i >= numeditor) {
								i = 0;
								_isCycle = true;
								_mainWindow.getEditorBuilder()
										.setSelectedEditorAt(i);
								_mainWindow.getEditorBuilder().getEditorAt(i)
										.getEditor().setCaretPosition(0);
								_btnSearch.doClick();

							} else {
								_mainWindow.getEditorBuilder()
										.setSelectedEditorAt(i);
								_mainWindow.getEditorBuilder().getEditorAt(i)
										.getEditor().setCaretPosition(0);
								_btnSearch.doClick();
							}
						}
					}
				}

				if ((_isEnd == true) && (_isFirst == true)) {
					if (cont == 0) {
						s.setOnTop(false);
						JOptionPane.showMessageDialog(null,
								_labels.getString("s576"));
						s.setOnTop(true);
						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s576"));
					} else {
						s.setOnTop(false);
						JOptionPane.showMessageDialog(null,
								_labels.getString("s586"));
						s.setOnTop(true);
						_mainWindow.getStatusBar().setMessage(
								_labels.getString("s586"));
					}
					_isFirst = false;
				}
			}
		}
	}

	/**
	 * 
	 * @return
	 */
	public static SearchGUI getInstance() {
		if (_instance == null)
			_instance = new SearchGUI();
		return _instance;
	}

	/**
	 * 
	 */
	public void inicialize() {
		_instance = null;
	}

	/**
	 * 
	 * @return
	 */
	public JButton getBtnSearch() {
		return _btnSearch;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isCycle() {
		return _isCycle;
	}

	/**
	 * 
	 * @param ci
	 */
	public void setCycle(boolean ci) {
		_isCycle = ci;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isEnd() {
		return _isEnd;
	}

	/**
	 * 
	 * @param f
	 */
	public void setEnd(boolean f) {
		_isEnd = f;
	}

	/**
	 * 
	 * @return
	 */
	public int getCurrentPosition() {
		return _currentPosition;
	}

	/**
	 * 
	 * @param currentPosition
	 */
	public void setCurrentPosition(int currentPosition) {
		_currentPosition = currentPosition;
	}

	/**
	 * 
	 * @return
	 */
	public Search getSearch() {
		return _search;
	}

	/**
	 * 
	 * @param search
	 */
	public void setSearch(Search search) {
		_search = search;
	}

	/**
	 * 
	 * @return
	 */
	public JRadioButton getRdBtnCurrentDocument() {
		return _rbBtnCurrentDocument;
	}

	/**
	 * 
	 * @param d
	 */
	public void setRdBtnCurrentDocument(boolean d) {
		_rbBtnCurrentDocument.setSelected(d);
	}

	/**
	 * 
	 * @return
	 */
	public JRadioButton getRdBtnAll() {
		return _rdBtnAll;
	}

	/**
	 * 
	 * @return
	 */
	public JRadioButton getRdBtnSelected() {
		return _rdBtnSelected;
	}

	/**
	 * 
	 * @param b
	 */
	public void setT1(String b) {
		t1.setText(b);
	}

	/**
	 * 
	 * @param b
	 */
	public void setRdBtnForward(boolean b) {
		_rdBtnForward.setSelected(b);
	}

	/**
	 * 
	 * @param isFirst
	 */
	public static void setFirst(boolean isFirst) {
		_isFirst = isFirst;
	}

	/**
	 * 
	 * @return
	 */
	public String getSelectedText() {
		return _selectedText;
	}

	/**
	 * 
	 * @param selectedText
	 */
	public void setSelectedText(String selectedText) {
		_selectedText = selectedText;
	}

	/**
	 * 
	 * @param b
	 */
	public void setOnTop(boolean b) {
		setAlwaysOnTop(b);
	}
}

