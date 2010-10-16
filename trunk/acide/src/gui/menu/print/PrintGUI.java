package gui.menu.print;

import gui.MainWindow;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import language.Language;
import operations.listeners.AcideWindowListener;
import properties.PropertiesManager;

/**
 * 
 */
public class PrintGUI {

	/**
	 * 
	 */
	private static final String ICON = "./resources/images/icon.png";
	/**
	 * 
	 */
	private JFrame _frame;
	/**
	 * 
	 */
	private JButton _btnPrint;
	/**
	 * 
	 */
	private JButton _btnCancel;
	/**
	 * 
	 */
	private JButton _btnConfigurePage;
	/**
	 * 
	 */
	private JPanel _panelButton;
	/**
	 * 
	 */
	private JPanel _panelGeneral;
	/**
	 * 
	 */
	private JLabel _lblNumPag;
	/**
	 * 
	 */
	private JCheckBox _checkBoxNumPage;
	/**
	 * 
	 */
	private JLabel _lblDate;
	/**
	 * 
	 */
	private JCheckBox _checkBoxDate;
	/**
	 * 
	 */
	private PrinterManager _printerManager;
	/**
	 * 
	 */
	private ResourceBundle _labels;

	/**
	 * 
	 */
	private static PrintGUI _instance;
	
	/**
	 * 
	 * @return
	 */
	public static PrintGUI getInstance() {
		if (_instance == null)
			_instance = new PrintGUI();
		return _instance;
	}

	/**
	 * Constructor of the class.
	 */
	@SuppressWarnings("static-access")
	public PrintGUI() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		MainWindow mainWindow = MainWindow.getInstance();
		_printerManager = new PrinterManager(mainWindow.getEditorBuilder().getSelectedEditor()
				.getEditor(), false, false);
		
		_checkBoxNumPage = new JCheckBox();
		_checkBoxNumPage.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (_checkBoxNumPage.isSelected())
					_printerManager.setPage(true);
				else
					_printerManager.setPage(false);
			}

		});

		_labels = language.getLabels();
		_frame = new JFrame();
		_frame.setTitle(_labels.getString("s964"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new GridBagLayout());
		_panelButton = new JPanel();
		_panelButton.setLayout(new GridBagLayout());
		_panelGeneral = new JPanel();
		_panelGeneral.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s965")));
		_panelGeneral.setLayout(new GridBagLayout());
		_lblNumPag = new JLabel(_labels.getString("s962"));
		_lblDate = new JLabel(_labels.getString("s963"));
		_checkBoxDate = new JCheckBox();
		_checkBoxDate.setEnabled(false);
		_checkBoxNumPage.setEnabled(false);
		_checkBoxDate.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (_checkBoxDate.isSelected())
					_printerManager.setDate(true);
				else
					_printerManager.setDate(false);
			}
		});

		_btnPrint = new JButton(_labels.getString("s624"));
		_btnPrint.setHorizontalAlignment(JButton.CENTER);
		_btnPrint.setToolTipText(_labels.getString("s624"));
		_btnPrint.setEnabled(false);
		_btnPrint.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				_printerManager.print();
				_frame.dispose();
				mainWindow.setEnabled(true);
			}
		});
		_btnCancel = new JButton(_labels.getString("s919"));
		_btnCancel.setHorizontalAlignment(JButton.CENTER);
		_btnCancel.setToolTipText(_labels.getString("s919"));
		_btnCancel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				_frame.dispose();
			}
		});

		_btnConfigurePage = new JButton(_labels.getString("s961"));
		_btnConfigurePage.setHorizontalAlignment(JButton.CENTER);
		_btnConfigurePage.setToolTipText(_labels.getString("s961"));
		_btnConfigurePage.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				
				_printerManager.configurePage();
				
				if (_printerManager.getFormat() != null) {
					_btnPrint.setEnabled(true);
					_checkBoxDate.setEnabled(true);
					_checkBoxNumPage.setEnabled(true);
				}
			}
		});
		
		GridBagConstraints constraints = new GridBagConstraints();
		
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(10, 10, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_panelGeneral.add(_btnConfigurePage, constraints);
		constraints.gridy = 1;
		_panelGeneral.add(_lblNumPag, constraints);
		constraints.gridx = 1;
		_panelGeneral.add(_checkBoxNumPage, constraints);
		constraints.gridy = 2;
		constraints.gridx = 0;
		_panelGeneral.add(_lblDate, constraints);
		constraints.gridx = 1;
		_panelGeneral.add(_checkBoxDate, constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_frame.add(_panelGeneral, constraints);

		_panelButton.add(_btnPrint, constraints);
		constraints.gridx = 1;
		_panelButton.add(_btnCancel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_frame.add(_panelButton, constraints);
		_frame.setLocationRelativeTo(null);
		_frame.pack();
		_frame.setVisible(true);
		_frame.setResizable(false);
		_btnConfigurePage.addKeyListener(new PrintGUIKeyboardListener());
		_btnPrint.addKeyListener(new PrintGUIKeyboardListener());
		_btnCancel.addKeyListener(new PrintGUIKeyboardListener());
		mainWindow.setEnabled(false);
		AcideWindowListener window = new AcideWindowListener();
		_frame.addWindowListener(window);
	}

	/**
	 * 
	 */
	class PrintGUIKeyboardListener extends KeyAdapter {
		public void keyPressed(KeyEvent evt) {
			if (evt.getKeyCode() == KeyEvent.VK_ESCAPE) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				_frame.dispose();
				mainWindow.setAlwaysOnTop(true);
				mainWindow.setAlwaysOnTop(false);
			}
		}
	}

	/**
	 * 
	 * @return
	 */
	public JButton getBtnConfigurePage() {
		return _btnConfigurePage;
	}

	/**
	 * 
	 * @param configPage
	 */
	public void setBtnConfigurePage(JButton configPage) {
		_btnConfigurePage = configPage;
	}

	/**
	 * 
	 * @return
	 */
	public JButton getBtnPrint() {
		return _btnPrint;
	}

	/**
	 * 
	 * @param print
	 */
	public void setConfigurePrint(JButton print) {
		_btnPrint = print;
	}

	/**
	 * 
	 * @return
	 */
	public JFrame getFrame() {
		return _frame;
	}

	/**
	 * 
	 * @param frame
	 */
	public void setFrame(JFrame frame) {
		_frame = frame;
	}
}
