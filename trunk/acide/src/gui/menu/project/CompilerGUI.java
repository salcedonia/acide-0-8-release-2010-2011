package gui.menu.project;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;

import es.text.ExtensionFilter;
import es.text.TextFile;
import gui.MainWindow;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import language.Language;
import operations.listeners.AcideWindowListener;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/** 
 *
 */
public class CompilerGUI extends JFrame {

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
	private JFrame _frame;
	/**
	 * 
	 */
	private JTextField _tfNameExec;
	/**
	 * 
	 */
	private JButton _btnExaminePath;
	/**
     * 
     */
	private JTextField _tfArguments;
	/**
     * 
     */
	private JLabel _lblNameExec;
	/**
     * 
     */
	private JLabel _lblArguments;
	/**
     * 
     */
	private JPanel _mainPanel1;
	/**
     * 
     */
	private JPanel _mainPanel2;
	/**
     * 
     */
	private JPanel _buttonPanel;
	/**
     * 
     */
	private JCheckBox _chbCompiler;
	/**
     * 
     */
	private JLabel _lblCheck;
	/**
     * 
     */
	private JLabel _lblSeparator;
	/**
     * 
     */
	private JTextField _tfSeparator;
	/**
     * 
     */
	private JLabel _lblExtension;
	/**
     * 
     */
	private JTextField _tfExtension;
	/**
     * 
     */
	private JButton _btnOk;
	/**
     * 
     */
	private JButton _btnCancel;
	/**
     * 
     */
	private Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private ResourceBundle _labels;

	/**
	 * Constructor of the class.
	 */
	public CompilerGUI() {

		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		_labels = language.getLabels();
		MainWindow mainWindow = MainWindow.getInstance();
		mainWindow.setEnabled(false);
		mainWindow.getProjectConfiguration().setCheckCompiler(false);
		_logger.info(_labels.getString("s646"));
		
		// FRAME
		_frame = new JFrame();
		_frame.setLayout(new GridBagLayout());
		_frame.setTitle(_labels.getString("s647"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setLayout(new GridBagLayout());
		
		// MAIN PANEL
		_mainPanel1 = new JPanel();
		_mainPanel1.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s644")));
		_mainPanel1.setLayout(new GridBagLayout());
		
		// MAIN PANEL 
		_mainPanel2 = new JPanel();
		_mainPanel2.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s645")));
		_mainPanel2.setLayout(new GridBagLayout());
		
		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());
		
		// EXEC
		_lblNameExec = new JLabel(_labels.getString("s606"));
		_tfNameExec = new JTextField();
		_tfNameExec.setToolTipText(_labels.getString("s607"));
		
		// ARGUMENTS
		_lblArguments = new JLabel(_labels.getString("s609"));
		_tfArguments = new JTextField();
		_tfArguments.setToolTipText(_labels.getString("s610"));
		
		// CHECK
		_lblCheck = new JLabel(_labels.getString("s650"));
		
		// SEPARATOR
		_lblSeparator = new JLabel(_labels.getString("s649"));
		
		// EXTENSION
		_lblExtension = new JLabel(_labels.getString("s653"));
		_tfExtension = new JTextField(7);
		_tfExtension.setToolTipText(_labels.getString("s652"));
		_tfExtension.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
			
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getProjectConfiguration().setFileExtension(
						_tfExtension.getText());
			}
		});
		
		// COMPILER
		_chbCompiler = new JCheckBox();
		_chbCompiler.setToolTipText(_labels.getString("s650"));
		_chbCompiler.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				if (_chbCompiler.isSelected()) {
					mainWindow.getProjectConfiguration().setCheckCompiler(true);
					_tfExtension.setText("");
					_tfExtension.setEnabled(false);
					_tfSeparator.setEnabled(true);
				} else {
					mainWindow.getProjectConfiguration().setCheckCompiler(false);
					_tfSeparator.setText("");
					_tfSeparator.setEnabled(false);
					_tfExtension.setEnabled(true);
				}
			}
		});
		
		// SEPARATOR
		_tfSeparator = new JTextField(1);
		_tfSeparator.setToolTipText(_labels.getString("s651"));
		_tfSeparator.setEnabled(false);
		_tfSeparator.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getProjectConfiguration().setSeparatorFile(
						_tfSeparator.getText());
			}
		});
		
		// EXAMINE PATH
		_btnExaminePath = new JButton(_labels.getString("s596"));
		_btnExaminePath.setHorizontalAlignment(JButton.CENTER);
		_btnExaminePath.setToolTipText(_labels.getString("s641"));
		_btnExaminePath.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				TextFile f = new TextFile();
				String[] ExtAcide = new String[] { "exe" };
				f.getFileChooser()
						.addChoosableFileFilter(
								new ExtensionFilter(ExtAcide,
										"Compiler source (*.exe)"));
				String path = f.read();
				_tfNameExec.setText(path);
			}
		});
		
		// OK BUTTON
		_btnOk = new JButton(_labels.getString("s154"));
		_btnOk.setHorizontalAlignment(JButton.CENTER);
		_btnOk.setToolTipText(_labels.getString("s154"));
		_btnOk.addActionListener(new ActionListener() {
			/*
			 * 
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getProjectConfiguration().setCompilerPath(
						_tfNameExec.getText());
				mainWindow.getProjectConfiguration().setCompilerArguments(
						_tfArguments.getText());
				mainWindow.getProjectConfiguration().setFileExtension(
						_tfExtension.getText());
				mainWindow.getProjectConfiguration().setSeparatorFile(
						_tfSeparator.getText());
				mainWindow.setEnabled(true);
				
				if (mainWindow.getProjectGUI() != null)
					mainWindow.getProjectGUI().setB1(true);
				
				_frame.dispose();
			}
		});
		
		// CANCEL BUTTON
		_btnCancel = new JButton(_labels.getString("s162"));
		_btnCancel.setHorizontalAlignment(JButton.CENTER);
		_btnCancel.setToolTipText(_labels.getString("s162"));
		_btnCancel.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				_frame.dispose();
			}
		});
		
		_frame.addKeyListener(new CompilerGUIKeyboardListener());
		
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel1.add(_lblNameExec, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 200;
		_mainPanel1.add(_tfNameExec, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel1.add(_btnExaminePath, constraints);

		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel1.add(_lblArguments, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 150;
		_mainPanel1.add(_tfArguments, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;

		_buttonPanel.add(_btnOk, constraints);
		constraints.gridx = 1;
		_buttonPanel.add(_btnCancel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_frame.add(_mainPanel1, constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel2.add(_lblCheck, constraints);
		constraints.gridx = 1;
		_mainPanel2.add(_chbCompiler, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel2.add(_lblSeparator, constraints);
		constraints.gridx = 1;
		constraints.gridy = 1;
		_mainPanel2.add(_tfSeparator, constraints);
		constraints.gridx = 2;
		constraints.gridy = 1;
		_mainPanel2.add(_lblExtension, constraints);
		constraints.gridx = 3;
		constraints.gridy = 1;
		_mainPanel2.add(_tfExtension, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_frame.add(_mainPanel2, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		
		_frame.add(_buttonPanel, constraints);
		_frame.setResizable(false);
		_frame.pack();
		_frame.setVisible(true);
		_frame.setLocationRelativeTo(null);
		_frame.addWindowListener(new AcideWindowListener());
		
		mainWindow.setEnabled(false);
		mainWindow.closeDefaultProject();
	}

	/**
	 * 
	 */
	class CompilerGUIKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
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
}
