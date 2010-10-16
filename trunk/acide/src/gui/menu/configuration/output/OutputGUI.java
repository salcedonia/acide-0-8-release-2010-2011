package gui.menu.configuration.output;

import es.text.TextFile;
import gui.MainWindow;
import java.awt.Checkbox;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.util.ResourceBundle;
import java.util.StringTokenizer;

import javax.swing.AbstractButton;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;

import language.Language;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

/**
 * 
 */
public class OutputGUI {

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
	private JPanel _mainPanel;
	/**
	 * 
	 */
	private JLabel _lblEchoCommand;
	/**
	 * 
	 */
	private JLabel _lblExec;
	/**
	 * 
	 */
	private final JTextField _tfExec;
	/**
	 * 
	 */
	private JLabel _lblOutputCommand;
	/**
	 * 
	 */
	private final Checkbox _chbEchoCommand;
	/**
	 * 
	 */
	private final JTextField _tfOutputCommand;
	/**
	 * 
	 */
	private final JLabel _lblExecPath;
	/**
	 * 
	 */
	private final JTextField _tfExecPath;
	/**
	 * 
	 */
	private JButton _btnExamine;
	/**
	 * 
	 */
	private final JButton _btnExamine2;
	/**
	 * 
	 */
	private JLabel _lblManualPath;
	/**
	 * 
	 */
	private final Checkbox _chbManualPath;
	/**
	 * 
	 */
	private JButton _btnApply;
	/**
	 *
	 */
	private Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 */
	public OutputGUI() {

		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();

		_logger.info(labels.getString("s331"));
		
		// FRAME 
		_frame = new JFrame();
		_frame.setSize(new Dimension(500, 250));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_frame.setTitle(labels.getString("s334"));

		// PANEL
		_mainPanel = new JPanel();
		_mainPanel.setLayout(new GridBagLayout());
		
		// EXEC PATH
		_lblExecPath = new JLabel(labels.getString("s337"),
				JLabel.LEFT);
		_tfExecPath = new JTextField();
		_lblExecPath.setEnabled(false);
		_tfExecPath.setEnabled(false);
		
		// MANUAL PATH
		_lblManualPath = new JLabel(labels.getString("s350"),
				JLabel.LEFT);
		_chbManualPath = new Checkbox();
		
		// EXEC
		_lblExec = new JLabel(labels.getString("s338"), JLabel.LEFT);
		_tfExec = new JTextField();
		
		// OUTPUT COMMAND
		_lblOutputCommand = new JLabel(labels.getString("s339"), JLabel.LEFT);
		_tfOutputCommand = new JTextField();
		
		// ECHO COMMAND
		_lblEchoCommand = new JLabel(labels.getString("s340"), JLabel.LEFT);
		_chbEchoCommand = new Checkbox();
		try {
			_tfExecPath.setText(PropertiesManager.getProperty("execPath"));
			_tfExec.setText(PropertiesManager.getProperty("exec"));
			_tfOutputCommand.setText(PropertiesManager
					.getProperty("exitCommand"));
			_chbEchoCommand.setState(Boolean.parseBoolean(PropertiesManager
					.getProperty("echoCommand")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// APPLY BUTTON
		_btnApply = new JButton(labels.getString("s335"));
		_btnApply.setVerticalTextPosition(AbstractButton.CENTER);
		_btnApply.setHorizontalTextPosition(AbstractButton.LEADING);
		_btnApply.setMnemonic(KeyEvent.VK_A);
		_btnApply.setToolTipText(labels.getString("s336"));
		
		// EXAMINE BUTTON
		_btnExamine = new JButton(labels.getString("s142"));
		_btnExamine.setToolTipText(labels.getString("s301"));
		
		// EXAMINE2 BUTTON
		_btnExamine2 = new JButton(labels.getString("s142"));
		_btnExamine2.setToolTipText(labels.getString("s301"));
		
		// LISTENERS
		_btnExamine.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				TextFile file = new TextFile();
				String path = file.read();
				_tfExec.setText(path);
			}
		});
		
		_btnExamine2.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				String path = "";
				JFileChooser fc = new JFileChooser();
				fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
				int option = fc.showOpenDialog(null);

				if (option == JFileChooser.APPROVE_OPTION)
					path = fc.getSelectedFile().getAbsolutePath();

				_tfExecPath.setText(path);
			}
		});
		_btnExamine2.setEnabled(false);
		
		_chbManualPath.addItemListener(new ItemListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
			 */
			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					_lblExecPath.setEnabled(true);
					_tfExecPath.setEnabled(true);
					_btnExamine2.setEnabled(true);
				} else {
					_lblExecPath.setEnabled(false);
					_tfExecPath.setEnabled(false);
					_btnExamine2.setEnabled(false);
				}
			}
		});
		
		_btnApply.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				try {
					MainWindow.getInstance().getOutput().executeExitCommand();
					if (_tfExecPath.isEnabled()) {
						PropertiesManager.setProperty("execPath",
								_tfExecPath.getText());
						MainWindow.getInstance().getProjectConfiguration()
								.setShellDir(_tfExecPath.getText());
					} else {
						String calculatedPath = "";
						String saux = _tfExec.getText();
						StringTokenizer st = new StringTokenizer(saux, "\\");
						
						int limit = st.countTokens();
						for (int i = 0; i < limit - 1; i++) {
							calculatedPath = calculatedPath + st.nextToken()
									+ "\\";
						}
						
						PropertiesManager
								.setProperty("execPath", calculatedPath);
						MainWindow.getInstance().getProjectConfiguration()
								.setShellDir(calculatedPath);
					}
					PropertiesManager.setProperty("exec", _tfExec.getText());
					PropertiesManager.setProperty("echoCommand",
							Boolean.toString(_chbEchoCommand.getState()));
					PropertiesManager.setProperty("exitCommand",
							_tfOutputCommand.getText());
					MainWindow.getInstance().getOutput().resetOutput();
					MainWindow.getInstance().getProjectConfiguration()
							.setShellPath(_tfExec.getText());
					MainWindow
							.getInstance()
							.getProjectConfiguration()
							.setEchoCommand(
									Boolean.toString(_chbEchoCommand
											.getState()));
					MainWindow.getInstance().getProjectConfiguration()
							.setExitCommand(_tfOutputCommand.getText());

					String prj = null;
					try {
						prj = PropertiesManager
								.getProperty("defaultAcideProject");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if (!(prj.equals("./configuration/default.acidePrj") && MainWindow
							.getInstance().getProjectConfiguration().getName()
							.equals(""))) {
						MainWindow.getInstance().getProjectConfiguration()
								.setModified(true);
					}

					if (MainWindow.getInstance().getProjectGUI() != null)
						MainWindow.getInstance().getProjectGUI().setB2(true);

				} catch (Exception e1) {
					e1.printStackTrace();
				}
				_frame.dispose();
			}
		});

		ActionListener l = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_frame.dispose();
			}
		};

		_btnApply.registerKeyboardAction(l, "EscapeKey", KeyStroke.getKeyStroke(
				java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// SET THE COMPONENTS WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.HORIZONTAL;
		
		// EXEC
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_mainPanel.add(_lblExec, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 100;
		_mainPanel.add(_tfExec, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_btnExamine, constraints);

		constraints.ipadx = 0;
		constraints.insets = new Insets(10, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_lblManualPath, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 100;
		_mainPanel.add(_chbManualPath, constraints);

		constraints.gridx = 0;
		constraints.gridy = 2;
		_mainPanel.add(_lblExecPath, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 100;
		_mainPanel.add(_tfExecPath, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_btnExamine2, constraints);

		constraints.gridx = 0;
		constraints.gridy = 3;
		_mainPanel.add(_lblOutputCommand, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 100;
		_mainPanel.add(_tfOutputCommand, constraints);

		constraints.gridx = 0;
		constraints.gridy = 4;
		_mainPanel.add(_lblEchoCommand, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 100;
		_mainPanel.add(_chbEchoCommand, constraints);

		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 5;
		_mainPanel.add(_btnApply, constraints);

		_frame.add(_mainPanel);
		_frame.setVisible(true);
		_frame.setResizable(false);
		_frame.pack();
		_frame.setLocationRelativeTo(null);
	}
}
