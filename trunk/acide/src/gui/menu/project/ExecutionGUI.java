package gui.menu.project;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.ResourceBundle;

import es.text.ExtensionFilter;
import es.text.TextFile;
import gui.MainWindow;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import language.Language;

import operations.listeners.AcideWindowListener;
import properties.PropertiesManager;

/**
 *
 */
public class ExecutionGUI extends JFrame {

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
	private JPanel _mainPanel;
	/**
	 * 
	 */
	private JLabel _lblExecution;
	/**
	 * 
	 */
	private JButton _btnExaminePath;
	/**
	 * 
	 */
	private JLabel _lblArguments;
	/**
	 * 
	 */
	private JTextField _tfArguments;
	/**
	 * 
	 */
	private JButton _btnRun;
	/**
	 * 
	 */
	private JButton _btnCancel;
	/**
	 * 
	 */
	private JPanel _buttonPanel;
	/**
	 * 
	 */
	private ResourceBundle _labels;
	/**
	 * 
	 */
	private JTextField _tfExecution;

	/**
	 * Constructor of the class.
	 */
	public ExecutionGUI() {

		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		_labels = language.getLabels();
		_frame = new JFrame(_labels.getString("s639"));
		_frame.setLayout(new GridBagLayout());
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		_mainPanel = new JPanel();
		_mainPanel.setBorder(BorderFactory.createTitledBorder(_labels
				.getString("s640")));
		_mainPanel.setLayout(new GridBagLayout());
		_lblExecution = new JLabel(_labels.getString("s606"));
		_tfExecution = new JTextField();
		_tfExecution.setToolTipText(_labels.getString("s638"));
		_btnExaminePath = new JButton(_labels.getString("s596"));
		_btnExaminePath.setHorizontalAlignment(JButton.CENTER);
		_btnExaminePath.setToolTipText(_labels.getString("s641"));
		_btnExaminePath.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				TextFile f = new TextFile();
				String[] ExtAcide = new String[] { "exe" };
				f.getFileChooser().addChoosableFileFilter(
						new ExtensionFilter(ExtAcide,
								"Executable source (*.exe)"));
				String path = f.read();
				_tfExecution.setText(path);
			}
		});
		_lblArguments = new JLabel(_labels.getString("s609"));
		_tfArguments = new JTextField();
		_tfArguments.setToolTipText(_labels.getString("s610"));
		
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());
		
		_btnRun = new JButton(_labels.getString("s154"));
		_btnRun.setHorizontalAlignment(JButton.CENTER);
		_btnRun.setToolTipText(_labels.getString("s154"));
		_btnRun.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				try {
					
					String execution = _tfExecution.getText();
					String arguments = _tfArguments.getText();

					if (MainWindow.getInstance().getEditorBuilder()
							.getNumEditors() > 0) {
						execution = execution.replace("$activeFile$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getPath());
						execution = execution.replace("$activeFilePath$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFilePath());
						execution = execution.replace("$activeFileExt$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileExtension());
						execution = execution.replace("$activeFileName$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileName());

						arguments = arguments.replace("$activeFile$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getPath());
						arguments = arguments.replace("$activeFilePath$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFilePath());
						arguments = arguments.replace("$activeFileExt$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileExtension());
						arguments = arguments.replace("$activeFileName$", MainWindow
								.getInstance().getEditorBuilder()
								.getSelectedEditor().getFileName());
					}

					String prj = null;
					try {
						prj = PropertiesManager
								.getProperty("defaultAcideProject");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if ((prj.equals("./configuration/default.acidePrj") && mainWindow
							.getProjectConfiguration().getName().equals(""))) {
						// No project
						if (MainWindow.getInstance().getEditorBuilder()
								.getMainEditor() != null) {
							arguments = arguments.replace("$mainFile$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getPath());
							execution = execution.replace("$mainFile$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getPath());
							arguments = arguments.replace("$mainFilePath$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFilePath());
							execution = execution.replace("$mainFilePath$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFilePath());
							arguments = arguments.replace("$mainFileExt$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileExtension());
							execution = execution.replace("$mainFileExt$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileExtension());
							arguments = arguments.replace("$mainFileName$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileName());
							execution = execution.replace("$mainFileName$", MainWindow
									.getInstance().getEditorBuilder()
									.getMainEditor().getFileName());
						} else {
							// No hay main file
						}
					} else {// Project
						int j = -1;
						for (int i = 0; i < mainWindow.getProjectConfiguration()
								.getNumFilesFromList(); i++) {
							if (mainWindow.getProjectConfiguration().getFile(i)
									.isMainFile())
								j = i;
						}
						if (j != -1) {
							arguments = arguments.replace("$mainFile$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getPath());
							execution = execution.replace("$mainFile$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getPath());
							arguments = arguments.replace("$mainFilePath$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFilePath());
							execution = execution.replace("$mainFilePath$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFilePath());
							arguments = arguments.replace("$mainFileExt$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFileExt());
							execution = execution.replace("$mainFileExt$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFileExt());
							arguments = arguments.replace("$mainFileName$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFileName());
							execution = execution.replace("$mainFileName$", mainWindow
									.getProjectConfiguration().getFile(j)
									.getFileName());
						} else {
							// No main file
						}
					}

					// Runtime.getRuntime().exec(executionField.getText()+" "+argumentsField.getText());
					System.out.println(execution + " " + arguments);
					Runtime.getRuntime().exec(execution + " " + arguments);

					// Runtime.getRuntime().exec("\"e:\\MiKTeX 2.7\\miktex\\bin\\latex.exe\" e:\\hlocal\\mig\\mig2.tex"+"\n");
					// Runtime.getRuntime().exec("cmd");
				} catch (IOException ex) {
					JOptionPane.showMessageDialog(null, ex.getMessage());
				}
				mainWindow.setEnabled(true);
				_frame.dispose();
			}
		});

		_btnCancel = new JButton(_labels.getString("s162"));
		_btnCancel.setHorizontalAlignment(JButton.CENTER);
		_btnCancel.setToolTipText(_labels.getString("s162"));
		_btnCancel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.setEnabled(true);
				_frame.dispose();
			}
		});
		_btnRun.addKeyListener(new ExecutionGUIKeyboardListener());
		_btnCancel.addKeyListener(new ExecutionGUIKeyboardListener());
		_tfExecution.addKeyListener(new ExecutionGUIKeyboardListener());
		_tfArguments.addKeyListener(new ExecutionGUIKeyboardListener());
		_btnExaminePath.addKeyListener(new ExecutionGUIKeyboardListener());
		
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel.add(_lblExecution, constraints);

		constraints.gridx = 1;
		constraints.ipadx = 200;
		_mainPanel.add(_tfExecution, constraints);

		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_btnExaminePath, constraints);

		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_lblArguments, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 150;
		_mainPanel.add(_tfArguments, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		_buttonPanel.add(_btnRun, constraints);
		constraints.gridx = 1;
		_buttonPanel.add(_btnCancel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_frame.add(_mainPanel, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_frame.add(_buttonPanel, constraints);
		_frame.setResizable(false);
		_frame.pack();
		_frame.setVisible(true);
		_frame.setLocationRelativeTo(null);
		_frame.addWindowListener(new AcideWindowListener());
		
		MainWindow.getInstance();setEnabled(false);
	}

	/**
	 * 
	 */
	class ExecutionGUIKeyboardListener extends KeyAdapter {
		
		/**
		 * 
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
