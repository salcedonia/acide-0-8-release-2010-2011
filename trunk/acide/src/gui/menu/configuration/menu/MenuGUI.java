package gui.menu.configuration.menu;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import language.Language;
import operations.configuration.MenuConfiguration;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

import es.text.TextFile;
import es.text.TextFileFilter;
import gui.MainWindow;

/**
 * 
 */
public class MenuGUI {

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
	private JPanel _filePanel;
	/**
	 * 
	 */
	private JPanel _editPanel;
	/**
	 * 
	 */
	private JPanel _projectPanel;
	/**
	 * 
	 */
	private JPanel _viewPanel;
	/**
	 * 
	 */
	private JPanel _configurationPanel;
	/**
	 * 
	 */
	private JPanel _languagePanel;
	/**
	 * 
	 */
	private JPanel _helpPanel;
	/**
	 * 
	 */
	private JPanel _buttonPanel;
	/**
	 * 
	 */
	private JButton _bntAccept;
	/**
	 * 
	 */
	private JButton _btnCancel;
	/**
	 * 
	 */
	private JButton _btnLoad;
	/**
	 * 
	 */
	private JButton _btnSave;
	/**
	 * 
	 */
	private JButton _btnSelectAll;
	/**
	 * 
	 */
	private JButton _btnSelectNone;
	/**
	 * 
	 */
	private final JCheckBox _cmbNewFile;
	/**
	 * 
	 */
	private final JCheckBox _cmbOpenFile;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveFileAs;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveFile;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveAllFiles;
	/**
	 * 
	 */
	private final JCheckBox _cmbCloseFile;
	/**
	 * 
	 */
	private final JCheckBox _cmbCloseAllFiles;
	/**
	 * 
	 */
	private final JCheckBox _cmbSetFile2;
	/**
	 * 
	 */
	private final JCheckBox _cmbUnsetFile2;
	/**
	 * 
	 */
	private final JCheckBox _cmbSetMain2;
	/**
	 * 
	 */
	private final JCheckBox _cmbUnsetMain2;
	/**
	 * 
	 */
	private final JCheckBox _cmbPrintFile;
	/**
	 * 
	 */
	private final JCheckBox _cmbExit;
	/**
	 * 
	 */
	private final JCheckBox _cmbUndo;
	/**
	 * 
	 */
	private final JCheckBox _cmbRedo;
	/**
	 * 
	 */
	private final JCheckBox _cmbCopy;
	/**
	 * 
	 */
	private final JCheckBox _cmbPaste;
	/**
	 * 
	 */
	private final JCheckBox _cmbCut;
	/**
	 * 
	 */
	private final JCheckBox _cmbSelectAll;
	/**
	 * 
	 */
	private final JCheckBox _cmbGoToLine;
	/**
	 * 
	 */
	private final JCheckBox _cmbSearch;
	/**
	 * 
	 */
	private final JCheckBox _cmbReplace;
	/**
	 * 
	 */
	private final JCheckBox _cmbNewProject;
	/**
	 * 
	 */
	private final JCheckBox _cmbOpenProject;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveProject;
	/**
	 * 
	 */
	private final JCheckBox _cmbCloseProject;
	/**
	 * 
	 */
	private final JCheckBox _cmbNewProjectFile;
	/**
	 * 
	 */
	private final JCheckBox _cmbAddFile;
	/**
	 * 
	 */
	private final JCheckBox _cmbRemoveFile;
	/**
	 * 
	 */
	private final JCheckBox _cmbDeleteFile;
	/**
	 * 
	 */
	private final JCheckBox _cmbAddFolder;
	/**
	 * 
	 */
	private final JCheckBox _cmbRemoveFolder;
	/**
	 * 
	 */
	private final JCheckBox _cmbCompile;
	/**
	 * 
	 */
	private final JCheckBox _cmbExecute;
	/**
	 * 
	 */
	private final JCheckBox _cmbSetFile;
	/**
	 * 
	 */
	private final JCheckBox _cmbUnsetFile;
	/**
	 * 
	 */
	private final JCheckBox _cmbSetMain;
	/**
	 * 
	 */
	private final JCheckBox _cmbUnsetMain;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveAsProject;
	/**
	 * 
	 */
	private final JCheckBox _cmbShowLog;
	/**
	 * 
	 */
	private final JCheckBox _cmbShowBrowser;
	/**
	 * 
	 */
	private final JCheckBox _cmbShowShellWindow;
	/**
	 * 
	 */
	private final JCheckBox _cmbNewLexical;
	/**
	 * 
	 */
	private final JCheckBox _cmbLoadParameters;
	/**
	 * 
	 */
	private final JCheckBox _cmbLexical;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveLexical;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveAsLexical;
	/**
	 * 
	 */
	private final JCheckBox _cmbNewGrammar;
	/**
	 * 
	 */
	private final JCheckBox _cmbLoadGrammar;
	/**
	 * 
	 */
	private final JCheckBox _cmbModifyGrammar;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveGrammar;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveAsGrammar;
	/**
	 * 
	 */
	private final JCheckBox _cmbSetPaths;
	/**
	 * 
	 */
	private final JCheckBox _cmbAutoGrammarAnalysis;
	/**
	 * 
	 */
	private final JCheckBox _cmbConfigure;
	/**
	 * 
	 */
	private final JCheckBox _cmbExternalCommand;
	/**
	 * 
	 */
	private final JCheckBox _cmbSpanish;
	/**
	 * 
	 */
	private final JCheckBox _cmbEnglish;
	/**
	 * 
	 */
	private final JCheckBox _cmbNewMenu;
	/**
	 * 
	 */
	private final JCheckBox _cmbLoadMenu;
	/**
	 * 
	 */
	private final JCheckBox _cmbModifyMenu;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveMenu;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveAsMenu;
	/**
	 * 
	 */
	private final JCheckBox _cmbNewToolBar;
	/**
	 * 
	 */
	private final JCheckBox _cmbLoadToolBar;
	/**
	 * 
	 */
	private final JCheckBox _cmbModifyToolBar;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveToolBar;
	/**
	 * 
	 */
	private final JCheckBox _cmbSaveAsToolBar;
	/**
	 * 
	 */
	private final JCheckBox _cmbCompiler;
	/**
	 * 
	 */
	private final JCheckBox _cmbShowHelp;
	/**
	 * 
	 */
	private final JCheckBox _cmbShowAboutUs;
	/**
	 * 
	 */
	private final JCheckBox _cmbMenu;
	/**
	 * 
	 */
	private final JCheckBox _cmbToolBar;	
	/**
	 * 
	 */
	private static boolean _changesAreSaved;
	/**
	 * 
	 */
	private Logger _logger = Log.getLog();

	/**
	 * Constructor of the class.
	 * 
	 * @param isModify
	 */
	public MenuGUI(boolean isModify) {

		_changesAreSaved = true;

		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		} catch (Exception e) {
			e.printStackTrace();
		}
		final ResourceBundle labels = language.getLabels();
		_logger.info(labels.getString("s531"));

		// FRAME
		_frame = new JFrame();
		_frame.setLayout(new GridBagLayout());
		
		if (isModify) {
			
			String s = null;
			
			try {
				s = PropertiesManager.getProperty("currentMenuConfiguration");
				int index = s.lastIndexOf("\\");
				if (index == -1)
					index = s.lastIndexOf("/");
				s = s.substring(index + 1, s.length() - 8);
			} catch (Exception e2) {
				JOptionPane.showMessageDialog(null, e2.getMessage(),
						labels.getString("s295"), JOptionPane.ERROR_MESSAGE);
			}
			_frame.setTitle(labels.getString("s532") + " - " + s);
		}
		else
			_frame.setTitle(labels.getString("s298"));
		_frame.setIconImage(new ImageIcon(ICON).getImage());
		
		// FILE PANEL
		_filePanel = new JPanel();
		_filePanel.setLayout(new GridLayout(0, 3));
		_filePanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s500")));
		_cmbNewFile = new JCheckBox(labels.getString("s8"));
		_cmbOpenFile = new JCheckBox(labels.getString("s9"));
		_cmbSaveFileAs = new JCheckBox(labels.getString("s10"));
		_cmbSaveFile = new JCheckBox(labels.getString("s617"));
		_cmbSaveAllFiles = new JCheckBox(labels.getString("s217"));
		_cmbCloseFile = new JCheckBox(labels.getString("s238"));
		_cmbCloseAllFiles = new JCheckBox(labels.getString("s239"));
		_cmbSetFile2 = new JCheckBox(labels.getString("s254"));
		_cmbUnsetFile2 = new JCheckBox(labels.getString("s255"));
		_cmbSetMain2 = new JCheckBox(labels.getString("s256"));
		_cmbUnsetMain2 = new JCheckBox(labels.getString("s952"));
		_cmbPrintFile = new JCheckBox(labels.getString("s624"));
		_cmbExit = new JCheckBox(labels.getString("s13"));

		// EDICION PANEL
		_editPanel = new JPanel();
		_editPanel.setLayout(new GridLayout(0, 3));
		_editPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s501")));
		_cmbUndo = new JCheckBox(labels.getString("s21"));
		_cmbRedo = new JCheckBox(labels.getString("s22"));
		_cmbCopy = new JCheckBox(labels.getString("s23"));
		_cmbPaste = new JCheckBox(labels.getString("s25"));
		_cmbCut = new JCheckBox(labels.getString("s24"));
		_cmbSelectAll = new JCheckBox(labels.getString("s190"));
		_cmbGoToLine = new JCheckBox(labels.getString("s222"));
		_cmbSearch = new JCheckBox(labels.getString("s26"));
		_cmbReplace = new JCheckBox(labels.getString("s27"));

		// PROJECT PANEL
		_projectPanel = new JPanel();
		_projectPanel.setLayout(new GridLayout(0, 3));
		_projectPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s502")));
		_cmbNewProject = new JCheckBox(labels.getString("s14"));
		_cmbOpenProject = new JCheckBox(labels.getString("s15"));
		_cmbSaveProject = new JCheckBox(labels.getString("s16"));
		_cmbCloseProject = new JCheckBox(labels.getString("s228"));
		_cmbNewProjectFile = new JCheckBox(
				labels.getString("s947"));
		_cmbAddFile = new JCheckBox(
				labels.getString("s17"));
		_cmbRemoveFile = new JCheckBox(labels.getString("s218"));
		_cmbDeleteFile = new JCheckBox(labels.getString("s950"));
		_cmbAddFolder = new JCheckBox(labels.getString("s219"));
		_cmbRemoveFolder = new JCheckBox(labels.getString("s220"));
		_cmbCompile = new JCheckBox(labels.getString("s18"));
		_cmbExecute = new JCheckBox(labels.getString("s19"));
		_cmbSetFile = new JCheckBox(labels.getString("s254"));
		_cmbUnsetFile = new JCheckBox(labels.getString("s255"));
		_cmbSetMain = new JCheckBox(labels.getString("s256"));
		_cmbUnsetMain = new JCheckBox(labels.getString("s952"));
		_cmbSaveAsProject = new JCheckBox(
				labels.getString("s926"));
		
		// VIEW PANEL
		_viewPanel = new JPanel();
		_viewPanel.setLayout(new GridLayout(0, 3));
		_viewPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s503")));
		_cmbShowLog = new JCheckBox(labels.getString("s28"));
		_cmbShowBrowser = new JCheckBox(labels.getString("s221"));
		_cmbShowShellWindow = new JCheckBox(labels.getString("s223"));

		// CONFIGURATION PANEL
		_configurationPanel = new JPanel();
		_configurationPanel.setLayout(new GridLayout(0, 5));
		_configurationPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s504")));

		// LANGUAGE PANEL
		_languagePanel = new JPanel();
		_languagePanel.setLayout(new GridLayout(0, 3));
		_languagePanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s505")));
		// LEXICAL SUBMENU
		_cmbNewLexical = new JCheckBox(labels.getString("s224")
				+ " - " + labels.getString("s249"));
		_cmbLoadParameters = new JCheckBox(labels.getString("s224")
				+ " - " + labels.getString("s35"));
		_cmbLexical = new JCheckBox(labels.getString("s224")
				+ " - " + labels.getString("s29"));
		_cmbSaveLexical = new JCheckBox(labels.getString("s224")
				+ " - " + labels.getString("s250"));
		_cmbSaveAsLexical = new JCheckBox(
				labels.getString("s224") + " - " + labels.getString("s285"));

		// GRAMMAR SUBMENU
		_cmbNewGrammar = new JCheckBox(labels.getString("s225")
				+ " - " + labels.getString("s30"));
		_cmbLoadGrammar = new JCheckBox(labels.getString("s225")
				+ " - " + labels.getString("s226"));
		_cmbModifyGrammar = new JCheckBox(labels.getString("s225")
				+ " - " + labels.getString("s227"));
		_cmbSaveGrammar = new JCheckBox(labels.getString("s225")
				+ " - " + labels.getString("s251"));
		_cmbSaveAsGrammar = new JCheckBox(
				labels.getString("s225") + " - " + labels.getString("s286"));
		_cmbSetPaths = new JCheckBox(labels.getString("s225")
				+ " - " + labels.getString("s912"));
		_cmbAutoGrammarAnalysis = new JCheckBox(
				labels.getString("s225") + " - " + labels.getString("s911"));

		// OUTPUT SUBMENU
		_cmbConfigure = new JCheckBox(labels.getString("s332")
				+ " - " + labels.getString("s333"));
		_cmbExternalCommand = new JCheckBox(
				labels.getString("s332") + " - " + labels.getString("s341"));

		// LANGUAGE SUBMENU
		_cmbSpanish = new JCheckBox(labels.getString("s6")
				+ " - " + labels.getString("s11"));
		_cmbEnglish = new JCheckBox(labels.getString("s6")
				+ " - " + labels.getString("s12"));

		// MENU SUBMENU
		_cmbMenu = new JCheckBox();
		_cmbNewMenu = new JCheckBox(labels.getString("s34")
				+ " - " + labels.getString("s275"));
		_cmbLoadMenu = new JCheckBox(labels.getString("s34")
				+ " - " + labels.getString("s276"));
		_cmbModifyMenu = new JCheckBox(labels.getString("s34")
				+ " - " + labels.getString("s277"));
		_cmbSaveMenu = new JCheckBox(labels.getString("s34")
				+ " - " + labels.getString("s278"));
		_cmbSaveAsMenu = new JCheckBox(labels.getString("s34")
				+ " - " + labels.getString("s279"));

		// TOOL BAR SUBMENU
		_cmbToolBar = new JCheckBox();
		_cmbNewToolBar = new JCheckBox(labels.getString("s169")
				+ " - " + labels.getString("s280"));
		_cmbLoadToolBar = new JCheckBox(labels.getString("s169")
				+ " - " + labels.getString("s281"));
		_cmbModifyToolBar = new JCheckBox(labels.getString("s169")
				+ " - " + labels.getString("s282"));
		_cmbSaveToolBar = new JCheckBox(labels.getString("s169")
				+ " - " + labels.getString("s283"));
		_cmbSaveAsToolBar = new JCheckBox(labels.getString("s169")
				+ " - " + labels.getString("s284"));
		
		// COMPILER
		_cmbCompiler = new JCheckBox(labels.getString("s240"));

		// HELP AYUDA
		_helpPanel = new JPanel();
		_helpPanel.setLayout(new GridLayout(0, 3));
		_helpPanel.setBorder(BorderFactory.createTitledBorder(labels
				.getString("s506")));
		_cmbShowHelp = new JCheckBox(labels.getString("s38"));
		_cmbShowAboutUs = new JCheckBox(labels.getString("s39"));

		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new GridBagLayout());

		// ACCEPT BUTTON
		_bntAccept = new JButton(labels.getString("s507"));
		_bntAccept.setToolTipText(labels.getString("s508"));

		// CANCEL BUTTON
		_btnCancel = new JButton(labels.getString("s509"));
		_btnCancel.setToolTipText(labels.getString("s510"));

		// LOAD BUTTON
		_btnLoad = new JButton(labels.getString("s511"));
		_btnLoad.setToolTipText(labels.getString("s512"));

		// SAVE BUTTON
		_btnSave = new JButton(labels.getString("s513"));
		_btnSave.setToolTipText(labels.getString("s514"));

		// SELECT ALL BUTTON
		_btnSelectAll = new JButton(labels.getString("s515"));
		_btnSelectAll.setToolTipText(labels.getString("s516"));

		// SELECT NONE BUTTON
		_btnSelectNone = new JButton(labels.getString("s517"));
		_btnSelectNone.setToolTipText(labels.getString("s518"));

		_cmbModifyMenu.setSelected(true);
		_cmbModifyMenu.setEnabled(false);

		// LISTENERS
		_bntAccept.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {

				boolean[] values = new boolean[73];

				values[0] = _cmbNewFile.isSelected();
				values[1] = _cmbOpenFile.isSelected();
				values[2] = _cmbSaveFileAs.isSelected();
				values[3] = _cmbSaveFile.isSelected();
				values[4] = _cmbSaveAllFiles.isSelected();
				values[5] = _cmbPrintFile.isSelected();
				values[6] = _cmbExit.isSelected();
				values[7] = _cmbUndo.isSelected();
				values[8] = _cmbRedo.isSelected();
				values[9] = _cmbCopy.isSelected();
				values[10] = _cmbPaste.isSelected();
				values[11] = _cmbCut.isSelected();
				values[12] = _cmbSelectAll.isSelected();
				values[13] = _cmbGoToLine.isSelected();
				values[14] = _cmbSearch.isSelected();
				values[15] = _cmbReplace.isSelected();
				values[16] = _cmbNewProject.isSelected();
				values[17] = _cmbOpenProject.isSelected();
				values[18] = _cmbSaveProject.isSelected();
				values[19] = _cmbAddFile.isSelected();
				values[20] = _cmbRemoveFile.isSelected();
				values[21] = _cmbAddFolder.isSelected();
				values[22] = _cmbRemoveFolder.isSelected();
				values[23] = _cmbCompile.isSelected();
				values[24] = _cmbExecute.isSelected();
				values[25] = _cmbShowLog.isSelected();
				values[26] = _cmbShowBrowser.isSelected();
				values[27] = _cmbShowShellWindow.isSelected();
				values[28] = _cmbLoadParameters.isSelected();
				values[29] = _cmbLexical.isSelected();
				values[30] = _cmbNewGrammar.isSelected();
				values[31] = _cmbLoadGrammar.isSelected();
				values[32] = _cmbModifyGrammar.isSelected();
				values[33] = _cmbConfigure.isSelected();
				values[34] = _cmbExternalCommand.isSelected();
				values[35] = _cmbSpanish.isSelected();
				values[36] = _cmbEnglish.isSelected();
				values[37] = true; // Menu option always ON
				values[38] = _cmbToolBar.isSelected();
				values[39] = _cmbShowHelp.isSelected();
				values[40] = _cmbShowAboutUs.isSelected();
				values[41] = _cmbCloseProject.isSelected();
				values[42] = _cmbCloseFile.isSelected();
				values[43] = _cmbCloseAllFiles.isSelected();
				values[44] = _cmbCompiler.isSelected();
				values[45] = _cmbNewLexical.isSelected();
				values[46] = _cmbSaveLexical.isSelected();
				values[47] = _cmbSaveGrammar.isSelected();
				values[48] = _cmbSetFile.isSelected();
				values[49] = _cmbUnsetFile.isSelected();
				values[50] = _cmbSetMain.isSelected();
				values[51] = _cmbNewMenu.isSelected();
				values[52] = _cmbLoadMenu.isSelected();
				values[53] = true; // modifyMenuCB option always on
				values[54] = _cmbSaveMenu.isSelected();
				values[55] = _cmbSaveAsMenu.isSelected();
				values[56] = _cmbNewToolBar.isSelected();
				values[57] = _cmbLoadToolBar.isSelected();
				values[58] = _cmbModifyToolBar.isSelected();
				values[59] = _cmbSaveToolBar.isSelected();
				values[60] = _cmbSaveAsToolBar.isSelected();
				values[61] = _cmbSaveAsGrammar.isSelected();
				values[62] = _cmbSaveAsLexical.isSelected();
				values[63] = _cmbSetPaths.isSelected();
				values[64] = _cmbAutoGrammarAnalysis.isSelected();
				values[65] = _cmbSaveAsProject.isSelected();
				values[66] = _cmbNewProjectFile.isSelected();
				values[67] = _cmbDeleteFile.isSelected();
				values[68] = _cmbUnsetMain.isSelected();
				values[69] = _cmbSetFile2.isSelected();
				values[70] = _cmbUnsetFile2.isSelected();
				values[71] = _cmbSetMain2.isSelected();
				values[72] = _cmbUnsetMain2.isSelected();

				MenuConfiguration.setAll(values);
				String newName = "./configuration/menu/newMenu.menuCfg";
				MenuConfiguration.saveMenuConfigurationFile(newName, values);
				try {
					String previousMenu = PropertiesManager
							.getProperty("currentMenuConfiguration");
					if (_changesAreSaved)
						PropertiesManager.setProperty("previousMenuCfg",
								previousMenu);
					PropertiesManager.setProperty("currentMenuConfiguration",
							newName);

					MainWindow mainWindow = MainWindow.getInstance();
					mainWindow.getMenu().getConfiguration().getMenu()
							.getSaveMenu().setEnabled(false);
					mainWindow.getMenu().buildMenu();
					mainWindow.validate();
					mainWindow.repaint();

					_changesAreSaved = false;
					_frame.dispose();
					_logger.info(labels.getString("s519"));

					String prj = null;
					try {
						prj = PropertiesManager
								.getProperty("defaultAcideProject");
					} catch (Exception e1) {
						e1.printStackTrace();
					}
					if (!(prj.equals("./configuration/default.acidePrj") && mainWindow
							.getProjectConfiguration().getName().equals(""))) {
						MainWindow.getInstance().getProjectConfiguration()
								.setModified(true);
					}

				} catch (Exception e1) {
					JOptionPane.showMessageDialog(null, e1.getMessage(),
							labels.getString("s292"), JOptionPane.ERROR_MESSAGE);
				}
			}
		});

		_btnCancel.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_logger.info(labels.getString("s520"));
				_frame.dispose();
			}
		});
		
		_btnLoad.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				TextFile f = new TextFile();
				TextFileFilter filter = new TextFileFilter(labels
						.getString("s172"));
				filter.addExtension("menuCfg");
				String path = f.read(filter);
				
				boolean[] values;
				try {
					
					values = MenuConfiguration.loadMenuConfigurationFile(path);
					PropertiesManager.setProperty("currentMenuConfiguration",
							path);
					
					_cmbNewFile.setSelected(values[0]);
					_cmbOpenFile.setSelected(values[1]);
					_cmbSaveFileAs.setSelected(values[2]);
					_cmbSaveFile.setSelected(values[3]);
					_cmbSaveAllFiles.setSelected(values[4]);
					_cmbPrintFile.setSelected(values[5]);
					_cmbExit.setSelected(values[6]);
					_cmbUndo.setSelected(values[7]);
					_cmbRedo.setSelected(values[8]);
					_cmbCopy.setSelected(values[9]);
					_cmbPaste.setSelected(values[10]);
					_cmbCut.setSelected(values[11]);
					_cmbSelectAll.setSelected(values[12]);
					_cmbGoToLine.setSelected(values[13]);
					_cmbSearch.setSelected(values[14]);
					_cmbReplace.setSelected(values[15]);
					_cmbNewProject.setSelected(values[16]);
					_cmbOpenProject.setSelected(values[17]);
					_cmbSaveProject.setSelected(values[18]);
					_cmbAddFile.setSelected(values[19]);
					_cmbRemoveFile.setSelected(values[20]);
					_cmbAddFolder.setSelected(values[21]);
					_cmbRemoveFolder.setSelected(values[22]);
					_cmbCompile.setSelected(values[23]);
					_cmbExecute.setSelected(values[24]);
					_cmbShowLog.setSelected(values[25]);
					_cmbShowBrowser.setSelected(values[26]);
					_cmbShowShellWindow.setSelected(values[27]);
					_cmbLoadParameters.setSelected(values[28]);
					_cmbLexical.setSelected(values[29]);
					_cmbNewGrammar.setSelected(values[30]);
					_cmbLoadGrammar.setSelected(values[31]);
					_cmbModifyGrammar.setSelected(values[32]);
					_cmbConfigure.setSelected(values[33]);
					_cmbExternalCommand.setSelected(values[34]);
					_cmbSpanish.setSelected(values[35]);
					_cmbEnglish.setSelected(values[36]);
					_cmbMenu.setSelected(true);
					_cmbToolBar.setSelected(values[38]);
					_cmbShowHelp.setSelected(values[39]);
					_cmbShowAboutUs.setSelected(values[40]);
					_cmbCloseProject.setSelected(values[41]);
					_cmbCloseFile.setSelected(values[42]);
					_cmbCloseAllFiles.setSelected(values[43]);
					_cmbCompiler.setSelected(values[44]);
					_cmbNewLexical.setSelected(values[45]);
					_cmbSaveLexical.setSelected(values[46]);
					_cmbSaveGrammar.setSelected(values[47]);
					_cmbSetFile.setSelected(values[48]);
					_cmbUnsetFile.setSelected(values[49]);
					_cmbSetMain.setSelected(values[50]);
					_cmbNewMenu.setSelected(values[51]);
					_cmbLoadMenu.setSelected(values[52]);
					_cmbModifyMenu.setSelected(true); // modifyMenuCB option
													// always on
					_cmbSaveMenu.setSelected(values[54]);
					_cmbSaveAsMenu.setSelected(values[55]);
					_cmbNewToolBar.setSelected(values[56]);
					_cmbLoadToolBar.setSelected(values[57]);
					_cmbModifyToolBar.setSelected(values[58]);
					_cmbSaveToolBar.setSelected(values[59]);
					_cmbSaveAsToolBar.setSelected(values[60]);
					_cmbSaveAsGrammar.setSelected(values[61]);
					_cmbSaveAsLexical.setSelected(values[62]);
					_cmbSetPaths.setSelected(values[63]);
					_cmbAutoGrammarAnalysis.setSelected(values[64]);
					_cmbSaveAsProject.setSelected(values[65]);
					_cmbNewProjectFile.setSelected(values[66]);
					_cmbDeleteFile.setSelected(values[67]);
					_cmbUnsetMain.setSelected(values[68]);
					_cmbSetFile2.setSelected(values[69]);
					_cmbUnsetFile2.setSelected(values[70]);
					_cmbSetMain2.setSelected(values[71]);
					_cmbUnsetMain2.setSelected(values[72]);

					_changesAreSaved = true;
					_logger.info(labels.getString("s522") + path
							+ labels.getString("s523"));
				} catch (Exception e1) {
					_logger.info(labels.getString("s521") + path + " "
							+ e1.getMessage());
				}
			}
		});
		
		_btnSave.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				boolean[] values = new boolean[73];
				
				values[0] = _cmbNewFile.isSelected();
				values[1] = _cmbOpenFile.isSelected();
				values[2] = _cmbSaveFileAs.isSelected();
				values[3] = _cmbSaveFile.isSelected();
				values[4] = _cmbSaveAllFiles.isSelected();
				values[5] = _cmbPrintFile.isSelected();
				values[6] = _cmbExit.isSelected();
				values[7] = _cmbUndo.isSelected();
				values[8] = _cmbRedo.isSelected();
				values[9] = _cmbCopy.isSelected();
				values[10] = _cmbPaste.isSelected();
				values[11] = _cmbCut.isSelected();
				values[12] = _cmbSelectAll.isSelected();
				values[13] = _cmbGoToLine.isSelected();
				values[14] = _cmbSearch.isSelected();
				values[15] = _cmbReplace.isSelected();
				values[16] = _cmbNewProject.isSelected();
				values[17] = _cmbOpenProject.isSelected();
				values[18] = _cmbSaveProject.isSelected();
				values[19] = _cmbAddFile.isSelected();
				values[20] = _cmbRemoveFile.isSelected();
				values[21] = _cmbAddFolder.isSelected();
				values[22] = _cmbRemoveFolder.isSelected();
				values[23] = _cmbCompile.isSelected();
				values[24] = _cmbExecute.isSelected();
				values[25] = _cmbShowLog.isSelected();
				values[26] = _cmbShowBrowser.isSelected();
				values[27] = _cmbShowShellWindow.isSelected();
				values[28] = _cmbLoadParameters.isSelected();
				values[29] = _cmbLexical.isSelected();
				values[30] = _cmbNewGrammar.isSelected();
				values[31] = _cmbLoadGrammar.isSelected();
				values[32] = _cmbModifyGrammar.isSelected();
				values[33] = _cmbConfigure.isSelected();
				values[34] = _cmbExternalCommand.isSelected();
				values[35] = _cmbSpanish.isSelected();
				values[36] = _cmbEnglish.isSelected();
				values[37] = true; // Option menu always ON
				values[38] = _cmbToolBar.isSelected();
				values[39] = _cmbShowHelp.isSelected();
				values[40] = _cmbShowAboutUs.isSelected();
				values[41] = _cmbCloseProject.isSelected();
				values[42] = _cmbCloseFile.isSelected();
				values[43] = _cmbCloseAllFiles.isSelected();
				values[44] = _cmbCompiler.isSelected();
				values[45] = _cmbNewLexical.isSelected();
				values[46] = _cmbSaveLexical.isSelected();
				values[47] = _cmbSaveGrammar.isSelected();
				values[48] = _cmbSetFile.isSelected();
				values[49] = _cmbUnsetFile.isSelected();
				values[50] = _cmbSetMain.isSelected();
				values[51] = _cmbNewMenu.isSelected();
				values[52] = _cmbLoadMenu.isSelected();
				values[53] = true; // Modify Menu always ON
				values[54] = _cmbSaveMenu.isSelected();
				values[55] = _cmbSaveAsMenu.isSelected();
				values[56] = _cmbNewToolBar.isSelected();
				values[57] = _cmbLoadToolBar.isSelected();
				values[58] = _cmbModifyToolBar.isSelected();
				values[59] = _cmbSaveToolBar.isSelected();
				values[60] = _cmbSaveAsToolBar.isSelected();
				values[61] = _cmbSaveAsGrammar.isSelected();
				values[62] = _cmbSaveAsLexical.isSelected();
				values[63] = _cmbSetPaths.isSelected();
				values[64] = _cmbAutoGrammarAnalysis.isSelected();
				values[65] = _cmbSaveAsProject.isSelected();
				values[66] = _cmbNewProjectFile.isSelected();
				values[67] = _cmbDeleteFile.isSelected();
				values[68] = _cmbUnsetMain.isSelected();
				values[69] = _cmbSetFile2.isSelected();
				values[70] = _cmbUnsetFile2.isSelected();
				values[71] = _cmbSetMain2.isSelected();
				values[72] = _cmbUnsetMain2.isSelected();

				JFileChooser selector = new JFileChooser();
				TextFileFilter filter = new TextFileFilter(labels
						.getString("s126"));
				filter.addExtension("menuCfg");
				selector.setFileFilter(filter);
				
				String fileName = "";
				
				int value = selector.showSaveDialog(selector);
				if (value == JFileChooser.APPROVE_OPTION) {
					fileName = selector.getSelectedFile()
							.getAbsolutePath();
					if (!fileName.endsWith(".menuCfg"))
						fileName += ".menuCfg";
					MenuConfiguration.saveMenuConfigurationFile(fileName,
							values);
					PropertiesManager.setProperty("currentMenuConfiguration",
							fileName);
					_changesAreSaved = true;
					_logger.info(labels.getString("s528") + fileName
							+ labels.getString("s529"));
				} else if (value == JFileChooser.CANCEL_OPTION) {
					selector.cancelSelection();
					_logger.info(labels.getString("s527"));
				}
			}
		});
		_btnSelectAll.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				_cmbNewFile.setSelected(true);
				_cmbOpenFile.setSelected(true);
				_cmbSaveFileAs.setSelected(true);
				_cmbSaveFile.setSelected(true);
				_cmbSaveAllFiles.setSelected(true);
				_cmbPrintFile.setSelected(true);
				_cmbExit.setSelected(true);
				_cmbUndo.setSelected(true);
				_cmbRedo.setSelected(true);
				_cmbCopy.setSelected(true);
				_cmbPaste.setSelected(true);
				_cmbCut.setSelected(true);
				_cmbSelectAll.setSelected(true);
				_cmbGoToLine.setSelected(true);
				_cmbSearch.setSelected(true);
				_cmbReplace.setSelected(true);
				_cmbNewProject.setSelected(true);
				_cmbOpenProject.setSelected(true);
				_cmbSaveProject.setSelected(true);
				_cmbAddFile.setSelected(true);
				_cmbRemoveFile.setSelected(true);
				_cmbAddFolder.setSelected(true);
				_cmbRemoveFolder.setSelected(true);
				_cmbCompile.setSelected(true);
				_cmbExecute.setSelected(true);
				_cmbShowLog.setSelected(true);
				_cmbShowBrowser.setSelected(true);
				_cmbShowShellWindow.setSelected(true);
				_cmbLoadParameters.setSelected(true);
				_cmbLexical.setSelected(true);
				_cmbNewGrammar.setSelected(true);
				_cmbLoadGrammar.setSelected(true);
				_cmbModifyGrammar.setSelected(true);
				_cmbConfigure.setSelected(true);
				_cmbExternalCommand.setSelected(true);
				_cmbSpanish.setSelected(true);
				_cmbEnglish.setSelected(true);
				_cmbMenu.setSelected(true);
				_cmbToolBar.setSelected(true);
				_cmbShowHelp.setSelected(true);
				_cmbShowAboutUs.setSelected(true);
				_cmbCloseProject.setSelected(true);
				_cmbCloseFile.setSelected(true);
				_cmbCloseAllFiles.setSelected(true);
				_cmbCompiler.setSelected(true);
				_cmbNewLexical.setSelected(true);
				_cmbSaveLexical.setSelected(true);
				_cmbSaveGrammar.setSelected(true);
				_cmbSetFile.setSelected(true);
				_cmbUnsetFile.setSelected(true);
				_cmbSetMain.setSelected(true);
				_cmbNewMenu.setSelected(true);
				_cmbLoadMenu.setSelected(true);
				_cmbModifyMenu.setSelected(true);
				_cmbSaveMenu.setSelected(true);
				_cmbSaveAsMenu.setSelected(true);
				_cmbNewToolBar.setSelected(true);
				_cmbLoadToolBar.setSelected(true);
				_cmbModifyToolBar.setSelected(true);
				_cmbSaveToolBar.setSelected(true);
				_cmbSaveAsToolBar.setSelected(true);
				_cmbSaveAsGrammar.setSelected(true);
				_cmbSaveAsLexical.setSelected(true);
				_cmbSetPaths.setSelected(true);
				_cmbAutoGrammarAnalysis.setSelected(true);
				_cmbSaveAsProject.setSelected(true);
				_cmbNewProjectFile.setSelected(true);
				_cmbDeleteFile.setSelected(true);
				_cmbUnsetMain.setSelected(true);
				_cmbSetFile2.setSelected(true);
				_cmbUnsetFile2.setSelected(true);
				_cmbSetMain2.setSelected(true);
				_cmbUnsetMain2.setSelected(true);
			}
		});
		
		_btnSelectNone.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				
				_cmbNewFile.setSelected(false);
				_cmbOpenFile.setSelected(false);
				_cmbSaveFileAs.setSelected(false);
				_cmbSaveFile.setSelected(false);
				_cmbSaveAllFiles.setSelected(false);
				_cmbPrintFile.setSelected(false);
				_cmbExit.setSelected(false);		
				_cmbUndo.setSelected(false);
				_cmbRedo.setSelected(false);
				_cmbCopy.setSelected(false);
				_cmbPaste.setSelected(false);
				_cmbCut.setSelected(false);
				_cmbSelectAll.setSelected(false);
				_cmbGoToLine.setSelected(false);
				_cmbSearch.setSelected(false);
				_cmbReplace.setSelected(false);
				_cmbNewProject.setSelected(false);
				_cmbOpenProject.setSelected(false);
				_cmbSaveProject.setSelected(false);
				_cmbAddFile.setSelected(false);
				_cmbRemoveFile.setSelected(false);
				_cmbAddFolder.setSelected(false);
				_cmbRemoveFolder.setSelected(false);
				_cmbCompile.setSelected(false);
				_cmbExecute.setSelected(false);
				_cmbShowLog.setSelected(false);
				_cmbShowBrowser.setSelected(false);
				_cmbShowShellWindow.setSelected(false);
				_cmbLoadParameters.setSelected(false);
				_cmbLexical.setSelected(false);
				_cmbNewGrammar.setSelected(false);
				_cmbLoadGrammar.setSelected(false);
				_cmbModifyGrammar.setSelected(false);
				_cmbConfigure.setSelected(false);
				_cmbExternalCommand.setSelected(false);
				_cmbSpanish.setSelected(false);
				_cmbEnglish.setSelected(false);
				_cmbMenu.setSelected(true);
				_cmbToolBar.setSelected(false);
				_cmbShowHelp.setSelected(false);
				_cmbShowAboutUs.setSelected(false);
				_cmbCloseProject.setSelected(false);
				_cmbCloseFile.setSelected(false);
				_cmbCloseAllFiles.setSelected(false);
				_cmbCompiler.setSelected(false);
				_cmbNewLexical.setSelected(false);
				_cmbSaveLexical.setSelected(false);
				_cmbSaveGrammar.setSelected(false);
				_cmbSetFile.setSelected(false);
				_cmbUnsetFile.setSelected(false);
				_cmbSetMain.setSelected(false);
				_cmbNewMenu.setSelected(false);
				_cmbLoadMenu.setSelected(false);
				_cmbModifyMenu.setSelected(true);
				_cmbSaveMenu.setSelected(false);
				_cmbSaveAsMenu.setSelected(false);
				_cmbNewToolBar.setSelected(false);
				_cmbLoadToolBar.setSelected(false);
				_cmbModifyToolBar.setSelected(false);
				_cmbSaveToolBar.setSelected(false);
				_cmbSaveAsToolBar.setSelected(false);
				_cmbSaveAsGrammar.setSelected(false);
				_cmbSaveAsLexical.setSelected(false);
				_cmbSetPaths.setSelected(false);
				_cmbAutoGrammarAnalysis.setSelected(false);
				_cmbSaveAsProject.setSelected(false);
				_cmbNewProjectFile.setSelected(false);
				_cmbDeleteFile.setSelected(false);
				_cmbUnsetMain.setSelected(false);
				_cmbSetFile2.setSelected(false);
				_cmbUnsetFile2.setSelected(false);
				_cmbSetMain2.setSelected(false);
				_cmbUnsetMain2.setSelected(false);
			}
		});
		ActionListener escPressed = new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				_frame.dispose();
			}
		};
		_btnCancel.registerKeyboardAction(escPressed, "EscapeKey", KeyStroke
				.getKeyStroke(java.awt.event.KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		_filePanel.add(_cmbNewFile);
		_filePanel.add(_cmbOpenFile);
		_filePanel.add(_cmbSaveFileAs);
		_filePanel.add(_cmbSaveFile);
		_filePanel.add(_cmbSaveAllFiles);
		_filePanel.add(_cmbCloseFile);
		_filePanel.add(_cmbCloseAllFiles);
		_filePanel.add(_cmbPrintFile);
		_filePanel.add(_cmbExit);

		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridwidth = 6;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		_frame.add(_filePanel, constraints);

		_editPanel.add(_cmbUndo);
		_editPanel.add(_cmbRedo);
		_editPanel.add(_cmbCopy);
		_editPanel.add(_cmbPaste);
		_editPanel.add(_cmbCut);
		_editPanel.add(_cmbSelectAll);
		_editPanel.add(_cmbGoToLine);
		_editPanel.add(_cmbSearch);
		_editPanel.add(_cmbReplace);
		constraints.gridy = 1;
		_frame.add(_editPanel, constraints);

		_projectPanel.add(_cmbNewProject);
		_projectPanel.add(_cmbOpenProject);
		_projectPanel.add(_cmbCloseProject);
		_projectPanel.add(_cmbSaveProject);
		_projectPanel.add(_cmbSaveAsProject);
		_projectPanel.add(_cmbNewProjectFile);
		_projectPanel.add(_cmbAddFile);
		_projectPanel.add(_cmbRemoveFile);
		_projectPanel.add(_cmbDeleteFile);
		_projectPanel.add(_cmbAddFolder);
		_projectPanel.add(_cmbRemoveFolder);
		_projectPanel.add(_cmbCompile);
		_projectPanel.add(_cmbExecute);
		_projectPanel.add(_cmbSetFile);
		_projectPanel.add(_cmbUnsetFile);
		_projectPanel.add(_cmbSetMain);
		_projectPanel.add(_cmbUnsetMain);
		constraints.gridy = 2;
		_frame.add(_projectPanel, constraints);

		_viewPanel.add(_cmbShowLog);
		_viewPanel.add(_cmbShowBrowser);
		_viewPanel.add(_cmbShowShellWindow);
		constraints.gridy = 3;
		_frame.add(_viewPanel, constraints);

		_configurationPanel.add(_cmbCompiler);
		_configurationPanel.add(_cmbNewLexical);
		_configurationPanel.add(_cmbNewGrammar);
		_configurationPanel.add(_cmbNewMenu);
		_configurationPanel.add(_cmbNewToolBar);
		_configurationPanel.add(_cmbConfigure);
		_configurationPanel.add(_cmbLoadParameters);
		_configurationPanel.add(_cmbLoadGrammar);
		_configurationPanel.add(_cmbLoadMenu);
		_configurationPanel.add(_cmbLoadToolBar);
		_configurationPanel.add(_cmbExternalCommand);
		_configurationPanel.add(_cmbLexical);
		_configurationPanel.add(_cmbModifyGrammar);
		_configurationPanel.add(_cmbModifyMenu);
		_configurationPanel.add(_cmbModifyToolBar);
		_configurationPanel.add(_cmbSpanish);
		_configurationPanel.add(_cmbSaveLexical);
		_configurationPanel.add(_cmbSaveGrammar);
		_configurationPanel.add(_cmbSaveMenu);
		_configurationPanel.add(_cmbSaveToolBar);
		_configurationPanel.add(_cmbEnglish);
		_configurationPanel.add(_cmbSaveAsLexical);
		_configurationPanel.add(_cmbSaveAsGrammar);
		_configurationPanel.add(_cmbSaveAsMenu);
		_configurationPanel.add(_cmbSaveAsToolBar);
		_configurationPanel.add(new JLabel(""));
		_configurationPanel.add(new JLabel(""));
		_configurationPanel.add(_cmbSetPaths);
		_configurationPanel.add(new JLabel(""));
		_configurationPanel.add(new JLabel(""));
		_configurationPanel.add(new JLabel(""));
		_configurationPanel.add(new JLabel(""));
		_configurationPanel.add(_cmbAutoGrammarAnalysis);
		constraints.gridy = 4;
		_frame.add(_configurationPanel, constraints);

		_helpPanel.add(_cmbShowHelp);
		_helpPanel.add(_cmbShowAboutUs);
		constraints.gridy = 5;
		_frame.add(_helpPanel, constraints);

		constraints.gridwidth = 1;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = new Insets(5, 5, 5, 5);
		_buttonPanel.add(_btnSelectAll, constraints);
		constraints.gridx = 1;
		_buttonPanel.add(_btnSelectNone, constraints);
		constraints.gridx = 2;
		constraints.gridx = 3;
		constraints.gridx = 4;
		_buttonPanel.add(_bntAccept, constraints);
		constraints.gridx = 5;
		_buttonPanel.add(_btnCancel, constraints);
		constraints.gridy = 6;
		constraints.insets = new Insets(0, 0, 0, 0);

		_frame.add(_buttonPanel, constraints);
		_frame.setVisible(true);
		_frame.setResizable(false);
		_frame.pack();
		_frame.setLocationRelativeTo(null);
		_logger.info(labels.getString("s530"));
	}

	/**
	 * 
	 * @return
	 */
	public static boolean isChangesSaved() {
		return _changesAreSaved;
	}

	/**
	 * 
	 * @param changes
	 */
	public static void setChangesSaved(boolean changes) {
		_changesAreSaved = changes;
	}
}
