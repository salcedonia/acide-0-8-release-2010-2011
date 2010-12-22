package gui.menuBar.configurationMenu.menuMenu.gui;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
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
import javax.swing.border.TitledBorder;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;
import es.configuration.menu.MenuConfiguration;
import es.configuration.menu.MenuItemInformation;
import es.text.TextFileFilter;
import gui.mainWindow.MainWindow;
import gui.menuBar.configurationMenu.ConfigurationMenu;
import gui.menuBar.configurationMenu.grammarMenu.GrammarMenu;
import gui.menuBar.configurationMenu.languageMenu.LanguageMenu;
import gui.menuBar.configurationMenu.lexiconMenu.LexiconMenu;
import gui.menuBar.configurationMenu.menuMenu.MenuMenu;
import gui.menuBar.configurationMenu.outputMenu.OutputMenu;
import gui.menuBar.configurationMenu.toolBarMenu.ToolBarMenu;
import gui.menuBar.editMenu.EditMenu;
import gui.menuBar.fileMenu.FileMenu;
import gui.menuBar.helpMenu.HelpMenu;
import gui.menuBar.projectMenu.ProjectMenu;
import gui.menuBar.viewMenu.ViewMenu;

/************************************************************************
 * Menu configuration window of ACIDE - A Configurable IDE.
 * 
 * <p>
 * <b>ACIDE - A Configurable IDE</b>
 * </p>
 * <p>
 * <b>Official web site:</b> @see http://acide.sourceforge.net
 * </p>
 * 
 ************************************************************************ 
 * @author <ul>
 *         <li><b>Fernando Sáenz Pérez (Team Director)</b></li>
 *         <li><b>Version 0.1-0.6:</b>
 *         <ul>
 *         Diego Cardiel Freire
 *         </ul>
 *         <ul>
 *         Juan José Ortiz Sánchez
 *         </ul>
 *         <ul>
 *         Delfín Rupérez Cañas
 *         </ul>
 *         </li>
 *         <li><b>Version 0.7:</b>
 *         <ul>
 *         Miguel Martín Lázaro
 *         </ul>
 *         </li>
 *         <li><b>Version 0.8:</b>
 *         <ul>
 *         Javier Salcedo Gómez
 *         </ul>
 *         </li>
 *         </ul>
 ************************************************************************ 
 * @version 0.8
 * @see JFrame
 ***********************************************************************/
public class MenuConfigurationWindow extends JFrame {

	/**
	 * Menu configuration window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Menu configuration window image icon.
	 */
	private static final String WINDOW_ICON = "./resources/images/icon.png";
	/**
	 * Menu configuration window file panel.
	 */
	private JPanel _filePanel;
	/**
	 * Menu configuration window edit panel.
	 */
	private JPanel _editPanel;
	/**
	 * Menu configuration window project panel.
	 */
	private JPanel _projectPanel;
	/**
	 * Menu configuration window view panel.
	 */
	private JPanel _viewPanel;
	/**
	 * Menu configuration window configuration panel.
	 */
	private JPanel _configurationPanel;
	/**
	 * Menu configuration window language panel.
	 */
	private JPanel _languagePanel;
	/**
	 * Menu configuration window help panel.
	 */
	private JPanel _helpPanel;
	/**
	 * Menu configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * Menu configuration window accept button.
	 */
	private JButton _acceptButton;
	/**
	 * Menu configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * Menu configuration window save button.
	 */
	private JButton _saveButton;
	/**
	 * Menu configuration window select all button.
	 */
	private JButton _selectAllButton;
	/**
	 * Menu configuration window select none button.
	 */
	private JButton _selectNoneButton;
	/**
	 * Menu configuration window new file check box.
	 */
	private final JCheckBox _newFileCheckBox;
	/**
	 * Menu configuration window open file check box.
	 */
	private final JCheckBox _openFileCheckBox;
	/**
	 * Menu configuration window open all files check box.
	 */
	private final JCheckBox _openAllFilesCheckBox;
	/**
	 * Menu configuration window save file as check box.
	 */
	private final JCheckBox _saveFileAsCheckBox;
	/**
	 * Menu configuration window save file check box.
	 */
	private final JCheckBox _saveFileCheckBox;
	/**
	 * Menu configuration window save all files check box.
	 */
	private final JCheckBox _saveAllFilesCheckBox;
	/**
	 * Menu configuration window close file check box.
	 */
	private final JCheckBox _closeFileCheckBox;
	/**
	 * Menu configuration window close all files check box.
	 */
	private final JCheckBox _closeAllFilesCheckBox;
	/**
	 * Menu configuration window print file check box.
	 */
	private final JCheckBox _printFileCheckBox;
	/**
	 * Menu configuration window exit check box.
	 */
	private final JCheckBox _exitCheckBox;
	/**
	 * Menu configuration window undo check box.
	 */
	private final JCheckBox _undoCheckBox;
	/**
	 * Menu configuration window redo check box.
	 */
	private final JCheckBox _redoCheckBox;
	/**
	 * Menu configuration window copy check box.
	 */
	private final JCheckBox _copyCheckBox;
	/**
	 * Paste check box.
	 */
	private final JCheckBox _pasteCheckBox;
	/**
	 * Cut check box.
	 */
	private final JCheckBox _cutCheckBox;
	/**
	 * Select all files check box.
	 */
	private final JCheckBox _selectAllFilesCheckBox;
	/**
	 * Go to line check box.
	 */
	private final JCheckBox _goToLineCheckBox;
	/**
	 * Search check box.
	 */
	private final JCheckBox _searchCheckBox;
	/**
	 * Replace check box.
	 */
	private final JCheckBox _replaceCheckBox;
	/**
	 * New project check box.
	 */
	private final JCheckBox _newProjectCheckBox;
	/**
	 * Open project check box.
	 */
	private final JCheckBox _openProjectCheckBox;
	/**
	 * Save project check box.
	 */
	private final JCheckBox _saveProjectCheckBox;
	/**
	 * Close project check box.
	 */
	private final JCheckBox _closeProjectCheckBox;
	/**
	 * New project file check box.
	 */
	private final JCheckBox _newProjectFileCheckBox;
	/**
	 * Add file check box.
	 */
	private final JCheckBox _addFileCheckBox;
	/**
	 * Remove file check box.
	 */
	private final JCheckBox _removeFileCheckBox;
	/**
	 * Delete file check box.
	 */
	private final JCheckBox _deleteFileCheckBox;
	/**
	 * Add folder check box.
	 */
	private final JCheckBox _addFolderCheckBox;
	/**
	 * Remove folder check box.
	 */
	private final JCheckBox _removeFolderCheckBox;
	/**
	 * Compile check box.
	 */
	private final JCheckBox _compileCheckBox;
	/**
	 * Execute check box.
	 */
	private final JCheckBox _executeCheckBox;
	/**
	 * Set compilable file check box.
	 */
	private final JCheckBox _setCompilableFileCheckBox;
	/**
	 * Unset compilable check box.
	 */
	private final JCheckBox _unsetCompilableFileCheckBox;
	/**
	 * Set main file check box.
	 */
	private final JCheckBox _setMainFileCheckBox;
	/**
	 * Unset main file check box.
	 */
	private final JCheckBox _unsetMainFileCheckBox;
	/**
	 * Save project as check box.
	 */
	private final JCheckBox _saveProjectAsCheckBox;
	/**
	 * Show log tab check box.
	 */
	private final JCheckBox _showLogTabCheckBox;
	/**
	 * Show explorer panel check box.
	 */
	private final JCheckBox _showExplorerPanelCheckBox;
	/**
	 * Show output panel check box.
	 */
	private final JCheckBox _showOutputPanelCheckBox;
	/**
	 * New lexicon check box.
	 */
	private final JCheckBox _newLexiconCheckBox;
	/**
	 * Load lexicon check box.
	 */
	private final JCheckBox _loadLexiconCheckBox;
	/**
	 * Modify lexicon check box.
	 */
	private final JCheckBox _modifyLexiconCheckBox;
	/**
	 * Save lexicon check box.
	 */
	private final JCheckBox _saveLexiconCheckBox;
	/**
	 * Save lexicon as check box.
	 */
	private final JCheckBox _saveLexiconAsCheckBox;
	/**
	 * New grammar check box.
	 */
	private final JCheckBox _newGrammarCheckBox;
	/**
	 * Load grammar check box.
	 */
	private final JCheckBox _loadGrammarCheckBox;
	/**
	 * Modify grammar check box.
	 */
	private final JCheckBox _modifyGrammarCheckBox;
	/**
	 * Save grammar check box.
	 */
	private final JCheckBox _saveGrammarCheckBox;
	/**
	 * Save grammar as check box.
	 */
	private final JCheckBox _saveGrammarAsCheckBox;
	/**
	 * Set paths check box.
	 */
	private final JCheckBox _setPathsCheckBox;
	/**
	 * Auto analysis check box.
	 */
	private final JCheckBox _autoAnalysisCheckBox;
	/**
	 * Configure check box.
	 */
	private final JCheckBox _configureCheckBox;
	/**
	 * External command check box.
	 */
	private final JCheckBox _externalCommandCheckBox;
	/**
	 * Spanish check box.
	 */
	private final JCheckBox _spanishCheckBox;
	/**
	 * English check box.
	 */
	private final JCheckBox _englishCheckBox;
	/**
	 * New menu check box.
	 */
	private final JCheckBox _newMenuCheckBox;
	/**
	 * Load menu check box.
	 */
	private final JCheckBox _loadMenuCheckBox;
	/**
	 * Modify menu check box.
	 */
	private final JCheckBox _modifyMenu;
	/**
	 * Save menu check box.
	 */
	private final JCheckBox _saveMenuCheckBox;
	/**
	 * Save menu as check box.
	 */
	private final JCheckBox _saveMenuAsCheckBox;
	/**
	 * New tool bar check box.
	 */
	private final JCheckBox _newToolBarCheckBox;
	/**
	 * Load tool bar check box.
	 */
	private final JCheckBox _loadToolBarCheckBox;
	/**
	 * Modify tool bar check box.
	 */
	private final JCheckBox _modifyToolBarCheckBox;
	/**
	 * Save tool bar check box.
	 */
	private final JCheckBox _saveToolBarCheckBox;
	/**
	 * Save tool bar as check box.
	 */
	private final JCheckBox _saveToolBarAsCheckBox;
	/**
	 * Compiler check box.
	 */
	private final JCheckBox _compilerCheckBox;
	/**
	 * Show help check box.
	 */
	private final JCheckBox _showHelpCheckBox;
	/**
	 * Show about us check box.
	 */
	private final JCheckBox _showAboutUsCheckBox;
	/**
	 * Menu check box.
	 */
	private final JCheckBox _menuCheckBox;
	/**
	 * Tool bar check box.
	 */
	private final JCheckBox _toolBarCheckBox;
	/**
	 * Shell display options check box.
	 */
	private final JCheckBox _shellDisplayOptionsCheckBox;
	/**
	 * Flag that indicates if the changes are saved or not.
	 */
	private static boolean _changesAreSaved;

	/**
	 * Creates a new menu configuration window.
	 * 
	 * @param isModify
	 *            indicates if the window is used for modify the menu
	 *            configuration or for create it.
	 */
	public MenuConfigurationWindow(boolean isModify) {

		_changesAreSaved = true;

		// Gets the language
		AcideLanguage language = AcideLanguage.getInstance();
		try {
			language.getLanguage(ResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Updates the log
		AcideLog.getLog().info(labels.getString("s531"));

		// Sets the layout
		setLayout(new GridBagLayout());

		// Sets the window icon
		setIconImage(new ImageIcon(WINDOW_ICON).getImage());

		// Sets the title
		if (isModify) {

			String currentMenuConfiguration = null;

			try {

				// Gets the current menu configuration
				currentMenuConfiguration = ResourceManager.getInstance()
						.getProperty("currentMenuConfiguration");

				// Gets the name
				int index = currentMenuConfiguration.lastIndexOf("\\");
				if (index == -1)
					index = currentMenuConfiguration.lastIndexOf("/");
				currentMenuConfiguration = currentMenuConfiguration.substring(
						index + 1, currentMenuConfiguration.length() - 8);
			} catch (Exception exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());

				// Error message
				JOptionPane.showMessageDialog(null, exception.getMessage(),
						labels.getString("s295"), JOptionPane.ERROR_MESSAGE);
			}

			// Sets the window title
			setTitle(labels.getString("s532") + " - "
					+ currentMenuConfiguration);
		} else
			// Sets the window title
			setTitle(labels.getString("s298"));

		// FILE PANEL
		_filePanel = new JPanel();
		_filePanel.setLayout(new GridLayout(0, 3));
		_filePanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s500"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_newFileCheckBox = new JCheckBox(labels.getString("s8"));
		_openFileCheckBox = new JCheckBox(labels.getString("s9"));
		_openAllFilesCheckBox = new JCheckBox(labels.getString("s1004"));
		_saveFileAsCheckBox = new JCheckBox(labels.getString("s10"));
		_saveFileCheckBox = new JCheckBox(labels.getString("s617"));
		_saveAllFilesCheckBox = new JCheckBox(labels.getString("s217"));
		_closeFileCheckBox = new JCheckBox(labels.getString("s238"));
		_closeAllFilesCheckBox = new JCheckBox(labels.getString("s239"));
		_printFileCheckBox = new JCheckBox(labels.getString("s624"));
		_exitCheckBox = new JCheckBox(labels.getString("s13"));

		// EDIT PANEL
		_editPanel = new JPanel();
		_editPanel.setLayout(new GridLayout(0, 3));
		_editPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s501"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_undoCheckBox = new JCheckBox(labels.getString("s21"));
		_redoCheckBox = new JCheckBox(labels.getString("s22"));
		_copyCheckBox = new JCheckBox(labels.getString("s23"));
		_pasteCheckBox = new JCheckBox(labels.getString("s25"));
		_cutCheckBox = new JCheckBox(labels.getString("s24"));
		_selectAllFilesCheckBox = new JCheckBox(labels.getString("s190"));
		_goToLineCheckBox = new JCheckBox(labels.getString("s222"));
		_searchCheckBox = new JCheckBox(labels.getString("s26"));
		_replaceCheckBox = new JCheckBox(labels.getString("s27"));

		// PROJECT PANEL
		_projectPanel = new JPanel();
		_projectPanel.setLayout(new GridLayout(0, 3));
		_projectPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s502"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_newProjectCheckBox = new JCheckBox(labels.getString("s14"));
		_openProjectCheckBox = new JCheckBox(labels.getString("s15"));
		_saveProjectCheckBox = new JCheckBox(labels.getString("s16"));
		_closeProjectCheckBox = new JCheckBox(labels.getString("s228"));
		_newProjectFileCheckBox = new JCheckBox(labels.getString("s947"));
		_addFileCheckBox = new JCheckBox(labels.getString("s17"));
		_removeFileCheckBox = new JCheckBox(labels.getString("s218"));
		_deleteFileCheckBox = new JCheckBox(labels.getString("s950"));
		_addFolderCheckBox = new JCheckBox(labels.getString("s219"));
		_removeFolderCheckBox = new JCheckBox(labels.getString("s220"));
		_compileCheckBox = new JCheckBox(labels.getString("s18"));
		_executeCheckBox = new JCheckBox(labels.getString("s19"));
		_setCompilableFileCheckBox = new JCheckBox(labels.getString("s254"));
		_unsetCompilableFileCheckBox = new JCheckBox(labels.getString("s255"));
		_setMainFileCheckBox = new JCheckBox(labels.getString("s256"));
		_unsetMainFileCheckBox = new JCheckBox(labels.getString("s952"));
		_saveProjectAsCheckBox = new JCheckBox(labels.getString("s926"));

		// VIEW PANEL
		_viewPanel = new JPanel();
		_viewPanel.setLayout(new GridLayout(0, 3));
		_viewPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s503"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_showLogTabCheckBox = new JCheckBox(labels.getString("s28"));
		_showExplorerPanelCheckBox = new JCheckBox(labels.getString("s221"));
		_showOutputPanelCheckBox = new JCheckBox(labels.getString("s223"));

		// CONFIGURATION PANEL
		_configurationPanel = new JPanel();
		_configurationPanel.setLayout(new GridLayout(0, 5));
		_configurationPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s504"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// LANGUAGE PANEL
		_languagePanel = new JPanel();
		_languagePanel.setLayout(new GridLayout(0, 3));
		_languagePanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s505"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// LEXICAL SUBMENU
		_newLexiconCheckBox = new JCheckBox(labels.getString("s224") + " - "
				+ labels.getString("s249"));
		_loadLexiconCheckBox = new JCheckBox(labels.getString("s224") + " - "
				+ labels.getString("s35"));
		_modifyLexiconCheckBox = new JCheckBox(labels.getString("s224") + " - "
				+ labels.getString("s29"));
		_saveLexiconCheckBox = new JCheckBox(labels.getString("s224") + " - "
				+ labels.getString("s250"));
		_saveLexiconAsCheckBox = new JCheckBox(labels.getString("s224") + " - "
				+ labels.getString("s285"));

		// GRAMMAR SUBMENU
		_newGrammarCheckBox = new JCheckBox(labels.getString("s225") + " - "
				+ labels.getString("s30"));
		_loadGrammarCheckBox = new JCheckBox(labels.getString("s225") + " - "
				+ labels.getString("s226"));
		_modifyGrammarCheckBox = new JCheckBox(labels.getString("s225") + " - "
				+ labels.getString("s227"));
		_saveGrammarCheckBox = new JCheckBox(labels.getString("s225") + " - "
				+ labels.getString("s251"));
		_saveGrammarAsCheckBox = new JCheckBox(labels.getString("s225") + " - "
				+ labels.getString("s286"));
		_setPathsCheckBox = new JCheckBox(labels.getString("s225") + " - "
				+ labels.getString("s912"));
		_autoAnalysisCheckBox = new JCheckBox(labels.getString("s225") + " - "
				+ labels.getString("s911"));

		// OUTPUT SUBMENU
		_configureCheckBox = new JCheckBox(labels.getString("s332") + " - "
				+ labels.getString("s333"));
		_externalCommandCheckBox = new JCheckBox(labels.getString("s332")
				+ " - " + labels.getString("s341"));
	    _shellDisplayOptionsCheckBox = new JCheckBox(labels.getString("s332") + " - "
				+ labels.getString("s986"));
	    
		// LANGUAGE SUBMENU
		_spanishCheckBox = new JCheckBox(labels.getString("s6") + " - "
				+ labels.getString("s11"));
		_englishCheckBox = new JCheckBox(labels.getString("s6") + " - "
				+ labels.getString("s12"));
		
		// MENU SUBMENU
		_menuCheckBox = new JCheckBox();
		_newMenuCheckBox = new JCheckBox(labels.getString("s34") + " - "
				+ labels.getString("s275"));
		_loadMenuCheckBox = new JCheckBox(labels.getString("s34") + " - "
				+ labels.getString("s276"));
		_modifyMenu = new JCheckBox(labels.getString("s34") + " - "
				+ labels.getString("s277"));
		_saveMenuCheckBox = new JCheckBox(labels.getString("s34") + " - "
				+ labels.getString("s278"));
		_saveMenuAsCheckBox = new JCheckBox(labels.getString("s34") + " - "
				+ labels.getString("s279"));

		// TOOL BAR SUBMENU
		_toolBarCheckBox = new JCheckBox();
		_newToolBarCheckBox = new JCheckBox(labels.getString("s169") + " - "
				+ labels.getString("s280"));
		_loadToolBarCheckBox = new JCheckBox(labels.getString("s169") + " - "
				+ labels.getString("s281"));
		_modifyToolBarCheckBox = new JCheckBox(labels.getString("s169") + " - "
				+ labels.getString("s282"));
		_saveToolBarCheckBox = new JCheckBox(labels.getString("s169") + " - "
				+ labels.getString("s283"));
		_saveToolBarAsCheckBox = new JCheckBox(labels.getString("s169") + " - "
				+ labels.getString("s284"));

		// COMPILER
		_compilerCheckBox = new JCheckBox(labels.getString("s240"));

		// HELP PANEL
		_helpPanel = new JPanel();
		_helpPanel.setLayout(new GridLayout(0, 3));
		_helpPanel.setBorder(BorderFactory.createTitledBorder(null,
				labels.getString("s506"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		_showHelpCheckBox = new JCheckBox(labels.getString("s38"));
		_showAboutUsCheckBox = new JCheckBox(labels.getString("s39"));

		// BUTTON PANEL
		_buttonPanel = new JPanel();
		_buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		// ACCEPT BUTTON
		_acceptButton = new JButton(labels.getString("s507"));
		_acceptButton.setToolTipText(labels.getString("s508"));

		// CANCEL BUTTON
		_cancelButton = new JButton(labels.getString("s509"));
		_cancelButton.setToolTipText(labels.getString("s510"));

		// SAVE BUTTON
		_saveButton = new JButton(labels.getString("s513"));
		_saveButton.setToolTipText(labels.getString("s514"));

		// SELECT ALL BUTTON
		_selectAllButton = new JButton(labels.getString("s515"));
		_selectAllButton.setToolTipText(labels.getString("s516"));

		// SELECT NONE BUTTON
		_selectNoneButton = new JButton(labels.getString("s517"));
		_selectNoneButton.setToolTipText(labels.getString("s518"));

		// Modify menu always enabled
		_modifyMenu.setSelected(true);
		_modifyMenu.setEnabled(false);

		// If the window is for modifying the menu items
		if(isModify)
			// Sets the check boxes from the menu item list
			setCheckBoxesFromMenuItemList();
		
		// Listeners
		_acceptButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Creates the menu item information list
				ArrayList<MenuItemInformation> menuItemList = buildMenuItemInformationList();

				// Stores the new menu item information list
				MenuConfiguration.getInstance().setMenuElementList(menuItemList);
				
				// Saves the new configuration
				String newName = "./configuration/menu/newMenu.menuCfg";
				MenuConfiguration.getInstance().saveMenuConfigurationFile(newName, menuItemList);
				
				try {

					String previousMenu = ResourceManager.getInstance()
							.getProperty("currentMenuConfiguration");

					if (_changesAreSaved)

						// Updates the RESOURCE MANAGER
						ResourceManager.getInstance().setProperty(
								"previousMenuConfiguration", previousMenu);

					// Updates the RESOURCE MANAGER
					ResourceManager.getInstance().setProperty(
							"currentMenuConfiguration", newName);

					MainWindow.getInstance().getMenu().getConfiguration()
							.getMenu().getSaveMenu().setEnabled(false);
					MainWindow.getInstance().getMenu().buildMenu();
					MainWindow.getInstance().validate();
					MainWindow.getInstance().repaint();

					_changesAreSaved = false;

					// Closes the window
					dispose();

					// Updates the log
					AcideLog.getLog().info(labels.getString("s519"));

					// Not default project
					if (!MainWindow.getInstance().getProjectConfiguration()
							.isDefaultProject())
						// The project has been modified
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);

				} catch (Exception exception) {

					// Updates the log
					AcideLog.getLog().error(exception.getMessage());

					// Error message
					JOptionPane.showMessageDialog(null, exception.getMessage(),
							labels.getString("s292"), JOptionPane.ERROR_MESSAGE);
				}
			}
		});

		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Updates the log
				AcideLog.getLog().info(labels.getString("s520"));

				// Closes the window
				dispose();
			}
		});

		_saveButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Creates the menu item information list
				ArrayList<MenuItemInformation> menuItemList = buildMenuItemInformationList();

				// Creates the file chooser which only accepts
				// menuCfg extensions
				JFileChooser fileChooser = new JFileChooser();
				TextFileFilter filter = new TextFileFilter(labels
						.getString("s126"));
				filter.addExtension("menuCfg");
				fileChooser.setFileFilter(filter);

				String fileName = "";

				// Ask for saving
				int chosenOption = fileChooser.showSaveDialog(fileChooser);

				// If yes
				if (chosenOption == JFileChooser.APPROVE_OPTION) {

					// If the name of the file does not content menuCfg
					// adds to it
					fileName = fileChooser.getSelectedFile().getAbsolutePath();
					if (!fileName.endsWith(".menuCfg"))
						fileName += ".menuCfg";

					// Save the menu configuration
					MenuConfiguration.getInstance().saveMenuConfigurationFile(fileName,
							menuItemList);

					// Updates the RESOURCE MANAGER
					ResourceManager.getInstance().setProperty(
							"currentMenuConfiguration", fileName);
					_changesAreSaved = true;

					// Updates the log
					AcideLog.getLog().info(
							labels.getString("s528") + fileName
									+ labels.getString("s529"));
				} else if (chosenOption == JFileChooser.CANCEL_OPTION) {

					// Cancel selection
					fileChooser.cancelSelection();

					// Updates the log
					AcideLog.getLog().info(labels.getString("s527"));
				}
			}
		});
		_selectAllButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// FILE MENU
				_newFileCheckBox.setSelected(true);
				_openFileCheckBox.setSelected(true);
				_openAllFilesCheckBox.setSelected(true);
				_saveFileAsCheckBox.setSelected(true);
				_saveFileCheckBox.setSelected(true);
				_saveAllFilesCheckBox.setSelected(true);
				_closeFileCheckBox.setSelected(true);
				_closeAllFilesCheckBox.setSelected(true);
				_printFileCheckBox.setSelected(true);
				_exitCheckBox.setSelected(true);

				// EDIT MENU
				_undoCheckBox.setSelected(true);
				_redoCheckBox.setSelected(true);
				_copyCheckBox.setSelected(true);
				_pasteCheckBox.setSelected(true);
				_cutCheckBox.setSelected(true);
				_selectAllFilesCheckBox.setSelected(true);
				_goToLineCheckBox.setSelected(true);
				_searchCheckBox.setSelected(true);
				_replaceCheckBox.setSelected(true);

				// PROJECT MENU
				_newProjectCheckBox.setSelected(true);
				_openProjectCheckBox.setSelected(true);
				_saveProjectCheckBox.setSelected(true);
				_addFileCheckBox.setSelected(true);
				_removeFileCheckBox.setSelected(true);
				_addFolderCheckBox.setSelected(true);
				_removeFolderCheckBox.setSelected(true);
				_closeProjectCheckBox.setSelected(true);
				_compileCheckBox.setSelected(true);
				_executeCheckBox.setSelected(true);
				_setCompilableFileCheckBox.setSelected(true);
				_unsetCompilableFileCheckBox.setSelected(true);
				_setMainFileCheckBox.setSelected(true);
				_saveProjectAsCheckBox.setSelected(true);
				_newProjectFileCheckBox.setSelected(true);
				_deleteFileCheckBox.setSelected(true);
				_unsetMainFileCheckBox.setSelected(true);
				
				// VIEW MENU
				_showLogTabCheckBox.setSelected(true);
				_showExplorerPanelCheckBox.setSelected(true);
				_showOutputPanelCheckBox.setSelected(true);
				
				// LEXICON MENU
				_newLexiconCheckBox.setSelected(true);
				_loadLexiconCheckBox.setSelected(true);
				_modifyLexiconCheckBox.setSelected(true);
				_saveLexiconCheckBox.setSelected(true);
				_saveLexiconAsCheckBox.setSelected(true);
				
				// GRAMMAR MENU
				_newGrammarCheckBox.setSelected(true);
				_loadGrammarCheckBox.setSelected(true);
				_modifyGrammarCheckBox.setSelected(true);
				_saveGrammarCheckBox.setSelected(true);
				_saveGrammarAsCheckBox.setSelected(true);
				_setPathsCheckBox.setSelected(true);
				_autoAnalysisCheckBox.setSelected(true);
				
				// OUTPUT MENU
				_configureCheckBox.setSelected(true);
				_externalCommandCheckBox.setSelected(true);
				_shellDisplayOptionsCheckBox.setSelected(true);
				
				// LANGUAGE MENU
				_spanishCheckBox.setSelected(true);
				_englishCheckBox.setSelected(true);
				
				// CONFIGURATION MENU
				_menuCheckBox.setSelected(true);
				_toolBarCheckBox.setSelected(true);
				_compilerCheckBox.setSelected(true);
				
				// HELP MENU
				_showHelpCheckBox.setSelected(true);
				_showAboutUsCheckBox.setSelected(true);		
				
				// MENU MENU
				_newMenuCheckBox.setSelected(true);
				_loadMenuCheckBox.setSelected(true);
				_modifyMenu.setSelected(true);
				_saveMenuCheckBox.setSelected(true);
				_saveMenuAsCheckBox.setSelected(true);
				
				// TOOL BAR MENU
				_newToolBarCheckBox.setSelected(true);
				_loadToolBarCheckBox.setSelected(true);
				_modifyToolBarCheckBox.setSelected(true);
				_saveToolBarCheckBox.setSelected(true);
				_saveToolBarAsCheckBox.setSelected(true);
			}
		});

		_selectNoneButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// FILE MENU
				_newFileCheckBox.setSelected(false);
				_openFileCheckBox.setSelected(false);
				_openAllFilesCheckBox.setSelected(false);
				_saveFileAsCheckBox.setSelected(false);
				_saveFileCheckBox.setSelected(false);
				_saveAllFilesCheckBox.setSelected(false);
				_closeFileCheckBox.setSelected(false);
				_closeAllFilesCheckBox.setSelected(false);
				_printFileCheckBox.setSelected(false);
				_exitCheckBox.setSelected(false);

				// EDIT MENU
				_undoCheckBox.setSelected(false);
				_redoCheckBox.setSelected(false);
				_copyCheckBox.setSelected(false);
				_pasteCheckBox.setSelected(false);
				_cutCheckBox.setSelected(false);
				_selectAllFilesCheckBox.setSelected(false);
				_goToLineCheckBox.setSelected(false);
				_searchCheckBox.setSelected(false);
				_replaceCheckBox.setSelected(false);

				// PROJECT MENU
				_newProjectCheckBox.setSelected(false);
				_openProjectCheckBox.setSelected(false);
				_saveProjectCheckBox.setSelected(false);
				_addFileCheckBox.setSelected(false);
				_removeFileCheckBox.setSelected(false);
				_addFolderCheckBox.setSelected(false);
				_removeFolderCheckBox.setSelected(false);
				_closeProjectCheckBox.setSelected(false);
				_compileCheckBox.setSelected(false);
				_executeCheckBox.setSelected(false);
				_setCompilableFileCheckBox.setSelected(false);
				_unsetCompilableFileCheckBox.setSelected(true);
				_setMainFileCheckBox.setSelected(false);
				_saveProjectAsCheckBox.setSelected(false);
				_newProjectFileCheckBox.setSelected(false);
				_deleteFileCheckBox.setSelected(false);
				_unsetMainFileCheckBox.setSelected(false);
				
				// VIEW MENU
				_showLogTabCheckBox.setSelected(false);
				_showExplorerPanelCheckBox.setSelected(false);
				_showOutputPanelCheckBox.setSelected(false);
				
				// LEXICON MENU
				_newLexiconCheckBox.setSelected(false);
				_loadLexiconCheckBox.setSelected(false);
				_modifyLexiconCheckBox.setSelected(false);
				_saveLexiconCheckBox.setSelected(false);
				_saveLexiconAsCheckBox.setSelected(false);
				
				// GRAMMAR MENU
				_newGrammarCheckBox.setSelected(false);
				_loadGrammarCheckBox.setSelected(false);
				_modifyGrammarCheckBox.setSelected(false);
				_saveGrammarCheckBox.setSelected(false);
				_saveGrammarAsCheckBox.setSelected(false);
				_setPathsCheckBox.setSelected(false);
				_autoAnalysisCheckBox.setSelected(false);
				
				// OUTPUT MENU
				_configureCheckBox.setSelected(false);
				_externalCommandCheckBox.setSelected(false);
				_shellDisplayOptionsCheckBox.setSelected(false);
				
				// LANGUAGE MENU
				_spanishCheckBox.setSelected(false);
				_englishCheckBox.setSelected(false);
				
				// CONFIGURATION MENU
				_menuCheckBox.setSelected(true);
				_toolBarCheckBox.setSelected(false);
				_compilerCheckBox.setSelected(false);
				
				// HELP MENU
				_showHelpCheckBox.setSelected(false);
				_showAboutUsCheckBox.setSelected(false);		
				
				// MENU MENU
				_newMenuCheckBox.setSelected(false);
				_loadMenuCheckBox.setSelected(false);
				_modifyMenu.setSelected(true);
				_saveMenuCheckBox.setSelected(false);
				_saveMenuAsCheckBox.setSelected(false);
				
				// TOOL BAR MENU
				_newToolBarCheckBox.setSelected(false);
				_loadToolBarCheckBox.setSelected(false);
				_modifyToolBarCheckBox.setSelected(false);
				_saveToolBarCheckBox.setSelected(false);
				_saveToolBarAsCheckBox.setSelected(false);
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
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Closes the window
				dispose();
			}
		};

		// Adds a escape key action to the cancel button
		_cancelButton.registerKeyboardAction(escPressed, "EscapeKey",
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0, true),
				JComponent.WHEN_IN_FOCUSED_WINDOW);

		// ADDS THE COMPONENTS WITH THE LAYOUT TO THE WINDOW
		_filePanel.add(_newFileCheckBox);
		_filePanel.add(_openFileCheckBox);
		_filePanel.add(_openAllFilesCheckBox);
		_filePanel.add(_saveFileAsCheckBox);
		_filePanel.add(_saveFileCheckBox);
		_filePanel.add(_saveAllFilesCheckBox);
		_filePanel.add(_closeFileCheckBox);
		_filePanel.add(_closeAllFilesCheckBox);
		_filePanel.add(_printFileCheckBox);
		_filePanel.add(_exitCheckBox);

		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(0, 0, 0, 0);
		constraints.gridwidth = 6;
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		add(_filePanel, constraints);

		_editPanel.add(_undoCheckBox);
		_editPanel.add(_redoCheckBox);
		_editPanel.add(_copyCheckBox);
		_editPanel.add(_pasteCheckBox);
		_editPanel.add(_cutCheckBox);
		_editPanel.add(_selectAllFilesCheckBox);
		_editPanel.add(_goToLineCheckBox);
		_editPanel.add(_searchCheckBox);
		_editPanel.add(_replaceCheckBox);
		constraints.gridy = 1;
		add(_editPanel, constraints);

		_projectPanel.add(_newProjectCheckBox);
		_projectPanel.add(_openProjectCheckBox);
		_projectPanel.add(_closeProjectCheckBox);
		_projectPanel.add(_saveProjectCheckBox);
		_projectPanel.add(_saveProjectAsCheckBox);
		_projectPanel.add(_newProjectFileCheckBox);
		_projectPanel.add(_addFileCheckBox);
		_projectPanel.add(_removeFileCheckBox);
		_projectPanel.add(_deleteFileCheckBox);
		_projectPanel.add(_addFolderCheckBox);
		_projectPanel.add(_removeFolderCheckBox);
		_projectPanel.add(_compileCheckBox);
		_projectPanel.add(_executeCheckBox);
		_projectPanel.add(_setCompilableFileCheckBox);
		_projectPanel.add(_unsetCompilableFileCheckBox);
		_projectPanel.add(_setMainFileCheckBox);
		_projectPanel.add(_unsetMainFileCheckBox);
		constraints.gridy = 2;
		add(_projectPanel, constraints);

		_viewPanel.add(_showLogTabCheckBox);
		_viewPanel.add(_showExplorerPanelCheckBox);
		_viewPanel.add(_showOutputPanelCheckBox);
		constraints.gridy = 3;
		add(_viewPanel, constraints);

		_configurationPanel.add(_compilerCheckBox);
		_configurationPanel.add(_newLexiconCheckBox);
		_configurationPanel.add(_newGrammarCheckBox);
		_configurationPanel.add(_newMenuCheckBox);
		_configurationPanel.add(_newToolBarCheckBox);
		_configurationPanel.add(_configureCheckBox);
		_configurationPanel.add(_loadLexiconCheckBox);
		_configurationPanel.add(_loadGrammarCheckBox);
		_configurationPanel.add(_loadMenuCheckBox);
		_configurationPanel.add(_loadToolBarCheckBox);
		_configurationPanel.add(_externalCommandCheckBox);
		_configurationPanel.add(_modifyLexiconCheckBox);
		_configurationPanel.add(_modifyGrammarCheckBox);
		_configurationPanel.add(_modifyMenu);
		_configurationPanel.add(_modifyToolBarCheckBox);
		_configurationPanel.add(_spanishCheckBox);
		_configurationPanel.add(_saveLexiconCheckBox);
		_configurationPanel.add(_saveGrammarCheckBox);
		_configurationPanel.add(_saveMenuCheckBox);
		_configurationPanel.add(_saveToolBarCheckBox);
		_configurationPanel.add(_englishCheckBox);
		_configurationPanel.add(_saveLexiconAsCheckBox);
		_configurationPanel.add(_saveGrammarAsCheckBox);
		_configurationPanel.add(_saveMenuAsCheckBox);
		_configurationPanel.add(_saveToolBarAsCheckBox);
		_configurationPanel.add(new JLabel(""));
		_configurationPanel.add(new JLabel(""));
		_configurationPanel.add(_setPathsCheckBox);
		_configurationPanel.add(_shellDisplayOptionsCheckBox);
		_configurationPanel.add(new JLabel(""));
		_configurationPanel.add(new JLabel(""));
		_configurationPanel.add(new JLabel(""));
		_configurationPanel.add(_autoAnalysisCheckBox);
		constraints.gridy = 4;
		add(_configurationPanel, constraints);

		_helpPanel.add(_showHelpCheckBox);
		_helpPanel.add(_showAboutUsCheckBox);
		constraints.gridy = 5;
		add(_helpPanel, constraints);

		_buttonPanel.add(_selectAllButton);
		_buttonPanel.add(_selectNoneButton);
		_buttonPanel.add(_acceptButton);
		_buttonPanel.add(_cancelButton);
		constraints.gridy = 6;
		constraints.insets = new Insets(0, 0, 0, 0);

		add(_buttonPanel, constraints);
		setVisible(true);
		setResizable(false);
		pack();
		setLocationRelativeTo(null);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s530"));
	}

	/**
	 * Returns the changes are saved flag.
	 * 
	 * @return the changes saved flag.
	 */
	public static boolean getChangesAreSaved() {
		return _changesAreSaved;
	}

	/**
	 * Sets a new value to the changes are saved flag.
	 * 
	 * @param changesAreSaved
	 *            new value to set.
	 */
	public static void setChangesAreSaved(boolean changesAreSaved) {
		_changesAreSaved = changesAreSaved;
	}

	/**
	 * Sets the check box values from the menu item list of the menu configuration.
	 */
	public void setCheckBoxesFromMenuItemList() {
		
		// FILE MENU
		_newFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(FileMenu.NEW_FILE_NAME));
		_openFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(FileMenu.OPEN_FILE_NAME));
		_openAllFilesCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(FileMenu.OPEN_ALL_FILES_NAME));
		_saveFileAsCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(FileMenu.SAVE_FILE_AS_NAME));
		_saveFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(FileMenu.SAVE_FILE_NAME));
		_saveAllFilesCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(FileMenu.SAVE_ALL_FILES_NAME));
		_closeFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(FileMenu.CLOSE_FILE_NAME));
		_closeAllFilesCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(FileMenu.CLOSE_ALL_FILES_NAME));
		_printFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(FileMenu.PRINT_FILE_NAME));
		_exitCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(FileMenu.EXIT_NAME));

		// EDIT FILE
		_undoCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(EditMenu.UNDO_NAME));
		_redoCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(EditMenu.REDO_NAME));
		_copyCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(EditMenu.COPY_NAME));
		_pasteCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(EditMenu.PASTE_NAME));
		_cutCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(EditMenu.CUT_NAME));
		_selectAllFilesCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(EditMenu.SELECT_ALL_FILES_NAME));
		_goToLineCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(EditMenu.GO_TO_LINE_NAME));
		_searchCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(EditMenu.SEARCH_NAME));
		_replaceCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(EditMenu.REPLACE_NAME));

		// PROJECT MENU
		_newProjectCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.NEW_PROJECT_NAME));
		_openProjectCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.OPEN_PROJECT_NAME));
		_saveProjectCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.SAVE_PROJECT_NAME));
		_addFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.ADD_FILE_NAME));
		_removeFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.REMOVE_FILE_NAME));
		_addFolderCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.ADD_FOLDER_NAME));
		_removeFolderCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.REMOVE_FOLDER_NAME));
		_compileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.COMPILE_NAME));
		_executeCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.EXECUTE_NAME));
		_saveProjectAsCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.SAVE_PROJECT_AS_NAME));
		_newProjectFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.NEW_PROJECT_FILE_NAME));
		_closeProjectCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.CLOSE_PROJECT_NAME));
		_deleteFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.DELETE_FILE_NAME));
		_setCompilableFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.SET_COMPILABLE_FILE_NAME));
		_unsetCompilableFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.UNSET_COMPILABLE_FILE_NAME));
		_setMainFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.SET_MAIN_FILE_NAME));
		_unsetMainFileCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ProjectMenu.UNSET_MAIN_FILE_NAME));
		
		// VIEW MENU
		_showLogTabCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ViewMenu.SHOW_LOG_TAB_NAME));
		_showExplorerPanelCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ViewMenu.SHOW_EXPLORER_PANEL_NAME));
		_showOutputPanelCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ViewMenu.SHOW_OUTPUT_PANEL_NAME));
		
		// LEXICON MENU
		_loadLexiconCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(LexiconMenu.LOAD_LEXICON_NAME));
		_modifyLexiconCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(LexiconMenu.MODIFY_LEXICON_NAME));
		_newLexiconCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(LexiconMenu.NEW_LEXICON_NAME));
		_saveLexiconCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(LexiconMenu.SAVE_LEXICON_NAME));
		_saveLexiconAsCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(LexiconMenu.SAVE_LEXICON_AS_NAME));
		
		// GRAMMAR MENU
		_newGrammarCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(GrammarMenu.NEW_GRAMMAR_NAME));
		_loadGrammarCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(GrammarMenu.LOAD_GRAMMAR_NAME));
		_modifyGrammarCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(GrammarMenu.MODIFY_GRAMMAR_NAME));
		_saveGrammarCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(GrammarMenu.SAVE_GRAMMAR_NAME));
		_saveGrammarAsCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(GrammarMenu.SAVE_GRAMMAR_AS_NAME));
		_setPathsCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(GrammarMenu.SET_PATHS_NAME));
		_autoAnalysisCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(GrammarMenu.AUTO_ANALYSIS_NAME));
		
		// OUTPUT MENU
		_configureCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(OutputMenu.CONFIGURE_NAME));
		_externalCommandCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(OutputMenu.EXTERNAL_COMMAND_NAME));
		_shellDisplayOptionsCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(OutputMenu.SHELL_DISPLAY_OPTIONS_NAME));
		
		// LANGUAGE MENU
		_spanishCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(LanguageMenu.SPANISH_NAME));
		_englishCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(LanguageMenu.ENGLISH_NAME));
		
		// CONFIGURATION MENU
		_menuCheckBox.setSelected(true);
		_toolBarCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ConfigurationMenu.TOOLBAR_NAME));
		_compilerCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ConfigurationMenu.COMPILER_NAME));
		
		// HELP MENU
		_showHelpCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(HelpMenu.SHOW_HELP_NAME));
		_showAboutUsCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(HelpMenu.SHOW_ABOUT_US_NAME));
				
		// MENU MENU
		_newMenuCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(MenuMenu.NEW_MENU_NAME));
		_loadMenuCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(MenuMenu.LOAD_MENU_NAME));
		_modifyMenu.setSelected(true);
		_saveMenuCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(MenuMenu.SAVE_MENU_NAME));
		_saveMenuAsCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(MenuMenu.SAVE_MENU_AS_NAME));
		
		// TOOL BAR MENU
		_newToolBarCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ToolBarMenu.NEW_TOOLBAR_NAME));
		_loadToolBarCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ToolBarMenu.LOAD_TOOLBAR_NAME));
		_modifyToolBarCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ToolBarMenu.MODIFY_TOOLBAR_NAME));
		_saveToolBarCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ToolBarMenu.SAVE_TOOLBAR_NAME));
		_saveToolBarAsCheckBox.setSelected(MenuConfiguration
				.getInstance().getIsDisplayed(ToolBarMenu.SAVE_TOOLBAR_AS_NAME));
	}
	
	/**
	 * Builds the menu item information list with the check box values.
	 * 
	 * @return the menu item information list with the check box values.
	 */
	public ArrayList<MenuItemInformation> buildMenuItemInformationList() {
		
		ArrayList<MenuItemInformation> menuItemList = new ArrayList<MenuItemInformation>();
		
		// FILE MENU
		addFileMenuInformation(menuItemList);

		// EDIT MENU
		addEditMenuInformation(menuItemList);

		// PROJECT MENU
		addProjectMenuInformation(menuItemList);

		// CONFIGURATION MENU
		addConfigurationMenuInformation(menuItemList);

		// LEXICON MENU
		addLexiconMenuInformation(menuItemList);

		// GRAMMAR MENU
		AddGrammarMenuInformation(menuItemList);

		// LANGUAGE
		addLanguageMenuInformation(menuItemList);

		// MENU
		addMenuMenuInformation(menuItemList);

		// OUTPUT
		addOutputMenuInformation(menuItemList);

		// TOOLBAR
		addToolBarMenuInformation(menuItemList);

		// VIEW MENU
		addViewMenuInformation(menuItemList);

		// HELP MENU
		addHelpMenuInformation(menuItemList);
		
		return menuItemList;
	}
	
	/**
	 * Adds the project menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addProjectMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {

		menuItemList.add(new MenuItemInformation(ProjectMenu.NEW_PROJECT_NAME,
				_newProjectCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(ProjectMenu.OPEN_PROJECT_NAME,
				_openProjectCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(ProjectMenu.SAVE_PROJECT_NAME,
				_saveProjectCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(ProjectMenu.ADD_FILE_NAME,
				_addFileCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ProjectMenu.CLOSE_PROJECT_NAME, _closeProjectCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(ProjectMenu.REMOVE_FILE_NAME,
				_removeFileCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(ProjectMenu.ADD_FOLDER_NAME,
				_addFolderCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ProjectMenu.REMOVE_FOLDER_NAME, _removeFolderCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(ProjectMenu.COMPILE_NAME,
				_compileCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(ProjectMenu.EXECUTE_NAME,
				_executeCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ProjectMenu.SAVE_PROJECT_AS_NAME, _saveProjectAsCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ProjectMenu.NEW_PROJECT_FILE_NAME, _newProjectFileCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(ProjectMenu.DELETE_FILE_NAME,
				_deleteFileCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ProjectMenu.SET_COMPILABLE_FILE_NAME,
				_setCompilableFileCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ProjectMenu.UNSET_COMPILABLE_FILE_NAME,
				_unsetCompilableFileCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ProjectMenu.SET_MAIN_FILE_NAME, _setMainFileCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ProjectMenu.UNSET_MAIN_FILE_NAME, _unsetMainFileCheckBox
						.isSelected()));
	}

	/**
	 * Adds the configuration menu information to the menu item list, based on
	 * the window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addConfigurationMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {

		menuItemList.add(new MenuItemInformation(ConfigurationMenu.MENU_NAME,
				true));
		menuItemList.add(new MenuItemInformation(
				ConfigurationMenu.TOOLBAR_NAME, _toolBarCheckBox.isSelected()));
		menuItemList
				.add(new MenuItemInformation(ConfigurationMenu.COMPILER_NAME,
						_compilerCheckBox.isSelected()));
	}

	/**
	 * Adds the lexicon menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addLexiconMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {

		menuItemList.add(new MenuItemInformation(LexiconMenu.LOAD_LEXICON_NAME,
				_loadLexiconCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				LexiconMenu.MODIFY_LEXICON_NAME, _modifyLexiconCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(LexiconMenu.NEW_LEXICON_NAME,
				_newLexiconCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(LexiconMenu.SAVE_LEXICON_NAME,
				_saveLexiconCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				LexiconMenu.SAVE_LEXICON_AS_NAME, _saveLexiconAsCheckBox
						.isSelected()));
	}

	/**
	 * Adds the grammar menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void AddGrammarMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {

		menuItemList.add(new MenuItemInformation(GrammarMenu.LOAD_GRAMMAR_NAME,
				_newGrammarCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(GrammarMenu.LOAD_GRAMMAR_NAME,
				_loadGrammarCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				GrammarMenu.MODIFY_GRAMMAR_NAME, _modifyGrammarCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(GrammarMenu.SAVE_GRAMMAR_NAME,
				_saveGrammarCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				GrammarMenu.SAVE_GRAMMAR_AS_NAME, _saveGrammarAsCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(GrammarMenu.SET_PATHS_NAME,
				_setPathsCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				GrammarMenu.AUTO_ANALYSIS_NAME, _autoAnalysisCheckBox
						.isSelected()));
	}

	/**
	 * Adds the menu menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addMenuMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {
		menuItemList.add(new MenuItemInformation(MenuMenu.NEW_MENU_NAME,
				_newMenuCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(MenuMenu.LOAD_MENU_NAME,
				_loadMenuCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(MenuMenu.MODIFY_MENU_NAME,
				true));
		menuItemList.add(new MenuItemInformation(MenuMenu.SAVE_MENU_NAME,
				_saveMenuCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(MenuMenu.SAVE_MENU_AS_NAME,
				_saveMenuAsCheckBox.isSelected()));
	}

	/**
	 * Adds the language menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addLanguageMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {
		menuItemList.add(new MenuItemInformation(LanguageMenu.ENGLISH_NAME,
				_englishCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(LanguageMenu.SPANISH_NAME,
				_spanishCheckBox.isSelected()));
	}

	/**
	 * Adds the output menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addOutputMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {
		menuItemList.add(new MenuItemInformation(OutputMenu.CONFIGURE_NAME,
				_configureCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				OutputMenu.EXTERNAL_COMMAND_NAME, _externalCommandCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(
				OutputMenu.SHELL_DISPLAY_OPTIONS_NAME, _shellDisplayOptionsCheckBox
						.isSelected()));
	}

	/**
	 * Adds the tool bar menu information to the menu item list, based on the
	 * window check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addToolBarMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {
		menuItemList.add(new MenuItemInformation(ToolBarMenu.NEW_TOOLBAR_NAME,
				_newToolBarCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(ToolBarMenu.LOAD_TOOLBAR_NAME,
				_loadToolBarCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ToolBarMenu.MODIFY_TOOLBAR_NAME, _modifyToolBarCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(ToolBarMenu.SAVE_TOOLBAR_NAME,
				_saveToolBarCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ToolBarMenu.SAVE_TOOLBAR_AS_NAME, _saveToolBarAsCheckBox
						.isSelected()));
	}

	/**
	 * Adds the view menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addViewMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {
		menuItemList.add(new MenuItemInformation(ViewMenu.SHOW_LOG_TAB_NAME,
				_showLogTabCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ViewMenu.SHOW_EXPLORER_PANEL_NAME, _showExplorerPanelCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(
				ViewMenu.SHOW_OUTPUT_PANEL_NAME, _showOutputPanelCheckBox
						.isSelected()));
	}

	/**
	 * Adds the help menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addHelpMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {
		menuItemList.add(new MenuItemInformation(HelpMenu.SHOW_HELP_NAME,
				_showHelpCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(HelpMenu.SHOW_ABOUT_US_NAME,
				_showAboutUsCheckBox.isSelected()));
	}

	/**
	 * Adds the edit menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addEditMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {

		menuItemList.add(new MenuItemInformation(EditMenu.UNDO_NAME,
				_undoCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(EditMenu.REDO_NAME,
				_redoCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(EditMenu.COPY_NAME,
				_copyCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(EditMenu.PASTE_NAME,
				_pasteCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(EditMenu.CUT_NAME,
				_cutCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(
				EditMenu.SELECT_ALL_FILES_NAME, _selectAllFilesCheckBox
						.isSelected()));
		menuItemList.add(new MenuItemInformation(EditMenu.GO_TO_LINE_NAME,
				_goToLineCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(EditMenu.SEARCH_NAME,
				_searchCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(EditMenu.REPLACE_NAME,
				_replaceCheckBox.isSelected()));
	}

	/**
	 * Adds the file menu information to the menu item list, based on the window
	 * check box values.
	 * 
	 * @param menuItemList
	 *            menu item list to be generated.
	 */
	public void addFileMenuInformation(
			ArrayList<MenuItemInformation> menuItemList) {

		menuItemList.add(new MenuItemInformation(FileMenu.NEW_FILE_NAME,
				_newFileCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(FileMenu.OPEN_FILE_NAME,
				_openFileCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(FileMenu.SAVE_FILE_AS_NAME,
				_saveFileAsCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(FileMenu.SAVE_FILE_NAME,
				_saveFileCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(FileMenu.SAVE_ALL_FILES_NAME,
				_saveAllFilesCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(FileMenu.CLOSE_FILE_NAME,
				_closeFileCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(FileMenu.CLOSE_ALL_FILES_NAME,
				_closeAllFilesCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(FileMenu.PRINT_FILE_NAME,
				_printFileCheckBox.isSelected()));
		menuItemList.add(new MenuItemInformation(FileMenu.EXIT_NAME,
				_exitCheckBox.isSelected()));
	}
}
