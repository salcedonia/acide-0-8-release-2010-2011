package gui.toolBarPanel;

import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;

import language.AcideLanguage;
import operations.log.AcideLog;
import resources.ResourceManager;

/************************************************************************
 * Static tool bar of ACIDE - A Configurable IDE. Its buttons execute internal
 * commands to ACIDE - A Configurable IDE.
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
 * @see JToolBar
 ***********************************************************************/
public class StaticToolBar extends JToolBar {

	/**
	 * Class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image of the new file button.
	 */
	private static final String NEW_FILE = "./resources/icons/toolBar/newFile.png";
	/**
	 * Image of the open file button.
	 */
	private static final String OPEN_FILE = "./resources/icons/toolBar/openFile.png";
	/**
	 * Image of the save file button.
	 */
	private static final String SAVE_FILE = "./resources/icons/toolBar/saveFile.png";
	/**
	 * Image of the save all files button.
	 */
	private static final String SAVE_ALL_FILES = "./resources/icons/toolBar/saveAllFiles.png";
	/**
	 * Image of the new project button.
	 */
	private static final String NEW_PROJECT = "./resources/icons/toolBar/newProject.png";
	/**
	 * Image of the open project button.
	 */
	private static final String OPEN_PROJECT = "./resources/icons/toolBar/openProject.png";
	/**
	 * Image of the save project button.
	 */
	private static final String SAVE_PROJECT = "./resources/icons/toolBar/saveProject.png";
	/**
	 * New file button.
	 */
	private static JButton _btnNewFile;
	/**
	 * Open file button.
	 */
	private static JButton _btnOpenFile;
	/**
	 * Save file button.
	 */
	private static JButton _btnSaveFile;
	/**
	 * Save all files button.
	 */
	private static JButton _btnSaveAllFiles;
	/**
	 * New project button.
	 */
	private static JButton _btnNewProject;
	/**
	 * Open project button.
	 */
	private static JButton _btnOpenProject;
	/**
	 * Save project button.
	 */
	private static JButton _btnSaveProject;

	/**
	 * Builds the tool bar of the application.
	 * 
	 * @return the tool bar of the application.
	 */
	public StaticToolBar() {

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
		AcideLog.getLog().info(labels.getString("s102"));

		// NEW FILE BUTTON
		_btnNewFile = new JButton(new ImageIcon(NEW_FILE));
		_btnNewFile.setToolTipText(labels.getString("s103"));
		_btnNewFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// NEW FILE
				MainWindow.getInstance().getMenu().getFile().getNewFile()
						.doClick();

				// As a new tab is opened in the editor, there is no need to
				// set the focus on the selected editor
			}
		});

		// OPEN FILE BUTTON
		_btnOpenFile = new JButton(new ImageIcon(OPEN_FILE));
		_btnOpenFile.setToolTipText(labels.getString("s106"));
		_btnOpenFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// OPEN FILE
				MainWindow.getInstance().getMenu().getFile().getOpenFile()
						.doClick();

				// As a new tab is opened in the editor, there is no need
				// to set the focus on the selected editor
			}
		});

		// SAVE FILE BUTTON
		_btnSaveFile = new JButton(new ImageIcon(SAVE_FILE));
		_btnSaveFile.setToolTipText(labels.getString("s114"));
		_btnSaveFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// SAVE FILE
				MainWindow.getInstance().getMenu().getFile().getSaveFile()
						.setEnabled(true);
				MainWindow.getInstance().getMenu().getFile().getSaveFile()
						.doClick();

				SwingUtilities.invokeLater(new Runnable() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see java.lang.Runnable#run()
					 */
					@Override
					public void run() {
						
						// Sets the focus in the active editor
						int selectedEditorIndex = MainWindow.getInstance()
								.getFileEditorManager().getSelectedFileEditorPanelIndex();

						// Sets the selected editor
						if (selectedEditorIndex != -1){
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.setSelectedIndex(
											MainWindow.getInstance()
													.getProjectConfiguration()
													.getSelectedEditorIndex());
						
							MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getCaret().setVisible(true);
						}	
					}
				});
			}
		});

		// SAVE ALL FILES BUTTON
		_btnSaveAllFiles = new JButton(new ImageIcon(SAVE_ALL_FILES));
		_btnSaveAllFiles.setToolTipText(labels.getString("s229"));
		_btnSaveAllFiles.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// SAVE ALL FILES
				MainWindow.getInstance().getMenu().getFile().getSaveAllFiles()
						.setEnabled(true);
				MainWindow.getInstance().getMenu().getFile().getSaveAllFiles()
						.doClick();

				SwingUtilities.invokeLater(new Runnable() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see java.lang.Runnable#run()
					 */
					@Override
					public void run() {
						
						// Sets the focus in the active editor
						int selectedEditorIndex = MainWindow.getInstance()
								.getFileEditorManager().getSelectedFileEditorPanelIndex();

						// Sets the selected editor
						if (selectedEditorIndex != -1){
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.setSelectedIndex(
											MainWindow.getInstance()
													.getProjectConfiguration()
													.getSelectedEditorIndex());
							MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getCaret().setVisible(true);
						}
					}
				});
			}
		});

		// NEW PROJECT BUTTON
		_btnNewProject = new JButton(new ImageIcon(NEW_PROJECT));
		_btnNewProject.setToolTipText(labels.getString("s122"));
		_btnNewProject.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// NEW PROJECT
				MainWindow.getInstance().getMenu().getProject().getNewProject()
						.doClick();

				SwingUtilities.invokeLater(new Runnable() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see java.lang.Runnable#run()
					 */
					@Override
					public void run() {
						
						// Sets the focus in the active editor
						int selectedEditorIndex = MainWindow.getInstance()
								.getFileEditorManager().getSelectedFileEditorPanelIndex();

						// Sets the selected editor
						if (selectedEditorIndex != -1){
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.setSelectedIndex(
											MainWindow.getInstance()
													.getProjectConfiguration()
													.getSelectedEditorIndex());
							MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getCaret().setVisible(true);
						}
					}
				});
			}
		});

		// OPEN PROJECT BUTTON
		_btnOpenProject = new JButton(new ImageIcon(OPEN_PROJECT));
		_btnOpenProject.setToolTipText(labels.getString("s123"));
		_btnOpenProject.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// OPEN PROJECT
				MainWindow.getInstance().getMenu().getProject()
						.getOpenProject().doClick();
				
				// As a new tab is opened in the editor, there is no need to
				// set the focus on the selected editor
			}
		});

		// SAVE PROJECT BUTTON
		_btnSaveProject = new JButton(new ImageIcon(SAVE_PROJECT));
		_btnSaveProject.setToolTipText(labels.getString("s124"));
		_btnSaveProject.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// SAVE PROJECT
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().setEnabled(true);
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().doClick();

				SwingUtilities.invokeLater(new Runnable() {
					/*
					 * (non-Javadoc)
					 * 
					 * @see java.lang.Runnable#run()
					 */
					@Override
					public void run() {
						
						// Sets the focus in the active editor
						int selectedEditorIndex = MainWindow.getInstance()
								.getFileEditorManager().getSelectedFileEditorPanelIndex();

						// Sets the selected editor
						if (selectedEditorIndex != -1){
							MainWindow
									.getInstance()
									.getFileEditorManager()
									.getTabbedPane()
									.setSelectedIndex(
											MainWindow.getInstance()
													.getProjectConfiguration()
													.getSelectedEditorIndex());
							MainWindow.getInstance()
							.getFileEditorManager().getFileEditorPanelAt(selectedEditorIndex)
							.getActiveTextEditionArea().getCaret().setVisible(true);
						}
					}
				});
			}
		});

		/*
		 * JButton analyzeSintButton = new JButton(labels.getString("s206"));
		 * analyzeSintButton.addActionListener(new ActionListener() { public
		 * void actionPerformed(ActionEvent e) { /* boolean recException =
		 * false; boolean tokException = false; boolean noException = true;
		 */
		// Ventana v = Ventana.getInstance();
		// String text = v.getCreadorEditor().dameEditorI(
		// v.getCreadorEditor().getEditorSeleccionado())
		// .getEditor().getText();
		// Prueba.analyze(text);
		// C:\jdk1.5.0_05\jre1.5.0_05\bin\java -jar acide.jar principal.Acide
		/*
		 * try { String currentGrammar =
		 * PropertiesManager.getProperty("currentGrammar"); String javaPath =
		 * PropertiesManager.getProperty("javaPath");
		 * //Runtime.getRuntime().exec("\"" + javaPath + "\" -jar \"" +
		 * currentGrammar + "\" operaciones.sintacticas.Analyzer");
		 * ProcessThread p = new ProcessThread(); Output s = new Output(false);
		 * p.executeCommand("cmd",".","\"" + javaPath + "\" -jar \"" +
		 * currentGrammar + "\" operaciones.sintacticas.Analyzer","exit",s);
		 * System.out.println("\"" + javaPath + "\" -jar \"" + currentGrammar +
		 * "\" operaciones.sintacticas.Analyzer"); JFrame output = new
		 * JFrame(labels.getString("s946")); output.add(s); output.setSize(new
		 * Dimension(300,400)); output.setVisible(true); } catch (Exception e1)
		 * {
		 * JOptionPane.showMessageDialog(null,"Error analyzer","Error",JOptionPane
		 * .ERROR_MESSAGE); } } });
		 */
		removeAll();

		add(_btnNewFile);
		add(_btnOpenFile);
		add(_btnSaveFile);
		add(_btnSaveAllFiles);
		add(_btnNewProject);
		add(_btnOpenProject);
		add(_btnSaveProject);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s125"));
	}
}
