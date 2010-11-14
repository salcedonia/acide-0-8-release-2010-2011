package gui.toolBar;

import javax.swing.*;

import language.Language;
import operations.log.Log;

import es.configuration.toolBar.ModifiableCommand;
import es.configuration.toolBar.ModifiableCommandList;

import properties.PropertiesManager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;

import gui.mainWindow.MainWindow;

/************************************************************************																
 * Tool bar of ACIDE - A Configurable IDE										
 *					
 * 		   <p>															
 *         <b>ACIDE - A Configurable IDE</b>							
 *         </p>															
 *         <p>															
 *         <b>Official web site:</b> @see http://acide.sourceforge.net	
 *         </p>   
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
 * @see JPanel																													
 ***********************************************************************/
public class ToolBar extends JPanel {

	/**
	 * Class serial version UID
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Image of the new file button
	 */
	private static final String NEW_FILE = "./resources/icons/toolBar/newFile.png";
	/**
	 * Image of the open file button
	 */
	private static final String OPEN_FILE = "./resources/icons/toolBar/openFile.png";
	/**
	 * Image of the save file button
	 */
	private static final String SAVE_FILE = "./resources/icons/toolBar/saveFile.png";
	/**
	 * Image of the save all files button
	 */
	private static final String SAVE_ALL_FILES = "./resources/icons/toolBar/saveAllFiles.png";
	/**
	 * Image of the new project button
	 */
	private static final String NEW_PROJECT = "./resources/icons/toolBar/newProject.png";
	/**
	 * Image of the open project button
	 */
	private static final String OPEN_PROJECT = "./resources/icons/toolBar/openProject.png";
	/**
	 * Image of the save project button
	 */
	private static final String SAVE_PROJECT = "./resources/icons/toolBar/saveProject.png";
	/**
	 * Command tool bar
	 */
	private static JToolBar _toolBar = new JToolBar();
	/**
	 * New file button
	 */
	private static JButton _btnNewFile;
	/**
	 * Open file button
	 */
	private static JButton _btnOpenFile;
	/**
	 * Save file button
	 */
	private static JButton _btnSaveFile;
	/**
	 * Save all files button
	 */
	private static JButton _btnSaveAllFiles;
	/**
	 * New project button
	 */
	private static JButton _btnNewProject;
	/**
	 * Open project button
	 */
	private static JButton _btnOpenProject;
	/**
	 * Save project button
	 */
	private static JButton _btnSaveProject;

	/**
	 * Builds the tool bar of the application
	 * 
	 * @return the tool bar of the application
	 */
	public static JToolBar buildStaticToolBar() {

		// Gets the language
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();
		
		// Updates the log
		Log.getLog().info(labels.getString("s102"));

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
				MainWindow.getInstance().getMenu().getFile().getNewFile()
						.doClick();
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
				MainWindow.getInstance().getMenu().getFile().getOpenFile()
						.doClick();
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
				MainWindow.getInstance().getMenu().getFile().getSaveFile()
						.setEnabled(true);
				MainWindow.getInstance().getMenu().getFile().getSaveFile()
						.doClick();
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
				MainWindow.getInstance().getMenu().getFile().getSaveAllFiles()
						.setEnabled(true);
				MainWindow.getInstance().getMenu().getFile().getSaveAllFiles()
						.doClick();
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
				MainWindow.getInstance().getMenu().getProject().getNewProject()
						.doClick();
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
				MainWindow.getInstance().getMenu().getProject()
						.getOpenProject().doClick();
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
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().setEnabled(true);
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().doClick();
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
		_toolBar.removeAll();
		_toolBar.add(_btnNewFile);
		_toolBar.add(_btnOpenFile);
		_toolBar.add(_btnSaveFile);
		_toolBar.add(_btnSaveAllFiles);
		_toolBar.add(_btnNewProject);
		_toolBar.add(_btnOpenProject);
		_toolBar.add(_btnSaveProject);
		Log.getLog().info(labels.getString("s125"));
		return _toolBar;
	}

	/**
	 * Builds the modifiable command tool bar
	 * 
	 * @return the modifiable command tool bar
	 * @see JToolBar
	 */
	public static JToolBar buildModifiableToolBar() {

		// Gets the language
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			Log.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		// Updates the log
		Log.getLog().info(labels.getString("s130"));
		_toolBar.addSeparator();

		JButton button;

		for (int i = 0; i < ModifiableCommandList.getSize(); i++) {

			final ModifiableCommand command = ModifiableCommandList
					.getCommandAt(i);

			// SET THE ICON OF THE COMMAND
			if (command.getHasIcon())
				button = new JButton(new ImageIcon(command.getIcon()));
			else
			// SET THE NAME OF THE COMMAND
			if (!(command.getName().equals("")))
				button = new JButton(command.getName());
			else
				button = new JButton((new Integer(i + 1)).toString());

			// SET TOOL TIP TEXT OF THE COMMAND
			if (!(command.getHelpText().equals("")))
				button.setToolTipText(command.getHelpText());

			// ADD THE BUTTON
			_toolBar.add(button);

			// SET THE ACTION LISTENER
			button.addActionListener(new ActionListener() {
				/*
				 * (non-Javadoc)
				 * 
				 * @see
				 * java.awt.event.ActionListener#actionPerformed(java.awt.event
				 * .ActionEvent)
				 */
				@Override
				public void actionPerformed(ActionEvent actionEvent) {
					MainWindow.getInstance().getOutput()
							.executeCommand(command.getCommand());
				}
			});
		}

		// Updates the log
		Log.getLog().info(labels.getString("s131"));
		return _toolBar;
	}
}
