package gui.toolBarButton;

import javax.swing.*;

import language.Language;
import operations.configuration.EditableToolBarCommand;
import operations.configuration.EditableToolBarCommandList;
import operations.log.Log;

import org.apache.log4j.Logger;

import properties.PropertiesManager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ResourceBundle;
import gui.MainWindow;

/**
 * 
 */
public class ToolBarCommand extends JPanel {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private static final String NEW_FILE = "./resources/icons/toolBar/newFile.png";
	/**
	 * 
	 */
	private static final String OPEN_FILE = "./resources/icons/toolBar/openFile.png";
	/**
	 * 
	 */
	private static final String SAVE_FILE = "./resources/icons/toolBar/saveFile.png";
	/**
	 * 
	 */
	private static final String SAVE_ALL_FILES = "./resources/icons/toolBar/saveAllFiles.png";
	/**
	 * 
	 */
	private static final String NEW_PROJECT = "./resources/icons/toolBar/newProject.png";
	/**
	 * 
	 */
	private static final String OPEN_PROJECT = "./resources/icons/toolBar/openProject.png";
	/**
	 * 
	 */
	private static final String SAVE_PROJECT = "./resources/icons/toolBar/saveProject.png";
	
	/**
	 * 
	 */
	private static Logger _logger = Log.getLog();
	/**
	 * 
	 */
	private static JToolBar _toolBar = new JToolBar();
	/**
	 * 
	 */
	private static JButton _btnNewFile;
	/**
	 * 
	 */
	private static JButton _btnOpenFile;
	/**
	 * 
	 */
	private static JButton _btnSaveFile;
	/**
	 * 
	 */
	private static JButton _btnSaveAllFiles;
	/**
	 * 
	 */
	private static JButton _btnNewProject;
	/**
	 * 
	 */
	private static JButton _btnOpenProject;
	/**
	 * 
	 */
	private static JButton _btnSaveProject;
	
	/**
	 *
	 * 
	 * @return 
	 */
	public static JToolBar buildToolBar() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		
		final ResourceBundle labels = language.getLabels();
		_logger.info(labels.getString("s102"));
		
		_btnNewFile = new JButton(new ImageIcon(NEW_FILE));
		_btnNewFile.setToolTipText(labels.getString("s103"));
		_btnNewFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getFile().getNewFile().doClick();
			}
		});
		
		_btnOpenFile = new JButton(new ImageIcon(OPEN_FILE));
		_btnOpenFile.setToolTipText(labels.getString("s106"));
		_btnOpenFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getFile().getOpenFile().doClick();
			}
		});
		
		_btnSaveFile = new JButton(new ImageIcon(SAVE_FILE));
		_btnSaveFile.setToolTipText(labels.getString("s114"));
		_btnSaveFile.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getFile().getSaveFile().setEnabled(true);
				mainWindow.getMenu().getFile().getSaveFile().doClick();
			}
		});
		
		_btnSaveAllFiles = new JButton(
				new ImageIcon(SAVE_ALL_FILES));
		_btnSaveAllFiles.setToolTipText(labels.getString("s229"));
		_btnSaveAllFiles.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getFile().getSaveAllFiles().setEnabled(true);
				mainWindow.getMenu().getFile().getSaveAllFiles().doClick();
			}
		});
		
		_btnNewProject = new JButton(new ImageIcon(NEW_PROJECT));
		_btnNewProject.setToolTipText(labels.getString("s122"));
		_btnNewProject.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getNewProject().doClick();
			}
		});
		
		_btnOpenProject = new JButton(new ImageIcon(OPEN_PROJECT));
		_btnOpenProject.setToolTipText(labels.getString("s123"));
		_btnOpenProject.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getOpenProject().doClick();
			}
		});
		
		_btnSaveProject = new JButton(new ImageIcon(SAVE_PROJECT));
		_btnSaveProject.setToolTipText(labels.getString("s124"));
		_btnSaveProject.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			public void actionPerformed(ActionEvent e) {			
				MainWindow mainWindow = MainWindow.getInstance();
				mainWindow.getMenu().getProject().getSaveProject().setEnabled(true);
				mainWindow.getMenu().getProject().getSaveProject().doClick();
			}
		});
/*		JButton analyzeSintButton = new JButton(labels.getString("s206"));
		analyzeSintButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				/*
				 * boolean recException = false; boolean tokException = false;
				 * boolean noException = true;
				 */
//				Ventana v = Ventana.getInstance();
//				String text = v.getCreadorEditor().dameEditorI(
//						v.getCreadorEditor().getEditorSeleccionado())
//						.getEditor().getText();
//				Prueba.analyze(text);
				//C:\jdk1.5.0_05\jre1.5.0_05\bin\java -jar acide.jar principal.Acide
	/*			try {
					String currentGrammar = PropertiesManager.getProperty("currentGrammar");
					String javaPath = PropertiesManager.getProperty("javaPath");
					//Runtime.getRuntime().exec("\"" + javaPath + "\" -jar \"" + currentGrammar + "\" operaciones.sintacticas.Analyzer");
					ProcessThread p = new ProcessThread();
					Output s = new Output(false);
					p.executeCommand("cmd",".","\"" + javaPath + "\" -jar \"" + currentGrammar + "\" operaciones.sintacticas.Analyzer","exit",s);
					System.out.println("\"" + javaPath + "\" -jar \"" + currentGrammar + "\" operaciones.sintacticas.Analyzer");
					JFrame output = new JFrame(labels.getString("s946"));
					output.add(s);
					output.setSize(new Dimension(300,400));
					output.setVisible(true);
				}
				catch (Exception e1) {
					JOptionPane.showMessageDialog(null,"Error analyzer","Error",JOptionPane.ERROR_MESSAGE);
				}
			}
		});*/
		_toolBar.removeAll();
		_toolBar.add(_btnNewFile);
		_toolBar.add(_btnOpenFile);
		_toolBar.add(_btnSaveFile);
		_toolBar.add(_btnSaveAllFiles);
		_toolBar.add(_btnNewProject);
		_toolBar.add(_btnOpenProject);
		_toolBar.add(_btnSaveProject);
		_logger.info(labels.getString("s125"));
		return _toolBar;
	}

	/**
	 *
	 */
	public static JToolBar buildEditableToolBar() {
		
		Language language = Language.getInstance();
		try {
			language.getLanguage(Integer.parseInt(PropertiesManager
					.getProperty("language")));
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		ResourceBundle labels = language.getLabels();
		_logger.info(labels.getString("s130"));
		_toolBar.addSeparator();
		
		JButton button;
		for (int i = 0; i < EditableToolBarCommandList.getSize(); i++) {
			final EditableToolBarCommand icon = EditableToolBarCommandList.getCommandAt(i);
			if (icon.getHasIcon()) button = new JButton(new ImageIcon(icon
					.getIcon()));
			else if (!(icon.getName().equals(""))) button = new JButton(icon
					.getName());
			else button = new JButton((new Integer(i + 1)).toString());
			if (!(icon.getHelpText().equals(""))) button.setToolTipText(icon
					.getHelpText());
			_toolBar.add(button);
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					MainWindow.getInstance().getOutput().executeCommand(
							icon.getCommand());
				}
			});
		}
		_logger.info(labels.getString("s131"));
		return _toolBar;
	}
}
