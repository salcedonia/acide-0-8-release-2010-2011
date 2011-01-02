/*
 * ACIDE - A Configurable IDE
 * Official web site: http://acide.sourceforge.net
 * 
 * Copyright (C) 2007-2011  
 * Authors:
 * 		- Fernando Sáenz Pérez (Team Director).
 *      - Version from 0.1 to 0.6:
 *      	- Diego Cardiel Freire.
 *			- Juan José Ortiz Sánchez.
 *          - Delfín Rupérez Cañas.
 *      - Version 0.7:
 *          - Miguel Martín Lázaro.
 *      - Version 0.8:
 *      	- Javier Salcedo Gómez.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package gui.toolBarPanel.staticToolBar;

import gui.mainWindow.MainWindow;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.ResourceBundle;

import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.SwingUtilities;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE static tool bar. 
 * 
 * Its buttons execute internal commands to ACIDE - A Configurable IDE.
 * 
 * @version 0.8
 * @see ArrayList
 */
public class AcideStaticToolBar extends ArrayList<Component> {

	/**
	 * ACIDE - A Configurable IDE static tool bar class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE static tool bar unique class instance.
	 */
	private static AcideStaticToolBar _instance;
	/**
	 * ACIDE - A Configurable IDE static tool bar new file button image icon.
	 */
	private static final String NEW_FILE = "./resources/icons/toolBar/newFile.png";
	/**
	 * ACIDE - A Configurable IDE static tool bar open file button image icon.
	 */
	private static final String OPEN_FILE = "./resources/icons/toolBar/openFile.png";
	/**
	 * ACIDE - A Configurable IDE static tool bar save file button image icon.
	 */
	private static final String SAVE_FILE = "./resources/icons/toolBar/saveFile.png";
	/**
	 * ACIDE - A Configurable IDE static tool bar save all files button image icon.
	 */
	private static final String SAVE_ALL_FILES = "./resources/icons/toolBar/saveAllFiles.png";
	/**
	 * ACIDE - A Configurable IDE static tool bar new project button image icon.
	 */
	private static final String NEW_PROJECT = "./resources/icons/toolBar/newProject.png";
	/**
	 * ACIDE - A Configurable IDE static tool bar open project button image icon.
	 */
	private static final String OPEN_PROJECT = "./resources/icons/toolBar/openProject.png";
	/**
	 * ACIDE - A Configurable IDE static tool bar save project button image icon.
	 */
	private static final String SAVE_PROJECT = "./resources/icons/toolBar/saveProject.png";
	/**
	 * ACIDE - A Configurable IDE static tool bar new file button.
	 */
	private static JButton _btnNewFile;
	/**
	 * ACIDE - A Configurable IDE static tool bar open file button.
	 */
	private static JButton _btnOpenFile;
	/**
	 * ACIDE - A Configurable IDE static tool bar save file button.
	 */
	private static JButton _btnSaveFile;
	/**
	 * ACIDE - A Configurable IDE static tool bar save all files button.
	 */
	private static JButton _btnSaveAllFiles;
	/**
	 * ACIDE - A Configurable IDE static tool bar new project button.
	 */
	private static JButton _btnNewProject;
	/**
	 * ACIDE - A Configurable IDE static tool bar open project button.
	 */
	private static JButton _btnOpenProject;
	/**
	 * ACIDE - A Configurable IDE static tool bar save project button.
	 */
	private static JButton _btnSaveProject;

	/**
	 * Creates a new ACIDE - A Configurable IDE static tool bar.
	 */
	public AcideStaticToolBar() {
		super();
	}

	/**
	 * Returns the ACIDE - A Configurable IDE static tool bar unique class instance.
	 * 
	 * @return the ACIDE - A Configurable IDE static tool bar unique class instance.
	 */
	public static AcideStaticToolBar getInstance() {

		if (_instance == null)
			_instance = new AcideStaticToolBar();
		return _instance;
	}

	/**
	 * Builds the ACIDE - A Configurable IDE static tool bar button list.
	 * 
	 * @return the ACIDE - A Configurable IDE static tool bar button list.
	 */
	public AcideStaticToolBar build() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
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

				// Does the new file menu item action
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

				// Does the open file menu item action
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

				// Enables the save file menu item
				MainWindow.getInstance().getMenu().getFile().getSaveFile()
						.setEnabled(true);
				
				// Does the save menu item action
				MainWindow.getInstance().getMenu().getFile().getSaveFile()
						.doClick();

				// Puts the focus on the selected file editor panel
				putFocusOnSelectedFileEditorPanel();
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

				// Enables the save all files menu item
				MainWindow.getInstance().getMenu().getFile().getSaveAllFiles()
						.setEnabled(true);
				
				// Does the save all files menu item action
				MainWindow.getInstance().getMenu().getFile().getSaveAllFiles()
						.doClick();

				// Puts the focus on the selected file editor panel
				putFocusOnSelectedFileEditorPanel();
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

				// Does the new project menu item action
				MainWindow.getInstance().getMenu().getProject().getNewProject()
						.doClick();

				// Puts the focus on the selected file editor panel
				putFocusOnSelectedFileEditorPanel();
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

				// Does the open project menu item action
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

				// Enables the save project menu item
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().setEnabled(true);
				
				// Does the save project menu item action
				MainWindow.getInstance().getMenu().getProject()
						.getSaveProject().doClick();

				// Puts the focus on the selected file editor panel
				putFocusOnSelectedFileEditorPanel();
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

		// Removes all the components
		clear();

		// Adds the static buttons
		add(Box.createRigidArea(new Dimension(5, 5)));
		add(_btnNewFile);
		add(Box.createRigidArea(new Dimension(5, 5)));
		add(_btnOpenFile);
		add(Box.createRigidArea(new Dimension(5, 5)));
		add(_btnSaveFile);
		add(Box.createRigidArea(new Dimension(5, 5)));
		add(_btnSaveAllFiles);
		add(Box.createRigidArea(new Dimension(5, 5)));
		add(_btnNewProject);
		add(Box.createRigidArea(new Dimension(5, 5)));
		add(_btnOpenProject);
		add(Box.createRigidArea(new Dimension(5, 5)));
		add(_btnSaveProject);
		add(Box.createRigidArea(new Dimension(5, 5)));

		// Updates the log
		AcideLog.getLog().info(labels.getString("s125"));

		return this;
	}
	
	/**
	 * Puts the focus on the selected file editor panel in the main window.
	 */
	private void putFocusOnSelectedFileEditorPanel(){
		
		// Gets the selected editor index
		final int selectedFileEditorPanelIndex = MainWindow.getInstance()
				.getFileEditorManager()
				.getSelectedFileEditorPanelIndex();

		// If there are opened tabs
		if (selectedFileEditorPanelIndex != -1) {
			
			// Updates the selected file editor panel
			MainWindow
					.getInstance()
					.getFileEditorManager()
					.getTabbedPane()
					.setSelectedIndex(selectedFileEditorPanelIndex);

			// Puts the focus on the active text edition area 
			MainWindow
					.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt(selectedFileEditorPanelIndex).getActiveTextEditionArea().requestFocusInWindow();

			SwingUtilities.invokeLater(new Runnable() {
				/*
				 * (non-Javadoc)
				 * 
				 * @see java.lang.Runnable#run()
				 */
				@Override
				public void run() {

					// Sets the focus in the active editor
					MainWindow
							.getInstance()
							.getFileEditorManager()
							.getFileEditorPanelAt(
									selectedFileEditorPanelIndex)
							.getActiveTextEditionArea().getCaret()
							.setVisible(true);
				}
			});
		}
	}
}
