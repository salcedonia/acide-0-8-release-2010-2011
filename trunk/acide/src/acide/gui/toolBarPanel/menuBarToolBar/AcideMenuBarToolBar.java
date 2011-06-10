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
package acide.gui.toolBarPanel.menuBarToolBar;

import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;

import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JTextPane;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE menu bar tool bar.
 * 
 * Its buttons execute internal commands to ACIDE - A Configurable IDE.
 * 
 * @version 0.8
 * @see ArrayList
 */
public class AcideMenuBarToolBar extends ArrayList<Component> {

	/**
	 * ACIDE - A Configurable IDE menu bar tool bar class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar new file button image icon.
	 */
	private static final String NEW_FILE = "./resources/icons/toolBar/newFile.png";
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar open file button image icon.
	 */
	private static final String OPEN_FILE = "./resources/icons/toolBar/openFile.png";
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar save file button image icon.
	 */
	private static final String SAVE_FILE = "./resources/icons/toolBar/saveFile.png";
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar save all files button image
	 * icon.
	 */
	private static final String SAVE_ALL_FILES = "./resources/icons/toolBar/saveAllFiles.png";
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar new project button image
	 * icon.
	 */
	private static final String NEW_PROJECT = "./resources/icons/toolBar/newProject.png";
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar open project button image
	 * icon.
	 */
	private static final String OPEN_PROJECT = "./resources/icons/toolBar/openProject.png";
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar save project button image
	 * icon.
	 */
	private static final String SAVE_PROJECT = "./resources/icons/toolBar/saveProject.png";
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar new file button.
	 */
	private static JButton _newFileButton;
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar open file button.
	 */
	private static JButton _openFileButton;
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar save file button.
	 */
	private static JButton _saveFileButton;
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar save all files button.
	 */
	private static JButton _saveAllFilesButton;
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar new project button.
	 */
	private static JButton _newProjectButton;
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar open project button.
	 */
	private static JButton _openProjectButton;
	/**
	 * ACIDE - A Configurable IDE menu bar tool bar save project button.
	 */
	private static JButton _saveProjectButton;

	/**
	 * Creates a new ACIDE - A Configurable IDE menu bar tool bar.
	 */
	public AcideMenuBarToolBar() {
		super();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE menu bar tool bar button list.
	 * 
	 * @return the ACIDE - A Configurable IDE menu bar tool bar button list.
	 */
	public AcideMenuBarToolBar build() {

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s102"));

		// Builds the menu bar tool bar components
		buildComponents();

		// Sets the listeners of menu bar tool bar components
		setListeners();

		// Adds the components
		addComponents();

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s125"));

		return this;
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE menu bar tool bar.
	 */
	private void addComponents() {

		// Removes all the components from the menu bar tool bar
		clear();

		// Adds a separator to the menu bar tool bar
		add(Box.createRigidArea(new Dimension(5, 5)));

		// Adds the new file button to the menu bar tool bar
		add(_newFileButton);

		// Adds a separator to the menu bar tool bar
		add(Box.createRigidArea(new Dimension(5, 5)));

		// Adds the open file button to the menu bar tool bar
		add(_openFileButton);

		// Adds a separator to the menu bar tool bar
		add(Box.createRigidArea(new Dimension(5, 5)));

		// Adds the save file button to the menu bar tool bar
		add(_saveFileButton);

		// Adds a separator to the menu bar tool bar
		add(Box.createRigidArea(new Dimension(5, 5)));

		// Adds the save all files button to the menu bar tool bar
		add(_saveAllFilesButton);

		// Adds a separator to the menu bar tool bar
		add(Box.createRigidArea(new Dimension(5, 5)));

		// Adds the new project button to the menu bar tool bar
		add(_newProjectButton);

		// Adds a separator to the menu bar tool bar
		add(Box.createRigidArea(new Dimension(5, 5)));

		// Adds the open project button to the menu bar tool bar
		add(_openProjectButton);

		// Adds a separator to the menu bar tool bar
		add(Box.createRigidArea(new Dimension(5, 5)));

		// Adds the save project button to the menu bar tool bar
		add(_saveProjectButton);

		// Adds a separator to the menu bar tool bar
		add(Box.createRigidArea(new Dimension(5, 5)));
	}

	/**
	 * Builds the ACIDE - A Configurable IDE menu bar tool bar components.
	 */
	private void buildComponents() {

		// Creates the new file button
		_newFileButton = new JButton(new ImageIcon(NEW_FILE));

		// Sets the new file button tool tip text
		_newFileButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s103"));

		// Sets its border painted as false
		_newFileButton.setBorderPainted(false);

		// Creates the open file button
		_openFileButton = new JButton(new ImageIcon(OPEN_FILE));

		// Sets the open file button tool tip text
		_openFileButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s106"));

		// Sets its border painted as false
		_openFileButton.setBorderPainted(false);

		// Creates the save file button
		_saveFileButton = new JButton(new ImageIcon(SAVE_FILE));

		// Sets the save file button tool tip text
		_saveFileButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s114"));

		// Disables the save file button
		_saveFileButton.setEnabled(false);

		// Sets its border painted as false
		_saveFileButton.setBorderPainted(false);

		// Creates the save all files button
		_saveAllFilesButton = new JButton(new ImageIcon(SAVE_ALL_FILES));

		// Sets the save all files button tool tip text
		_saveAllFilesButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s229"));

		// Disables the save all files button
		_saveAllFilesButton.setEnabled(false);

		// Sets its border painted as false
		_saveAllFilesButton.setBorderPainted(false);

		// Creates the new project button
		_newProjectButton = new JButton(new ImageIcon(NEW_PROJECT));

		// Sets the new project button tool tip text
		_newProjectButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s122"));

		// Sets its border painted as false
		_newProjectButton.setBorderPainted(false);

		// Creates the open project button
		_openProjectButton = new JButton(new ImageIcon(OPEN_PROJECT));

		// Sets the open project button tool tip text
		_openProjectButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s123"));

		// Sets its border painted as false
		_openProjectButton.setBorderPainted(false);

		// Creates the save project button
		_saveProjectButton = new JButton(new ImageIcon(SAVE_PROJECT));

		// Sets the save project button tool tip text
		_saveProjectButton.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s124"));

		// Sets its border painted as false
		_saveProjectButton.setBorderPainted(false);

		// Disables the save project button
		_saveProjectButton.setEnabled(false);

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
	}

	/**
	 * Sets the listeners of the ACIDE - A Configurable IDE menu bar tool bar
	 * components.
	 */
	private void setListeners() {

		// Sets the new file button mouse listener
		_newFileButton.addMouseListener(new NewFileButtonMouseListener());

		// Sets the open file button mouse listener
		_openFileButton.addMouseListener(new OpenFileButtonMouseListener());

		// Sets the save file button mouse listener
		_saveFileButton.addMouseListener(new SaveFileButtonMouseListener());

		// Sets the save all files button mouse listener
		_saveAllFilesButton
				.addMouseListener(new SaveAllFilesButtonMouseListener());

		// Sets the new project button mouse listener
		_newProjectButton.addMouseListener(new NewProjectButtonMouseListener());

		// Sets the open project button mouse listener
		_openProjectButton
				.addMouseListener(new OpenProjectButtonMouseListener());

		// Sets the save project button mouse listener
		_saveProjectButton
				.addMouseListener(new SaveProjectButtonMouseListener());
	}

	/**
	 * <p>
	 * Updates the state of the buttons in the menu bar tool bar depending on
	 * the state of the selected file editor panel.
	 * </p>
	 * <p>
	 * The save file will be enabled the the selected file in the file editor is
	 * modified and disabled in other case.
	 * </p>
	 * <p>
	 * The save all files will be enabled there is, at least, one modified file
	 * editor and disabled in other case.
	 * </p>
	 */
	public void updateStateOfFileButtons() {

		// IMPORTANT: This if avoids an index out of bounds exception when the
		// workbench is
		// being loading

		// If the workbench configuration has been loaded already
		if (AcideWorkbenchConfiguration.getInstance().isWorkbenchLoaded()) {

			// If there are opened file editors
			if (AcideMainWindow.getInstance().getFileEditorManager()
					.getNumberOfFileEditorPanels() > 0) {

				// If the editor is modified or is the NEW FILE
				if (AcideMainWindow.getInstance().getFileEditorManager()
						.isRedButton()
						|| AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getSelectedFileEditorPanel()
								.getAbsolutePath()
								.equals(AcideLanguageManager.getInstance()
										.getLabels().getString("s79")))

					// Enables the save file button in the menu bar tool bar
					_saveFileButton.setEnabled(true);
				else

					// Disables the save file button in the menu bar tool bar
					_saveFileButton.setEnabled(false);

				// Checks the opened file editors
				boolean isAnyModified = false;
				for (int index = 0; index < AcideMainWindow.getInstance()
						.getFileEditorManager().getNumberOfFileEditorPanels(); index++) {

					// If any of them is modified
					if (AcideMainWindow.getInstance().getFileEditorManager()
							.isRedButton(index))
						isAnyModified = true;
				}

				// Disables or enables the save all files button in the menu bar
				// tool bar
				_saveAllFilesButton.setEnabled(isAnyModified);
			}
		}
	}

	/**
	 * <p>
	 * Updates the state of the save project button.
	 * </p>
	 * <p>
	 * The button is enabled when it is not the default project and the project
	 * has been modified.
	 * </p>
	 * The method is invoked each time the isModified flag in the project
	 * configuration is updated. </p>
	 * 
	 * @param isProjectModified
	 *            indicates if the project has been modified or not.
	 */
	public void updateSaveProjectButtonState(boolean isProjectModified) {

		// Enables or disables the save project button
		_saveProjectButton.setEnabled(!AcideProjectConfiguration.getInstance()
				.isDefaultProject() && isProjectModified);
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu bar tool bar save file
	 * button.
	 * 
	 * @return the ACIDE - A Configurable IDE menu bar tool bar save file
	 *         button.
	 */
	public JButton getSaveFileButton() {
		return _saveFileButton;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu bar tool bar save all files
	 * button.
	 * 
	 * @return the ACIDE - A Configurable IDE menu bar tool bar save all files
	 *         button.
	 */
	public JButton getSaveAllFilesButton() {
		return _saveAllFilesButton;
	}

	/**
	 * Returns the ACIDE - A Configurable IDE menu bar tool bar save project
	 * button.
	 * 
	 * @return the ACIDE - A Configurable IDE menu bar tool bar save project
	 *         button.
	 */
	public JButton getSaveProjectButton() {
		return _saveProjectButton;
	}

	/**
	 * ACIDE - A Configurable IDE menu bar tool bar new file button mouse
	 * listener
	 * 
	 * @version 0.8
	 * @see MouseListener
	 */
	class NewFileButtonMouseListener implements MouseListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent
		 * )
		 */
		@Override
		public void mouseReleased(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent )
		 */
		@Override
		public void mousePressed(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseExited(MouseEvent mouseEvent) {
			_newFileButton.setBorderPainted(false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseEntered(MouseEvent mouseEvent) {
			_newFileButton.setBorderPainted(true);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseClicked(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/**
		 * Dispatches the mouse event.
		 * 
		 * @param mouseEvent
		 *            mouse event.
		 */
		public void dispatchEvent(MouseEvent mouseEvent) {

			// Does the new file menu item action
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getNewFileMenuItem().doClick();

			/*
			 * IMPORTANT: If the last component in focus was a file editor, it
			 * will cause troubles with the focus.
			 */

			// If the last component on focus was not a text pane
			if (!(AcideMainWindow.getInstance().getLastElementOnFocus() instanceof JTextPane))
				// Sets the focus in the last element focus in window (console
				// or file editor)
				AcideMainWindow.getInstance().getLastElementOnFocus()
						.requestFocusInWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu bar tool bar open file button mouse
	 * listener
	 * 
	 * @version 0.8
	 * @see MouseListener
	 */
	class OpenFileButtonMouseListener implements MouseListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent
		 * )
		 */
		@Override
		public void mouseReleased(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent )
		 */
		@Override
		public void mousePressed(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseExited(MouseEvent mouseEvent) {
			_openFileButton.setBorderPainted(false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseEntered(MouseEvent mouseEvent) {
			_openFileButton.setBorderPainted(true);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseClicked(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/**
		 * Dispatches the mouse event.
		 * 
		 * @param mouseEvent
		 *            mouse event.
		 */
		public void dispatchEvent(MouseEvent mouseEvent) {

			// Does the open file menu item action
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getOpenFileMenuItem().doClick();

			/*
			 * IMPORTANT: If the last component in focus was a file editor, it
			 * will cause troubles with the focus.
			 */

			// If the last component on focus was not a text pane
			if (!(AcideMainWindow.getInstance().getLastElementOnFocus() instanceof JTextPane))
				// Sets the focus in the last element focus in window (console
				// or file editor)
				AcideMainWindow.getInstance().getLastElementOnFocus()
						.requestFocusInWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu bar tool bar save file button mouse
	 * listener
	 * 
	 * @version 0.8
	 * @see MouseListener
	 */
	class SaveFileButtonMouseListener implements MouseListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent
		 * )
		 */
		@Override
		public void mouseReleased(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent )
		 */
		@Override
		public void mousePressed(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseExited(MouseEvent mouseEvent) {
			if (_saveFileButton.isEnabled())
				_saveFileButton.setBorderPainted(false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseEntered(MouseEvent mouseEvent) {
			if (_saveFileButton.isEnabled())
				_saveFileButton.setBorderPainted(true);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseClicked(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/**
		 * Dispatches the mouse event.
		 * 
		 * @param mouseEvent
		 *            mouse event.
		 */
		public void dispatchEvent(MouseEvent mouseEvent) {

			// Enables the save file menu item
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getSaveFileMenuItem().setEnabled(true);

			// Does the save menu item action
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getSaveFileMenuItem().doClick();

			/*
			 * IMPORTANT: If the last component in focus was a file editor, it
			 * will cause troubles with the focus.
			 */

			// If the last component on focus was not a text pane
			if (!(AcideMainWindow.getInstance().getLastElementOnFocus() instanceof JTextPane))
				// Sets the focus in the last element focus in window (console
				// or file editor)
				AcideMainWindow.getInstance().getLastElementOnFocus()
						.requestFocusInWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu bar tool bar save all files button mouse
	 * listener
	 * 
	 * @version 0.8
	 * @see MouseListener
	 */
	class SaveAllFilesButtonMouseListener implements MouseListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent
		 * )
		 */
		@Override
		public void mouseReleased(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent )
		 */
		@Override
		public void mousePressed(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseExited(MouseEvent mouseEvent) {

			if (_saveAllFilesButton.isEnabled())
				_saveAllFilesButton.setBorderPainted(false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseEntered(MouseEvent mouseEvent) {

			if (_saveAllFilesButton.isEnabled())
				_saveAllFilesButton.setBorderPainted(true);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseClicked(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/**
		 * Dispatches the mouse event.
		 * 
		 * @param mouseEvent
		 *            mouse event.
		 */
		public void dispatchEvent(MouseEvent mouseEvent) {

			// Does the save all files menu item action
			AcideMainWindow.getInstance().getMenu().getFileMenu()
					.getSaveAllFilesMenuItem().doClick();

			/*
			 * IMPORTANT: If the last component in focus was a file editor, it
			 * will cause troubles with the focus.
			 */

			// If the last component on focus was not a text pane
			if (!(AcideMainWindow.getInstance().getLastElementOnFocus() instanceof JTextPane))
				// Sets the focus in the last element focus in window (console
				// or file editor)
				AcideMainWindow.getInstance().getLastElementOnFocus()
						.requestFocusInWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu bar tool bar new project button mouse
	 * listener
	 * 
	 * @version 0.8
	 * @see MouseListener
	 */
	class NewProjectButtonMouseListener implements MouseListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent
		 * )
		 */
		@Override
		public void mouseReleased(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent )
		 */
		@Override
		public void mousePressed(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseExited(MouseEvent mouseEvent) {
			_newProjectButton.setBorderPainted(false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseEntered(MouseEvent mouseEvent) {
			_newProjectButton.setBorderPainted(true);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseClicked(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/**
		 * Dispatches the mouse event.
		 * 
		 * @param mouseEvent
		 *            mouse event.
		 */
		public void dispatchEvent(MouseEvent mouseEvent) {

			// Does the new project menu item action
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getNewProjectMenuItem().doClick();

			// Sets the focus in the last element focus in window (console
			// or file editor)
			AcideMainWindow.getInstance().getLastElementOnFocus()
					.requestFocusInWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu bar tool bar open project button mouse
	 * listener
	 * 
	 * @version 0.8
	 * @see MouseListener
	 */
	class OpenProjectButtonMouseListener implements MouseListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent
		 * )
		 */
		@Override
		public void mouseReleased(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent )
		 */
		@Override
		public void mousePressed(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseExited(MouseEvent mouseEvent) {
			_openProjectButton.setBorderPainted(false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseEntered(MouseEvent mouseEvent) {
			_openProjectButton.setBorderPainted(true);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseClicked(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);
		}

		/**
		 * Dispatches the mouse event.
		 * 
		 * @param mouseEvent
		 *            mouse event.
		 */
		public void dispatchEvent(MouseEvent mouseEvent) {

			// Does the open project menu item action
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getOpenProjectMenuItem().doClick();

			// Sets the focus in the last element focus in window (console
			// or file editor)
			AcideMainWindow.getInstance().getLastElementOnFocus()
					.requestFocusInWindow();
		}
	}

	/**
	 * ACIDE - A Configurable IDE menu bar tool bar save project button mouse
	 * listener
	 * 
	 * @version 0.8
	 * @see MouseListener
	 */
	class SaveProjectButtonMouseListener implements MouseListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent
		 * )
		 */
		@Override
		public void mouseReleased(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);

			if (!_saveProjectButton.isEnabled())
				_saveProjectButton.setBorderPainted(false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent )
		 */
		@Override
		public void mousePressed(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);

			if (!_saveProjectButton.isEnabled())
				_saveProjectButton.setBorderPainted(false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseExited(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseExited(MouseEvent mouseEvent) {
			
			if (_saveProjectButton.isEnabled())
				_saveProjectButton.setBorderPainted(false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseEntered(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseEntered(MouseEvent mouseEvent) {
			
			if (_saveProjectButton.isEnabled())
				_saveProjectButton.setBorderPainted(true);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent )
		 */
		@Override
		public void mouseClicked(MouseEvent mouseEvent) {
			dispatchEvent(mouseEvent);

			if (!_saveProjectButton.isEnabled())
				_saveProjectButton.setBorderPainted(false);
		}

		/**
		 * Dispatches the mouse event.
		 * 
		 * @param mouseEvent
		 *            mouse event.
		 */
		public void dispatchEvent(MouseEvent mouseEvent) {

			// Enables the save project menu item
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getSaveProjectMenuItem().setEnabled(true);

			// Does the save project menu item action
			AcideMainWindow.getInstance().getMenu().getProjectMenu()
					.getSaveProjectMenuItem().doClick();

			// Sets the focus in the last element focus in window (console
			// or file editor)
			AcideMainWindow.getInstance().getLastElementOnFocus()
					.requestFocusInWindow();
		}
	}
}
