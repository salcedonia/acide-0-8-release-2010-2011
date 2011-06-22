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
package acide.gui.toolBarPanel.externaAppsToolBar;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;

import acide.configuration.project.AcideProjectConfiguration;
import acide.configuration.toolBar.AcideToolBarConfiguration;
import acide.configuration.toolBar.externalAppsToolBar.AcideExternalAppsToolBarButtonConf;
import acide.gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.mainWindow.utils.AcideLastElementOnFocus;
import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE external applications tool bar.
 * 
 * Its buttons execute external applications to ACIDE - A Configurable IDE.
 * 
 * @version 0.8
 * @see ArrayList
 */
public class AcideExternalAppsToolBar extends ArrayList<Component> {

	/**
	 * ACIDE - A Configurable IDE external applications tool bar class serial
	 * version UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new ACIDE - A Configurable IDE external application tool bar.
	 */
	public AcideExternalAppsToolBar() {
		super();
	}

	/**
	 * Builds the ACIDE - A Configurable IDE external applications tool bar.
	 * 
	 * @return the ACIDE - A Configurable IDE external applications tool bar.
	 */
	public AcideExternalAppsToolBar build() {

		// Removes all the buttons
		clear();

		// Adds a separator
		add(Box.createRigidArea(new Dimension(10, 10)));

		// Adds all the buttons
		JButton button;
		for (int index = 0; index < AcideToolBarConfiguration.getInstance()
				.getExternalAppsToolBarConfiguration().getSize(); index++) {

			// Creates the new button configuration to add to the tool bar panel
			AcideExternalAppsToolBarButtonConf newButtonConfiguration = AcideToolBarConfiguration
					.getInstance().getExternalAppsToolBarConfiguration()
					.getButtonConfigurationAt(index);

			// If it has icon
			if (newButtonConfiguration.getHasIcon())

				// Creates the button with the icon
				button = new JButton(new ImageIcon(
						newButtonConfiguration.getIcon()));
			else {

				// If the new button configuration has name
				if (!(newButtonConfiguration.getName().equals("")))

					// Creates the button with the name
					button = new JButton(newButtonConfiguration.getName());
				else
					// Creates the button with a default name
					button = new JButton((new Integer(index + 1)).toString());
			}
			// If the new button configuration has hint text
			if (!(newButtonConfiguration.getHintText().equals("")))

				// Sets the new button tool tip text
				button.setToolTipText(newButtonConfiguration.getHintText());

			// Sets the bold font to the button
			button.setFont(new Font(button.getFont().getName(), Font.BOLD,
					button.getFont().getSize()));
			
			// Adds the button to the tool bar
			add(button);

			// Adds the button action listener
			button.addActionListener(new ButtonAction(newButtonConfiguration));

			// Adds a separator
			add(Box.createRigidArea(new Dimension(5, 5)));
		}
		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s131"));

		return this;
	}

	/**
	 * Parses the ACIDE - A Configurable IDE variables to real paths in order to
	 * send them properly to the Operative System shell for its execution.
	 * 
	 * @param buttonAction
	 *            button action to execute from the Tool Bar configuration.
	 * 
	 * @return the parsed string that contains to command to execute in the
	 *         Operative System shell.
	 */
	private String parseAcideVariables(String buttonAction) {

		// Gets the command to execute
		String command = buttonAction;

		// If there are opened file editors
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getNumberOfFileEditorPanels() > 0) {

			// Replaces the active file variable for its real value
			command = command.replace("$activeFile$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getAbsolutePath());

			// Replaces the active file path variable for its real value
			command = command.replace("$activeFilePath$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFilePath());

			// Replaces the active files extension for its real value
			command = command.replace("$activeFileExt$", AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getFileExtension());

			// Replaces the active files name for its real value
			command = command
					.replace("$activeFileName$", AcideMainWindow.getInstance()
							.getFileEditorManager()
							.getSelectedFileEditorPanel()
							.getFileNameWithoutExtension());
		}

		// If it is the default project
		if (AcideProjectConfiguration.getInstance().isDefaultProject()) {

			// Gets the main file editor panel
			AcideFileEditorPanel mainFileEditorPanel = AcideMainWindow
					.getInstance().getFileEditorManager()
					.getMainFileEditorPanel();

			// If exists
			if (mainFileEditorPanel != null) {

				// Replaces the $mainFile$ variable for its real value
				command = command.replace("$mainFile$",
						mainFileEditorPanel.getAbsolutePath());

				// Replaces the $mainFilePath$ variable for its real value
				command = command.replace("$mainFilePath$",
						mainFileEditorPanel.getFilePath());

				// Replaces the $mainFileExt$ variable for its real value
				command = command.replace("$mainFileExt$",
						mainFileEditorPanel.getFileExtension());

				// Replaces the $mainFileName$ variable for its real value
				command = command.replace("$mainFileName$",
						mainFileEditorPanel.getFileNameWithoutExtension());
			}
		} else {

			// Not default project

			// Searches for the MAIN file into the ACIDE - A Configurable IDE
			// project configuration
			int mainFileEditorPanelIndex = -1;
			for (int index = 0; index < AcideProjectConfiguration.getInstance()
					.getNumberOfFilesFromList(); index++) {
				if (AcideProjectConfiguration.getInstance().getFileAt(index)
						.isMainFile())
					mainFileEditorPanelIndex = index;
			}

			// If exists
			if (mainFileEditorPanelIndex != -1) {

				// Replaces the $mainFile$ variable for its real value
				command = command.replace(
						"$mainFile$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileEditorPanelIndex)
								.getAbsolutePath());

				// Replaces the $mainFilePath$ variable for its real value
				command = command.replace(
						"$mainFilePath$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileEditorPanelIndex)
								.getRelativePath());

				// Replaces the $mainFileExt$ variable for its real value
				command = command.replace(
						"$mainFileExt$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileEditorPanelIndex)
								.getFileExtension());

				// Replaces the $mainFileName$ variable for its real value
				command = command.replace(
						"$mainFileName$",
						AcideProjectConfiguration.getInstance()
								.getFileAt(mainFileEditorPanelIndex)
								.getFileName());
			}
		}

		return command;
	}
	
	/**
	 * ACIDE - A Configurable IDE console panel tool bar button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ButtonAction implements ActionListener {

		/**
		 * New button configuration.
		 */
		AcideExternalAppsToolBarButtonConf _newButtonConfiguration;

		/**
		 * Creates a new button action listener.
		 * 
		 * @param newButtonConfiguration
		 *            new button configuration.
		 */
		public ButtonAction(
				AcideExternalAppsToolBarButtonConf newButtonConfiguration) {

			// Stores the new button configuration
			_newButtonConfiguration = newButtonConfiguration;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event
		 * .ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			new Thread(new Runnable() {
				/*
				 * (non-Javadoc)
				 * 
				 * @see java.lang.Runnable#run()
				 */
				@Override
				public void run() {

					try {

						// Executes the parsed command in the OS shell
						Process process = Runtime.getRuntime().exec(
								parseAcideVariables(_newButtonConfiguration.getPath()));

						// Waits for the process to end
						process.waitFor();
					} catch (Exception exception) {

						// Updates the log
						AcideLog.getLog().error(exception.getMessage());
						exception.printStackTrace();
					}
				}
			}).start();

			// Sets the focus in the last element on focus in ACIDE - A
			// Configurable IDE
			AcideLastElementOnFocus
					.setFocusOnLastElementOnFocus(AcideMainWindow.getInstance()
							.getLastElementOnFocus());
		}
	}
}
