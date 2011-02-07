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
package gui.fileEditor.fileEditorManager.utils.logic.closeButton.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.text.AcideTextFile;
import gui.fileEditor.fileEditorManager.utils.logic.closeButton.AcideFileEditorCloseButton;
import gui.mainWindow.MainWindow;

import java.awt.event.ActionEvent;
import java.util.ResourceBundle;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import javax.swing.plaf.UIResource;

import language.AcideLanguageManager;
import operations.factory.AcideIOFactory;
import operations.log.AcideLog;
import resources.AcideResourceManager;

/**
 * ACIDE - A Configurable IDE file editor close button action listener.
 * 
 * @version 0.8
 * @see UIResource
 * @see AbstractAction
 */
public class AcideFileEditorCloseButtonActionListener extends AbstractAction {

	/**
	 * ACIDE - A Configurable IDE file editor close button action listener class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE close button editor index.
	 */
	private int _index;

	/**
	 * Creates a new close button action listener.
	 * 
	 * @param index
	 *            close button editor index.
	 */
	public AcideFileEditorCloseButtonActionListener(int index) {
		super();
		_index = index;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event
	 * .ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		boolean isCancelOption = false;

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

		// Is the file modified?
		if (MainWindow.getInstance().getFileEditorManager().isRedButton(_index)) {

			// Ask the user if he wants to save it
			int chosenOption = JOptionPane.showConfirmDialog(null,
					labels.getString("s643"), labels.getString("s994"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If OK
			if (chosenOption == JOptionPane.OK_OPTION) {

				// Is it the new file?
				if (MainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(_index).getAbsolutePath()
						.equals(labels.getString("s79"))) {

					AcideTextFile textFile = AcideIOFactory.getInstance()
							.buildFile();
					String filePath = " ";
					filePath = textFile.askSavingFileEditorFile();

					if (!filePath.equals(" ")) {

						// Saves the file
						boolean savingResult = textFile.write(filePath,
								MainWindow.getInstance().getFileEditorManager()
										.getFileEditorPanelAt(_index)
										.getTextEditionAreaContent());

						// If it could save the file?
						if (savingResult) {

							// Sets the green button
							MainWindow.getInstance().getFileEditorManager()
									.setGreenButtonAt(_index);

							// Sets the path
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.setAbsolutePath(filePath);

							// Sets the tool type text
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.setToolTipText(filePath);

							// Gets the name
							int index = filePath.lastIndexOf("\\");
							if (index == -1)
								index = filePath.lastIndexOf("/");
							String name = filePath.substring(index + 1,
									filePath.length());
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_index).setName(name);

							// Updates the status bar
							MainWindow.getInstance().getStatusBar()
									.setStatusMessage(" ");
						}
					}
				} else {

					// Is not the new file

					AcideTextFile textFile = AcideIOFactory.getInstance()
							.buildFile();

					// Saves the file
					boolean savingResult = textFile.write(MainWindow
							.getInstance().getFileEditorManager()
							.getFileEditorPanelAt(_index).getAbsolutePath(),
							MainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.getTextEditionAreaContent());

					// If it could save it
					if (savingResult)

						// Updates the closing button
						MainWindow.getInstance().getFileEditorManager()
								.setGreenButtonAt(_index);
				}

				// Sets opened to false to the project file
				for (int index = 0; index < AcideProjectConfiguration.getInstance().getFileListSize(); index++) {

					if (AcideProjectConfiguration.getInstance()
							.getFileAt(index)
							.getAbsolutePath()
							.equals(MainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.getAbsolutePath())) {
						AcideProjectConfiguration.getInstance()
								.getFileAt(index).setIsOpened(false);
					}
				}

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance()
						.isDefaultProject())

					// Sets the project to modified
					AcideProjectConfiguration.getInstance()
							.setIsModified(true);

				// Removes the tab
				MainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(_index);

			} else if (chosenOption == JOptionPane.NO_OPTION) {

				// Removes the tab
				MainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(_index);

				// Updates the status bar
				MainWindow.getInstance().getStatusBar().setStatusMessage(" ");
			}
			else
				isCancelOption = true;
		} else {

			// Is not modified

			// Sets opened to false to the project file
			for (int filePos = 0; filePos < AcideProjectConfiguration.getInstance().getFileListSize(); filePos++) {

				if (AcideProjectConfiguration.getInstance()
						.getFileAt(filePos)
						.getAbsolutePath()
						.equals(MainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(_index).getAbsolutePath())) {
					AcideProjectConfiguration.getInstance()
							.getFileAt(filePos).setIsOpened(false);
				}
			}

			// Not default project
			if (!AcideProjectConfiguration.getInstance()
					.isDefaultProject())

				// Sets the project ot modified
				AcideProjectConfiguration.getInstance()
						.setIsModified(true);

			// Removes the tab
			MainWindow.getInstance().getFileEditorManager().getTabbedPane()
					.remove(_index);

			// Updates the status bar
			MainWindow.getInstance().getStatusBar().setStatusMessage(" ");
		}

		// No more tabs?
		if (MainWindow.getInstance().getFileEditorManager().getTabbedPane()
				.getTabCount() == 0) {

			// Disables the file menu
			MainWindow.getInstance().getMenu().disableFileMenu();

			// Disables the edit menu
			MainWindow.getInstance().getMenu().disableEditMenu();
		}

		if (!isCancelOption) {
			
			// Exchanges the closing buttons
			for (int index = _index; index < MainWindow.getInstance()
					.getFileEditorManager().getTestPlaf().getCloseButtons()
					.size() - 1; index++) {

				AcideFileEditorCloseButton closeButton = (AcideFileEditorCloseButton) MainWindow
						.getInstance().getFileEditorManager().getTestPlaf()
						.getCloseButtons().get(index);
				AcideFileEditorCloseButton nextCloseButton = (AcideFileEditorCloseButton) MainWindow
						.getInstance().getFileEditorManager().getTestPlaf()
						.getCloseButtons().get(index + 1);

				// Sets the button color
				if (nextCloseButton.isRedButton())
					closeButton.setRedCloseButton();
				else
					closeButton.setGreenCloseButton();

				// Sets the position
				MainWindow.getInstance().getFileEditorManager().getTestPlaf()
						.getCloseButtons().set(index, closeButton);
			}
		}
	}
}
