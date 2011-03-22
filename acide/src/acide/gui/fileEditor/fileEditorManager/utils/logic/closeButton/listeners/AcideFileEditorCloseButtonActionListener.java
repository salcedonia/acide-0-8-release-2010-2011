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
package acide.gui.fileEditor.fileEditorManager.utils.logic.closeButton.listeners;

import acide.configuration.project.AcideProjectConfiguration;
import acide.gui.fileEditor.fileEditorManager.utils.logic.closeButton.AcideFileEditorCloseButton;
import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import javax.swing.plaf.UIResource;

import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE file editor close button action listener.
 * 
 * @version 0.8
 * @see UIResource
 * @see AbstractAction
 */
public class AcideFileEditorCloseButtonActionListener extends AbstractAction {

	/**
	 * ACIDE - A Configurable IDE file editor close button action listener class
	 * serial version UID.
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

		boolean isCancelOption = false;

		// Is the file modified?
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.isRedButton(_index)) {

			// Asks the user if he wants to save it
			int returnValue = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s643"), AcideLanguageManager
							.getInstance().getLabels().getString("s994"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If OK
			if (returnValue == JOptionPane.OK_OPTION) {

				// Saves the file
				AcideMainWindow.getInstance().getMenu().getFileMenu()
						.saveFile(_index);

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// Sets opened to false to the project file
					for (int index = 0; index < AcideProjectConfiguration
							.getInstance().getFileListSize(); index++) {

						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index)
								.getAbsolutePath()
								.equals(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(_index)
										.getAbsolutePath())) {
							AcideProjectConfiguration.getInstance()
									.getFileAt(index).setIsOpened(false);
						}
					}

					// Sets the project to modified
					AcideProjectConfiguration.getInstance().setIsModified(true);
				}

			} else if (returnValue == JOptionPane.NO_OPTION) {

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

					// Sets opened to false to the project file
					for (int index = 0; index < AcideProjectConfiguration
							.getInstance().getFileListSize(); index++) {

						if (AcideProjectConfiguration
								.getInstance()
								.getFileAt(index)
								.getAbsolutePath()
								.equals(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getFileEditorPanelAt(_index)
										.getAbsolutePath())) {
							AcideProjectConfiguration.getInstance()
									.getFileAt(index).setIsOpened(false);
						}
					}

					// Sets the project to modified
					AcideProjectConfiguration.getInstance().setIsModified(true);
				}
			} else
				isCancelOption = true;
		} else {

			// Is not modified

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject()) {

				// Sets opened to false to the project file
				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getFileListSize(); index++) {

					if (AcideProjectConfiguration
							.getInstance()
							.getFileAt(index)
							.getAbsolutePath()
							.equals(AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.getAbsolutePath())) {
						AcideProjectConfiguration.getInstance()
								.getFileAt(index).setIsOpened(false);
					}
				}

				// Sets the project to modified
				AcideProjectConfiguration.getInstance().setIsModified(true);
			}
		}

		if (!isCancelOption) {

			// Removes the tab from the file editor
			AcideMainWindow.getInstance().getFileEditorManager()
					.getTabbedPane().remove(_index);

			// Validates the changes in the tabbed pane
			AcideMainWindow.getInstance().getFileEditorManager()
					.getTabbedPane().validate();

			// Exchanges the closing buttons
			for (int index = _index; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getTabbedPaneUI().getCloseButtons()
					.size() - 1; index++) {

				// Gets the current close button
				AcideFileEditorCloseButton currentCloseButton = (AcideFileEditorCloseButton) AcideMainWindow
						.getInstance().getFileEditorManager().getTabbedPaneUI()
						.getCloseButtons().get(index);

				// Gets the next close button
				AcideFileEditorCloseButton nextCloseButton = (AcideFileEditorCloseButton) AcideMainWindow
						.getInstance().getFileEditorManager().getTabbedPaneUI()
						.getCloseButtons().get(index + 1);

				// If it is red button
				if (nextCloseButton.isRedButton())

					// Sets the red button
					currentCloseButton.setRedCloseButton();
				else
					// Sets the green button
					currentCloseButton.setGreenCloseButton();

				// Sets the position
				AcideMainWindow.getInstance().getFileEditorManager()
						.getTabbedPaneUI().getCloseButtons()
						.set(index, currentCloseButton);
			}
		}

		// No more tabs?
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getTabbedPane().getTabCount() == 0) {

			// Disables the file menu
			AcideMainWindow.getInstance().getMenu().disableFileMenu();

			// Disables the edit menu
			AcideMainWindow.getInstance().getMenu().disableEditMenu();
		} else {

			// Updates the focus, the caret and so on...
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.updatesFileEditorAt(
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanelIndex());
		}
	}
}
