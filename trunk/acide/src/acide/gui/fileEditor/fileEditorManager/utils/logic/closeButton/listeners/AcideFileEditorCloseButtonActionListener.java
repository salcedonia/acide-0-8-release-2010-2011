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
import acide.files.AcideFileManager;
import acide.gui.fileEditor.fileEditorManager.utils.logic.closeButton.AcideFileEditorCloseButton;
import acide.gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
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
		if (AcideMainWindow.getInstance().getFileEditorManager().isRedButton(_index)) {

			// Asks the user if he wants to save it
			int resultValue = JOptionPane.showConfirmDialog(
					null,
					AcideLanguageManager.getInstance().getLabels()
							.getString("s643"), AcideLanguageManager
							.getInstance().getLabels().getString("s994"),
					JOptionPane.YES_NO_CANCEL_OPTION);

			// If OK
			if (resultValue == JOptionPane.OK_OPTION) {

				// Is it the new file?
				if (AcideMainWindow
						.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(_index)
						.getAbsolutePath()
						.equals(AcideLanguageManager.getInstance().getLabels()
								.getString("s79"))) {

					// Gets the file path
					String filePath = " ";
					filePath = AcideFileManager.getInstance()
							.askSavingFileEditorFile();

					if (!filePath.equals(" ")) {

						// Saves the file
						boolean savingResult = AcideFileManager.getInstance()
								.write(filePath,
										AcideMainWindow.getInstance()
												.getFileEditorManager()
												.getFileEditorPanelAt(_index)
												.getTextEditionAreaContent());

						// If it could save the file?
						if (savingResult) {

							// Sets the green button
							AcideMainWindow.getInstance().getFileEditorManager()
									.setGreenButtonAt(_index);

							// Sets the path
							AcideMainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.setAbsolutePath(filePath);

							// Sets the tool type text
							AcideMainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.setToolTipText(filePath);

							// Gets the name
							int index = filePath.lastIndexOf("\\");
							if (index == -1)
								index = filePath.lastIndexOf("/");
							String name = filePath.substring(index + 1,
									filePath.length());
							AcideMainWindow.getInstance().getFileEditorManager()
									.getFileEditorPanelAt(_index).setName(name);

							// Updates the status bar
							AcideMainWindow.getInstance().getStatusBar()
									.setStatusMessage(" ");
						}
					}
				} else {

					// Is not the new file

					// Saves the file
					boolean savingResult = AcideFileManager.getInstance()
							.write(AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getFileEditorPanelAt(_index)
									.getAbsolutePath(),
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getFileEditorPanelAt(_index)
											.getTextEditionAreaContent());

					// If it could save it
					if (savingResult)

						// Updates the closing button
						AcideMainWindow.getInstance().getFileEditorManager()
								.setGreenButtonAt(_index);
				}

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

				// If it is not the default project
				if (!AcideProjectConfiguration.getInstance().isDefaultProject())

					// Sets the project to modified
					AcideProjectConfiguration.getInstance().setIsModified(true);

				// Removes the tab
				AcideMainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(_index);

			} else if (resultValue == JOptionPane.NO_OPTION) {

				// Removes the tab
				AcideMainWindow.getInstance().getFileEditorManager().getTabbedPane()
						.remove(_index);

				// Updates the status bar
				AcideMainWindow.getInstance().getStatusBar().setStatusMessage(" ");
			} else
				isCancelOption = true;
		} else {

			// Is not modified

			// Sets opened to false to the project file
			for (int filePos = 0; filePos < AcideProjectConfiguration
					.getInstance().getFileListSize(); filePos++) {

				if (AcideProjectConfiguration
						.getInstance()
						.getFileAt(filePos)
						.getAbsolutePath()
						.equals(AcideMainWindow.getInstance().getFileEditorManager()
								.getFileEditorPanelAt(_index).getAbsolutePath())) {
					AcideProjectConfiguration.getInstance().getFileAt(filePos)
							.setIsOpened(false);
				}
			}

			// Not default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject())

				// Sets the project ot modified
				AcideProjectConfiguration.getInstance().setIsModified(true);

			// Removes the tab from the file editor
			AcideMainWindow.getInstance().getFileEditorManager().getTabbedPane()
					.remove(_index);

			// Updates the status message in the status bar
			AcideMainWindow.getInstance().getStatusBar().setStatusMessage(" ");
		}

		// No more tabs?
		if (AcideMainWindow.getInstance().getFileEditorManager().getTabbedPane()
				.getTabCount() == 0) {

			// Disables the file menu
			AcideMainWindow.getInstance().getMenu().disableFileMenu();

			// Disables the edit menu
			AcideMainWindow.getInstance().getMenu().disableEditMenu();
		}

		if (!isCancelOption) {

			// Exchanges the closing buttons
			for (int index = _index; index < AcideMainWindow.getInstance()
					.getFileEditorManager().getTabbedPaneUI().getCloseButtons()
					.size() - 1; index++) {

				AcideFileEditorCloseButton closeButton = (AcideFileEditorCloseButton) AcideMainWindow
						.getInstance().getFileEditorManager().getTabbedPaneUI()
						.getCloseButtons().get(index);
				AcideFileEditorCloseButton nextCloseButton = (AcideFileEditorCloseButton) AcideMainWindow
						.getInstance().getFileEditorManager().getTabbedPaneUI()
						.getCloseButtons().get(index + 1);

				// Sets the button color
				if (nextCloseButton.isRedButton())
					closeButton.setRedCloseButton();
				else
					closeButton.setGreenCloseButton();

				// Sets the position
				AcideMainWindow.getInstance().getFileEditorManager()
						.getTabbedPaneUI().getCloseButtons()
						.set(index, closeButton);
			}
		}

		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanelIndex() != -1) {

			// Sets the selected editor
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.setSelectedFileEditorPanelAt(
							AcideMainWindow.getInstance().getFileEditorManager()
									.getSelectedFileEditorPanelIndex());

			// Gets the active text pane
			AcideFileEditorPanel selectedFileEditorPanel = AcideMainWindow
					.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel();

			// Sets the caret position at its position
			selectedFileEditorPanel.setCaretPosition(selectedFileEditorPanel
					.getActiveTextEditionArea().getCaretPosition());

			// Sets the caret visible
			selectedFileEditorPanel.setCaretVisible(true);

			// Sets the focus on the active text component
			selectedFileEditorPanel.putFocusOnActiveTextArea();

			// If it is not the default project
			if (!AcideProjectConfiguration.getInstance().isDefaultProject())

				// Selects the node in the tree that
				// matches with the clicked file
				// editor
				AcideMainWindow.getInstance().getExplorerPanel()
						.selectTreeNodeFromFileEditor();
		}
	}
}
