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
package acide.gui.fileEditor.fileEditorManager.listeners;

import acide.files.AcideFileManager;
import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.toolBarPanel.menuBarToolBar.AcideMenuBarToolBar;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.JOptionPane;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE file editor manager mouse click listener.
 * 
 * @version 0.8
 * @see MouseAdapter
 */
public class AcideFileEditorManagerMouseClickListener extends MouseAdapter {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseClicked(MouseEvent mouseEvent) {
		dispatchEvent(mouseEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(MouseEvent mouseEvent) {
		dispatchEvent(mouseEvent);
	}

	/**
	 * Dispatches the mouse event.
	 * 
	 * @param mouseEvent
	 *            mouse event.
	 */
	private void dispatchEvent(MouseEvent mouseEvent) {

		// If there are opened editors
		if (AcideMainWindow.getInstance().getFileEditorManager()
				.getTabbedPane().getComponentCount() > 0) {

			// Gets the selected editor path
			String selectedEditorPath = AcideMainWindow.getInstance()
					.getFileEditorManager().getSelectedFileEditorPanel()
					.getAbsolutePath();

			// If has path
			if (selectedEditorPath != null) {

				// If it is not the NEW FILE or LOG TAB
				if (!selectedEditorPath.equals(AcideLanguageManager
						.getInstance().getLabels().getString("s79"))
						&& !selectedEditorPath.equals("Log")) {

					// Builds the file to check its last modification and size
					// properties
					File file = new File(selectedEditorPath);

					// If the file exists
					if (file.exists()) {

						// If something has modified the file content from
						// outside
						if ((file.lastModified() != AcideMainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getLastChange())
								|| (file.length() != AcideMainWindow
										.getInstance().getFileEditorManager()
										.getSelectedFileEditorPanel()
										.getLastSize())) {

							// Ask to the user for saving
							int returnValue = JOptionPane.showConfirmDialog(
									null, AcideLanguageManager.getInstance()
											.getLabels().getString("s65"));

							// If it is ok
							if (returnValue == JOptionPane.OK_OPTION) {

								// Gets its file content
								String fileContent = AcideFileManager
										.getInstance().load(selectedEditorPath);

								if (fileContent != null) {

									// Sets the file content
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.setFileContent(fileContent);

									// Sets last change
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.setLastChange(file.lastModified());

									// Sets last size
									AcideMainWindow.getInstance()
											.getFileEditorManager()
											.getSelectedFileEditorPanel()
											.setLastSize(file.length());
								}
							} else {

								// Sets last change
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.setLastChange(file.lastModified());

								// Sets last size
								AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanel()
										.setLastSize(file.length());
							}
						}
					} else {

						// Displays an error message
						JOptionPane.showMessageDialog(
								AcideMainWindow.getInstance(),
								AcideLanguageManager.getInstance().getLabels()
										.getString("s1046"), "Error",
								JOptionPane.ERROR_MESSAGE);

						// Removes the tab from the file editor manager
						// tabbed pane
						AcideMainWindow
								.getInstance()
								.getFileEditorManager()
								.getTabbedPane()
								.remove(AcideMainWindow.getInstance()
										.getFileEditorManager()
										.getSelectedFileEditorPanelIndex());

						// Validates the changes in the file editor
						// manager tabbed pane
						AcideMainWindow.getInstance().getFileEditorManager()
								.getTabbedPane().validate();
					}
				}
			}

			// Updates the button icons
			AcideMainWindow.getInstance().getFileEditorManager()
					.updatesButtonIcons();

			// Updates the file editor panel
			AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.updateRelatedComponentsAt(
							AcideMainWindow.getInstance()
									.getFileEditorManager()
									.getSelectedFileEditorPanelIndex());
			
			// Updates the static tool bar
			AcideMenuBarToolBar.getInstance().updateStateOfFileButtons();
		}
	}
}
