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
package gui.menuBar.projectMenu.listeners;

import es.configuration.project.AcideProjectConfiguration;
import es.configuration.project.workbench.AcideWorkbenchManager;
import es.project.AcideProjectFile;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.swing.JOptionPane;

import operations.log.AcideLog;

/**
 * ACIDE -A Configurable IDE project menu compile menu item listener.
 * 
 * @version 0.8
 * @see ActionListener
 */
public class CompileMenuItemListener implements ActionListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
	 * )
	 */
	@Override
	public void actionPerformed(ActionEvent actionEvent) {

		// Saves the file editor panel configuration
		AcideWorkbenchManager.getInstance().saveFileEditorPanelConfiguration();

		try {

			// Is it possible to compile
			if (AcideProjectConfiguration.getInstance().isCheckCompiler()) {

				String fileToCompile = "";

				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index++) {

					// IS COMPILABLE?
					if (AcideProjectConfiguration.getInstance()
							.getFileAt(index).isCompilableFile()) {

						fileToCompile = fileToCompile
								+ "\""
								+ AcideProjectConfiguration.getInstance()
										.getFileAt(index).getAbsolutePath()
								+ "\""
								+ AcideProjectConfiguration.getInstance()
										.getSeparatorFile();
					}
				}

				if (fileToCompile.length() > 0) {
					fileToCompile = fileToCompile.substring(0,
							fileToCompile.length() - 1);

					// If the compiler path has been defined
					if (AcideProjectConfiguration.getInstance()
							.getCompilerPath() != null)

						// EXECUTES THE COMPILATION
						Runtime.getRuntime().exec(
								AcideProjectConfiguration.getInstance()
										.getCompilerPath()
										+ " "
										+ AcideProjectConfiguration
												.getInstance()
												.getCompilerArguments()
										+ " "
										+ fileToCompile);
				}
			} else {

				// Gets the project file extension 
				String extension = AcideProjectConfiguration.getInstance()
						.getFileExtension();

				for (int index = 0; index < AcideProjectConfiguration
						.getInstance().getNumberOfFilesFromList(); index++) {

					// Gets the file from the project configuration at the index
					AcideProjectFile file = AcideProjectConfiguration
							.getInstance().getFileAt(index);

					// Is not a directory
					if (!file.isDirectory()) {

						// Gets the file absolute path
						String fileAbsolutePath = file.getAbsolutePath();

						// Gets the file extension
						String fileExtension = fileAbsolutePath.substring(
								fileAbsolutePath.lastIndexOf(".") + 1,
								fileAbsolutePath.length());

						if (fileExtension.equals(extension)) {

							if (AcideProjectConfiguration.getInstance()
									.getCompilerPath() != null) {

								// EXECUTES THE COMPILATION
								Runtime.getRuntime().exec(
										AcideProjectConfiguration.getInstance()
												.getCompilerPath()
												+ " "
												+ AcideProjectConfiguration
														.getInstance()
														.getCompilerArguments()
												+ " \""
												+ fileAbsolutePath
												+ "\"");
							}
						}
					}
				}
			}
		} catch (IOException exception) {

			// Error message
			JOptionPane.showMessageDialog(null, exception.getMessage());

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
		}
	}
}
