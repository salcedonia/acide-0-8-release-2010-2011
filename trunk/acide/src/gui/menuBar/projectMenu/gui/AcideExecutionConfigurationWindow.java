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
package gui.menuBar.projectMenu.gui;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.IOException;
import java.util.ResourceBundle;

import es.configuration.project.AcideProjectConfiguration;
import es.text.ExtensionFilter;
import es.text.AcideTextFile;
import gui.listeners.AcideWindowListener;
import gui.mainWindow.MainWindow;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import language.AcideLanguageManager;

import operations.log.AcideLog;
import resources.AcideResourceManager;

/**																
 * ACIDE - A Configurable IDE execution configuration window.											
 *					
 * @version 0.8	
 * @see JFrame																													
 */
public class AcideExecutionConfigurationWindow extends JFrame {

	/**
	 * ACIDE - A Configurable IDE execution configuration window class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE execution configuration window image icon.
	 */
	private static final ImageIcon ICON = new ImageIcon("./resources/images/icon.png");
	/**
	 * ACIDE - A Configurable IDE execution configuration window main panel.
	 */
	private JPanel _mainPanel;
	/**
	 * ACIDE - A Configurable IDE execution configuration window execution label.
	 */
	private JLabel _executionLabel;
	/**
	 * ACIDE - A Configurable IDE execution configuration window examine path button.
	 */
	private JButton _examinePathButton;
	/**
	 * ACIDE - A Configurable IDE execution configuration window arguments label.
	 */
	private JLabel _argumentsLabel;
	/**
	 * ACIDE - A Configurable IDE execution configuration window arguments text field.
	 */
	private JTextField _argumentsTextField;
	/**
	 * ACIDE - A Configurable IDE execution configuration window run button.
	 */
	private JButton _runButton;
	/**
	 * ACIDE - A Configurable IDE execution configuration window cancel button.
	 */
	private JButton _cancelButton;
	/**
	 * ACIDE - A Configurable IDE execution configuration window button panel.
	 */
	private JPanel _buttonPanel;
	/**
	 * ACIDE - A Configurable IDE execution configuration window execution text field.
	 */
	private JTextField _executionTextField;

	/**
	 * Creates a new ACIDE - A Configurable IDE execution configuration window.
	 */
	public AcideExecutionConfigurationWindow() {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();
		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty("language"));
		} catch (Exception exception) {
			
			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		ResourceBundle labels = language.getLabels();
		
		// Sets the layout
		setLayout(new GridBagLayout());
		
		// MAIN PANEL
		_mainPanel = new JPanel(new GridBagLayout());
		_mainPanel.setBorder(BorderFactory.createTitledBorder(null, labels
				.getString("s640"),TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));
		
		// BUTTON PANEL
		_buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		
		// EXECUTION
		_executionLabel = new JLabel(labels.getString("s606"));
		_executionTextField = new JTextField();
		_executionTextField.setToolTipText(labels.getString("s638"));
		
		_argumentsLabel = new JLabel(labels.getString("s609"));
		_argumentsTextField = new JTextField();
		_argumentsTextField.setToolTipText(labels.getString("s610"));
		
		// EXAMINE PATH BUTTON
		_examinePathButton = new JButton(labels.getString("s596"));
		_examinePathButton.setHorizontalAlignment(JButton.CENTER);
		_examinePathButton.setToolTipText(labels.getString("s641"));
		_examinePathButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				AcideTextFile f = new AcideTextFile();
				String[] ExtAcide = new String[] { "exe" };
				f.getFileChooser().addChoosableFileFilter(
						new ExtensionFilter(ExtAcide,
								"Executable source (*.exe)"));
				String path = f.askAbsolutePath();
				_executionTextField.setText(path);
			}
		});
		
		// RUN BUTTON
		_runButton = new JButton(labels.getString("s154"));
		_runButton.setHorizontalAlignment(JButton.CENTER);
		_runButton.setToolTipText(labels.getString("s154"));
		_runButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				try {
					
					String execution = _executionTextField.getText();
					String arguments = _argumentsTextField.getText();

					if (MainWindow.getInstance().getFileEditorManager()
							.getNumberOfFileEditorPanels() > 0) {
						
						execution = execution.replace("$activeFile$", MainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath());
						execution = execution.replace("$activeFilePath$", MainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getFilePath());
						execution = execution.replace("$activeFileExt$", MainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getFileExtension());
						execution = execution.replace("$activeFileName$", MainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getFileName());

						arguments = arguments.replace("$activeFile$", MainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getAbsolutePath());
						arguments = arguments.replace("$activeFilePath$", MainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getFilePath());
						arguments = arguments.replace("$activeFileExt$", MainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getFileExtension());
						arguments = arguments.replace("$activeFileName$", MainWindow
								.getInstance().getFileEditorManager()
								.getSelectedFileEditorPanel().getFileName());
					}
					
					// DEFAULT PROJECT
					if (AcideProjectConfiguration.getInstance().isDefaultProject()) {
						
						// IF THERE'S ONE MAIN FILE
						if (MainWindow.getInstance().getFileEditorManager()
								.getMainEditor() != null) {
							arguments = arguments.replace("$mainFile$", MainWindow
									.getInstance().getFileEditorManager()
									.getMainEditor().getAbsolutePath());
							execution = execution.replace("$mainFile$", MainWindow
									.getInstance().getFileEditorManager()
									.getMainEditor().getAbsolutePath());
							arguments = arguments.replace("$mainFilePath$", MainWindow
									.getInstance().getFileEditorManager()
									.getMainEditor().getFilePath());
							execution = execution.replace("$mainFilePath$", MainWindow
									.getInstance().getFileEditorManager()
									.getMainEditor().getFilePath());
							arguments = arguments.replace("$mainFileExt$", MainWindow
									.getInstance().getFileEditorManager()
									.getMainEditor().getFileExtension());
							execution = execution.replace("$mainFileExt$", MainWindow
									.getInstance().getFileEditorManager()
									.getMainEditor().getFileExtension());
							arguments = arguments.replace("$mainFileName$", MainWindow
									.getInstance().getFileEditorManager()
									.getMainEditor().getFileName());
							execution = execution.replace("$mainFileName$", MainWindow
									.getInstance().getFileEditorManager()
									.getMainEditor().getFileName());
						}
					} else {
						
						// SEARCH THE MAIN FILE OPENED IN THE EDITOR
						int mainFileIndex = -1;
						for (int i = 0; i < AcideProjectConfiguration.getInstance()
								.getNumberOfFilesFromList(); i++) {
							if (AcideProjectConfiguration.getInstance().getFileAt(i)
									.isMainFile())
								mainFileIndex = i;
						}
						
						// IF EXISTS
						if (mainFileIndex != -1) {
							arguments = arguments.replace("$mainFile$", AcideProjectConfiguration.getInstance().getFileAt(mainFileIndex)
									.getAbsolutePath());
							execution = execution.replace("$mainFile$", AcideProjectConfiguration.getInstance().getFileAt(mainFileIndex)
									.getAbsolutePath());
							arguments = arguments.replace("$mainFilePath$", AcideProjectConfiguration.getInstance().getFileAt(mainFileIndex)
									.getRelativePath());
							execution = execution.replace("$mainFilePath$", AcideProjectConfiguration.getInstance().getFileAt(mainFileIndex)
									.getRelativePath());
							arguments = arguments.replace("$mainFileExt$", AcideProjectConfiguration.getInstance().getFileAt(mainFileIndex)
									.getFileExtension());
							execution = execution.replace("$mainFileExt$", AcideProjectConfiguration.getInstance().getFileAt(mainFileIndex)
									.getFileExtension());
							arguments = arguments.replace("$mainFileName$", AcideProjectConfiguration.getInstance().getFileAt(mainFileIndex)
									.getFileName());
							execution = execution.replace("$mainFileName$", AcideProjectConfiguration.getInstance().getFileAt(mainFileIndex)
									.getFileName());
						}
					}

					Runtime.getRuntime().exec(execution + " " + arguments);
				} catch (IOException exception) {
					
					// Updates the log
					AcideLog.getLog().error(exception.getMessage());
					
					// Error message
					JOptionPane.showMessageDialog(null, exception.getMessage());
				}
				
				MainWindow.getInstance().setEnabled(true);
				
				// Closes the configuration window
				dispose();
			}
		});

		// CANCEL BUTTON
		_cancelButton = new JButton(labels.getString("s162"));
		_cancelButton.setHorizontalAlignment(JButton.CENTER);
		_cancelButton.setToolTipText(labels.getString("s162"));
		_cancelButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {
				
				// Enables the main window
				MainWindow.getInstance().setEnabled(true);
				
				// Closes the window
				dispose();
			}
		});
		
		// Listeners
		_examinePathButton.addKeyListener(new ExecutionGUIKeyboardListener());
		_runButton.addKeyListener(new ExecutionGUIKeyboardListener());
		_cancelButton.addKeyListener(new ExecutionGUIKeyboardListener());
		_executionTextField.addKeyListener(new ExecutionGUIKeyboardListener());
		_argumentsTextField.addKeyListener(new ExecutionGUIKeyboardListener());
		
		// ADD THE COMPONENTS TO THE WINDOW WITH THE LAYOUT
		GridBagConstraints constraints = new GridBagConstraints();
		
		// MAIN PANEL
		constraints.fill = GridBagConstraints.BOTH;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		_mainPanel.add(_executionLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 200;
		_mainPanel.add(_executionTextField, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 2;
		constraints.ipadx = 0;
		_mainPanel.add(_examinePathButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		_mainPanel.add(_argumentsLabel, constraints);
		constraints.gridx = 1;
		constraints.ipadx = 150;
		_mainPanel.add(_argumentsTextField, constraints);
		constraints.ipadx = 0;
		constraints.gridx = 0;
		constraints.gridy = 0;
		add(_mainPanel, constraints);
		
		// BUTTON PANEL
		_buttonPanel.add(_runButton, constraints);
		_buttonPanel.add(_cancelButton, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_buttonPanel, constraints);
		
		// FRAME
		setTitle(labels.getString("s639"));
		setIconImage(ICON.getImage());
		setResizable(false);
		pack();
		setVisible(true);
		setLocationRelativeTo(null);
		addWindowListener(new AcideWindowListener());
		
		// Disables the main window
		MainWindow.getInstance().setEnabled(false);
	}

	/**																
	 * ACIDE - A Configurable IDE execution configuration window keyboard listener.
	 *					
	 * @version 0.8	
	 * @see KeyAdapter																													
	 */
	class ExecutionGUIKeyboardListener extends KeyAdapter {
		/*
		 * (non-Javadoc)
		 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyPressed(KeyEvent keyEvent) {
			
			if (keyEvent.getKeyCode() == KeyEvent.VK_ESCAPE) {
				
				MainWindow.getInstance().setEnabled(true);
				
				// Closes the configuration window
				dispose();
				MainWindow.getInstance().setAlwaysOnTop(true);
				MainWindow.getInstance().setAlwaysOnTop(false);
			}
		}
	}
}
