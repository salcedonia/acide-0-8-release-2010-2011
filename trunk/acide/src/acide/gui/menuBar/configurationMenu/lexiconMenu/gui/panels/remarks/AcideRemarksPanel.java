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
package acide.gui.menuBar.configurationMenu.lexiconMenu.gui.panels.remarks;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import acide.gui.mainWindow.AcideMainWindow;
import acide.gui.menuBar.configurationMenu.lexiconMenu.gui.panels.reserverdWords.AcideReservedWordsPanel;
import acide.language.AcideLanguageManager;

/**
 * ACIDE - A Configurable IDE remarks panel.
 * 
 * @version 0.8
 * @see JPanel
 */
public class AcideRemarksPanel extends JPanel {

	/**
	 * ACIDE - A Configurable IDE remarks panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE remarks panel color palette button image icon.
	 */
	private final static ImageIcon COLOR_PALETTE_IMAGE = new ImageIcon(
			"./resources/icons/buttons/colorPalette.png");
	/**
	 * ACIDE - A Configurable IDE remarks panel color palette label.
	 */
	private JLabel _colorPaletteLabel;
	/**
	 * ACIDE - A Configurable IDE remarks panel preview label.
	 */
	private JLabel _previewLabel;
	/**
	 * ACIDE - A Configurable IDE remarks panel preview text field.
	 */
	private JTextField _previewTextField;
	/**
	 * ACIDE - A Configurable IDE remarks panel remark symbol label.
	 */
	private JLabel _remarkSymbolLabel;
	/**
	 * ACIDE - A Configurable IDE remarks panel remark symbol text field.
	 */
	private JTextField _remarkSymbolTextField;
	/**
	 * ACIDE - A Configurable IDE remarks panel color frame dialog.
	 */
	private JButton _colorPaletteButton;
	/**
	 * ACIDE - A Configurable IDE remarks panel font type label.
	 */
	private JLabel _fontTypeLabel;
	/**
	 * ACIDE - A Configurable IDE remarks panel font type combo box.
	 */
	private JComboBox _fontTypeComboBox;
	/**
	 * ACIDE - A Configurable IDE remarks panel case sensitive label.
	 */
	private JLabel _caseSensitiveLabel;
	/**
	 * ACIDE - A Configurable IDE remarks panel case sensitive check box.
	 */
	private JCheckBox _caseSensitiveCheckBox;
	/**
	 * ACIDE - A Configurable IDE lexicon configuration window are there changes
	 * flag.
	 */
	private boolean _areThereChanges;
	
	/**
	 * Creates a new ACIDE - A Configurable IDE remarks panel.
	 */
	public AcideRemarksPanel(AcideReservedWordsPanel reservedWordsPanel) {

		super();
		
		// There are no changes yet
		_areThereChanges = false;
			
		// Builds the panel components
		buildComponents(reservedWordsPanel);

		// Sets the listeners of the window components
		setListeners();

		// Adds the components to the panel
		addComponents();
	}

	/**
	 * Builds the panel components.
	 * 
	 * @param reservedWordsPanel reserved words panel.
	 */
	private void buildComponents(AcideReservedWordsPanel reservedWordsPanel) {
		
		// Creates the case sensitive label
		_caseSensitiveLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s431"), JLabel.CENTER);

		// Creates the case sensitive check box
		_caseSensitiveCheckBox = new JCheckBox();

		// Creates the preview label
		_previewLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s392"), JLabel.CENTER);

		// Creates the preview text field
		_previewTextField = new JTextField();

		// Sets the tool tip text
		_previewTextField.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s393"));

		// It is not editable
		_previewTextField.setEditable(false);

		// Sets the horizontal alignment
		_previewTextField.setHorizontalAlignment(JTextField.CENTER);

		// Sets the symbol to the preview panel from the the remark
		// configuration
		_previewTextField.setText(AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel()
				.getLexiconConfiguration().getRemarksManager().getSymbol()
				+ " "
				+ AcideLanguageManager.getInstance().getLabels()
						.getString("s444"));

		// Sets the foreground color to the preview panel from the remark
		// configuration
		_previewTextField.setForeground(AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel()
				.getLexiconConfiguration().getRemarksManager().getColor());

		// Sets the font style to the preview panel from the remark
		// configuration
		_previewTextField.setFont(new Font(reservedWordsPanel
				.getReservedWordLabel().getFont().getFontName(),
				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel().getLexiconConfiguration()
						.getRemarksManager().getFontStyle(), reservedWordsPanel
						.getReservedWordLabel().getFont().getSize()));

		// Creates the remarks symbol label
		_remarkSymbolLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s433"), JLabel.CENTER);

		// Creates the remarks symbol text field
		_remarkSymbolTextField = new JTextField();

		// Sets the remark symbol from configuration
		_remarkSymbolTextField.setText(AcideMainWindow.getInstance()
				.getFileEditorManager().getSelectedFileEditorPanel()
				.getLexiconConfiguration().getRemarksManager().getSymbol());

		// Creates the color palette label
		_colorPaletteLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s391"), JLabel.CENTER);

		// Creates the color palette button
		_colorPaletteButton = new JButton(COLOR_PALETTE_IMAGE);

		// Creates the font type label
		_fontTypeLabel = new JLabel(AcideLanguageManager.getInstance()
				.getLabels().getString("s395"), JLabel.CENTER);

		// Creates the font type combo box
		_fontTypeComboBox = new JComboBox();

		// Adds the plain item to the font type combo box
		_fontTypeComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s396"));

		// Adds the italic item to the font type combo box
		_fontTypeComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s398"));

		// Adds the bold item to the font type combo box
		_fontTypeComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s397"));

		// Adds the bold + italic item to the font type combo box
		_fontTypeComboBox.addItem(AcideLanguageManager.getInstance()
				.getLabels().getString("s399"));

		// Sets the font style combo box as enabled
		_fontTypeComboBox.setEnabled(true);

		// Sets the tool tip text
		_fontTypeComboBox.setToolTipText(AcideLanguageManager.getInstance()
				.getLabels().getString("s400"));

		// Sets the selected item in the combo box
		setItemFontTypeComboBox();
	}

	/**
	 * Adds the components to the panel with the layout.
	 */
	private void addComponents() {
		
		// Sets the layout
		setLayout(new GridBagLayout());
		
		// Adds the components to the panel with the layout
		GridBagConstraints constraints = new GridBagConstraints();
		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		
		// Adds the remarks symbol label to the panel
		add(_remarkSymbolLabel, constraints);
		
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the remarks symbol text field to the panel
		add(_remarkSymbolTextField, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 1;
		
		// Adds the case sensitive label to the panel
		add(_caseSensitiveLabel, constraints);
		
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the case sensitive text field to the panel
		add(_caseSensitiveCheckBox, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 2;
		
		// Adds the color palette label to the panel
		add(_colorPaletteLabel, constraints);
		
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the color palette button to the panel
		add(_colorPaletteButton, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 3;
		
		// Adds the font type label to the panel
		add(_fontTypeLabel, constraints);
		
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the font type combo box to the panel
		add(_fontTypeComboBox, constraints);

		constraints.fill = GridBagConstraints.NONE;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 4;
		
		// Adds the preview label to the panel
		add(_previewLabel, constraints);
		
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		
		// Adds the preview text field to the panel
		add(_previewTextField, constraints);
	}

	/**
	 * Sets the selected item in the font type combo box from the remarks
	 * configuration.
	 */
	private void setItemFontTypeComboBox() {

		_fontTypeComboBox.setSelectedIndex(_previewTextField.getFont()
				.getStyle());
	}

	/**
	 * Sets the listeners of the window components.
	 */
	private void setListeners() {

		// Sets the remarks symbol text field key listener
		_remarkSymbolTextField
				.addKeyListener(new RemarkSymbolTextFieldAction());

		// Sets the color palette button action listener
		_colorPaletteButton.addActionListener(new ColorPaletteButtonAction());

		// Sets the font typ combo box action listener
		_fontTypeComboBox.addActionListener(new FontTypeComboBoxAction());
	}

	/**
	 * Returns the remark symbol text field.
	 * 
	 * @return the remark symbol text field.
	 */
	public JTextField getRemarkSymbolTextField() {
		return _remarkSymbolTextField;
	}

	/**
	 * Returns the preview text field.
	 * 
	 * @return the preview text field.
	 */
	public JTextField getPreviewTextField() {
		return _previewTextField;
	}

	/**
	 * Returns the is case sensitive check box.
	 * 
	 * @return the is case sensitive check box.
	 */
	public JCheckBox getIsCaseSensitiveCheckBox() {
		return _caseSensitiveCheckBox;
	}

	/**
	 * Returns the are there changes flag.
	 * 
	 * @return the are there changes flag.
	 */
	public boolean getAreThereChanges() {
		return _areThereChanges;
	}

	/**
	 * Sets a new value to the are there changes flag.
	 * 
	 * @param areThereChanges
	 *            new value to set.
	 */
	public void setAreThereChanges(boolean areThereChanges) {
		_areThereChanges = areThereChanges;
	}
	
	/**
	 * ACIDE - A Configurable IDE remarks panel color palette button action
	 * listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ColorPaletteButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Ask for the color to the user
			Color color = JColorChooser.showDialog(null, AcideLanguageManager
					.getInstance().getLabels().getString("s992"),
					_previewTextField.getForeground());

			// If the user has selected any
			if (color != null) {

				// Updates the preview text color
				_previewTextField.setForeground(color);
				
				// There are changes in the panel
				_areThereChanges = true;
			}
		}
	}

	/**
	 * ACIDE - A Configurable IDE remarks panel remark symbol text field key
	 * listener.
	 * 
	 * @version 0.8
	 * @see KeyListener
	 */
	class RemarkSymbolTextFieldAction implements KeyListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyTyped(KeyEvent keyEvent) {
			// previewTextField.setText(palabraField.getText());
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyPressed(KeyEvent keyEvent) {
			// previewTextField.setText(palabraField.getText());
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
		 */
		@Override
		public void keyReleased(KeyEvent keyEvent) {
			
			_previewTextField.setText(_remarkSymbolTextField.getText()
					+ " "
					+ AcideLanguageManager.getInstance().getLabels()
							.getString("s444"));
			
			// There are changes in the panel
			_areThereChanges = true;
		}
	}

	/**
	 * ACIDE - A Configurable IDE reserved words panel font type combo box
	 * action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class FontTypeComboBoxAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// Gets the font from the reserved word label
			Font font = _remarkSymbolTextField.getFont();

			// Gets the font type combo box selected item
			String selectedItem = (String) _fontTypeComboBox.getSelectedItem();

			if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s413")))

				// Sets the font in the preview text field
				_previewTextField.setFont(new Font(font.getFontName(),
						Font.PLAIN, font.getSize()));
			else if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s414")))

				// Sets the font in the preview text field
				_previewTextField.setFont(new Font(font.getFontName(),
						Font.ITALIC, font.getSize()));
			else if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s415")))

				// Sets the font in the preview text field
				_previewTextField.setFont(new Font(font.getFontName(),
						Font.BOLD, font.getSize()));
			else if (selectedItem.equals(AcideLanguageManager.getInstance()
					.getLabels().getString("s416")))

				// Sets the font in the preview text field
				_previewTextField.setFont(new Font(font.getFontName(),
						Font.BOLD + Font.ITALIC, font.getSize()));

			// If the reserved word text field is not empty
			if (_remarkSymbolTextField.getText() != "")

				// Sets the text in the preview text field
				_previewTextField.setText(_remarkSymbolTextField.getText()
						+ " "
						+ AcideLanguageManager.getInstance().getLabels()
								.getString("s444"));
			
			// There are changes in the panel
			_areThereChanges = true;
		}
	}
}
