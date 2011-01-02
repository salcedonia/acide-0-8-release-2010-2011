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
package gui.menuBar.configurationMenu.lexiconMenu.gui.panels;

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import language.AcideLanguageManager;
import operations.lexicon.Remark;

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
	private final JLabel _remarkSymbolLabel;
	/**
	 * ACIDE - A Configurable IDE remarks panel remark symbol text field.
	 */
	private final JTextField _remarkSymbolTextField;
	/**
	 * ACIDE - A Configurable IDE remarks panel color frame dialog.
	 */
	private JButton _colorPaletteButton;

	/**
	 * Creates a new ACIDE - A Configurable IDE remarks panel.
	 */
	public AcideRemarksPanel(AcideReservedWordsPanel reservedWordsPanel) {

		// Gets the labels
		final ResourceBundle _labels = AcideLanguageManager.getInstance()
				.getLabels();

		// Sets the layout
		setLayout(new GridBagLayout());

		// Sets the border
		setBorder(BorderFactory.createTitledBorder(null,
				_labels.getString("s430"), TitledBorder.LEADING,
				TitledBorder.DEFAULT_POSITION));

		// COLOR COMMENT LABEL
		_colorPaletteLabel = new JLabel(_labels.getString("s391"),
				JLabel.CENTER);

		// PREVIEW COMMENT
		_previewLabel = new JLabel(_labels.getString("s392"), JLabel.CENTER);
		_previewTextField = new JTextField();
		_previewTextField.setToolTipText(_labels.getString("s393"));
		_previewTextField.setEditable(false);
		_previewTextField.setHorizontalAlignment(JTextField.CENTER);
		_previewTextField.setFont(new Font(reservedWordsPanel
				.getReservedWordLabel().getFont().getFontName(), Font.PLAIN,
				reservedWordsPanel.getReservedWordLabel().getFont().getSize()));
		_previewTextField.setForeground(Color.BLACK);
		_previewTextField.setText(_labels.getString("s394"));
		_previewTextField.setText(Remark.getInstance().getContent() + " "
				+ _labels.getString("s444"));
		_previewTextField.setForeground(Remark.getInstance().getColor());
		_previewTextField.setFont(new Font(reservedWordsPanel
				.getReservedWordLabel().getFont().getFontName(), Font.ITALIC,
				reservedWordsPanel.getReservedWordLabel().getFont().getSize()));

		// REMARK SYMBOL
		_remarkSymbolLabel = new JLabel(_labels.getString("s433"),
				JLabel.CENTER);
		_remarkSymbolTextField = new JTextField();

		// Sets the remark symbol from configuration
		_remarkSymbolTextField.setText(Remark.getInstance().getContent());

		_remarkSymbolTextField.addKeyListener(new KeyListener() {
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
			 * @see
			 * java.awt.event.KeyListener#keyPressed(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyPressed(KeyEvent keyEvent) {
				// previewTextField.setText(palabraField.getText());
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent keyEvent) {
				_previewTextField.setText(_remarkSymbolTextField.getText());
			}

		});

		// COLOR PALETTE BUTTON
		_colorPaletteButton = new JButton(COLOR_PALETTE_IMAGE);
		_colorPaletteButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// Ask for the color to the user
				Color color = JColorChooser.showDialog(null,
						_labels.getString("s992"), _previewTextField.getForeground());

				// If the user has selected any
				if (color != null) {
										
					// Updates the preview text color
					_previewTextField.setForeground(color);
				}
			}
		});

		// Adds the components to the panel with the layout
		GridBagConstraints constraints = new GridBagConstraints();

		// REMARK SYMBOL
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.anchor = GridBagConstraints.EAST;
		constraints.insets = new Insets(5, 5, 5, 5);
		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.ipadx = 0;
		add(_remarkSymbolLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		constraints.ipadx = 0;
		add(_remarkSymbolTextField, constraints);

		// COLOR PALETTE
		constraints.anchor = GridBagConstraints.EAST;
		constraints.gridx = 0;
		constraints.gridy = 1;
		add(_colorPaletteLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.fill = GridBagConstraints.NONE;
		constraints.gridx = 1;
		add(_colorPaletteButton, constraints);

		// PREVIEW COMMENT
		constraints.anchor = GridBagConstraints.EAST;
		constraints.fill = GridBagConstraints.HORIZONTAL;
		constraints.gridx = 0;
		constraints.gridy = 2;
		add(_previewLabel, constraints);
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridx = 1;
		add(_previewTextField, constraints);
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
}
