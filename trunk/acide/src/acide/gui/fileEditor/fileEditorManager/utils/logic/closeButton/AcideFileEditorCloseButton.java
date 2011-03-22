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
package acide.gui.fileEditor.fileEditorManager.utils.logic.closeButton;

import acide.gui.fileEditor.fileEditorManager.utils.logic.closeButton.listeners.AcideFileEditorCloseButtonActionListener;

import java.awt.Insets;
import java.util.ResourceBundle;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.plaf.UIResource;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;
import acide.resources.AcideResourceManager;

/**																
 * ACIDE - A Configurable IDE file editor close button.
 * 
 * Implements used UIResource when the close button is added to the
 * TabbedPane.
 *					
 * @version 0.8	
 * @see UIResource																													
 */
public class AcideFileEditorCloseButton extends JButton implements UIResource {

	/**
	 * ACIDE - A Configurable IDE file editor close button class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Red icon for the closing button.
	 */
	private static final String RED_ICON = "./resources/icons/editor/closeModified.png";
	/**
	 * Green icon for the closing button.
	 */
	private static final String GREEN_ICON = "./resources/icons/editor/closeNotModified.png";
	/**
	 * Path of the displayed icon.
	 */
	private String _selectedIcon = "";

	/**
	 * Creates a new close button.
	 * 
	 * @param index
	 *            editor index.
	 */
	public AcideFileEditorCloseButton(int index) {

		super(new AcideFileEditorCloseButtonActionListener(index));
		
		// Sets the green icon
		_selectedIcon = GREEN_ICON;
		setIcon(new ImageIcon(_selectedIcon));
		
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
		final ResourceBundle labels = language.getLabels();
		
		// Sets the tool tip text
		setToolTipText(labels.getString("s316"));
		
		setMargin(new Insets(0, 0, 0, 0));
		validate();
	}

	/**
	 * Sets the color of the button to red.
	 */
	public void setRedCloseButton() {
		_selectedIcon = RED_ICON;
		setIcon(new ImageIcon(RED_ICON));
	}

	/**
	 * Sets the color of the button to green.
	 */
	public void setGreenCloseButton() {
		_selectedIcon = GREEN_ICON;
		setIcon(new ImageIcon(GREEN_ICON));
	}

	/**
	 * Returns true if the button is red and false in other case.
	 * 
	 * @return true if the button is red and false in other case.
	 */
	public boolean isRedButton() {
		return _selectedIcon.matches(RED_ICON);
	}
}
