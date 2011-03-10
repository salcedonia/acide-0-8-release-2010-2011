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
package acide.gui.fileEditor.fileEditorManager.utils.logic.UI;

import acide.gui.fileEditor.fileEditorManager.utils.logic.closeButton.AcideFileEditorCloseButton;

import java.awt.Container;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.plaf.basic.BasicTabbedPaneUI;
import javax.swing.plaf.basic.BasicTabbedPaneUI.TabbedPaneLayout;

/**																
 * ACIDE - A Configurable IDE file editor tabbed pane UI.
 * 
 * Handles the tabbed pane Look and Feel used in the file editor.											
 *					
 * @version 0.8	
 * @see BasicTabbedPaneUI																													
 */
public class AcideFileEditorTabbedPaneUI extends BasicTabbedPaneUI {

	/**
	 * ACIDE - A Configurable IDE file editor tabbed pane layout.
	 */
	private AcideFileEditorTabbedPaneLayout _tabbedPaneLayout;
	/**
	 * Close button list of the tabs in the tabbed pane.
	 */
	private static ArrayList<AcideFileEditorCloseButton> _closeButtonList = new ArrayList<AcideFileEditorCloseButton>();
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.plaf.basic.BasicTabbedPaneUI#createLayoutManager()
	 */
	@Override
	protected LayoutManager createLayoutManager() {

		// Creates the tabbed pane layout
		_tabbedPaneLayout = new AcideFileEditorTabbedPaneLayout();
		return _tabbedPaneLayout;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.plaf.basic.BasicTabbedPaneUI#getTabInsets(int, int)
	 */
	@Override
	protected Insets getTabInsets(int tabPlacement, int tabIndex) {

		Insets defaultInsets = (Insets) super.getTabInsets(tabPlacement,
				tabIndex).clone();

		defaultInsets.right += 40;
		defaultInsets.top += 4;
		defaultInsets.bottom += 4;

		return defaultInsets;
	}

	/**
	 * Returns the close button from the list at the position given as a
	 * parameter.
	 * 
	 * @param position
	 *            position of the close button.
	 * 
	 * @return the close button from the list at the position given as a
	 *         parameter.
	 */
	public AcideFileEditorCloseButton getCloseButtonAt(int position) {
		return _tabbedPaneLayout.getCloseButtonAt(position);
	}
	
	/**
	 * Returns the close button list.
	 * 
	 * @return the close button list.
	 */
	public ArrayList<AcideFileEditorCloseButton> getCloseButtons() {
		return _tabbedPaneLayout.getCloseButtonList();
	}
		
	/**																
	 * ACIDE - A Configurable IDE file editor tabbed pane layout.
	 *					
	 * @version 0.8	
	 * @see TabbedPaneLayout																													
	 */
	class AcideFileEditorTabbedPaneLayout extends TabbedPaneLayout {

		/**
		 * Returns the close button from the list at the position given as a
		 * parameter.
		 * 
		 * @param position
		 *            Position of the close button in the list.
		 * 
		 * @return the close button from the list at the position given as a
		 *         parameter.
		 */
		public AcideFileEditorCloseButton getCloseButtonAt(int position) {
			return (AcideFileEditorCloseButton) _closeButtonList.get(position);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.LayoutManager#layoutContainer(java.awt.Container)
		 */
		@Override
		public void layoutContainer(Container parent) {

			super.layoutContainer(parent);

			while (tabPane.getTabCount() > _closeButtonList.size())
				_closeButtonList.add(new AcideFileEditorCloseButton(_closeButtonList.size()));

			Rectangle rectangle = new Rectangle();
			int index;

			for (index = 0; index < tabPane.getTabCount(); index++) {

				rectangle = getTabBounds(index, rectangle);
				JButton closeButton = (JButton) _closeButtonList.get(index);
				closeButton.setLocation(rectangle.x + rectangle.width - 20,
						rectangle.y + 5);
				closeButton.setSize(15, 15);
				tabPane.add(closeButton);
			}

			for (; index < _closeButtonList.size(); index++) {
				tabPane.remove((JButton) _closeButtonList.get(index));
				_closeButtonList.remove(index);
			}
		}

		/**
		 * Returns the close button list.
		 * 
		 * @return The close button list.
		 */
		public ArrayList<AcideFileEditorCloseButton> getCloseButtonList() {
			return _closeButtonList;
		}	
	}
}
