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
package gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.listeners;

import gui.fileEditor.fileEditorPanel.AcideFileEditorPanel;
import gui.mainWindow.MainWindow;

import java.awt.Adjustable;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;

import javax.swing.SwingUtilities;

/**
 * Editor panel adjustment listener.
 * 
 * @version 0.8
 * @see AdjustmentListener
 */
public class AcideFileEditorAdjustmentListener implements
		AdjustmentListener {

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.AdjustmentListener#adjustmentValueChanged(java.awt
	 * .event.AdjustmentEvent)
	 */
	@Override
	public void adjustmentValueChanged(final AdjustmentEvent adjustmentEvent) {

		SwingUtilities.invokeLater(new Runnable() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				
				// Gets the selected file editor panel
				final AcideFileEditorPanel selectedFileEditorPanel = MainWindow
						.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel();
				
				// Gets the event source
				final Adjustable source = adjustmentEvent.getAdjustable();

				// If adjusting
				if (adjustmentEvent.getValueIsAdjusting()) {

					// VERTICAL VALUE 1
					selectedFileEditorPanel
							.getTextEditionPanelList()
							.get(0)
							.setVerticalValue(
									selectedFileEditorPanel.getTextEditionPanelList()
											.get(0).getScrollPane()
											.getVerticalScrollBar().getValue());

					// HORIZONTAL VALUE 1
					selectedFileEditorPanel
							.getTextEditionPanelList()
							.get(0)
							.setHorizontalValue(
									selectedFileEditorPanel.getTextEditionPanelList()
											.get(0).getScrollPane()
											.getHorizontalScrollBar()
											.getValue());

					// VERTICAL VALUE 2
					selectedFileEditorPanel
							.getTextEditionPanelList()
							.get(1)
							.setVerticalValue(
									selectedFileEditorPanel.getTextEditionPanelList()
											.get(1).getScrollPane()
											.getVerticalScrollBar().getValue());

					// HORIZONTAL VALUE 2
					selectedFileEditorPanel
							.getTextEditionPanelList()
							.get(1)
							.setHorizontalValue(
									selectedFileEditorPanel.getTextEditionPanelList()
											.get(1).getScrollPane()
											.getHorizontalScrollBar()
											.getValue());
					return;
				}
				int orientation = source.getOrientation();
				if (orientation == Adjustable.HORIZONTAL) {
				} else {
				}
				int type = adjustmentEvent.getAdjustmentType();
				switch (type) {
				case AdjustmentEvent.UNIT_INCREMENT:
					break;
				case AdjustmentEvent.UNIT_DECREMENT:
					break;
				case AdjustmentEvent.BLOCK_INCREMENT:
					break;
				case AdjustmentEvent.BLOCK_DECREMENT:
					break;
				case AdjustmentEvent.TRACK:
					break;
				}

				if (selectedFileEditorPanel != null) {
					
					// EDITOR1 is focused
					if (selectedFileEditorPanel.getTextEditionPanelList().get(0)
							.getScrollPane().isFocusOwner()
							|| selectedFileEditorPanel.getTextEditionPanelList().get(0)
									.getScrollPane().getVerticalScrollBar()
									.isFocusOwner()
							|| selectedFileEditorPanel.getTextEditionPanelList().get(0)
									.getScrollPane().getHorizontalScrollBar()
									.isFocusOwner()) {

						// Updates the values
						selectedFileEditorPanel
								.getTextEditionPanelList()
								.get(1)
								.getScrollPane()
								.getVerticalScrollBar()
								.setValue(
										selectedFileEditorPanel
												.getTextEditionPanelList()
												.get(1).getVerticalValue());
						selectedFileEditorPanel
								.getTextEditionPanelList()
								.get(1)
								.getScrollPane()
								.getHorizontalScrollBar()
								.setValue(
										selectedFileEditorPanel
												.getTextEditionPanelList()
												.get(1).getHorizontalValue());
						selectedFileEditorPanel
								.getTextEditionPanelList()
								.get(0)
								.setVerticalValue(
										selectedFileEditorPanel
												.getTextEditionPanelList()
												.get(0).getScrollPane()
												.getVerticalScrollBar()
												.getValue());
						selectedFileEditorPanel
								.getTextEditionPanelList()
								.get(0)
								.setHorizontalValue(
										selectedFileEditorPanel
												.getTextEditionPanelList()
												.get(0).getScrollPane()
												.getHorizontalScrollBar()
												.getValue());
					}

					// EDITOR2 is focused
					if (selectedFileEditorPanel.getTextEditionPanelList().get(1)
							.getScrollPane().isFocusOwner()
							|| selectedFileEditorPanel.getTextEditionPanelList().get(1)
									.getScrollPane().getVerticalScrollBar()
									.isFocusOwner()
							|| selectedFileEditorPanel.getTextEditionPanelList().get(1)
									.getScrollPane().getHorizontalScrollBar()
									.isFocusOwner()) {

						// Updates the values
						selectedFileEditorPanel
								.getTextEditionPanelList()
								.get(0)
								.getScrollPane()
								.getVerticalScrollBar()
								.setValue(
										selectedFileEditorPanel
												.getTextEditionPanelList()
												.get(0).getVerticalValue());
						selectedFileEditorPanel
								.getTextEditionPanelList()
								.get(0)
								.getScrollPane()
								.getHorizontalScrollBar()
								.setValue(
										selectedFileEditorPanel
												.getTextEditionPanelList()
												.get(0).getHorizontalValue());
						selectedFileEditorPanel
								.getTextEditionPanelList()
								.get(1)
								.setVerticalValue(
										selectedFileEditorPanel
												.getTextEditionPanelList()
												.get(1).getScrollPane()
												.getVerticalScrollBar()
												.getValue());
						selectedFileEditorPanel
								.getTextEditionPanelList()
								.get(1)
								.setHorizontalValue(
										selectedFileEditorPanel
												.getTextEditionPanelList()
												.get(1).getScrollPane()
												.getHorizontalScrollBar()
												.getValue());
					}
				}
			}
		});
	}
}
