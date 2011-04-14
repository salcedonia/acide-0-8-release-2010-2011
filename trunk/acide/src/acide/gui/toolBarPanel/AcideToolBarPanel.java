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
package acide.gui.toolBarPanel;

import acide.gui.toolBarPanel.consolePanelToolBar.AcideConsolePanelToolBar;
import acide.gui.toolBarPanel.externaAppsToolBar.AcideExternalAppsToolBar;
import acide.gui.toolBarPanel.menuBarToolBar.AcideMenuBarToolBar;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;

import acide.language.AcideLanguageManager;
import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE tool bar panel.
 * 
 * @version 0.8
 * @see JPanel
 */
public class AcideToolBarPanel extends JPanel {

	/**
	 * ACIDE - A Configurable IDE tool bar panel class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE tool bar panel image icon.
	 */
	private static final ImageIcon LEFT_IMAGE_ICON = new ImageIcon(
			"./resources/icons/toolBar/left.png");
	/**
	 * ACIDE - A Configurable IDE tool bar panel image icon.
	 */
	private static final ImageIcon RIGHT_IMAGE_ICON = new ImageIcon(
			"./resources/icons/toolBar/right.png");
	/**
	 * ACIDE - A Configurable IDE tool bar panel tool bar.
	 */
	private JToolBar _toolBar;
	/**
	 * ACIDE - A Configurable IDE tool bar panel tool bar scroll pane which
	 * contains the tool bar itself.
	 */
	private JScrollPane _toolBarScrollPane;
	/**
	 * ACIDE - A Configurable IDE tool bar panel menu bar tool bar.
	 */
	private AcideMenuBarToolBar _menuBarToolBar;
	/**
	 * ACIDE - A Configurable IDE tool bar panel console panel tool bar.
	 */
	private AcideConsolePanelToolBar _consolePanelToolBar;
	/**
	 * ACIDE - A Configurable IDE tool bar panel external applications tool bar.
	 */
	private AcideExternalAppsToolBar _externalAppsToolBar;
	
	/**
	 * ACIDE - A Configurable IDE tool bar panel button left panel.
	 * 
	 * It is used to encapsulate the button into it so the programmer can define
	 * the preferred size of the button. Otherwise because of the BorderLayout
	 * of the main panel displays the button without it.
	 */
	private JPanel _leftButtonPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar panel button right panel.
	 * 
	 * It is used to encapsulate the button into it so the programmer can define
	 * the preferred size of the button. Otherwise because of the BorderLayout
	 * of the main panel displays the button without it.
	 */
	private JPanel _rightButtonPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar panel scroll left button.
	 */
	private JButton _scrollLeftButton;
	/**
	 * ACIDE - A Configurable IDE tool bar panel scroll right button.
	 */
	private JButton _scrollRightButton;
	/**
	 * ACIDE - A Configurable IDE tool bar panel point component X.
	 * 
	 * It is used for calculating the place where the scroll bar has to be
	 * settled down.
	 */
	private int _pointX;
	/**
	 * ACIDE - A Configurable IDE tool bar panel total width of the toolbar
	 * menu.
	 */
	private int _totalWidth;
	/**
	 * ACIDE - A Configurable IDE tool bar panel point where the scroll bar has
	 * to be settled down.
	 */
	private Point _point;
	/**
	 * ACIDE - A Configurable IDE tool bar panel increment used for the
	 * displacement.
	 */
	private final int INCREMENT = 20;

	/**
	 * Creates a new ACIDE - A Configurable IDE tool bar panel.
	 */
	public AcideToolBarPanel() {

		super(new BorderLayout());

		// Creates the location point
		_point = new Point(0, 0);

		// Creates the tool bar
		_toolBar = new JToolBar();

		// The tool bar is static and it can not be moved
		_toolBar.setFloatable(false);
		
		// Creates the menu bar tool bar
		_menuBarToolBar = new AcideMenuBarToolBar();
		
		// Creates the console panel tool bar
		_consolePanelToolBar = new AcideConsolePanelToolBar();
		
		// Creates the external applications tool bar
		_externalAppsToolBar = new AcideExternalAppsToolBar();
	}

	/**
	 * Creates the ACIDE - A Configurable IDE tool bar panel components.
	 */
	private void buildComponents() {

		// Creates the scroll left button
		_scrollLeftButton = new JButton(LEFT_IMAGE_ICON);

		// Sets the scroll left button preferred size
		_scrollLeftButton.setPreferredSize(new Dimension(30, 32));

		// Creates the button left panel
		_leftButtonPanel = new JPanel();

		// Adds the scroll left button to the left button panel
		_leftButtonPanel.add(_scrollLeftButton);

		// Creates the scroll right button
		_scrollRightButton = new JButton(RIGHT_IMAGE_ICON);

		// Sets the scroll left button preferred size
		_scrollRightButton.setPreferredSize(new Dimension(30, 32));

		// Creates the right button panel
		_rightButtonPanel = new JPanel();

		// Adds the scroll right button to the right button panel
		_rightButtonPanel.add(_scrollRightButton);

		// Creates the tool bar scroll pane
		_toolBarScrollPane = new JScrollPane();

		// Hides the vertical scroll bar
		_toolBarScrollPane
				.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);

		// Hides the horizontal scroll bar
		_toolBarScrollPane
				.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

		// Sets the tool bar panel as its viewport view
		_toolBarScrollPane.setViewportView(_toolBar);

		// No border
		_toolBarScrollPane.setBorder(BorderFactory.createEmptyBorder());
	}

	/**
	 * Builds the ACIDE - A Configurable IDE tool bar panel.
	 */
	public void buildAcideToolBarPanel() {

		// Removes the previous components
		_toolBar.removeAll();

		// Builds the menu bar tool bar
		addMenuBarToolBar(_menuBarToolBar.build());

		// Builds the console panel tool bar
		addConsolePanelToolBar(_consolePanelToolBar.build());

		// Builds the external applications tool bar
		addExternalAppsToolBar(_externalAppsToolBar.build());

		// Builds the tool bar panel components
		buildComponents();

		// Sets the listeners for the tool bar panel components
		setListeners();

		// Adds the components to the tool bar panel
		addComponents();
	}

	/**
	 * Adds the components to the ACIDE - A Configurable IDE tool bar panel.
	 */
	private void addComponents() {

		// Adds the left button panel to the tool bar panel
		add(_leftButtonPanel, BorderLayout.LINE_START);

		// Adds the right button panel to the tool bar panel
		add(_rightButtonPanel, BorderLayout.LINE_END);

		// Adds the tool bar to the window
		add(_toolBarScrollPane, BorderLayout.CENTER);
	}

	/**
	 * Sets the listeners for the ACIDE - A Configurable IDE tool bar panel.
	 */
	private void setListeners() {

		// Sets the tool bar scroll pane component listener so it can resizes
		// when it changes
		_toolBarScrollPane
				.addComponentListener(new ToolBarScrollPaneComponentListener());

		// Sets the scroll right button action listener
		_scrollRightButton.addActionListener(new ScrollRightButtonAction());

		// Sets the scroll left button action listener
		_scrollLeftButton.addActionListener(new ScrollLeftButtonAction());
	}

	/**
	 * Adds the ACIDE - A Configurable IDE external applications tool bar to the
	 * ACIDE - A Configurable IDE tool bar panel.
	 * 
	 * @param externalAppsToolBar
	 *            ACIDE - A Configurable IDE external applications tool bar to
	 *            add.
	 */
	private void addExternalAppsToolBar(
			AcideExternalAppsToolBar externalAppsToolBar) {

		// Adds a separator to the tool bar
		_toolBar.addSeparator();

		// Adds the buttons to the tool bar
		for (Component button : externalAppsToolBar)
			_toolBar.add(button);
	}

	/**
	 * Adds the ACIDE - A Configurable IDE console panel tool bar to the ACIDE -
	 * A Configurable IDE tool bar panel.
	 * 
	 * @param consolePanelToolBar
	 *            ACIDE - A Configurable IDE console panel tool bar to add.
	 */
	public void addConsolePanelToolBar(
			AcideConsolePanelToolBar consolePanelToolBar) {

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s130"));

		// Adds a separator to the tool bar
		_toolBar.addSeparator();

		// Adds the buttons to the tool bar
		for (Component button : consolePanelToolBar)
			_toolBar.add(button);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s125"));
	}

	/**
	 * Adds the ACIDE - A Configurable IDE menu bar tool bar to the ACIDE - A
	 * Configurable IDE tool bar panel.
	 * 
	 * @param menuBarToolBar
	 *            ACIDE - A Configurable IDE menu bar tool bar to add.
	 */
	public void addMenuBarToolBar(AcideMenuBarToolBar menuBarToolBar) {

		// Adds the buttons to the tool bar
		for (Component button : menuBarToolBar)
			_toolBar.add(button);

		// Updates the log
		AcideLog.getLog().info(
				AcideLanguageManager.getInstance().getLabels()
						.getString("s125"));
	}

	/**
	 * ACIDE - A Configurable IDE tool bar scroll pane component listener.
	 * 
	 * @version 0.8
	 * @see ComponentAdapter
	 */
	class ToolBarScrollPaneComponentListener extends ComponentAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ComponentListener#componentResized(java.awt.event
		 * .ComponentEvent)
		 */
		@Override
		public void componentResized(ComponentEvent componentEvent) {

			// Calculates the total width
			_totalWidth = _toolBar.getBounds().width;

			if (getBounds().width < _totalWidth) {

				// Sets the left button panel as visible
				_leftButtonPanel.setVisible(true);

				// Sets the right button panel as visible
				_rightButtonPanel.setVisible(true);
			} else {

				// Sets the left button panel as not visible
				_leftButtonPanel.setVisible(false);

				// Sets the right button panel as not visible
				_rightButtonPanel.setVisible(false);

				// Initializes the pointX
				_pointX = 0;
			}

			// Validates the changes
			validate();
		}
	}

	/**
	 * ACIDE - A Configurable IDE tool bar scroll left button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ScrollLeftButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// If is is below the threshold
			if (_pointX - INCREMENT >= 0)

				// Decrements normally
				_pointX -= INCREMENT;
			else
				// Then takes the minimum value
				_pointX = 0;

			// Updates the point
			_point.x = _pointX;

			// Moves to the calculated point
			_toolBarScrollPane.getViewport().setViewPosition(_point);

			// Tells to the scroll pane to move there
			_toolBarScrollPane.validate();
		}
	}

	/**
	 * ACIDE - A Configurable IDE tool bar scroll right button action listener.
	 * 
	 * @version 0.8
	 * @see ActionListener
	 */
	class ScrollRightButtonAction implements ActionListener {

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.
		 * ActionEvent)
		 */
		@Override
		public void actionPerformed(ActionEvent actionEvent) {

			// If it is below the bounds
			if (_pointX + INCREMENT < _toolBarScrollPane.getVisibleRect().width)
				
				// Increments normally
				_pointX += INCREMENT;

			// Updates the point
			_point.x = _pointX;

			// Moves to the calculated point
			_toolBarScrollPane.getViewport().setViewPosition(_point);

			// Tells to the scroll pane to move there
			_toolBarScrollPane.validate();
		}
	}

	/**
	 * 
	 * @return
	 */
	public AcideMenuBarToolBar getMenuBarToolBar() {
		return _menuBarToolBar;
	}
	
	/**
	 * 
	 * @return
	 */
	public AcideConsolePanelToolBar getConsolePanelToolBar() {
		return _consolePanelToolBar;
	}
	
	/**
	 * 
	 * @return
	 */
	public AcideExternalAppsToolBar getExternalAppToolBar() {
		return _externalAppsToolBar;
	}
}
