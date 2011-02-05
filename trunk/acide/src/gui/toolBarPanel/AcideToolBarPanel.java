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
package gui.toolBarPanel;

import gui.toolBarPanel.consoleCommandToolBar.AcideConsoleCommandToolBar;
import gui.toolBarPanel.staticToolBar.AcideStaticToolBar;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.ResourceBundle;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;

import language.AcideLanguageManager;
import operations.log.AcideLog;
import resources.AcideResourceManager;

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
	 * ACIDE - A Configurable IDE tool bar panel button left panel.
	 * 
	 * It is used to encapsulate the button into it so the programmer can define
	 * the preferred size of the button. Otherwise because of the BorderLayout
	 * of the main panel displays the button without it.
	 */
	private JPanel _buttonLeftPanel;
	/**
	 * ACIDE - A Configurable IDE tool bar panel button right panel.
	 * 
	 * It is used to encapsulate the button into it so the programmer can define
	 * the preferred size of the button. Otherwise because of the BorderLayout
	 * of the main panel displays the button without it.
	 */
	private JPanel _buttonRightPanel;
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
	private Point point;
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

		point = new Point(0, 0);

		// TOOL BAR
		_toolBar = new JToolBar();
		_toolBar.setFloatable(false);
	}

	/**
	 * Creates the ACIDE - A Configurable IDE tool bar panel components.
	 */
	private void initComponents() {

		// SCROLL LEFT BUTTON
		_scrollLeftButton = new JButton(LEFT_IMAGE_ICON);
		_scrollLeftButton.setPreferredSize(new Dimension(30, 32));
		_scrollLeftButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
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
				point.x = _pointX;

				// Moves to the calculated point
				_toolBarScrollPane.getViewport().setViewPosition(point);

				// Tells to the scroll pane to move there
				_toolBarScrollPane.validate();
			}
		});
		_buttonLeftPanel = new JPanel();
		_buttonLeftPanel.add(_scrollLeftButton);
		add(_buttonLeftPanel, BorderLayout.LINE_START);

		// SCROLL RIGHT BUTTON
		_scrollRightButton = new JButton(RIGHT_IMAGE_ICON);
		_scrollRightButton.setPreferredSize(new Dimension(30, 32));
		_scrollRightButton.addActionListener(new ActionListener() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ActionListener#actionPerformed(java.awt.event.
			 * ActionEvent)
			 */
			@Override
			public void actionPerformed(ActionEvent actionEvent) {

				// If it is below the bounds
				if (_pointX + INCREMENT < _toolBarScrollPane.getVisibleRect().width)
					// Increments normally
					_pointX += INCREMENT;

				// Updates the point
				point.x = _pointX;

				// Moves to the calculated point
				_toolBarScrollPane.getViewport().setViewPosition(point);

				// Tells to the scroll pane to move there
				_toolBarScrollPane.validate();
			}
		});
		_buttonRightPanel = new JPanel();
		_buttonRightPanel.add(_scrollRightButton);
		add(_buttonRightPanel, BorderLayout.LINE_END);

		// TOOL BAR SCROLL PANE
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
		
		// Adds the tool bar to the window
		add(_toolBarScrollPane, BorderLayout.CENTER);
	}

	/**
	 * Builds the ACIDE - A Configurable IDE tool bar panel.
	 */
	public void buildAcideToolBarPanel() {

		// Builds the static tool bar
		addStaticToolBar(AcideStaticToolBar.getInstance().build());

		// Builds the modifiable tool bar
		addConsoleCommandToolBar(AcideConsoleCommandToolBar.getInstance().build());

		// Creates the components
		initComponents();

		// Add the component listener so it can resizes when it changes
		_toolBarScrollPane.addComponentListener(new ComponentAdapter() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.ComponentListener#componentResized(java.awt.event
			 * .ComponentEvent)
			 */
			@Override
			public void componentResized(ComponentEvent componentEvent) {

				_totalWidth = _toolBar.getBounds().width;
				if (getBounds().width < _totalWidth) {
					// added left/right buttons for side scrolling
					_buttonLeftPanel.setVisible(true);
					_buttonRightPanel.setVisible(true);
				} else {
					_buttonLeftPanel.setVisible(false);
					_buttonRightPanel.setVisible(false);
					_pointX = 0;
				}
				validate();
			}
		});
	}

	/**
	 * Adds a console command given as a parameter to the ACIDE - A Configurable
	 * IDE tool bar panel.
	 * 
	 * @param consoleCommandToolBar
	 *            new console command to add.
	 */
	public void addConsoleCommandToolBar(
			AcideConsoleCommandToolBar consoleCommandToolBar) {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Updates the log
		AcideLog.getLog().info(labels.getString("s130"));
		_toolBar.addSeparator();

		// Adds the console command buttons
		for (Component consoleCommandButton : consoleCommandToolBar)
			_toolBar.add(consoleCommandButton);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s125"));
	}

	/**
	 * Adds the static tool bar command given as a parameter to the ACIDE - A
	 * Configurable IDE tool bar panel.
	 * 
	 * @param staticToolBar
	 *            static tool bar command to add.
	 */
	public void addStaticToolBar(AcideStaticToolBar staticToolBar) {

		// Gets the language
		AcideLanguageManager language = AcideLanguageManager.getInstance();

		try {
			language.getLanguage(AcideResourceManager.getInstance().getProperty(
					"language"));
		} catch (Exception exception) {

			// Updates the log
			AcideLog.getLog().error(exception.getMessage());
			exception.printStackTrace();
		}

		// Gets the labels
		final ResourceBundle labels = language.getLabels();

		// Removes the previous components
		_toolBar.removeAll();

		// Adds the static buttons
		for (Component staticButton : staticToolBar)
			_toolBar.add(staticButton);

		// Updates the log
		AcideLog.getLog().info(labels.getString("s125"));
	}
}
