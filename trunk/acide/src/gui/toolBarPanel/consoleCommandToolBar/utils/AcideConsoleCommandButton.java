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
package gui.toolBarPanel.consoleCommandToolBar.utils;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.font.FontRenderContext;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.util.ArrayList;

import javax.swing.ImageIcon;
import javax.swing.JButton;

/**
 * ACIDE - A Configurable IDE console command button.
 * 
 * @version 0.8
 * @see ArrayList
 */
public class AcideConsoleCommandButton extends JButton {
	
    /**
	 * ACIDE - A Configurable IDE console command button class serial version UID.
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Creates a new ACIDE - A Configurable IDE console command button.
	 * 
	 * @param string message to display in the button.
	 */
	public AcideConsoleCommandButton(String string) {
        super(string);
        setBorderPainted(false);
        setContentAreaFilled(false);
        setFont(new Font("Arial", Font.BOLD, 12));
        setForeground(new Color(100,100,100));
    }
    
	/**
	 * Creates a new ACIDE - A Configurable IDE console command button.
	 * 
	 * @param imageIcon image icon to display in the button.
	 */
    public AcideConsoleCommandButton(ImageIcon imageIcon) {
        super(imageIcon);
        setBorderPainted(false);
        setContentAreaFilled(false);
        setFont(new Font("Arial", Font.BOLD, 12));
        setForeground(new Color(100,100,100));
    }

    /*
     * (non-Javadoc)
     * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
     */
    @Override
    public void paintComponent(Graphics g) {
        Graphics2D g2 = (Graphics2D) g;
        super.paintComponent(g);
        
        // Set the rendering
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        
        // Paint the filled rectangle
        g2.setPaint(new Color(235,235,255));
        g2.fill(new RoundRectangle2D.Double(1, 1, (getWidth() - 3),
                (getHeight() - 3), 12, 8));
        
        // Paint the border
        g2.setPaint(new Color(100,100,100));
        g2.setStroke(new BasicStroke(1.2f));
        g2.draw(new RoundRectangle2D.Double(1, 1, (getWidth() - 3),
                (getHeight() - 3), 12, 8));
        
        // Determine the label size so can center it 
        FontRenderContext frc = new FontRenderContext(null, false, false); 
        Rectangle2D r = getFont().getStringBounds(getText(), frc); 
        float xMargin = (float)(getWidth()-r.getWidth())/2; 
        float yMargin = (float)(getHeight()-getFont().getSize())/2 - 2; 
        
        // Draw the text in the center 
        g2.drawString(getText(),xMargin, (float)getFont().getSize()+yMargin); 
        g2.dispose();
    }
}