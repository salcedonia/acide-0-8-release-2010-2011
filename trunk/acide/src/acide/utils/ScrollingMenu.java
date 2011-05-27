package acide.utils;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
 
public class ScrollingMenu {
    JMenu menu3;
    JPopupMenu popup;
 
    private JMenuBar getMenuBar() {
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(new JMenu("menu 1"));
        menuBar.add(getMenu2());
        menuBar.add(getMenu3());
        return menuBar;
    }
 
    private JMenu getMenu2() {
        JMenu menu = new JMenu("menu 2");
        menu.getPopupMenu().setLayout(new GridLayout(0,9));
        for(int j = 0; j < 100; j++)
            menu.add(new JMenuItem("item " + (j+1)));
        return menu;
    }
 
    private JMenu getMenu3() {
        menu3 = new JMenu("menu 3");
        popup = new JPopupMenu();
        popup.setLayout(new BorderLayout());
        popup.setPopupSize(500, 200);
        popup.setInvoker(menu3);
        menu3.addMouseListener(menuMouseListener);
        popup.addFocusListener(popupFocusListener);
        popup.addMouseListener(popupMouseListener);
        ActionListener l = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                System.out.println(e.getActionCommand());
                popup.setVisible(false);
            }
        };
        JPanel panel = new JPanel(new GridLayout(0,9));
        for(int j = 0; j < 100; j++) {
            JMenuItem item = new JMenuItem("item " + (j+1));
            item.addActionListener(l);
            panel.add(item);
        }
        JScrollPane scrollPane = new JScrollPane(panel);
        popup.add(scrollPane);
        return menu3;
    }
 
    private void showMenu() {
         Rectangle r = menu3.getBounds();
         Point p = new Point(r.x, r.y+r.height);
         SwingUtilities.convertPointToScreen(p, menu3.getParent());
         popup.setLocation(p.x, p.y);
         popup.setVisible(true);
    }
 
    private MouseListener menuMouseListener = new MouseAdapter() {
         public void mousePressed(MouseEvent e) {
             if(!popup.isVisible())
                 showMenu();
             else
                 popup.setVisible(false);
         }
 
        public void mouseEntered(MouseEvent e) {
            if(!popup.isVisible() && menu3.isSelected())
                showMenu();
        }
 
        public void mouseExited(MouseEvent e) {
            if(popup.isVisible() && !popup.contains(e.getPoint())) {
                popup.setVisible(false);
            }
        }
    };
 
    private MouseListener popupMouseListener = new MouseAdapter() {
        public void mouseExited(MouseEvent e) {
            if(!menu3.isSelected())
                 popup.setVisible(false);
        }
    };
 
    private FocusListener popupFocusListener = new FocusAdapter() {
        public void focusLost(FocusEvent e) {
            if(popup.isVisible() && !menu3.isSelected())
                popup.setVisible(false);
        }
    };
 
    public static void main(String[] args) {
        JFrame f = new JFrame();
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.setJMenuBar(new ScrollingMenu().getMenuBar());
        f.setSize(200,100);
        f.setLocation(200,200);
        f.setVisible(true);
    }
}
