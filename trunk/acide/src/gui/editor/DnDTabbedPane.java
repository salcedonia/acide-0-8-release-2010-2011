package gui.editor;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.dnd.InvalidDnDOperationException;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

/**
 * 
 */
public class DnDTabbedPane extends JTabbedPane {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private static final int LINE_WIDTH = 3;
	/**
	 * 
	 */
	private static final String NAME = "test";
	/**
	 * 
	 */
	private final GhostGlassPane _glassPane = new GhostGlassPane();
	/**
	 * 
	 */
	private final Rectangle2D _lineRect = new Rectangle2D.Double();
	/**
	 * 
	 */
	private final Color _lineColor = new Color(0, 100, 255);
	/**
	 * 
	 */
	private int _dragTabIndex = -1;
	/**
	 * 
	 */
	private ImageIcon _icon;

	/**
	 * Constructor of the class.
	 */
	public DnDTabbedPane() {

		super();

		final DragSourceListener dsl = new DragSourceListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.dnd.DragSourceListener#dragEnter(java.awt.dnd.DragSourceDragEvent)
			 */
			public void dragEnter(DragSourceDragEvent e) {
				e.getDragSourceContext().setCursor(DragSource.DefaultMoveDrop);
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.dnd.DragSourceListener#dragExit(java.awt.dnd.DragSourceEvent)
			 */
			public void dragExit(DragSourceEvent e) {
				e.getDragSourceContext()
						.setCursor(DragSource.DefaultMoveNoDrop);
				_lineRect.setRect(0, 0, 0, 0);
				_glassPane.setPoint(new Point(-1000, -1000));
				_glassPane.repaint();
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.dnd.DragSourceListener#dragOver(java.awt.dnd.DragSourceDragEvent)
			 */
			public void dragOver(DragSourceDragEvent e) {

				// This method returns a Point indicating the cursor location in
				// screen coordinates at the moment
				Point tabPt = e.getLocation();
				SwingUtilities
						.convertPointFromScreen(tabPt, DnDTabbedPane.this);
				Point glassPt = e.getLocation();
				SwingUtilities.convertPointFromScreen(glassPt, _glassPane);
				int targetIdx = getTargetTabIndex(glassPt);
				if (getTabAreaBound().contains(tabPt) && targetIdx >= 0
						&& targetIdx != _dragTabIndex
						&& targetIdx != _dragTabIndex + 1) {
					e.getDragSourceContext().setCursor(
							DragSource.DefaultMoveDrop);
				} else {
					e.getDragSourceContext().setCursor(
							DragSource.DefaultMoveNoDrop);
				}
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.dnd.DragSourceListener#dragDropEnd(java.awt.dnd.DragSourceDropEvent)
			 */
			public void dragDropEnd(DragSourceDropEvent e) {
				_lineRect.setRect(0, 0, 0, 0);
				_dragTabIndex = -1;
				if (hasGhost()) {
					_glassPane.setVisible(false);
					_glassPane.setImage(null);
				}
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.dnd.DragSourceListener#dropActionChanged(java.awt.dnd.DragSourceDragEvent)
			 */
			public void dropActionChanged(DragSourceDragEvent e) {
			}
		};
		
		final Transferable t = new Transferable() {
			
			private final DataFlavor FLAVOR = new DataFlavor(
					DataFlavor.javaJVMLocalObjectMimeType, NAME);

			/*
			 * (non-Javadoc)
			 * @see java.awt.datatransfer.Transferable#getTransferData(java.awt.datatransfer.DataFlavor)
			 */
			public Object getTransferData(DataFlavor flavor) {
				return DnDTabbedPane.this;
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.datatransfer.Transferable#getTransferDataFlavors()
			 */
			public DataFlavor[] getTransferDataFlavors() {
				DataFlavor[] f = new DataFlavor[1];
				f[0] = this.FLAVOR;
				return f;
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.datatransfer.Transferable#isDataFlavorSupported(java.awt.datatransfer.DataFlavor)
			 */
			public boolean isDataFlavorSupported(DataFlavor flavor) {
				return flavor.getHumanPresentableName().equals(NAME);
			}
		};
		
		final DragGestureListener dgl = new DragGestureListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.dnd.DragGestureListener#dragGestureRecognized(java.awt.dnd.DragGestureEvent)
			 */
			public void dragGestureRecognized(DragGestureEvent e) {
				Point tabPt = e.getDragOrigin();
				_dragTabIndex = indexAtLocation(tabPt.x, tabPt.y);
				if (_dragTabIndex < 0)
					return;
				initGlassPane(e.getComponent(), e.getDragOrigin());
				try {
					e.startDrag(DragSource.DefaultMoveDrop, t, dsl);
				} catch (InvalidDnDOperationException idoe) {
					idoe.printStackTrace();
				}
			}
		};

		new DropTarget(_glassPane, DnDConstants.ACTION_COPY_OR_MOVE,
				new CDropTargetListener(), true);
		new DragSource().createDefaultDragGestureRecognizer(this,
				DnDConstants.ACTION_COPY_OR_MOVE, dgl);
	}

	/**
	 * 
	 */
	class CDropTargetListener implements DropTargetListener {

		/*
		 * (non-Javadoc)
		 * @see java.awt.dnd.DropTargetListener#dragEnter(java.awt.dnd.DropTargetDragEvent)
		 */
		public void dragEnter(DropTargetDragEvent e) {
			System.out.println(getTargetTabIndex(e.getLocation()));
			if (isDragAcceptable(e))
				e.acceptDrag(e.getDropAction());
			else
				e.rejectDrag();
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.dnd.DropTargetListener#dragExit(java.awt.dnd.DropTargetEvent)
		 */
		public void dragExit(DropTargetEvent e) {
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.dnd.DropTargetListener#dropActionChanged(java.awt.dnd.DropTargetDragEvent)
		 */
		public void dropActionChanged(DropTargetDragEvent e) {
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.dnd.DropTargetListener#dragOver(java.awt.dnd.DropTargetDragEvent)
		 */
		public void dragOver(final DropTargetDragEvent e) {
			if (getTabPlacement() == JTabbedPane.TOP
					|| getTabPlacement() == JTabbedPane.BOTTOM) {
				initTargetLeftRightLine(getTargetTabIndex(e.getLocation()));
			} else {
				initTargetTopBottomLine(getTargetTabIndex(e.getLocation()));
			}
			repaint();
			if (hasGhost()) {
				_glassPane.setPoint(e.getLocation());
				_glassPane.repaint();
			}
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.dnd.DropTargetListener#drop(java.awt.dnd.DropTargetDropEvent)
		 */
		public void drop(DropTargetDropEvent e) {
			if (isDropAcceptable(e)) {
				convertTab(_dragTabIndex, getTargetTabIndex(e.getLocation()));
				e.dropComplete(true);
			} else {
				e.dropComplete(false);
			}
			repaint();
		}

		/**
		 * 
		 * @param e
		 * @return
		 */
		public boolean isDragAcceptable(DropTargetDragEvent e) {
			Transferable t = e.getTransferable();
			if (t == null)
				return false;
			DataFlavor[] f = e.getCurrentDataFlavors();
			if (t.isDataFlavorSupported(f[0]) && _dragTabIndex >= 0) {
				return true;
			}
			return false;
		}

		/**
		 * 
		 * @param e
		 * @return
		 */
		public boolean isDropAcceptable(DropTargetDropEvent e) {
			Transferable t = e.getTransferable();
			if (t == null)
				return false;
			DataFlavor[] f = t.getTransferDataFlavors();
			if (t.isDataFlavorSupported(f[0]) && _dragTabIndex >= 0) {
				return true;
			}
			return false;
		}
	}

	/**
	 * 
	 */
	private boolean hasGhost = true;

	/**
	 * 
	 * @param flag
	 */
	public void setPaintGhost(boolean flag) {
		hasGhost = flag;
	}

	/**
	 * 
	 * @return
	 */
	public boolean hasGhost() {
		return hasGhost;
	}

	/**
	 * 
	 * @param glassPt
	 * @return
	 */
	private int getTargetTabIndex(Point glassPt) {
		Point tabPt = SwingUtilities.convertPoint(_glassPane, glassPt,
				DnDTabbedPane.this);
		boolean isTB = getTabPlacement() == JTabbedPane.TOP
				|| getTabPlacement() == JTabbedPane.BOTTOM;
		for (int i = 0; i < getTabCount(); i++) {
			Rectangle r = getBoundsAt(i);
			if (isTB)
				r.setRect(r.x - r.width / 2, r.y, r.width, r.height);
			else
				r.setRect(r.x, r.y - r.height / 2, r.width, r.height);
			if (r.contains(tabPt))
				return i;
		}
		Rectangle r = getBoundsAt(getTabCount() - 1);
		if (isTB)
			r.setRect(r.x + r.width / 2, r.y, r.width, r.height);
		else
			r.setRect(r.x, r.y + r.height / 2, r.width, r.height);
		return r.contains(tabPt) ? getTabCount() : -1;
	}

	/**
	 * 
	 * @param prev
	 * @param next
	 */
	private void convertTab(int prev, int next) {
		if (next < 0 || prev == next) {
			return;
		}
		Component cmp = getComponentAt(prev);
		String str = getTitleAt(prev);
		if (next == getTabCount()) {
			remove(prev);
			addTab(str, _icon, cmp);
			setSelectedIndex(getTabCount() - 1);
		} else if (prev > next) {
			// System.out.println(" >: press="+prev+" next="+next);
			remove(prev);
			insertTab(str, _icon, cmp, null, next);
			setSelectedIndex(next);
		} else {
			remove(prev);
			insertTab(str, _icon, cmp, null, next - 1);
			setSelectedIndex(next - 1);
		}
		cmp.repaint();
	}

	/**
	 * 
	 * @param next
	 */
	private void initTargetLeftRightLine(int next) {
		if (next < 0 || _dragTabIndex == next || next - _dragTabIndex == 1) {
			_lineRect.setRect(0, 0, 0, 0);
		} else if (next == getTabCount()) {
			Rectangle rect = getBoundsAt(getTabCount() - 1);
			_lineRect.setRect(rect.x + rect.width - LINE_WIDTH / 2, rect.y,
					LINE_WIDTH, rect.height);
		} else if (next == 0) {
			Rectangle rect = getBoundsAt(0);
			_lineRect.setRect(-LINE_WIDTH / 2, rect.y, LINE_WIDTH, rect.height);
		} else {
			Rectangle rect = getBoundsAt(next - 1);
			_lineRect.setRect(rect.x + rect.width - LINE_WIDTH / 2, rect.y,
					LINE_WIDTH, rect.height);
		}
	}

	/**
	 * 
	 * @param next
	 */
	private void initTargetTopBottomLine(int next) {
		if (next < 0 || _dragTabIndex == next || next - _dragTabIndex == 1) {
			_lineRect.setRect(0, 0, 0, 0);
		} else if (next == getTabCount()) {
			Rectangle rect = getBoundsAt(getTabCount() - 1);
			_lineRect.setRect(rect.x, rect.y + rect.height - LINE_WIDTH / 2,
					rect.width, LINE_WIDTH);
		} else if (next == 0) {
			Rectangle rect = getBoundsAt(0);
			_lineRect.setRect(rect.x, -LINE_WIDTH / 2, rect.width, LINE_WIDTH);
		} else {
			Rectangle rect = getBoundsAt(next - 1);
			_lineRect.setRect(rect.x, rect.y + rect.height - LINE_WIDTH / 2,
					rect.width, LINE_WIDTH);
		}
	}

	/**
	 * 
	 * @param c
	 * @param tabPt
	 */
	private void initGlassPane(Component c, Point tabPt) {

		getRootPane().setGlassPane(_glassPane);
		if (hasGhost()) {
			Rectangle rect = getBoundsAt(_dragTabIndex);
			BufferedImage image = new BufferedImage(c.getWidth(),
					c.getHeight(), BufferedImage.TYPE_INT_ARGB);
			Graphics g = image.getGraphics();
			c.paint(g);
			image = image.getSubimage(rect.x, rect.y, rect.width, rect.height);
			image.getSubimage(28, 4, 18, 18);
			_glassPane.setImage(image);
		}
		Point glassPt = SwingUtilities.convertPoint(c, tabPt, _glassPane);
		_glassPane.setPoint(glassPt);
		_glassPane.setVisible(true);
	}

	/**
	 * 
	 * @return
	 */
	private Rectangle getTabAreaBound() {
		Rectangle lastTab = getUI().getTabBounds(this, getTabCount() - 1);
		return new Rectangle(0, 0, getWidth(), lastTab.y + lastTab.height);
	}

	/**
	 * 
	 */
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		if (_dragTabIndex >= 0) {
			Graphics2D g2 = (Graphics2D) g;
			g2.setPaint(_lineColor);
			g2.fill(_lineRect);
			//Rectangle rect = getBoundsAt(_dragTabIndex);
			// image = image.getSubimage(rect.x, rect.y, rect.width,
			// rect.height);
			// g2.drawImage(image,rect.x,rect.y, null);
		}
	}

	/**
	 * 
	 * @param i
	 */
	public void setIcon(ImageIcon i) {
		_icon = i;
	}
}

/**
 * 
 */
class GhostGlassPane extends JPanel {
	
	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * 
	 */
	private final AlphaComposite _composite;
	/**
	 * 
	 */
	private Point _location = new Point(0, 0);
	/**
	 * 
	 */
	private BufferedImage _draggingGhost = null;

	/**
	 * Constructor of the class.
	 */
	public GhostGlassPane() {
		setOpaque(false);
		_composite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f);
	}

	/**
	 * 
	 * @param draggingGhost
	 */
	public void setImage(BufferedImage draggingGhost) {
		_draggingGhost = draggingGhost;
	}

	/**
	 * 
	 * @param location
	 */
	public void setPoint(Point location) {
		_location = location;
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
	 */
	public void paintComponent(Graphics g) {
		if (_draggingGhost == null)
			return;
		Graphics2D g2 = (Graphics2D) g;
		g2.setComposite(_composite);
		double xx = _location.getX() - (_draggingGhost.getWidth(this) / 2d);
		double yy = _location.getY() - (_draggingGhost.getHeight(this) / 2d);
		g2.drawImage(_draggingGhost, (int) xx, (int) yy, null);
	}
}