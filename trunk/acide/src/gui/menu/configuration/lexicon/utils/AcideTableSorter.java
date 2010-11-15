package gui.menu.configuration.lexicon.utils;

import java.util.*;

import javax.swing.table.TableModel;
import javax.swing.event.TableModelEvent;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.InputEvent;
import javax.swing.JTable;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumnModel;

/**
 * Table sorter for the JTable of the Lexicon GUI of the application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class AcideTableSorter extends AcideTableMap {

	/**
	 * serialVersionUID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * Index array.
	 */
	private int _indexes[];
	/**
	 * Sorting columns.
	 */
	private Vector<Integer> _sortingColumns = new Vector<Integer>();
	/**
	 * Flag that indicates if the sorting is ascending or not.
	 */
	private boolean _isAscending = true;
	/**
	 * Compares.
	 */
	private int _compares;
	/**
	 * Column.
	 */
	private int _column = -1;

	/**
	 * Constructor of the class.
	 */
	public AcideTableSorter() {

		_indexes = new int[0];
	}

	/**
	 * Constructor of the class.
	 * 
	 * @param model
	 */
	public AcideTableSorter(TableModel model) {
		setModel(model);
	}

	/**
	 * Set a new value to the model.
	 * 
	 * @param model
	 *            New value to set.
	 */
	public void setModel(TableModel model) {
		super.setModel(model);
		reallocateIndexes();
	}

	/**
	 * Compares the rows of the model by columns.
	 * 
	 * @param row1
	 *            First row.
	 * @param row2
	 *            Second row.
	 * @param column
	 *            Column.
	 * 
	 * @return The result of the compares.
	 */
	public int compareRowsByColumn(int row1, int row2, int column) {

		Class<?> type = _model.getColumnClass(column);
		TableModel data = _model;

		// CHECK FOR NULLS
		Object o1 = data.getValueAt(row1, column);
		Object o2 = data.getValueAt(row2, column);

		if (o1 == null && o2 == null) {
			return 0;
		} else if (o1 == null) {

			// DEFINE NULL LESS THAN EVERYTHING.
			return -1;
		} else if (o2 == null) {
			return 1;
		}

		if (type.getSuperclass() == java.lang.Number.class) {
			
			Number n1 = (Number) data.getValueAt(row1, column);
			double d1 = n1.doubleValue();
			Number n2 = (Number) data.getValueAt(row2, column);
			double d2 = n2.doubleValue();

			if (d1 < d2)
				return -1;
			else if (d1 > d2)
				return 1;
			else
				return 0;
		} else if (type == java.util.Date.class) {
			Date d1 = (Date) data.getValueAt(row1, column);
			long n1 = d1.getTime();
			Date d2 = (Date) data.getValueAt(row2, column);
			long n2 = d2.getTime();

			if (n1 < n2)
				return -1;
			else if (n1 > n2)
				return 1;
			else
				return 0;
		} else if (type == String.class) {
			String s1 = (String) data.getValueAt(row1, column);
			String s2 = (String) data.getValueAt(row2, column);
			int result = s1.compareTo(s2);

			if (result < 0)
				return -1;
			else if (result > 0)
				return 1;
			else
				return 0;
		} else if (type == Boolean.class) {
			Boolean bool1 = (Boolean) data.getValueAt(row1, column);
			boolean b1 = bool1.booleanValue();
			Boolean bool2 = (Boolean) data.getValueAt(row2, column);
			boolean b2 = bool2.booleanValue();

			if (b1 == b2)
				return 0;
			else if (b1) 
				// DEFINE FALSE < TRUE
				return 1;
			else
				return -1;
		} else {
			Object v1 = data.getValueAt(row1, column);
			String s1 = v1.toString();
			Object v2 = data.getValueAt(row2, column);
			String s2 = v2.toString();
			int result = s1.compareTo(s2);

			if (result < 0)
				return -1;
			else if (result > 0)
				return 1;
			else
				return 0;
		}
	}

	/**
	 * Compares two rows.
	 * 
	 * @param row1
	 *            First row.
	 * @param row2
	 *            Second row.
	 * 
	 * @return The result of the compares.
	 */
	public int compare(int row1, int row2) {

		_compares++;
		for (int level = 0; level < _sortingColumns.size(); level++) {
			Integer column = (Integer) _sortingColumns.elementAt(level);
			int result = compareRowsByColumn(row1, row2, column.intValue());
			if (result != 0)
				return _isAscending ? result : -result;
		}
		return 0;
	}

	/**
	 * Reallocate the indexes.
	 */
	public void reallocateIndexes() {

		int rowCount = _model.getRowCount();

		// SET UP A NEW ARRAY OF INDEXES WITH THE RIGHT NUMBER OF ELEMENTS FOR THE DATA MODEL
		_indexes = new int[rowCount];

		// INITIALIZE WITH THE IDENTITY MAPPING
		for (int row = 0; row < rowCount; row++)
			_indexes[row] = row;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * gui.menu.configuration.lexical.TableMap#tableChanged(javax.swing.event
	 * .TableModelEvent)
	 */
	public void tableChanged(TableModelEvent e) {

		reallocateIndexes();
		super.tableChanged(e);
	}

	/**
	 * Check the model.
	 */
	public void checkModel() {

		if (_indexes.length != _model.getRowCount()) {
			System.err.println("Sorter not informed of a change in model.");
		}
	}

	/**
	 * Sort the model.
	 * 
	 * @param sender
	 *            Sender object.
	 */
	public void sort(Object sender) {

		checkModel();
		_compares = 0;
		shuttlesort((int[]) _indexes.clone(), _indexes, 0, _indexes.length);
	}

	/**
	 * Sort n2 the model.
	 */
	public void n2sort() {
		for (int i = 0; i < getRowCount(); i++) {
			for (int j = i + 1; j < getRowCount(); j++) {
				if (compare(_indexes[i], _indexes[j]) == -1) {
					swap(i, j);
				}
			}
		}
	}

	/**
	 * Sort the model with the shuttle algorithm.
	 * 
	 * @param from
	 *            From rows.
	 * @param to
	 *            To rows.
	 * @param low
	 *            Low value.
	 * @param high
	 *            High value.
	 */
	public void shuttlesort(int from[], int to[], int low, int high) {

		if (high - low < 2) {
			return;
		}
		int middle = (low + high) / 2;
		shuttlesort(to, from, low, middle);
		shuttlesort(to, from, middle, high);

		int p = low;
		int q = middle;

		if (high - low >= 4 && compare(from[middle - 1], from[middle]) <= 0) {
			for (int i = low; i < high; i++) {
				to[i] = from[i];
			}
			return;
		}

		for (int i = low; i < high; i++) {
			if (q >= high || (p < middle && compare(from[p], from[q]) <= 0)) {
				to[i] = from[p++];
			} else {
				to[i] = from[q++];
			}
		}
	}

	/**
	 * Swap two positions.
	 * 
	 * @param i
	 *            First index.
	 * @param j
	 *            Second index to swap.
	 */
	public void swap(int i, int j) {
		int tmp = _indexes[i];
		_indexes[i] = _indexes[j];
		_indexes[j] = tmp;
	}

	/**
	 * Returns the value at the position defined as an argument of the list of
	 * indexes.
	 * 
	 * @param aRow
	 *            Row to select.
	 * @param aColumn
	 *            Column to select.
	 * 
	 * @return The value at the position defined as an argument of the list of
	 *         indexes.
	 */
	public Object getValueAt(int aRow, int aColumn) {
		checkModel();
		return _model.getValueAt(_indexes[aRow], aColumn);
	}

	/**
	 * Set a value at the position specified as a parameter.
	 * 
	 * @param aValue
	 *            Value to set.
	 * @param aRow
	 *            Row of the position to set.
	 * @param aColumn
	 *            Column of the position to set.
	 */
	public void setValueAt(Object aValue, int aRow, int aColumn) {
		checkModel();
		_model.setValueAt(aValue, _indexes[aRow], aColumn);
	}

	/**
	 * Sort the table by column.
	 * 
	 * @param column
	 *            Column to sort by.
	 */
	public void sortByColumn(int column) {
		_column = column;
		sortByColumn(column, true);
	}

	/**
	 * 
	 * @param column
	 * @param ascending
	 */
	public void sortByColumn(int column, boolean ascending) {
		_column = column;
		_isAscending = ascending;
		_sortingColumns.removeAllElements();
		_sortingColumns.addElement(new Integer(column));
		sort(this);
		super.tableChanged(new TableModelEvent(this));
	}

	/**
	 * Add the table header mouse Listeners.
	 * 
	 * @param table
	 *            Table.
	 */
	public void addTableHeaderMouseListeners(JTable table) {

		final AcideTableSorter sorter = this;
		final JTable tableView = table;
		tableView.setColumnSelectionAllowed(false);
		MouseAdapter listMouseListener = new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see
			 * java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent
			 * )
			 */
			public void mouseClicked(MouseEvent e) {
				TableColumnModel columnModel = tableView.getColumnModel();
				int viewColumn = columnModel.getColumnIndexAtX(e.getX());
				int column = tableView.convertColumnIndexToModel(viewColumn);

				if (e.getClickCount() == 1 && column != -1) {

					int shiftPressed = e.getModifiers() & InputEvent.SHIFT_MASK;
					boolean ascending = (shiftPressed == 0);
					sorter.sortByColumn(column, ascending);
				}
			}
		};

		JTableHeader tableHeader = tableView.getTableHeader();
		tableHeader.addMouseListener(listMouseListener);
	}

	/**
	 * Returns the column.
	 * 
	 * @return The column
	 */
	public int getColumn() {
		return _column;
	}

	/**
	 * Set a new value to the column.
	 * 
	 * @param column
	 *            New value to set.
	 */
	public void setColumn(int column) {
		_column = column;
	}
}
