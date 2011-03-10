package acide.gui.menuBar.editMenu.utils;

import acide.gui.mainWindow.AcideMainWindow;

import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.event.*;
import javax.swing.undo.*;

import acide.log.AcideLog;

/**
 * ACIDE - A Configurable IDE undo manager.
 * 
 * This class will merge individual edits into a single larger edit. That is,
 * characters entered sequentially will be grouped together and undone as a
 * group. Any attribute changes will be considered as part of the group and will
 * therefore be undone when the group is undone.
 */
public class CompoundUndoManager extends UndoManager implements
		UndoableEditListener, DocumentListener {

	/**
	 * ACIDE - A Configurable IDE undo manager class serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE undo manager unique instance.
	 */
	private static CompoundUndoManager _instance;
	/**
	 * 
	 */
	private UndoManager _undoManager;
	/**
	 * 
	 */
	private CompoundEdit _compoundEdit;
	/**
	 * 
	 */
	private UndoAction _undoAction;
	/**
	 * 
	 */
	private RedoAction _redoAction;

	/*
	 * These fields are used to help determine whether the edit is an
	 * incremental edit. The offset and length should increase by 1 for each
	 * character added or decrease by 1 for each character removed.
	 */

	/**
	 * 
	 */
	private int _lastOffset;
	/**
	 * 
	 */
	private int _lastLength;

	/**
	 * Returns the ACIDE - A Configurable IDE undo manager unique instance.
	 * 
	 * @return the ACIDE - A Configurable IDE undo manager unique instance.
	 */
	public static CompoundUndoManager getInstance() {

		if (_instance == null)
			_instance = new CompoundUndoManager();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE undo manager.
	 */
	public CompoundUndoManager() {

		_undoManager = this;
		_undoAction = new UndoAction();
		_redoAction = new RedoAction();
	}

	/**
	 * Adds the new document to be checked.
	 */
	public void update() {

		// Adds the undoable edit listener to support undoable events on it
		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getStyledDocument()
				.addUndoableEditListener(this);
	}

	/**
	 * Add a DocumentLister before the undo is done so we can position the caret
	 * correctly as each edit is undone.
	 */
	public void undo() {

		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getStyledDocument()
				.addDocumentListener(this);

		super.undo();

		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getStyledDocument()
				.removeDocumentListener(this);
	}

	/**
	 * Add a DocumentLister before the redo is done so we can position the caret
	 * correctly as each edit is redone.
	 */
	public void redo() {

		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getStyledDocument()
				.addDocumentListener(this);

		super.redo();

		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getStyledDocument()
				.removeDocumentListener(this);
	}

	/**
	 * Whenever an UndoableEdit happens the edit will either be absorbed by the
	 * current compound edit or a new compound edit will be started.
	 */
	public void undoableEditHappened(UndoableEditEvent undoableEditEvent) {

		// Start a new compound edit

		if (_compoundEdit == null) {
			_compoundEdit = startCompoundEdit(undoableEditEvent.getEdit());
			return;
		}

		int offsetChange = AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getActiveTextEditionArea()
				.getCaretPosition()
				- _lastOffset;

		int lengthChange = AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getActiveTextEditionArea()
				.getDocument().getLength()
				- _lastLength;

		// Check for an attribute change
		AbstractDocument.DefaultDocumentEvent event = (AbstractDocument.DefaultDocumentEvent) undoableEditEvent
				.getEdit();

		if (event.getType().equals(DocumentEvent.EventType.CHANGE)) {
			if (offsetChange == 0) {
				_compoundEdit.addEdit(undoableEditEvent.getEdit());
				return;
			}
		}

		// Check for an incremental edit or backspace.
		// The Change in Caret position and Document length should both be
		// either 1 or -1.
		// int offsetChange = textComponent.getCaretPosition() - lastOffset;
		// int lengthChange = textComponent.getDocument().getLength() -
		// lastLength;

		if (offsetChange == lengthChange && Math.abs(offsetChange) == 1) {

			_compoundEdit.addEdit(undoableEditEvent.getEdit());

			_lastOffset = AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getActiveTextEditionArea()
					.getCaretPosition();

			_lastLength = AcideMainWindow.getInstance().getFileEditorManager()
					.getSelectedFileEditorPanel().getActiveTextEditionArea()
					.getDocument().getLength();
			return;
		}

		// Not incremental edit, end previous edit and start a new one
		_compoundEdit.end();
		_compoundEdit = startCompoundEdit(undoableEditEvent.getEdit());
	}

	/**
	 * Each CompoundEdit will store a group of related incremental edits (ie.
	 * each character typed or backspaced is an incremental edit).
	 * 
	 * @param anEdit
	 * @return
	 */
	private CompoundEdit startCompoundEdit(UndoableEdit anEdit) {

		// Track Caret and Document information of this compound edit
		_lastOffset = AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getActiveTextEditionArea()
				.getCaretPosition();

		_lastLength = AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getActiveTextEditionArea()
				.getDocument().getLength();

		// The compound edit is used to store incremental edits
		_compoundEdit = new AcideCompoundEdit();
		_compoundEdit.addEdit(anEdit);

		// The compound edit is added to the UndoManager. All incremental
		// edits stored in the compound edit will be undone/redone at once
		addEdit(_compoundEdit);

		_undoAction.updateUndoState();
		_redoAction.updateRedoState();

		return _compoundEdit;
	}

	/*
	 * 
	 * The Action to Undo changes to the Document.
	 * 
	 * The state of the Action is managed by the CompoundUndoManager
	 */
	public Action getUndoAction() {
		return _undoAction;
	}

	/*
	 * 
	 * The Action to Redo changes to the Document.
	 * 
	 * The state of the Action is managed by the CompoundUndoManager
	 */

	public Action getRedoAction() {
		return _redoAction;
	}

	/*
	 * Updates to the Document as a result of Undo/Redo will cause the caret to
	 * be repositioned.
	 * 
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#insertUpdate(javax.swing.event.
	 * DocumentEvent)
	 */
	public void insertUpdate(final DocumentEvent documentEvent) {

		SwingUtilities.invokeLater(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {

				int offset = documentEvent.getOffset()
						+ documentEvent.getLength();
				offset = Math.min(offset, AcideMainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanel()
						.getActiveTextEditionArea().getDocument().getLength());

				AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanel()
						.getActiveTextEditionArea().setCaretPosition(offset);
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event.
	 * DocumentEvent)
	 */
	public void removeUpdate(DocumentEvent documentEvent) {

		AcideMainWindow.getInstance().getFileEditorManager()
				.getSelectedFileEditorPanel().getActiveTextEditionArea()
				.setCaretPosition(documentEvent.getOffset());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event.
	 * DocumentEvent)
	 */
	public void changedUpdate(DocumentEvent documentEvent) {
	}

	/**
	 * 
	 * @author Salcedonia
	 * 
	 */
	class AcideCompoundEdit extends CompoundEdit {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/*
		 * (non-Javadoc)
		 * 
		 * @see javax.swing.undo.CompoundEdit#isInProgress()
		 */
		public boolean isInProgress() {

			// in order for the canUndo() and canRedo() methods to work
			// assume that the compound edit is never in progress

			return false;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see javax.swing.undo.CompoundEdit#undo()
		 */
		public void undo() throws CannotUndoException {

			// End the edit so future edits don't get absorbed by this edit
			if (_compoundEdit != null)
				_compoundEdit.end();

			super.undo();

			// Always start a new compound edit after an undo
			_compoundEdit = null;
		}
	}

	/**
	 * Perform the Undo and update the state of the undo/redo Actions.
	 */
	class UndoAction extends AbstractAction {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/**
		 * 
		 */
		public UndoAction() {

			putValue(Action.NAME, "Undo");
			putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_U));
			putValue(Action.ACCELERATOR_KEY,
					KeyStroke.getKeyStroke("control Z"));

			setEnabled(false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent actionEvent) {

			try {

				// Gets the selected file editor panel index
				int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanelIndex();

				// If there are opened editors
				if (selectedFileEditorPanelIndex != -1) {

					if (_undoManager.canUndo())
						_undoManager.undo();
					
					// Updates the undo menu item option
					AcideMainWindow.getInstance().getMenu().getEditMenu().getUndoMenuItem()
							.setEnabled(AcideUndoRedoManager.getInstance().canUndo());
				}
			} catch (CannotUndoException exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}

			updateUndoState();
			_redoAction.updateRedoState();
		}

		/**
		 * 
		 */
		private void updateUndoState() {
			setEnabled(_undoManager.canUndo());
		}
	}

	/*
	 * 
	 * Perform the Redo and update the state of the undo/redo Actions
	 */

	class RedoAction extends AbstractAction {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/**
		 * 
		 */
		public RedoAction() {

			putValue(Action.NAME, "Redo");
			putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
			putValue(Action.MNEMONIC_KEY, new Integer(KeyEvent.VK_R));
			putValue(Action.ACCELERATOR_KEY,
					KeyStroke.getKeyStroke(KeyEvent.VK_Y, InputEvent.CTRL_MASK));

			setEnabled(false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent
		 * )
		 */
		public void actionPerformed(ActionEvent actionEvent) {

			try {

				// Gets the selected file editor panel index
				int selectedFileEditorPanelIndex = AcideMainWindow.getInstance()
						.getFileEditorManager().getSelectedFileEditorPanelIndex();

				// If there are opened editors
				if (selectedFileEditorPanelIndex != -1) {
					
					if (_undoManager.canRedo())
						_undoManager.redo();

					// Updates the redo menu item option
					AcideMainWindow.getInstance().getMenu().getEditMenu().getUndoMenuItem()
							.setEnabled(AcideUndoRedoManager.getInstance().canRedo());
				}
			} catch (CannotRedoException exception) {

				// Updates the log
				AcideLog.getLog().error(exception.getMessage());
				exception.printStackTrace();
			}
			
			updateRedoState();
			_undoAction.updateUndoState();
		}

		/**
		 * 
		 */
		protected void updateRedoState() {
			setEnabled(_undoManager.canRedo());
		}
	}
}
