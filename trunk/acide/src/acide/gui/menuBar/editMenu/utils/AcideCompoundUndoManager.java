package acide.gui.menuBar.editMenu.utils;

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.event.*;
import javax.swing.undo.*;

import acide.configuration.workbench.AcideWorkbenchConfiguration;
import acide.gui.fileEditor.fileEditorPanel.fileEditorTextEditionArea.utils.AcideFileEditorStyledDocument;
import acide.gui.mainWindow.AcideMainWindow;

/**
 * <p>
 * ACIDE - A Configurable IDE compound undo manager.
 * </p>
 * <p>
 * This class will merge individual edits into a single larger edit. That is,
 * characters entered sequentially will be grouped together and undone as a
 * group. Any attribute changes will be considered as part of the group and will
 * therefore be undone when the group is undone.
 * </p>
 * 
 * @version 0.8
 * @see UndoManager
 * @see UndoableEditListener
 * @see DocumentListener
 */
public class AcideCompoundUndoManager extends UndoManager implements
		UndoableEditListener, DocumentListener {

	/**
	 * ACIDE - A Configurable IDE compound undo manager serial version UID.
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * ACIDE - A Configurable IDE compound undo manager compound edit.
	 */
	private CompoundEdit _compoundEdit;
	/**
	 * ACIDE - A Configurable IDE compound undo manager text component.
	 */
	private JTextComponent _textComponent;

	// These fields are used to help determine whether the edit is an
	// incremental edit. The offset and length should increase by 1 for
	// each character added or decrease by 1 for each character removed.

	/**
	 * ACIDE - A Configurable IDE compound undo manager last offset.
	 */
	private int _lastOffset;
	/**
	 * ACIDE - A Configurable IDE compound undo manager last length.
	 */
	private int _lastLength;

	/**
	 * ACIDE - A Configurable IDE compound undo manager unique class instance.
	 */
	private static AcideCompoundUndoManager _instance;

	/**
	 * Returns the ACIDE - A Configurable IDE compound undo manager unique class
	 * instance.
	 * 
	 * @return the ACIDE - A Configurable IDE compound undo manager unique class
	 *         instance.
	 */
	public static AcideCompoundUndoManager getInstance() {

		if (_instance == null)
			_instance = new AcideCompoundUndoManager();
		return _instance;
	}

	/**
	 * Creates a new ACIDE - A Configurable IDE compound undo manager.
	 */
	public AcideCompoundUndoManager() {

	}

	/**
	 * Sets a new value to the ACIDE - A Configurable IDE compound undo manager
	 * text component.
	 * 
	 * @param textComponent
	 */
	public void setTextComponent(JTextComponent textComponent) {

		// Stores the text component
		_textComponent = textComponent;

		// Adds the undoable edit listener
		_textComponent.getDocument().addUndoableEditListener(this);
	}

	/*
	 * Add a DocumentLister before the undo is done so we can position the Caret
	 * correctly as each edit is undone.
	 */
	@Override
	public void undo() {

		// Adds a document listener
		_textComponent.getDocument().addDocumentListener(this);

		// Performs the undo action
		super.undo();

		// Removes the document listener
		_textComponent.getDocument().removeDocumentListener(this);
	}

	/*
	 * Adds a DocumentLister before the redo is done so we can position the
	 * Caret correctly as each edit is redone.
	 */
	@Override
	public void redo() {

		// Adds a document listener
		_textComponent.getDocument().addDocumentListener(this);

		// Performs the redo action
		super.redo();

		// Removes the document listener
		_textComponent.getDocument().removeDocumentListener(this);

	}

	/*
	 * Whenever an UndoableEdit happens the edit will either be absorbed by the
	 * current compound edit or a new compound edit will be started
	 */
	public void undoableEditHappened(UndoableEditEvent e) {
		// Start a new compound edit

		if (_compoundEdit == null) {
			_compoundEdit = startCompoundEdit(e.getEdit());
			return;
		}

		// Check for an attribute change

		AbstractDocument.DefaultDocumentEvent event = (AbstractDocument.DefaultDocumentEvent) e
				.getEdit();

		if (event.getType().equals(DocumentEvent.EventType.CHANGE)) {
			_compoundEdit.addEdit(e.getEdit());
			return;
		}

		// Check for an incremental edit or backspace.
		// The Change in Caret position and Document length should both be
		// either 1 or -1.

		int offsetChange = _textComponent.getCaretPosition() - _lastOffset;
		int lengthChange = _textComponent.getDocument().getLength()
				- _lastLength;

		if (offsetChange == lengthChange && Math.abs(offsetChange) == 1) {
			_compoundEdit.addEdit(e.getEdit());
			_lastOffset = _textComponent.getCaretPosition();
			_lastLength = _textComponent.getDocument().getLength();
			return;
		}

		// Not incremental edit, end previous edit and start a new one

		_compoundEdit.end();
		_compoundEdit = startCompoundEdit(e.getEdit());
	}

	/*
	 * Each CompoundEdit will store a group of related incremental edits (ie.
	 * each character typed or backspaced is an incremental edit)
	 */
	private CompoundEdit startCompoundEdit(UndoableEdit anEdit) {
		// Track Caret and Document information of this compound edit

		_lastOffset = _textComponent.getCaretPosition();
		_lastLength = _textComponent.getDocument().getLength();

		// The compound edit is used to store incremental edits

		_compoundEdit = new AcideCompoundEdit();
		_compoundEdit.addEdit(anEdit);

		// The compound edit is added to the UndoManager. All incremental
		// edits stored in the compound edit will be undone/redone at once

		addEdit(_compoundEdit);
		return _compoundEdit;
	}

	// Implement DocumentListener
	//
	// Updates to the Document as a result of Undo/Redo will cause the
	// Caret to be repositioned

	@Override
	public void insertUpdate(final DocumentEvent documentEvent) {
		SwingUtilities.invokeLater(new Runnable() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				
				// Gets the offset
				int offset = documentEvent.getOffset() + documentEvent.getLength();
				offset = Math.min(offset, _textComponent.getDocument()
						.getLength());
				
				// Updates the caret position
				_textComponent.setCaretPosition(offset);
				
				// Dispatches the event
				dispatchEvent(documentEvent);
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#removeUpdate(javax.swing.event.
	 * DocumentEvent)
	 */
	@Override
	public void removeUpdate(DocumentEvent documentEvent) {
		
		// Updates the caret position
		_textComponent.setCaretPosition(documentEvent.getOffset());
		
		// Dispatches the event
		//dispatchEvent(documentEvent);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.swing.event.DocumentListener#changedUpdate(javax.swing.event.
	 * DocumentEvent)
	 */
	@Override
	public void changedUpdate(DocumentEvent documentEvent) {
		// Dispatches the event
		//dispatchEvent(documentEvent);
	}

	/**
	 * ACIDE - A Configurable IDE compound edit.
	 * 
	 * @version 0.8
	 * @see CompoundEdit
	 */
	class AcideCompoundEdit extends CompoundEdit {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/*
		 * (non-Javadoc)
		 * @see javax.swing.undo.CompoundEdit#isInProgress()
		 */
		@Override
		public boolean isInProgress() {
			// in order for the canUndo() and canRedo() methods to work
			// assume that the compound edit is never in progress

			return false;
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.undo.CompoundEdit#undo()
		 */
		@Override
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
	 * Updates the close button and the modification project state.
	 */
	public void dispatchEvent(DocumentEvent documentEvent) {

		// Gets the syntax document which contains
		AcideFileEditorStyledDocument document = (AcideFileEditorStyledDocument) documentEvent
				.getDocument();

		// If the workbench configuration has been loaded
		if (AcideWorkbenchConfiguration.getInstance().isWorkbenchLoaded()) {

			// Gets the file editor panel index which has to be on focus
			int indexOnFocus = AcideMainWindow
					.getInstance()
					.getFileEditorManager()
					.getFileEditorPanelAt((String) document.getProperty("name"));

			// If there is any
			if (indexOnFocus != -1) {

				if (AcideMainWindow.getInstance().getFileEditorManager()
						.getSelectedFileEditorPanelIndex() != indexOnFocus)

					// Sets the selected file editor
					AcideMainWindow.getInstance().getFileEditorManager()
							.setSelectedFileEditorPanelAt(indexOnFocus);

				// Selects the tree node
				AcideMainWindow.getInstance().getExplorerPanel()
						.selectTreeNodeFromFileEditor();

				// Gets the current content
				String fileContent = AcideMainWindow.getInstance()
						.getFileEditorManager()
						.getFileEditorPanelAt(indexOnFocus)
						.getTextEditionAreaContent();

				// If has been changes in the file
				if (!AcideMainWindow.getInstance().getFileEditorManager()
						.getFileEditorPanelAt(indexOnFocus)
						.isEqualToFileDiskCopy(fileContent)) {

					// Sets the red icon to the close button
					AcideMainWindow.getInstance().getFileEditorManager()
							.setRedButtonAt(indexOnFocus);

				} else {

					// Sets the green icon to the close button
					AcideMainWindow.getInstance().getFileEditorManager()
							.setGreenButtonAt(indexOnFocus);
				}

				// Updates the save project in the menu bar tool bar
				AcideMainWindow.getInstance().getToolBarPanel()
						.getMenuBarToolBar().updateStateOfFileButtons();
			}
		}
	}
}
