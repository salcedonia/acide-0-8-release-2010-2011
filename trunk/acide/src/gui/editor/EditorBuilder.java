package gui.editor;

import gui.MainWindow;
import gui.editor.EditorBuilder.TestPlaf.TestPlafLayout.CloseButton;
import gui.editor.utils.DragAndDropTabbedPane;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.*;
import javax.swing.tree.TreePath;

import language.Language;
import operations.factory.IOFactory;
import operations.log.Log;

import properties.PropertiesManager;

import es.explorer.ExplorerFile;
import es.text.TextFile;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.util.ArrayList;
import java.util.ResourceBundle;

/**
 * Handle the creation and destruction of the different tabs of editors of the
 * application.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class EditorBuilder {

	/**
	 * Path where all the resources of the application are.
	 */
	private static final String RESOURCE_PATH = "./resources/icons/editor/";
	/**
	 * TabbedPane for the editor.
	 */
	private DragAndDropTabbedPane _tabbedPane;
	/**
	 * TestPlaf for the editor.
	 */
	private TestPlaf _testPlaf;
	/**
	 * Array for close buttons of the tabs in the editor.
	 */
	private static ArrayList<CloseButton> _closeButtons = new ArrayList<CloseButton>();

	/**
	 * Constructor of the class.
	 */
	public EditorBuilder() {

		// GET THE LANGUAGE
		Language language = Language.getInstance();

		try {
			language.getLanguage(PropertiesManager.getProperty("language"));
		} catch (Exception ee) {
			ee.printStackTrace();
		}

		// GET THE LABELS
		final ResourceBundle labels = language.getLabels();

		try {
			EditorBuilderMouseListener mouse = new EditorBuilderMouseListener();
			_tabbedPane = new DragAndDropTabbedPane();
			_testPlaf = new TestPlaf();
			_tabbedPane.setUI(_testPlaf);
			_tabbedPane.addMouseListener(mouse);
		} catch (RuntimeException e) {
			
			// UPDATES THE LOG
			Log.getLog().info(labels.getString("s315"));
			e.printStackTrace();
		}
	}

	/**
	 * Creates an editor tab.
	 * 
	 * @param name
	 *            Name of the tab.
	 * @param tooltip
	 *            ToolTip for the tab.
	 * @param type
	 *            Type of the file.
	 */
	public void newEditor(String name, String tooltip, int type) {

		EditorPanel e = new EditorPanel();

		switch (type) {
		case 0: {
			_tabbedPane.addTab(name, null, e, tooltip);
			e.setIcon(null);
			break;
		}
		case 1: {
			_tabbedPane.addTab(name, new ImageIcon(RESOURCE_PATH + "main.PNG"),
					e, tooltip);
			e.setIcon(new ImageIcon(RESOURCE_PATH + "main.PNG"));
			break;
		}
		case 2: {
			_tabbedPane.addTab(name, new ImageIcon(RESOURCE_PATH
					+ "compilable.PNG"), e, tooltip);
			new ImageIcon(RESOURCE_PATH + "compilable.PNG");
			break;
		}
		}
		_tabbedPane.revalidate();
		_tabbedPane.repaint();
	}

	/**
	 * Close a tab in the position given as a parameter.
	 * 
	 * @param pos Tab position to close.
	 */
	public void removeTab(int pos) {
		_closeButtons.get(pos).doClick();
	}

	/**
	 * Returns the tabbedPane.
	 * 
	 * @return The tabbedPane.
	 */
	public JTabbedPane getTabbedPane() {
		return _tabbedPane;
	}

	/**
	 * Returns the editor at the position of the list given as a parameter.
	 * 
	 * @param pos
	 *            Position of the editor to get.
	 * 
	 * @return The editor at the position of the list given as a parameter.
	 */
	public EditorPanel getEditorAt(int pos) {
		if ((pos < _tabbedPane.getComponentCount()) && (pos >= 0)) {
			return (EditorPanel) _tabbedPane.getComponentAt(pos);
		} else {
			return null;
		}
	}

	/**
	 * Get the number of editors of the tabbedPane.
	 * 
	 * @return The number of editors of the tabbedPane.
	 */
	public int getNumEditors() {
		return _tabbedPane.getTabCount();
	}

	/**
	 * Creates a new tab in the tabbedPane.
	 * 
	 * @param path
	 *            path of the file to open.
	 * @param textContent
	 *            Text type.
	 * @param text
	 *            Content of the text to display.
	 * @param editable
	 *            Flag that indicates if the editor is editable or not.
	 */
	public void newTab(String path, String textContent, String text,
			boolean editable, int type) {

		// CHECK IF THE FILE IS ALREADY OPENED
		boolean found = false;
		int pos = 0;

		for (int i = 0; i < getNumEditors(); i++) {
			if (getEditorAt(i).getAbsolutePath() == textContent) {
				found = true;
				pos = i;
			}
		}

		// IF IS NOT OPENED
		if (!found) {

			File file = new File(textContent);

			// GET THE NAME
			int index = path.lastIndexOf("\\");
			if (index == -1)
				index = path.lastIndexOf("/");
			String subName = path.substring(index + 1);

			newEditor(subName, textContent, type);

			// SET IN THE LAST POSITION
			int n = getNumEditors() - 1;
			getEditorAt(n).loadText(text);
			getEditorAt(n).setEditable(editable);
			getTabbedPane().setSelectedIndex(n);
			getEditorAt(n).setAbsolutePath(textContent);
			getEditorAt(n).setLastChange(file.lastModified());
			getEditorAt(n).setLastSize(file.length());
		} else {

			// IF IT IS ALREADY OPEN SET THE FOCUS ON IT
			setSelectedEditorAt(pos);
		}
	}

	/**
	 * Set a new value to the editor at the position given as a parameter.
	 * 
	 * @param n
	 *            New value to set.
	 */
	public void setSelectedEditorAt(int n) {
		getTabbedPane().setSelectedIndex(n);
	}

	/**
	 * Get the index of the selected editor.
	 * 
	 * @return The index of the selected editor.
	 */
	public int getSelectedEditorIndex() {
		return getTabbedPane().getSelectedIndex();
	}

	/**
	 * Get the selected editor panel.
	 * 
	 * @return The selected editor panel.
	 */
	public EditorPanel getSelectedEditor() {
		return getEditorAt(getTabbedPane().getSelectedIndex());
	}

	/**
	 * Get the editor which is marked as Main File.
	 * 
	 * @return The main editor marked as Main File.
	 */
	public EditorPanel getMainEditor() {

		for (int i = 0; i < getNumEditors(); i++) {

			if (getEditorAt(i).isMainFile())
				return getEditorAt(i);
		}

		return null;
	}

	/**
	 * Handle the opened Editors.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	static class TestPlaf extends BasicTabbedPaneUI {

		/**
		 * TestPlafLayout for the tabbedPane.
		 */
		private TestPlafLayout _testPlafLayout;

		/*
		 * (non-Javadoc)
		 * 
		 * @see javax.swing.plaf.basic.BasicTabbedPaneUI#createLayoutManager()
		 */
		protected LayoutManager createLayoutManager() {

			_testPlafLayout = new TestPlafLayout();
			return _testPlafLayout;
		}

		/**
		 * Get the close button from the list at the position given as a
		 * parameter.
		 * 
		 * @param pos
		 *            Position of the button.
		 * 
		 * @return The close button from the list at the position given as a
		 *         parameter.
		 */
		public CloseButton getCloseButtonAt(int pos) {
			return _testPlafLayout.getCloseButtonAt(pos);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see javax.swing.plaf.basic.BasicTabbedPaneUI#getTabInsets(int, int)
		 */
		protected Insets getTabInsets(int tabPlacement, int tabIndex) {

			Insets defaultInsets = (Insets) super.getTabInsets(tabPlacement,
					tabIndex).clone();

			defaultInsets.right += 40;

			defaultInsets.top += 4;

			defaultInsets.bottom += 4;

			return defaultInsets;
		}

		/**
		 * Handle the layout of the tab.
		 * 
		 * @project ACIDE - A Configurable IDE (c).
		 * @version 0.8.
		 */
		class TestPlafLayout extends TabbedPaneLayout {

			/**
			 * Get the close button from the list at the position given as a
			 * parameter.
			 * 
			 * @param pos
			 *            Position of the button.
			 * 
			 * @return The close button from the list at the position given as a
			 *         parameter.
			 */
			public CloseButton getCloseButtonAt(int pos) {
				return (CloseButton) _closeButtons.get(pos);
			}

			/*
			 * (non-Javadoc)
			 * 
			 * @see java.awt.LayoutManager#layoutContainer(java.awt.Container)
			 */
			public void layoutContainer(Container parent) {

				super.layoutContainer(parent);

				while (tabPane.getTabCount() > _closeButtons.size())
					_closeButtons.add(new CloseButton(_closeButtons.size()));

				Rectangle rect = new Rectangle();
				int i;

				for (i = 0; i < tabPane.getTabCount(); i++) {

					rect = getTabBounds(i, rect);
					JButton closeButton = (JButton) _closeButtons.get(i);
					closeButton.setLocation(rect.x + rect.width - 20,
							rect.y + 5);
					closeButton.setSize(15, 15);
					tabPane.add(closeButton);
				}

				for (; i < _closeButtons.size(); i++) {
					tabPane.remove((JButton) _closeButtons.get(i));
					_closeButtons.remove(i);
				}
			}

			/**
			 * Implements used UIResource when the close button is added to the
			 * TabbedPane.
			 * 
			 * @project ACIDE - A Configurable IDE (c).
			 * @version 0.8.
			 */
			class CloseButton extends JButton implements UIResource {

				/**
				 * serialVersionUID.
				 */
				private static final long serialVersionUID = 1L;
				/**
				 * Red icon for the closing button.
				 */
				private static final String RED_ICON = "./resources/icons/editor/closeModified.png";
				/**
				 * Green icon for the closing button.
				 */
				private static final String GREEN_ICON = "./resources/icons/editor/closeNotModified.png";
				/**
				 * Path of the displayed icon.
				 */
				private String _selectedIcon = "";

				/**
				 * Constructor of the class.
				 * 
				 * @param index
				 *            Index of the editor.
				 */
				public CloseButton(int index) {

					super(new CloseButtonAction(index));
					
					// SET THE GREEN ICON
					_selectedIcon = GREEN_ICON;
					setIcon(new ImageIcon(_selectedIcon));
					
					// GET THE LANGUAGE
					Language language = Language.getInstance();

					try {
						language.getLanguage(PropertiesManager.getProperty("language"));
					} catch (Exception ee) {
						ee.printStackTrace();
					}

					// GET THE LABELS
					final ResourceBundle labels = language.getLabels();
					
					// SET THE TOOL TIP TEXT
					setToolTipText(labels.getString("s316"));
					
					setMargin(new Insets(0, 0, 0, 0));
					validate();
				}

				/**
				 * Set the color of the button to red.
				 */
				public void setRedButton() {
					_selectedIcon = RED_ICON;
					setIcon(new ImageIcon(RED_ICON));
				}

				/**
				 * Set the color of the button to green.
				 */
				public void setGreenButton() {
					_selectedIcon = GREEN_ICON;
					setIcon(new ImageIcon(GREEN_ICON));
				}

				/**
				 * Return true if the button is red and false in other case.
				 * 
				 * @return True if the button is red and false in other case.
				 */
				public boolean isRedButton() {
					return _selectedIcon.matches(RED_ICON);
				}
			}

			/**
			 * Close button action.
			 * 
			 * @project ACIDE - A Configurable IDE (c).
			 * @version 0.8.
			 */
			class CloseButtonAction extends AbstractAction {

				/**
				 * serialVersionUID
				 */
				private static final long serialVersionUID = 1L;
				/**
				 * Index of the button.
				 */
				private int _index;

				/**
				 * Constructor of the class.
				 * 
				 * @param index
				 *            Index of the button.
				 */
				public CloseButtonAction(int index) {
					super();
					_index = index;
				}

				/*
				 * (non-Javadoc)
				 * 
				 * @see
				 * java.awt.event.ActionListener#actionPerformed(java.awt.event
				 * .ActionEvent)
				 */
				public void actionPerformed(ActionEvent e) {

					// GET THE LANGUAGE
					Language language = Language.getInstance();

					try {
						language.getLanguage(PropertiesManager
								.getProperty("language"));
					} catch (Exception ee) {
						ee.printStackTrace();
					}

					// GET THE LABELS
					final ResourceBundle labels = language.getLabels();

					// IF THE FILE IS MODIFIED
					if (MainWindow.getInstance().getEditorBuilder().isRedButton(_index)) {

						int chosenOption = JOptionPane.showConfirmDialog(null,
								labels.getString("s643"));

						if (chosenOption == JOptionPane.OK_OPTION) {

							// IF NEW FILE
							if (MainWindow.getInstance().getEditorBuilder().getEditorAt(
									_index).getAbsolutePath().equals(
									labels.getString("s79"))) {

								TextFile textFile = IOFactory.getInstance()
										.buildFile();
								String filePath = " ";
								filePath = textFile.write();

								if (!filePath.equals(" ")) {

									// SAVE THE FILE
									boolean savingResult = textFile.save(
											filePath, MainWindow.getInstance()
													.getEditorBuilder()
													.getEditorAt(_index)
													.getText());

									// IF IT COULD SAVE THE FILE
									if (savingResult) {

										// SET THE GREEN BUTTON
										MainWindow.getInstance()
												.getEditorBuilder()
												.setGreenButtonAt(_index);

										// SET THE PATH
										MainWindow.getInstance().getEditorBuilder()
												.getEditorAt(_index)
												.setAbsolutePath(filePath);

										// SET THE TOOL TYPE TEXT
										MainWindow.getInstance().getEditorBuilder()
												.getEditorAt(_index)
												.setToolTipText(filePath);

										// GET THE NAME
										int index = filePath.lastIndexOf("\\");
										if (index == -1)
											index = filePath.lastIndexOf("/");
										String name = filePath.substring(
												index + 1, filePath.length());
										MainWindow.getInstance().getEditorBuilder()
												.getEditorAt(_index).setName(
														name);

										// UPDATES THE STATUS BAR
										MainWindow.getInstance().getStatusBar()
												.setMessage("");
									}
								}
							} else {

								// IS NOT THE NEW FILE
								
								TextFile textFile = IOFactory.getInstance()
										.buildFile();

								// SAVE THE FILE
								boolean savingResult = textFile.save(MainWindow.getInstance()
										.getEditorBuilder().getEditorAt(_index)
										.getAbsolutePath(), MainWindow.getInstance()
										.getEditorBuilder().getEditorAt(_index)
										.getText());

								// IF IT COULD SAVE IT
								if (savingResult)

									// UPDATES THE CLOSING BUTTON
									MainWindow.getInstance().getEditorBuilder()
											.setGreenButtonAt(_index);
							}

							// SET OPENED TO FALSE TO THE PROJECT FILE
							for (int filPos = 0; filPos < MainWindow.getInstance()
									.getProjectConfiguration()
									.getFileListSize(); filPos++) {

								if (MainWindow.getInstance().getProjectConfiguration()
										.getFileAt(filPos).getPath().equals(
												MainWindow.getInstance().getEditorBuilder()
														.getEditorAt(_index)
														.getAbsolutePath())) {
									MainWindow.getInstance().getProjectConfiguration()
											.getFileAt(filPos).setIsOpened(
													false);
								}
							}

							String project = null;
							try {
								project = PropertiesManager
										.getProperty("defaultAcideProject");
							} catch (Exception e1) {
								e1.printStackTrace();
							}

							// IF IT IS NOT THE DEFAULT PROJECT
							if (!(project
									.equals("./configuration/default.acidePrj") && MainWindow.getInstance()
									.getProjectConfiguration().getName()
									.equals("")))

								// SET THE PROJECT TO MODIFIED
								MainWindow.getInstance().getProjectConfiguration()
										.setIsModified(true);

							// REMOVE THE TAB
							MainWindow.getInstance().getEditorBuilder().getPane().remove(
									_index);

						} else if (chosenOption == JOptionPane.NO_OPTION) {
							
							// REMOVES THE TAB
							MainWindow.getInstance().getEditorBuilder().getPane().remove(
									_index);
							
							// UPDATES THE STATUS BAR
							MainWindow.getInstance().getStatusBar().setMessage("");
						}
					} else {

						// IS GREEN BUTTON
						
						// SET OPENED TO FALSE TO THE PROJECT FILE
						for (int filePos = 0; filePos < MainWindow.getInstance()
								.getProjectConfiguration().getFileListSize(); filePos++) {

							if (MainWindow.getInstance().getProjectConfiguration().getFileAt(
									filePos).getPath().equals(
											MainWindow.getInstance().getEditorBuilder().getEditorAt(
											_index).getAbsolutePath())) {
								MainWindow.getInstance().getProjectConfiguration().getFileAt(
										filePos).setIsOpened(false);
							}
						}

						String project = null;
						try {
							project = PropertiesManager
									.getProperty("defaultAcideProject");
						} catch (Exception e1) {
							e1.printStackTrace();
						}

						// IF IT IS NOT THE DEFAULT PROJECT
						if (!(project
								.equals("./configuration/default.acidePrj") && MainWindow.getInstance()
								.getProjectConfiguration().getName().equals(""))) {

							// SET THE PROJECT TO MODIFIED
							MainWindow.getInstance().getProjectConfiguration().setIsModified(
									true);
						}

						// REMOVE THE TAB
						MainWindow.getInstance().getEditorBuilder().getPane().remove(_index);

						// UPDATES THE STATUS BAR
						MainWindow.getInstance().getStatusBar().setMessage("");
					}

					// IF THERE ARE NOT MORE TABS
					if (MainWindow.getInstance().getEditorBuilder().getPane().getTabCount() == 0) {

						// DISABLE THE FILE MENU
						MainWindow.getInstance().getMenu().disableFileMenu();

						// DISABLE THE EDIT MENU
						MainWindow.getInstance().getMenu().disableEditMenu();
					}

					// EXCHANGE THE CLOSING BUTTONS
					for (int pos = _index; pos < _closeButtons.size() - 1; pos++) {
						
						CloseButton closeButton = (CloseButton) _closeButtons
								.get(pos);
						CloseButton nextCloseButton = (CloseButton) _closeButtons
								.get(pos + 1);

						// SET THE COLOR TO THE BUTTON
						if(nextCloseButton.isRedButton())
							closeButton.setRedButton();
						else
							closeButton.setGreenButton();
						
						// SET THE POSITION
						_closeButtons.set(pos, closeButton);
					}
				}
			}

			/**
			 * Get the list of buttons.
			 * 
			 * @return The list of buttons.
			 */
			public ArrayList<CloseButton> getCloseButtons() {
				return _closeButtons;
			}
		}
	}

	/**
	 * Listener of the EditorBuilder for mouse events.
	 * 
	 * @project ACIDE - A Configurable IDE (c).
	 * @version 0.8.
	 */
	class EditorBuilderMouseListener extends MouseAdapter {

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent arg0) {

			if (_tabbedPane.getComponentCount() != 0) {

				String selectedEditorPath = getSelectedEditor()
						.getAbsolutePath();

				if (selectedEditorPath != null) {

					File f = new File(selectedEditorPath);

					if ((f.lastModified() != getSelectedEditor()
							.getLastChange())
							|| (f.length() != getSelectedEditor().getLastSize())) {

						// GET THE LANGUAGE
						Language language = Language.getInstance();

						try {
							language.getLanguage(PropertiesManager
									.getProperty("language"));
						} catch (Exception e) {
							e.printStackTrace();
						}

						// GET THE LABELS
						ResourceBundle labels = language.getLabels();

						int choosenOption = JOptionPane.showConfirmDialog(null,
								labels.getString("s65"));

						if (choosenOption == 0) {

							TextFile newTextFile = new TextFile();
							getSelectedEditor().loadText(
									newTextFile.load(selectedEditorPath));
							getSelectedEditor().setLastChange(f.lastModified());
							getSelectedEditor().setLastSize(f.length());
						} else {
							getSelectedEditor().setLastChange(f.lastModified());
							getSelectedEditor().setLastSize(f.length());
						}
					}
				}
			}

			if (getSelectedEditor() != null) {

				// UPDATE THE STATUS BAR
				MainWindow.getInstance().getStatusBar().setMessage(
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().getAbsolutePath());

				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorBuilder()
											.getSelectedEditor()
											.getAbsolutePath()))

						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).isCompilableFile())

							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(i)
									.isMainFile())

								// MAIN FILE
								MainWindow.getInstance().getStatusBar()
										.setMessage(
												MainWindow.getInstance()
														.getEditorBuilder()
														.getSelectedEditor()
														.getAbsolutePath()
														+ " <MAIN>");
							else

								// COMPILABLE FILE
								MainWindow.getInstance().getStatusBar()
										.setMessage(
												MainWindow.getInstance()
														.getEditorBuilder()
														.getSelectedEditor()
														.getAbsolutePath()
														+ " <COMPILABLE>");
						else

							// UPDATES THE STATUS BAR
							MainWindow.getInstance().getStatusBar().setMessage(
									MainWindow.getInstance().getEditorBuilder()
											.getSelectedEditor()
											.getAbsolutePath());
				}

				String project = null;
				try {
					project = PropertiesManager
							.getProperty("defaultAcideProject");
				} catch (Exception e1) {
					e1.printStackTrace();
				}

				// IF IT IS THE DEFAULT CONFIGURATION
				if ((project.equals("./configuration/default.acidePrj") && MainWindow
						.getInstance().getProjectConfiguration().getName()
						.equals(""))) {

					// CHECK THE TYPE
					if (MainWindow.getInstance().getEditorBuilder()
							.getSelectedEditor().isCompilerFile())
						if (MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().isMainFile())

							// MAIN FILE
							MainWindow.getInstance().getStatusBar().setMessage(
									MainWindow.getInstance().getEditorBuilder()
											.getSelectedEditor()
											.getAbsolutePath()
											+ " <MAIN>");
						else

							// COMPILABLE FILE
							MainWindow.getInstance().getStatusBar().setMessage(
									MainWindow.getInstance().getEditorBuilder()
											.getSelectedEditor()
											.getAbsolutePath()
											+ " <COMPILABLE>");
					else
						// UPDATES THE STATUS BAR
						MainWindow.getInstance().getStatusBar().setMessage(
								MainWindow.getInstance().getEditorBuilder()
										.getSelectedEditor().getAbsolutePath());
				}
			}

			// PUT THE SELECTED FILE IN THE EDITOR IF THERE IS NOT OPENED
			// PROJECT
			String project = null;
			try {
				project = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			// IF IT IS NOT THE DEFAULT PROJECT
			if (!(project.equals("./configuration/default.acidePrj") && MainWindow
					.getInstance().getProjectConfiguration().getName().equals(
							""))) {

				// IF THERE IS AN EDITOR SELECTED
				if (getSelectedEditor() != null) {

					ExplorerFile f = new ExplorerFile();
					int y = -1;

					// SEARCH FOR THE FILE IN THE EXPLORER
					for (int j = 0; j < MainWindow.getInstance()
							.getProjectConfiguration().getNumFilesFromList(); j++) {

						// IF THE FILE BELONGS TO THE PROJECT
						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(j).getPath().equals(
										MainWindow.getInstance()
												.getEditorBuilder()
												.getSelectedEditor()
												.getAbsolutePath())) {

							f = MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(j);
							for (int m = 0; m < MainWindow.getInstance()
									.getProjectConfiguration()
									.getNumFilesFromList() + 1; m++) {

								if (MainWindow.getInstance().getExplorer()
										.getTree().getPathForRow(m)
										.getLastPathComponent().toString()
										.equals(f.getLastPathComponent())) {

									y = m;
								}
							}
						}
					}

					TreePath currentSelection = MainWindow.getInstance()
							.getExplorer().getTree().getPathForRow(y);
					MainWindow.getInstance().getExplorer().getTree()
							.setSelectionPath(currentSelection);
				}
			}

			// UPDATES THE ICONS
			if (MainWindow.getInstance().getEditorBuilder().getNumEditors() > 0) {

				int index = MainWindow.getInstance().getEditorBuilder()
						.getSelectedEditorIndex();

				ImageIcon icon = (ImageIcon) _tabbedPane.getIconAt(index);
				MainWindow.getInstance().getEditorBuilder().getPane().setIcon(
						icon);
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
		 */
		public void mousePressed(MouseEvent arg0) {

			if (MainWindow.getInstance().getEditorBuilder().getNumEditors() > 0) {

				int index = MainWindow.getInstance().getEditorBuilder()
						.getSelectedEditorIndex();
				ImageIcon icon = (ImageIcon) _tabbedPane.getIconAt(index);
				MainWindow.getInstance().getEditorBuilder().getPane().setIcon(
						icon);
			}
		}
	}

	/**
	 * Get the tabbedPane.
	 * 
	 * @return The tabbedPane.
	 */
	public DragAndDropTabbedPane getPane() {
		return _tabbedPane;
	}

	/**
	 * Get the TestPlaf of the tabbedPane.
	 * 
	 * @return The TestPlaf of the tabbedPane.
	 */
	public TestPlaf getTestPlaf() {
		return _testPlaf;
	}

	/**
	 * Put the button to green.
	 */
	public void setGreenButton() {
		MainWindow.getInstance().getEditorBuilder().getTestPlaf()
				.getCloseButtonAt(
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditorIndex()).setGreenButton();
	}

	/**
	 * Returns true if the button is red and false in the other case.
	 * 
	 * @return True if the button is red and false in the other case.
	 */
	public boolean isRedButton() {
		return MainWindow.getInstance().getEditorBuilder().getTestPlaf()
				.getCloseButtonAt(
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditorIndex()).isRedButton();
	}

	/**
	 * Set the green button of the editor at the position of the list given as a
	 * parameter.
	 * 
	 * @param position
	 *            Position of the button.
	 */
	public void setGreenButtonAt(int position) {
		MainWindow.getInstance().getEditorBuilder().getTestPlaf()
				.getCloseButtonAt(position).setGreenButton();
	}

	/**
	 * Returns true if the button is red and false in the other case.
	 * 
	 * @param position
	 *            Position of the button.
	 * 
	 * @return True if the button is red and false in the other case.
	 */
	public boolean isRedButton(int position) {
		return MainWindow.getInstance().getEditorBuilder().getTestPlaf()
				.getCloseButtonAt(position).isRedButton();
	}

	/**
	 * Set the file in the editor as compilable.
	 */
	public void setCompilableFile() {

		// IF IS NOT A COMPILABLE FILE OR MAIN AND COMPILABLE
		if (!MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
				.isCompilerFile()
				|| (MainWindow.getInstance().getEditorBuilder()
						.getSelectedEditor().isCompilerFile() && MainWindow
						.getInstance().getEditorBuilder().getSelectedEditor()
						.isMainFile())) {

			String project = null;
			try {
				project = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			// IF IT IS THE DEFAULT PROJECT
			if ((project.equals("./configuration/default.acidePrj") && MainWindow
					.getInstance().getProjectConfiguration().getName().equals(
							""))) {

				// SET THE FILE AS COMPILED
				MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
						.setCompilerFile(true);

				// IF IT IS ALREADY A MAIN FILE
				if (MainWindow.getInstance().getEditorBuilder()
						.getSelectedEditor().isMainFile())
					// IT REMOVES THE MAIN FILE PROPERTY
					MainWindow.getInstance().getEditorBuilder()
							.getSelectedEditor().setMainFile(false);

				// SEARCH THE FILE INTO THE LIST OF THE FILES WHICH BELONGS TO
				// THE PROJECT
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// IF EXISTS
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorBuilder()
											.getSelectedEditor()
											.getAbsolutePath()))
						// MARKS IT LIKE COMPILABLE
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsCompilableFile(true);

					// IF IT IS ALREADY A MAIN FILE
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).isMainFile())
						// IT REMOVES THAT MAIN PROPERTY
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsMainFile(false);
				}

				// PUT THE COMPILABLE ICON TO THE TAB
				MainWindow
						.getInstance()
						.getEditorBuilder()
						.getPane()
						.setIconAt(
								MainWindow.getInstance().getEditorBuilder()
										.getSelectedEditorIndex(),
								new ImageIcon(
										"./resources/icons/editor/compilable.PNG"));

				// UPDATES THE STATUS BAR
				MainWindow.getInstance().getStatusBar().setMessage(
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().getAbsolutePath()
								+ " <COMPILABLE>");
			} else {

				// PROJECT OPENED

				MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
						.setCompilerFile(true);

				if (MainWindow.getInstance().getEditorBuilder()
						.getSelectedEditor().isMainFile())
					MainWindow.getInstance().getEditorBuilder()
							.getSelectedEditor().setMainFile(false);

				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);

				// UPDATES THE STATUS BAR
				MainWindow.getInstance().getStatusBar().setMessage(
						MainWindow.getInstance().getEditorBuilder()
								.getSelectedEditor().getAbsolutePath()
								+ " <COMPILABLE>");

				// SEARCH THE FILE INTO THE FILE LIST ASSOCIATED TO THE PROJECT
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// IF EXISTS
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorBuilder()
											.getSelectedEditor()
											.getAbsolutePath())) {

						// IS COMPILABLE
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsCompilableFile(true);

						// IF IT IS A MAIN FILE
						if (MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).isMainFile())

							// REMOVES THAT CONDITION
							MainWindow.getInstance().getProjectConfiguration()
									.getFileAt(i).setIsMainFile(false);

						// PUT THE ICON IN THE TAB
						MainWindow
								.getInstance()
								.getEditorBuilder()
								.getPane()
								.setIconAt(
										MainWindow.getInstance()
												.getEditorBuilder()
												.getSelectedEditorIndex(),
										new ImageIcon(
												"./resources/icons/editor/compilable.PNG"));
					}
				}
			}
		}
	}

	/**
	 * Unset the file in the editor as compilable.
	 */
	public void unsetCompilableFile() {

		// IF IT IS COMPILABLE AND NOT MAIN FILE
		if (MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
				.isCompilerFile()
				&& !MainWindow.getInstance().getEditorBuilder()
						.getSelectedEditor().isMainFile()) {

			// SET COMPILER FILE TO FALSE
			MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
					.setCompilerFile(false);

			// UPDATES THE STATUS BAR
			MainWindow.getInstance().getStatusBar().setMessage(
					MainWindow.getInstance().getEditorBuilder()
							.getSelectedEditor().getAbsolutePath());

			// QUIT THE ICON FROM THE TAB
			MainWindow.getInstance().getEditorBuilder().getPane().setIconAt(
					MainWindow.getInstance().getEditorBuilder()
							.getSelectedEditorIndex(), null);

			String project = null;
			try {
				project = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			// IF THERE IS AN OPENED PROJECT
			if (!project.equals("./configuration/default.acidePrj")) {

				// MODIFIED
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);

				// SEARCH THE FILE INTO THE FILE LIST OF THE PROJECT
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorBuilder()
											.getSelectedEditor()
											.getAbsolutePath()))
						// QUIT THE COMPILABLE FILE
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsCompilableFile(false);
				}
			}
		}
	}

	/**
	 * Set a file of the editor as a main file.
	 */
	public void setMainFile() {

		// IF IT IS NOT MAIN FILE YET
		if (!MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
				.isMainFile()) {

			// REMOVE THE PREVIOUS MAIN
			for (int i = 0; i < MainWindow.getInstance().getEditorBuilder()
					.getNumEditors(); i++) {

				// FIND THE PREVIOUS MAIN
				if (MainWindow.getInstance().getEditorBuilder().getEditorAt(i)
						.isMainFile()) {

					// SET MAIN AS FALSE
					MainWindow.getInstance().getEditorBuilder().getEditorAt(i)
							.setMainFile(false);

					// SET COMPILER AS FALSE
					MainWindow.getInstance().getEditorBuilder().getEditorAt(i)
							.setCompilerFile(false);

					// UPDATES THE STATUS BAR
					MainWindow.getInstance().getStatusBar().setMessage(
							MainWindow.getInstance().getEditorBuilder()
									.getEditorAt(i).getAbsolutePath());

					// REMOVE THE ICON
					MainWindow.getInstance().getEditorBuilder().getPane()
							.setIconAt(i, null);
				}
			}

			// SET MAIN FILE AS TRUE
			MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
					.setMainFile(true);

			// SET COMPILER FILE AS TRUE
			MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
					.setCompilerFile(true);

			// UPDATES THE STATUS BAR
			MainWindow.getInstance().getStatusBar().setMessage(
					MainWindow.getInstance().getEditorBuilder()
							.getSelectedEditor().getAbsolutePath()
							+ " <MAIN>");

			String project = null;
			try {
				project = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			// NOT DEFAULT PROJECT
			if (!project.equals("./configuration/default.acidePrj")) {

				// UPDATES THE FILE IN THE PROJECT CONFIGURATION
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// IF EXISTS
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorBuilder()
											.getSelectedEditor()
											.getAbsolutePath())) {

						for (int j = 0; j < MainWindow.getInstance()
								.getProjectConfiguration().getFileListSize(); j++) {

							// IF IT IS MAIN FILE
							if (MainWindow.getInstance()
									.getProjectConfiguration().getFileAt(j)
									.isMainFile()) {

								// SET MAIN FILE TO FALSE
								MainWindow.getInstance()
										.getProjectConfiguration().getFileAt(j)
										.setIsMainFile(false);

								// SET COMPILABLE TO FALSE
								MainWindow.getInstance()
										.getProjectConfiguration().getFileAt(j)
										.setIsCompilableFile(false);

								for (int z = 0; z < MainWindow.getInstance()
										.getEditorBuilder().getNumEditors(); z++) {

									if (MainWindow
											.getInstance()
											.getEditorBuilder()
											.getEditorAt(z)
											.getAbsolutePath()
											.equals(
													MainWindow
															.getInstance()
															.getProjectConfiguration()
															.getFileAt(j)
															.getPath()))

										// REMOVE THE ICON FROM THE TAB
										MainWindow.getInstance()
												.getEditorBuilder().getPane()
												.setIconAt(z, null);
								}
							}
						}

						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsMainFile(true);
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsCompilableFile(true);
						MainWindow.getInstance().getProjectConfiguration()
								.setIsModified(true);

						// PUT THE ICON IN THE TAB
						MainWindow
								.getInstance()
								.getEditorBuilder()
								.getPane()
								.setIconAt(
										MainWindow.getInstance()
												.getEditorBuilder()
												.getSelectedEditorIndex(),
										new ImageIcon(
												"./resources/icons/editor/main.PNG"));

					}
				}
			}
		}
	}

	/**
	 * Unset the file in the editor as a main file.
	 */
	public void unsetMainFile() {

		// IF IT IS MAIN FILE
		if (MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
				.isMainFile()) {

			// SET THE MAIN FILE AS FALSE
			MainWindow.getInstance().getEditorBuilder().getSelectedEditor()
					.setMainFile(false);

			// UPDATES THE STATUS BAR
			MainWindow.getInstance().getStatusBar().setMessage(
					MainWindow.getInstance().getEditorBuilder()
							.getSelectedEditor().getAbsolutePath());
			// QUIT THE ICON FROM THE TAB
			MainWindow.getInstance().getEditorBuilder().getPane().setIconAt(
					MainWindow.getInstance().getEditorBuilder()
							.getSelectedEditorIndex(), null);

			String project = null;
			try {
				project = PropertiesManager.getProperty("defaultAcideProject");
			} catch (Exception e1) {
				e1.printStackTrace();
			}

			// IF IT IS THE DEFAULT PROJECT
			if (!project.equals("./configuration/default.acidePrj")) {

				// MODIFIED
				MainWindow.getInstance().getProjectConfiguration()
						.setIsModified(true);

				// SEARCH THE FILE INTO THE LISTS OF FILES IN THE PROJECT
				for (int i = 0; i < MainWindow.getInstance()
						.getProjectConfiguration().getNumFilesFromList(); i++) {

					// IF EXISTS
					if (MainWindow.getInstance().getProjectConfiguration()
							.getFileAt(i).getPath().equals(
									MainWindow.getInstance().getEditorBuilder()
											.getSelectedEditor()
											.getAbsolutePath()))
						// SET MAIN AS FALSE
						MainWindow.getInstance().getProjectConfiguration()
								.getFileAt(i).setIsMainFile(false);

				}
			}
		}
	}
}
