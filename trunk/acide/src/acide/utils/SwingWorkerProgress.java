package acide.utils;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;


public class SwingWorkerProgress extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	JTextArea downloadedDataArea;
	JProgressBar progressBar;
	
	public SwingWorkerProgress() {
		setTitle("Download data from internet!");
		setSize(200, 250);
		
		downloadedDataArea = new JTextArea();
		JScrollPane downloadDataAreaScrollPane = new JScrollPane(downloadedDataArea);
		
		progressBar = new JProgressBar();
		progressBar.setMinimum(0);
//		progressBar.setMaximum(100);
		
		JButton startButton = new JButton("Start");

		startButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				try {
					NumbersShow task = new NumbersShow(downloadedDataArea);
					task.addPropertyChangeListener(
						new PropertyChangeListener() {
							@Override
							public void propertyChange(PropertyChangeEvent evt) {
								if ("progress".equals(evt.getPropertyName())) {
									progressBar.setValue((Integer)evt.getNewValue());
								}
							}
						});
					task.execute();
						
									
				} catch (IOException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
			}
		});
		
		setLayout(new BorderLayout());
		add(progressBar, BorderLayout.NORTH);
		add(downloadDataAreaScrollPane, BorderLayout.CENTER);
		add(startButton, BorderLayout.SOUTH);
		setVisible(true);
	}
	
	public static void main(String[] args) {
		
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				new SwingWorkerProgress();
			}
		});
	}

}

class NumbersShow extends SwingWorker<String, String> {
	JTextArea textArea;
	final File path = new File("C:\\");
	
	
	public NumbersShow(JTextArea area) throws IOException {
		this.textArea = area;
	}
	
	@Override
	protected String doInBackground() throws Exception {
		StringBuilder sb = new StringBuilder();
		int size = path.list().length;
		int current = 0;
		
		for (String f : path.list()) {
			String temp = f;
			publish(temp);
			sb.append(temp);
			int progressAt = (int)(current / size * 100);
			setProgress(progressAt);
			current++;
			
		}

		return sb.toString();
	}	
	
	
	@Override
	protected void process(List<String> chunks) {
		for (String line : chunks) {
			textArea.append(line + "\n");
		}
		
	}
}