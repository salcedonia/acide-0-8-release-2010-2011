package acide.utils;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.concurrent.ExecutionException;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;


public class GetDataFromWeb extends JFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	JTextArea downloadedDataArea;
	
	public GetDataFromWeb() {
		setTitle("Download data from internet!");
		setSize(200, 250);
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		
		JPanel northPanel = new JPanel(new FlowLayout());
		final JTextField internetTextField = new JTextField(10);
		internetTextField.setText("http://www.google.com");
		final JButton downloadButton = new JButton("Go!");

		
		northPanel.add(internetTextField);
		northPanel.add(downloadButton);
		
		downloadedDataArea = new JTextArea();
		JScrollPane downloadDataAreaScrollPane = new JScrollPane(downloadedDataArea);
		
		setLayout(new BorderLayout());
		add(northPanel, BorderLayout.NORTH);
		add(downloadDataAreaScrollPane, BorderLayout.CENTER);
		
		downloadButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				String address = internetTextField.getText();
				if (address.length() > 0) {
					
					try {
						InternetDataTask task = new InternetDataTask(downloadedDataArea,
								address);
						task.execute();
					} catch (IOException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					
				}
			}
		});
		
		setVisible(true);
	}
	
	public static void main(String[] args) {
		
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				new GetDataFromWeb();
			}
		});
	}

}

class InternetDataTask extends SwingWorker<String, Void> {
	URL url;
	InputStream in;
	JTextArea textArea;
	String address;
	
	public InternetDataTask(JTextArea area, String address) throws IOException {
		this.textArea = area;
		this.address = address;
		url = new URL(address);
		url.openConnection();
		in = url.openStream();
	}
	
	@Override
	protected String doInBackground() throws Exception {
		StringBuilder sb = new StringBuilder();
		
		byte[] buffer = new byte[128];
		while ((in.read(buffer)) != -1) {
			for (byte b : buffer) {
//				System.out.print((char)b);
				sb.append((char)b);
			}
		}
		
		in.close();
//		textArea.append(sb + "\n");
		return sb.toString();
	}	
	
	@Override
	protected void done() {
		try {
			textArea.append(get());
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
