package operations.output;

import gui.MainWindow;
import gui.output.Output;

import java.io.*;

import properties.PropertiesManager;

/**
 * 
 */
public class ProcessThread extends Thread {

	/**
	 * 
	 */
	private static BufferedWriter _writer = null;
	/**
	 * 
	 */
	public Process _process = null;

	/**
	 * Constructor of the class.
	 */
	public ProcessThread() {

	}

	/**
	 * 
	 */
	public synchronized void run() {

		Runtime runtime = Runtime.getRuntime();

		// Use "/bin/sh" in Linux.
		String exec = null;
		String pathS = null;
		try {
			exec = PropertiesManager.getProperty("exec");
			pathS = PropertiesManager.getProperty("execPath");
		} catch (Exception e1) {
			e1.printStackTrace();
		}
		if (!exec.equals("") && !pathS.equals("") && !pathS.equals("null")
				&& !exec.equals("null")) {
			// if (!pathS.equals("null") && !exec.equals("null")) {
			try {
				File path = new File(pathS);
				_process = runtime.exec(exec, null, path);
			} catch (Exception e) {
				System.out.println("Información sobre exec incompleta");
				e.printStackTrace();
			}
			_writer = new BufferedWriter(new OutputStreamWriter(
					_process.getOutputStream()));

			ProcessInputThread inputThread = new ProcessInputThread(_writer,
					System.in);

			StreamGobbler errorGobbler = new StreamGobbler(
					_process.getErrorStream(), null, MainWindow.getInstance()
							.getOutput());

			StreamGobbler outputGobbler = new StreamGobbler(
					_process.getInputStream(), null, MainWindow.getInstance()
							.getOutput());

			errorGobbler.start();
			outputGobbler.start();
			inputThread.start();

			try {
				int exitVal;
				exitVal = _process.waitFor();
				System.out.println("ExitValue: " + exitVal);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * 
	 * @param args
	 */
	public static void main(String args[]) {
		ProcessThread pt = new ProcessThread();
		pt.start();
	}

	/**
	 * 
	 * @return
	 */
	public static BufferedWriter getWriter() {
		return _writer;
	}

	/**
	 * 
	 * @return
	 */
	public Process getProcess() {
		return _process;
	}

	/**
	 * 
	 * @param exec
	 * @param pathExec
	 * @param command
	 * @param exit
	 * @param output
	 */
	public void executeCommand(String exec, String pathExec, String command,
			String exit, Output output) {

		Runtime runtime = Runtime.getRuntime();

		Process process = null;
		String pathS = pathExec;

		try {

			File path = new File(pathS);
			process = runtime.exec(exec, null, path);
		} catch (Exception e) {
			System.out.println("Información sobre exec incompleta");
			e.printStackTrace();
		}

		BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(
				process.getOutputStream()));

		StreamGobbler errorGobbler = new StreamGobbler(
				process.getErrorStream(), null, output);

		StreamGobbler outputGobbler = new StreamGobbler(
				process.getInputStream(), null, output);

		errorGobbler.start();
		outputGobbler.start();

		try {
			writer.write(command + '\n');
			writer.flush();
			writer.write(exit + '\n');
			writer.flush();
		} catch (IOException e1) {
			e1.printStackTrace();
		}
	}
}
