package operations.output;

import gui.MainWindow;
import gui.output.Output;

import java.io.*;

import properties.PropertiesManager;

/**
 * Handles the process thread.
 * 
 * @project ACIDE - A Configurable IDE (c).
 * @version 0.8.
 */
public class ProcessThread extends Thread {

	/**
	 * Buffered writer.
	 */
	private static BufferedWriter _writer = null;
	/**
	 * Process to execute.
	 */
	public Process _process = null;

	/**
	 * Constructor of the class.
	 */
	public ProcessThread() {

	}

	/**
	 * Main method of the thread.
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
				e.printStackTrace();
			}
			_writer = new BufferedWriter(new OutputStreamWriter(
					_process.getOutputStream()));

			ProcessInputThread inputThread = new ProcessInputThread(_writer,
					System.in);

			StreamGobbler errorGobbler = new StreamGobbler(
					_process.getErrorStream(), MainWindow.getInstance()
							.getOutput());

			StreamGobbler outputGobbler = new StreamGobbler(
					_process.getInputStream(), MainWindow.getInstance()
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
	 * Execution method of the thread.
	 * 
	 * @param args Entry arguments.
	 */
	public static void main(String args[]) {
		ProcessThread pt = new ProcessThread();
		pt.start();
	}

	/**
	 * Returns the writer.
	 * 
	 * @return The writer.
	 */
	public static BufferedWriter getWriter() {
		return _writer;
	}

	/**
	 * Returns the process to execute.
	 * 
	 * @return The process to execute.
	 */
	public Process getProcess() {
		return _process;
	}

	/**
	 * Executes a command.
	 * 
	 * @param exec Executable.
	 * @param pathExec Path of the executable.
	 * @param command Command to execute.
	 * @param exit Exit command.
	 * @param output Output shell.
	 */
	public void executeCommand(String exec, String pathExec, String command,
			String exit, Output output) {

		Runtime runtime = Runtime.getRuntime();

		Process process = null;
		String pathOutput = pathExec;

		try {

			File filePath = new File(pathOutput);
			process = runtime.exec(exec, null, filePath);
		} catch (Exception e) {
			e.printStackTrace();
		}

		BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(
				process.getOutputStream()));

		StreamGobbler errorGobbler = new StreamGobbler(
				process.getErrorStream(), output);

		StreamGobbler outputGobbler = new StreamGobbler(
				process.getInputStream(), output);

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
