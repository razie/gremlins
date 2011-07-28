/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.StringTokenizer;

import com.razie.pub.base.log.Log;

/**
 * simple helper to execute windows commands
 * 
 * THANKS TO this article for the inspiration:
 * http://www.javaworld.com/javaworld/jw-12-2000/jw-1229-traps.html?page=1
 * 
 * see examples in SampleWinExec in test_src
 * 
 * @author razvanc
 */
public class WinExec {
	final static String QUOTES = "\'";
	final static String SPACE = " ";

	/**
	 * use this to execute a document - windows will use the proper proogram...
	 * 
	 * @param file
	 * @throws IOException
	 */
	public static void execFile(String file) throws IOException {
		// TODO linux vs windows
		execCmd("cmd.exe /C ", file);
	}

	/**
	 * this is a copy/paste of Runtime.exec(String) to build the first
	 * arguments. It also adds more explicit arguments...
	 * 
	 * @param firstLine
	 * @param rest
	 * @return an array intended to be used by Runtime.exec
	 */
	private static String[] makeArgs(String firstLine, String[] rest) {
		String[] cmdarray = null;

		// if you don't use a shell, then parse the single line
		if (firstLine.length() == 0) {
			StringTokenizer st = new StringTokenizer(rest[0]);
			int tokens = st.countTokens();
			cmdarray = new String[st.countTokens()
					+ (rest == null ? 0 : (rest.length - 1))];
			for (int i = 0; st.hasMoreTokens(); i++)
				cmdarray[i] = st.nextToken();
			for (int j = 1; j < rest.length; j++)
				cmdarray[tokens + j - 1] = rest[j];
		} else {
			StringTokenizer st = new StringTokenizer(firstLine);
			int tokens = st.countTokens();
			cmdarray = new String[st.countTokens()
					+ (rest == null ? 0 : rest.length)];
			for (int i = 0; st.hasMoreTokens(); i++)
				cmdarray[i] = st.nextToken();
			for (int j = 0; j < rest.length; j++)
				cmdarray[tokens + j] = rest[j];
		}

		return cmdarray;

	}

	/**
	 * use this to execute a program with arguments, which together form a
	 * command line. When passed to CMD, the arguments will be wrapped in quotes
	 * to preserve semantics.
	 * 
	 * Example <code>execCmd("ls", "-a", "*.*")</code>
	 * 
	 * Example <code>execCmd("ls -a *.*")</code>
	 * 
	 * Windows Example
	 * <code>execCmd("dir", "-a", "c:\\Documents and Settings\\*.*")</code>
	 * 
	 * @param program
	 *            - the program name, including path if needed
	 * @param args
	 *            - the arguments, simply are concatenated to form the command
	 *            line.
	 * @throws IOException
	 */
	public static StreamGobbler execCmd(String program, String... args)
			throws IOException {
		String[] cmdline = makeArgs(program, args);

		Runtime rt = Runtime.getRuntime();
		logger.log("EXECUTE_CMD: " + Arrays.toString(cmdline));
		Process proc = rt.exec(cmdline);
		// any error message?
		StreamGobbler errorGobbler = new StreamGobbler(proc.getErrorStream(),
				"ERROR", false);

		// any output?
		StreamGobbler outputGobbler = new StreamGobbler(proc.getInputStream(),
				"OUTPUT", false);

		// kick them off
		errorGobbler.start();
		outputGobbler.start();

		return outputGobbler;
	}

	/**
	 * use this to execute a program with arguments, which together form a
	 * command line. When passed to CMD, the arguments will be wrapped in quotes
	 * to preserve semantics.
	 * 
	 * Example <code>execCmd("ls", "-a", "*.*")</code>
	 * 
	 * Example <code>execCmd("ls -a *.*")</code>
	 * 
	 * Windows Example
	 * <code>execCmd("dir", "-a", "c:\\Documents and Settings\\*.*")</code>
	 * 
	 * TODO 3 don't return the entire string - stream and buffer on demand or
	 * something...
	 * 
	 * @param program
	 *            - the program name, including path if needed
	 * @param args
	 *            - the arguments, simply are concatenated to form the command
	 *            line.
	 * @throws IOException
	 */
	public static StringBuilder execAndWait(String program, String... args)
			throws IOException {
		StreamGobbler outputGobbler = execCmd(program, args);

		try {
			outputGobbler.join();
		} catch (InterruptedException e) {
			Log.traceThis("WARN_ while waiting to join a cmd line gobbler: ", e);
		}

		return outputGobbler.result();
	}

	/**
	 * i think i found this somewhere on the net... if you don't do this
	 * something freaky hapens at times...boo.hoo.hoo
	 */
	static class StreamGobbler extends Thread {
		InputStream is;
		OutputStream os; // optionally
		String type;
		StringBuilder acc = new StringBuilder();
		boolean accumulate = false;

		StreamGobbler(InputStream is, String type, boolean accumulate) {
			this.is = is;
			this.type = type;
			this.accumulate = accumulate;
		}

		public void run() {
			try {
				InputStreamReader isr = new InputStreamReader(is);
				BufferedReader br = new BufferedReader(isr);
				String line = null;
				while ((line = br.readLine()) != null) {
					logger.trace(2, "   " + type + ">" + line);
					if (accumulate)
						acc.append(line).append("\n");
					if (os != null)
						os.write(new StringBuilder(line).append("\n")
								.toString().getBytes());
				}
			} catch (IOException ioe) {
				logger.alarm("while gobbling...", ioe);
			} finally {
				try {
					is.close();
					if (os != null)
						os.close();
				} catch (Throwable t) {
					// ignore it
				}
			}
		}

		public StringBuilder result() {
			return acc;
		}
	}

	static Log logger = Log.factory.create("UTILS", WinExec.class.getName());
}
