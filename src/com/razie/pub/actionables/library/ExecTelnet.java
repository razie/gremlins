package com.razie.pub.actionables.library;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.Socket;

import com.razie.pub.actionables.ActionableSpec;
import com.razie.pub.actionables.IExecutable;
import razie.base.AttrAccess;
import razie.base.AttrAccessImpl;
import com.razie.pub.base.log.Log;
import com.razie.pub.comms.Comms;

/**
 * basic implementation for a telnet command
 * 
 * TODO solve session spanning commands problem...
 * 
 * @author razvanc
 */
public class ExecTelnet implements IExecutable {

	public static ActionableSpec spec = new ActionableSpec(new AttrAccessImpl(
			"host=localhost,port=21,cmd"), new AttrAccessImpl("result"));

	@Override
	public AttrAccess execute(AttrAccess in) {
		AttrAccess newin = new razie.WrapAttrAccess(in);
		String host = (String) newin.getAttr("host");
		String port = (String) newin.getAttr("port");
		String cmd = (String) newin.getAttr("cmd");

		String result = "";
		try {
			Socket server = new Socket(host, Integer.parseInt(port));
			server.setSoTimeout(3000);

			DataInputStream instream = new DataInputStream(server
					.getInputStream());
			PrintStream out = new PrintStream(server.getOutputStream());

			Log.logThis("TELNET <" + host + ":" + port + "> SEND CMD: " + cmd);
			out.println(cmd);
			if (newin.a("noread") == null)
				result = Comms.readStream(instream);

			Log.logThis("TELNET <" + host + ":" + port + "> REPLY: " + result);

		} catch (IOException ioe) {
			Log.logThis("TELNET <" + host + ":" + port + "> EXCEPTION: ", ioe);
			ioe.printStackTrace();
		}

		AttrAccess output = new AttrAccessImpl();
		output.setAttr("result", result);
		return output;
	}

}
