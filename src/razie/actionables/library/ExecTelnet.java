package razie.actionables.library;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.Socket;

import razie.actionables.ActionableSpec;
import razie.actionables.IExecutable;
import razie.base.ActionContext;
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
	public Object apply(ActionContext in, Object v) {
		AttrAccess newin = new razie.WrapAttrAccess((AttrAccess)in);
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

		return result;
	}

}
