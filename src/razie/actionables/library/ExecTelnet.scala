/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.actionables.library;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.Socket;

import razie.actionables.{ActionableSpec}
import razie.base.ActionContext;
import razie.base.AttrAccess;
import razie.base.AttrAccessImpl;

import com.razie.pub.base.log.Log;
import com.razie.pub.comms.Comms;

/**
 * execute a telnet command
 * 
 * TODO solve session spanning commands problem...
 * 
 * @author razvanc
 */
class ExecTelnet extends razie.wf.JWFunc {

	def spec = new ActionableSpec(new AttrAccessImpl(
			"host=localhost,port=21,cmd"), new AttrAccessImpl("result"));

	override def apply(in:ActionContext , v:Any) : Any = {
		val newin = new razie.WrapAttrAccess(in.asInstanceOf[AttrAccess])
		val host = newin sa "host"
		val port = newin sa "port"
		val cmd  = newin sa "cmd"

		var result = "";
		try {
			val server = new Socket(host, Integer.parseInt(port));
			server.setSoTimeout(3000);

			val instream = new DataInputStream(server.getInputStream());
			val out = new PrintStream(server.getOutputStream());

			Log.logThis("TELNET <" + host + ":" + port + "> SEND CMD: " + cmd);
			out.println(cmd);
			if (newin.a("noread") == null)
				result = Comms.readStream(instream);

			Log.logThis("TELNET <" + host + ":" + port + "> REPLY: " + result);

		} catch {
		   case ioe:IOException =>  {
			  Log.logThis("TELNET <" + host + ":" + port + "> EXCEPTION: ", ioe);
			  ioe.printStackTrace();
		   }
		}

		result;
	}

}
