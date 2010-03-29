package com.razie.pub.test

import com.razie.pub.actionables.util.WinExec

/** samples and tests for executing files - there are too many problems across platforms */
object SampleWinExec {

   def main (args:Array[String]) {
      
      simpleLinuxScript
      vlcLinux

   }
   
   def simpleLinuxScript = {
         WinExec.execCmd ("ls -la");
         WinExec.execCmd ("/bin/sh -c ls -la");
   }
   
   def simpleWinScript = {
         WinExec.execCmd ("dir C:\\My Programs");
         WinExec.execCmd ("cmd.exe /C dir C:\\My Programs");
   }

   def vlcLinux = {
//        WinExec.execCmd ("/bin/sh -c /usr/bin/vlc -f --extraintf http --http-host localhost:4448 --extraintf rc --rc-host localhost:4449  '/host/Video/Submarine_Attack[IIPC00000027].mkv'")
        WinExec.execCmd ("/bin/sh -c", "/usr/bin/vlc -f --extraintf http --http-host localhost:4448 --extraintf rc --rc-host localhost:4449  '/host/Video/Submarine_Attack[IIPC00000027].mkv'")
   }
   
}
