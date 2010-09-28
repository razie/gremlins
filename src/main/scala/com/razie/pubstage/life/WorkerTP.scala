/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package com.razie.pubstage.life;

import com.razie.pub.base.ExecutionContext;
import razie.base.ActionItem

/**
 * stupid thread pool model
 * 
 * TODO make nice, dynamic priorities and all that
 * 
 * @author razvanc99
 */
class WorkerTP(val maxThreads: Int = 5, val ec: ExecutionContext = ExecutionContext.DFLT_CTX) {
  val beings = new java.util.concurrent.LinkedBlockingQueue[Worker]()
  private var threads = Array.fill (maxThreads) { mkRunner }

  // self init

  var shutdown = false

  private def mkRunner = {
    val myThread = new MyThread();
    myThread.setName("Lifegiver" + myThread.getName());
    myThread.setDaemon(true);
    myThread.start();
    myThread
  }

  def die() {
    // TODO be more gracious: give beings a notification and maybe timeout/clean resources etc
    threads.foreach(_.stop())
  }

  def put(b: Worker) = {
    if (threads.isEmpty)
      throw new IllegalStateException("Lifegiver needs init() beforehand!");

    beings.put(b);
  }

  private class MyThread extends Thread {
    var currW: Option[Worker] = None

    override def run() {
      if (ec != null) ec.enter();

      while (!shutdown) {
        currW = None
        try {
          val b = beings.take()
          currW = Option(b)
          if (b != null)
            b.run
        } catch {
          case t: ThreadDeath => {
            // replenish the threads since I'm dying
            threads(threads.findIndexOf(_ == this)) = mkRunner
            throw t
          }
          case _ =>
        }
      }
    }
  }

  def pleaseShutdown {
    shutdown = true
    threads foreach (t => put(new Worker(razie.AI("killer"), ec) { override def process() {} }))
  }

  /** find the thread processing that worker and unsafely stop() it */
  def kill(w: Worker) {
    val i = threads.findIndexOf(tt => tt.currW.map(_ == w) getOrElse false)
    if (i > -1) {
      razie.Log(">>>>>>>>>>>>>>killing........" + threads(i))
      threads(i).stop()
    } else {
      razie.Warn ("tried to kill a worker but couldn't find thread..." + w)
    }
  }

  def sleep(ms: Int) {
    try {
      Thread.sleep(ms);
    } catch {
      case e =>
        // ignore
        e.printStackTrace();
    }
  }
}
